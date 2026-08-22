% ============================================================================
% CONSTRAINT STORY: imposition_pathway_kernel__endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_pathway_kernel__endogenous_climb_reading, []).

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
 *   constraint_id: imposition_pathway_kernel__endogenous_climb_reading
 *   human_readable: Endogenous Climb Reading of Commitment Displacement (Meiji Calendar/Dress Reform)
 *   domain: historical_sociology/state_formation
 *
 * SUMMARY:
 *   This story instantiates the endogenous_climb_reading of the
 *   imposition_pathway_kernel: the claim that all apparent top-down
 *   commitment displacements — including the 1872 Meiji calendar and dress
 *   reforms — are in fact compressed climbs where fringe adoption
 *   (treaty-port merchants, military modernizers) preceded and substantively
 *   completed the transition before the state decree merely ratified and
 *   accelerated it. The decree's dramatic, sudden appearance to rural and
 *   traditionalist populations is, on this reading, an artifact of their
 *   exclusion from the pre-existing fringe, not evidence that no climb
 *   occurred. The theater_ratio spike around 1872 captures the state's
 *   performative claiming of authorship over a change substantially already
 *   underway in the sectors most exposed to foreign contact.
 *
 * KEY AGENTS:
 *   - meiji_state_modernizers
 *   - treaty_port_merchant_class
 *   - military_modernization_faction
 *   - rural_agrarian_populace
 *   - traditionalist_court_officials
 *   - provincial_calendar_dependent_trades
 *   - historical_sociologists_of_meiji_reform
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_pathway_kernel__endogenous_climb_reading, 0.42).
domain_priors:suppression_score(imposition_pathway_kernel__endogenous_climb_reading, 0.55).
domain_priors:theater_ratio(imposition_pathway_kernel__endogenous_climb_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__endogenous_climb_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__endogenous_climb_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(imposition_pathway_kernel__endogenous_climb_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_pathway_kernel__endogenous_climb_reading, tangled_rope).
narrative_ontology:human_readable(imposition_pathway_kernel__endogenous_climb_reading, "Endogenous Climb Reading of Commitment Displacement (Meiji Calendar/Dress Reform)").
narrative_ontology:topic_domain(imposition_pathway_kernel__endogenous_climb_reading, "historical_sociology/state_formation").

domain_priors:requires_active_enforcement(imposition_pathway_kernel__endogenous_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_pathway_kernel__endogenous_climb_reading, '275a0d2d-d80a-41a6-a871-17fb3a6ebd05').
narrative_ontology:cs_kernel_codification('275a0d2d-d80a-41a6-a871-17fb3a6ebd05', distributed).
narrative_ontology:cs_authority_grounding('275a0d2d-d80a-41a6-a871-17fb3a6ebd05', practice).
narrative_ontology:cs_interpretation_layer_present('275a0d2d-d80a-41a6-a871-17fb3a6ebd05').
narrative_ontology:cs_reading_relation('275a0d2d-d80a-41a6-a871-17fb3a6ebd05', imposition_pathway_kernel__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('275a0d2d-d80a-41a6-a871-17fb3a6ebd05', imposition_pathway_kernel__hybrid_cascade_reading, influences).
narrative_ontology:cs_axiom('275a0d2d-d80a-41a6-a871-17fb3a6ebd05', foundational, climb_precedes_and_is_independent_of_state_action).
narrative_ontology:cs_axiom_status(climb_precedes_and_is_independent_of_state_action, holdable).
narrative_ontology:cs_axiom_grounding('275a0d2d-d80a-41a6-a871-17fb3a6ebd05', climb_precedes_and_is_independent_of_state_action, empirically_contingent).
narrative_ontology:cs_axiom('275a0d2d-d80a-41a6-a871-17fb3a6ebd05', secondary, decree_is_ratification_not_origination).
narrative_ontology:cs_axiom_status(decree_is_ratification_not_origination, holdable).
narrative_ontology:cs_axiom_grounding('275a0d2d-d80a-41a6-a871-17fb3a6ebd05', decree_is_ratification_not_origination, empirically_contingent).
narrative_ontology:cs_reference_frame('275a0d2d-d80a-41a6-a871-17fb3a6ebd05', commercial_military_practice_precedence).
narrative_ontology:cs_drift_state('275a0d2d-d80a-41a6-a871-17fb3a6ebd05', post_decree_national_ratification, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('275a0d2d-d80a-41a6-a871-17fb3a6ebd05', '').
narrative_ontology:cs_kernel_id(imposition_pathway_kernel__endogenous_climb_reading, imposition_pathway_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__endogenous_climb_reading, meiji_state_modernizers).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__endogenous_climb_reading, treaty_port_merchant_class).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__endogenous_climb_reading, military_modernization_faction).
narrative_ontology:constraint_victim(imposition_pathway_kernel__endogenous_climb_reading, rural_agrarian_populace).
narrative_ontology:constraint_victim(imposition_pathway_kernel__endogenous_climb_reading, traditionalist_court_officials).
narrative_ontology:constraint_victim(imposition_pathway_kernel__endogenous_climb_reading, provincial_calendar_dependent_trades).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issue the 1872 calendar decree and dress codes, presenting them as sudden top-down modernization. On this reading, they are ratifying and accelerating an adoption climb already underway among treaty-port merchants and reformist military units; the decree converts an existing fringe practice into the compulsory national standard and captures the legitimacy of having 'led' the change.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, meiji_state_modernizers, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(imposition_pathway_kernel__endogenous_climb_reading, meiji_state_modernizers, beneficiary).

% Adopted Western dress, timekeeping, and business calendars years before the decree because they transacted daily with foreign trading houses. Their pre-decree adoption is the invisible fringe stage this reading identifies as the true origin of the climb; the state decree formalizes practices they already profited from.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, treaty_port_merchant_class, beneficiary,
    moderate, biographical, mobile, regional).

% Adopted Western drill schedules, uniforms, and Gregorian scheduling for coordination with foreign military advisors and arms suppliers well before the national decree. Their early climb is cited as evidence that the eventual imposition compressed a process they had already substantially completed.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, military_modernization_faction, beneficiary,
    organized, generational, constrained, national).

% Had no exposure to treaty-port practice and no pre-decree fringe adoption; for them the calendar and dress changes arrived as an abrupt state mandate disrupting agricultural festival timing, tax cycles, and social ritual. They bear the enforcement cost of a climb they never participated in, which is the central strain this reading must explain rather than dismiss.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, rural_agrarian_populace, payer,
    powerless, biographical, trapped, local).

% Lost court ritual authority and calendar-keeping prerogatives when lunisolar reckoning was abolished. They resisted the change as illegitimate imposition; under this reading their resistance is read as opposition to ratifying a climb that had already bypassed them within elite modernizing circles, not opposition to a genuinely novel state act.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, traditionalist_court_officials, payer,
    powerful, biographical, constrained, national).

% Almanac printers, ritual calendar specialists, and seasonal-goods merchants whose business models depended on the old calendar. They had no fringe-adoption on-ramp and absorbed sudden demand collapse when the decree took effect; their disruption is the strongest empirical challenge to a pure endogenous-climb account.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, provincial_calendar_dependent_trades, payer,
    moderate, biographical, constrained, regional).

% Study archival records of treaty-port commercial practice, merchant diaries, and military correspondence pre-dating the 1872 decree to assess whether documented fringe adoption preceded and explains the speed of the eventual state mandate, or whether the mandate itself was the primary causal event.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, historical_sociologists_of_meiji_reform, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single shared temporal and sartorial standard enabling coordination with treaty partners, foreign creditors, and international shipping/diplomatic schedules — solving a real interoperability problem for the sectors already engaged with the outside world.
% TRANSFER_FUNCTION: Moves the cost of standardization from the early-adopting merchant and military factions, who had already absorbed it voluntarily, onto the rural and ritual-calendar-dependent populations, who bear compressed adjustment costs with no preceding on-ramp; simultaneously moves symbolic legitimacy toward the state as the apparent author of modernization.
% ABSENT_VOICES: Rural agrarian communities and calendar-trade guilds have no documented voice in the treaty-port or military circles where the climb allegedly incubated; their sudden-imposition experience is available mainly through complaint records and tax-resistance incidents, not through the merchant/military archive this reading privileges.
% DISAPPEARANCE_RATIONALE: If the 1872 decree had never been issued, treaty-port and military practice would likely have continued to diverge from rural practice indefinitely without national convergence — some structural coordination gain would be lost. But whether the broader society would have converged on Western calendar/dress on its own timeline, absent the decree, is exactly what is contested between this reading and its siblings.
% FOUNDING_PROBLEM: Japan needed a shared temporal and sartorial standard to negotiate treaties, manage foreign debt schedules, and command international respect as a non-colonized modern state; friction from mismatched calendars was already being felt and partially solved informally in treaty ports and the modernizing military before the state acted.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians studying treaty-port account books and foreign legation correspondence (outside both the Meiji state's own commemorative histories and the beneficiary merchant guilds) have documented pre-1872 Gregorian-calendar and Western-dress usage in commercial contracts, suggesting the coordination problem was already being locally resolved before the decree; no fully independent corroboration exists for the rural side's counter-claim beyond tax and ritual-calendar dispute records.
narrative_ontology:disappearance_verdict(imposition_pathway_kernel__endogenous_climb_reading, contested).
narrative_ontology:founding_problem_status(imposition_pathway_kernel__endogenous_climb_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_pathway_kernel__endogenous_climb_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(imposition_pathway_kernel__endogenous_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_pathway_kernel__endogenous_climb_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_pathway_kernel__endogenous_climb_reading_tests).
:- end_tests(imposition_pathway_kernel__endogenous_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises sharply into 1872 (0.45) as the decree converts informal treaty-port and military practice into compulsory law, then eases somewhat as the transition normalizes by 1890. Theater ratio peaks at 1872 (0.7) because the decree's public framing as sudden state-led modernization overstates the state's causal role relative to the merchant/military climb this reading holds to be the true engine — the state's ceremonial claiming of the change is largely performative relative to what had already occurred. Suppression requirement spikes at the moment of decree (0.65) because rural and traditionalist populations, having no fringe on-ramp, required active enforcement (fines, administrative compulsion) to comply, then declines as compliance becomes habitual.
 *
 * PERSPECTIVAL GAP:
 *   From the merchant/military beneficiary seats, the 1872 decree is barely a rupture — formalization of what they already did. From the rural agrarian and calendar-trade seats, it is a sudden, coercive imposition with no preceding adoption curve visible to them. The endogenous_climb_reading asserts these are the same event seen from different points on one climb curve; the engine should show a real seat divergence here, which is exactly the structural claim this reading is testing, not something to be smoothed away.
 *
 * DIRECTIONALITY LOGIC:
 *   Treaty-port merchants and the military faction are beneficiaries with low derived d — they adopted the practice under conditions of net benefit (commercial/coordination advantage) well before compulsion existed. Rural populations and calendar-trade guilds are victims with high derived d — trapped/constrained exit options and no antecedent benefit from the practice prior to compulsion. The state itself sits as agenda_setter capturing legitimacy rents from appearing to have authored the change.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (international interoperability of calendar and dress for treaty/diplomatic purposes) is functionally dead by 1890 — full national standardization is achieved — yet the state's mythologized framing of the decree as unilateral modernization persists as a legitimacy narrative long after the coordination problem it solved has been resolved, which is itself a downstream effect of the tangled-rope structure this reading identifies: genuine coordination function (real, but already substantially achieved endogenously) bundled with rent-capture (state legitimacy claiming credit for change it substantially inherited).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fringe_evidence_sufficiency,
    'Does the documented pre-1872 treaty-port and military adoption constitute a genuine ''climb'' sufficient to explain the speed and form of the 1872 decree, or is it a comparatively thin evidentiary base being asked to bear the full explanatory weight this reading assigns it?',
    'Quantitative archival analysis of the proportion of commercial contracts, military orders, and personal correspondence using Western calendar/dress conventions in the 1854-1872 window, compared against the population exposed to treaty-port/military institutions versus the national population governed by the eventual decree.',
    'If the pre-decree adoption base is narrow (a small merchant/military elite) relative to the national population instantaneously bound by the decree, the endogenous_climb_reading''s claim that the imposition was merely a ''compressed climb'' becomes strained, and the exogenous_override_reading''s account of a genuinely novel top-down act gains support for the majority of the affected population.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fringe_evidence_sufficiency, empirical, 'Whether documented fringe adoption is evidentially sufficient to support the endogenous-climb claim at national scale.').

omega_variable(
    artificial_versus_organic_fringe_origin,
    'Was the treaty-port/military fringe itself organically self-selected (supporting endogenous_climb_reading), or was it substantially created and staffed by earlier state policy decisions (opening treaty ports, establishing a conscript military), which would make it a state-manufactured fringe and support the hybrid_cascade_reading instead?',
    'Trace the institutional origin of the treaty ports and the modern military itself: were these institutions themselves products of prior state decision (making the ''fringe'' downstream of an earlier override), or did they emerge from decentralized commercial/regional initiative?',
    'If the fringe''s own existence is traceable to prior state action, the endogenous_climb_reading''s claim of a purely bottom-up originating climb collapses into the hybrid_cascade_reading''s account — the fringe would be state-manufactured, and this reading''s foundational premise (climb precedes and is independent of state action) would be undermined at its root.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(artificial_versus_organic_fringe_origin, conceptual, 'Whether the identified fringe adopters were themselves products of prior top-down state action, which would collapse the endogenous reading into the hybrid reading.').

omega_variable(
    rural_exclusion_naturalness,
    'Is the total absence of fringe-stage participation among rural and calendar-trade populations a natural feature of differential exposure to foreign contact (consistent with this reading), or does it indicate that ''climb'' is not actually the universal mechanism this reading claims, but rather one pathway among several coexisting mechanisms?',
    'Compare this case against other Meiji-era commitment displacements (land tax reform, conscription law, postal system) to see whether ALL of them show a documentable pre-decree fringe-adoption stage, or whether some show no fringe stage at all — which would falsify the kernel''s universal claim rather than just this reading''s application of it.',
    'If some Meiji-era displacements show genuinely no fringe precursor, the kernel-level claim that ALL commitment displacement occurs through fringe climb is falsified, not just weakened for this case — this bears on the kernel itself, not only this reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rural_exclusion_naturalness, empirical, 'Whether the universal claim underlying the endogenous_climb_reading survives comparison across multiple Meiji-era reforms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_pathway_kernel__endogenous_climb_reading, 1854, 1890).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t1854, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 1854, 0.2).
narrative_ontology:measurement(impo_tr_t1862, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 1862, 0.3).
narrative_ontology:measurement(impo_tr_t1868, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 1868, 0.42).
narrative_ontology:measurement(impo_tr_t1872, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 1872, 0.7).
narrative_ontology:measurement(impo_tr_t1878, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 1878, 0.62).
narrative_ontology:measurement(impo_tr_t1890, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 1890, 0.55).

% Extraction over time
narrative_ontology:measurement(impo_be_t1854, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 1854, 0.15).
narrative_ontology:measurement(impo_be_t1862, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 1862, 0.22).
narrative_ontology:measurement(impo_be_t1868, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 1868, 0.3).
narrative_ontology:measurement(impo_be_t1872, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 1872, 0.45).
narrative_ontology:measurement(impo_be_t1878, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 1878, 0.4).
narrative_ontology:measurement(impo_be_t1890, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 1890, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t1854, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 1854, 0.1).
narrative_ontology:measurement(impo_su_t1862, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 1862, 0.15).
narrative_ontology:measurement(impo_su_t1868, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 1868, 0.25).
narrative_ontology:measurement(impo_su_t1872, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 1872, 0.65).
narrative_ontology:measurement(impo_su_t1878, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 1878, 0.5).
narrative_ontology:measurement(impo_su_t1890, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 1890, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_pathway_kernel__endogenous_climb_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(imposition_pathway_kernel__endogenous_climb_reading, 0.1).
narrative_ontology:affects_constraint(imposition_pathway_kernel__endogenous_climb_reading, imposition_pathway_kernel__exogenous_override_reading).
narrative_ontology:affects_constraint(imposition_pathway_kernel__endogenous_climb_reading, imposition_pathway_kernel__hybrid_cascade_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the imposition_pathway_kernel, each authored as a separate ε-invariant story per the ε-invariance decomposition principle. endogenous_climb_reading holds ε moderate (0.42) reflecting genuine coordination benefit for early adopters plus real extraction imposed on excluded populations at the moment of ratification. The exogenous_override_reading and hybrid_cascade_reading siblings author their own independent ε values reflecting their different beneficiary/victim structures and different accounts of causal origin; none of the three should be read as measuring the 'same' constraint from different angles — they are structurally distinct claims about the mechanism of displacement, linked here for contamination-propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
