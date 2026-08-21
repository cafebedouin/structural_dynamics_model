% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__secular_democratic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_july_charter_sovereign_legitimacy__secular_democratic_reading, []).

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
 *   constraint_id: july_charter_sovereign_legitimacy__secular_democratic_reading
 *   human_readable: July Charter: Secular Democratic Mandate
 *   domain: Constitutional Law / Political Transitions / Post-Revolutionary State-Building
 *
 * SUMMARY:
 *   This constraint represents the 'secular democratic' reading of a
 *   foundational national charter, which mandates secular democratic
 *   institutions and explicitly subordinates the military to civilian
 *   authority. This reading is actively enforced by the civilian government
 *   and supported by secular political parties and international allies.
 *   However, it faces significant contestation from political Islam factions,
 *   whose participation is constrained, and from the military, which
 *   historically asserts an autonomous role as national guardian. The
 *   constraint's persistence relies on active enforcement and suppression of
 *   alternative interpretations.
 *
 * KEY AGENTS:
 *   - civilian_government: Agenda-setter/Beneficiary (institutional/constrained)
 *   - secular_political_parties: Beneficiary (organized/constrained)
 *   - political_islam_factions: Payer/Excluded (organized/identity_locked)
 *   - military_high_command: Payer/Agenda-setter (institutional/mobile)
 *   - international_democratic_allies: Observer/Beneficiary (institutional/analytical)
 *   - citizens_seeking_stability: Beneficiary/Payer (moderate/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.7).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.8).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__secular_democratic_reading, tangled_rope).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__secular_democratic_reading, "July Charter: Secular Democratic Mandate").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__secular_democratic_reading, "Constitutional Law / Political Transitions / Post-Revolutionary State-Building").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__secular_democratic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__secular_democratic_reading, 'ef5492f2-3d2b-4354-a730-1da353a2e12d').
narrative_ontology:cs_kernel_codification('ef5492f2-3d2b-4354-a730-1da353a2e12d', fixed_text).
narrative_ontology:cs_authority_grounding('ef5492f2-3d2b-4354-a730-1da353a2e12d', lineage).
narrative_ontology:cs_interpretation_layer_present('ef5492f2-3d2b-4354-a730-1da353a2e12d').
narrative_ontology:cs_reading_relation('ef5492f2-3d2b-4354-a730-1da353a2e12d', july_charter_sovereign_legitimacy__guided_nationalism_reading, forecloses).
narrative_ontology:cs_reading_relation('ef5492f2-3d2b-4354-a730-1da353a2e12d', july_charter_sovereign_legitimacy__military_custodian_reading, forecloses).
narrative_ontology:cs_axiom('ef5492f2-3d2b-4354-a730-1da353a2e12d', foundational, popular_sovereignty_secular_state).
narrative_ontology:cs_axiom_status(popular_sovereignty_secular_state, holdable).
narrative_ontology:cs_axiom_grounding('ef5492f2-3d2b-4354-a730-1da353a2e12d', popular_sovereignty_secular_state, deontological).
narrative_ontology:cs_axiom('ef5492f2-3d2b-4354-a730-1da353a2e12d', foundational, military_subordination_to_civilian_rule).
narrative_ontology:cs_axiom_status(military_subordination_to_civilian_rule, holdable).
narrative_ontology:cs_axiom_grounding('ef5492f2-3d2b-4354-a730-1da353a2e12d', military_subordination_to_civilian_rule, conventional).
narrative_ontology:cs_reference_frame('ef5492f2-3d2b-4354-a730-1da353a2e12d', founding_democratic_consensus).
narrative_ontology:cs_drift_state('ef5492f2-3d2b-4354-a730-1da353a2e12d', post_coup_attempt_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ef5492f2-3d2b-4354-a730-1da353a2e12d', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, civilian_government).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_political_parties).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, international_democratic_allies).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, political_islam_factions).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, military_high_command).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, citizens_seeking_stability).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, citizens_seeking_stability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seeks to implement the charter's secular democratic principles, but faces constant pressure from military and religious factions. Benefits from the charter's legitimacy but must actively defend it against internal and external challenges.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, civilian_government, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__secular_democratic_reading, civilian_government, beneficiary).

% Advocates for the full implementation of the charter's secular and democratic clauses. Benefits from the charter's framework as it legitimizes their political platform, but is often outmaneuvered or suppressed by other powerful actors.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_political_parties, beneficiary,
    organized, biographical, constrained, national).

% Their political participation and religious-based governance proposals are constrained or excluded by the charter's secular mandate. They bear the cost of this exclusion and actively resist it, viewing the charter as an illegitimate imposition.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, political_islam_factions, payer,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__secular_democratic_reading, political_islam_factions, excluded).

% Nominally subordinate to civilian authority by the charter, but historically holds significant power and often acts autonomously. Bears the cost of formal subordination but frequently challenges it, viewing itself as the ultimate guarantor of national stability and often intervening in politics.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, military_high_command, payer,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__secular_democratic_reading, military_high_command, agenda_setter).

% Supports the secular democratic reading of the charter, providing diplomatic and financial aid to the civilian government. Benefits from the perceived stability and alignment with democratic norms, but their influence is limited by national sovereignty.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, international_democratic_allies, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__secular_democratic_reading, international_democratic_allies, beneficiary).

% Desires a stable, predictable political environment. Benefits from the promise of democratic institutions but often pays the cost of political instability, economic disruption, and the suppression of dissenting voices.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, citizens_seeking_stability, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__secular_democratic_reading, citizens_seeking_stability, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(july_charter_sovereign_legitimacy__secular_democratic_reading, civilian_government).
narrative_ontology:fixing_cost_class(july_charter_sovereign_legitimacy__secular_democratic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a framework for a stable, secular democratic state by defining the roles of civilian government and subordinating the military, thereby coordinating the transition from a previous authoritarian regime.
% TRANSFER_FUNCTION: Transfers political authority and legitimacy from military or religious institutions to civilian, secular democratic ones. It also transfers the burden of political exclusion and suppression onto political Islam factions and the military's autonomous authority.
% ABSENT_VOICES: The voices of those who advocate for an Islamic state or for the military as the ultimate arbiter of national destiny are actively suppressed or excluded from the formal political process, despite representing significant segments of the population.
% DISAPPEARANCE_RATIONALE: If the charter's secular democratic mandate vanished, the country would likely descend into severe political instability, with military and religious factions immediately vying for supreme power, leading to potential civil conflict and a complete reorganization of the state's foundational principles.
% FOUNDING_PROBLEM: The problem of transitioning from a period of authoritarian rule and political instability to a stable, legitimate, and internationally recognized democratic system, while preventing the resurgence of non-democratic forces.
% FOUNDING_PROBLEM_CORROBORATION: The civilian government and secular parties attest that the problem of establishing stable democracy is still live, citing ongoing threats from military and religious factions. However, political Islam factions and some military figures argue the problem is either misidentified (should be an Islamic state) or that the charter itself is the source of instability, not its solution. International observers generally corroborate the civilian government's framing of the problem.
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__secular_democratic_reading, world_rearranges).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__secular_democratic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__secular_democratic_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__secular_democratic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(july_charter_sovereign_legitimacy__secular_democratic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__secular_democratic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(july_charter_sovereign_legitimacy__secular_democratic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.7) reflects the significant political and social costs imposed on factions whose visions for the state (Islamic or military-led) are excluded by this reading. Suppression (0.8) is high due to the active measures required to maintain the secular democratic framework against powerful internal opposition, including legal restrictions on political parties and occasional military interventions to 'restore order' under a different interpretation. Theater ratio (0.4) indicates that while genuine efforts are made towards democratic governance, a substantial portion of the state's activity is performative, aimed at legitimizing the secular democratic facade while managing underlying power struggles. The fluctuations in metrics reflect periods of democratic consolidation followed by military interventions or increased religious political activity.
 *
 * PERSPECTIVAL GAP:
 *   The civilian government and secular parties perceive this charter reading as a legitimate and necessary framework for national progress, viewing any extraction as a justified cost of establishing democracy. Conversely, political Islam factions and the military high command experience it as an illegitimate imposition that extracts their rightful authority or political space. International observers largely align with the secular democratic reading, but often underestimate the internal resistance and the coercive force required to maintain it.
 *
 * DIRECTIONALITY LOGIC:
 *   The civilian government and secular political parties are clear beneficiaries, as the charter legitimizes their power and policy agenda. International democratic allies also benefit from the alignment with their norms. Political Islam factions are targets, bearing the cost of exclusion and suppression. The military high command is also a target in terms of its formal subordination, but its historical power allows it to act as a counter-agenda setter, making its directionality complex. Citizens seeking stability are diffuse beneficiaries of the promise of order, but also payers through the instability and suppression inherent in enforcing this contested reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling this as a pure Rope (which would ignore the significant extraction and suppression) or a pure Snare (which would ignore the genuine, albeit contested, coordination function of establishing a democratic framework). The charter's mandate to establish secular democracy is still 'contested' rather than 'dead', indicating that the founding problem is still perceived as live by its proponents, even as its implementation involves substantial extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    charter_interpretive_ambiguity,
    'Is the charter''s language genuinely unambiguous in mandating secular democratic institutions and military subordination, or does it contain ambiguities that allow for Islamic-nationalist or military-custodian interpretations?',
    'Detailed textual analysis by independent constitutional scholars, combined with historical legislative intent documents and judicial interpretations over time.',
    'If ambiguous, the ''secular democratic'' reading relies more heavily on active enforcement and suppression rather than inherent textual authority, potentially reclassifying it closer to a Snare. If unambiguous, the resistance to this reading is a direct challenge to the charter''s clear intent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(charter_interpretive_ambiguity, conceptual, 'Ambiguity in the charter''s foundational text regarding state identity and military role.').

omega_variable(
    military_subordination_enforceability,
    'Can civilian authority genuinely enforce military subordination as mandated by the charter, or is the military''s power structural and beyond effective civilian control, making the subordination largely performative?',
    'Empirical observation of military budget control, appointment/dismissal of high command, and non-intervention in political crises over a sustained period (e.g., 10+ years) without external pressure.',
    'If subordination is largely performative, the ''military_high_command'' stakeholder''s ''payer'' role is significantly reduced, and the constraint''s ''suppression'' and ''theater_ratio'' metrics would need upward revision, potentially shifting the classification towards a Snare for civilian actors.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(military_subordination_enforceability, empirical, 'The true extent of civilian control over the military, despite charter mandates.').

omega_variable(
    legitimacy_of_exclusion,
    'Is the exclusion/constraint of political Islam factions a legitimate act of democratic self-preservation (preventing anti-democratic forces from seizing power), or an extractive suppression of a valid political voice, undermining the charter''s democratic claims?',
    'Analysis of the excluded factions'' actual commitment to democratic processes (e.g., acceptance of electoral outcomes, respect for minority rights) and the availability of non-coercive mechanisms for managing ideological differences.',
    'If the exclusion is deemed illegitimate, the ''extractiveness'' and ''suppression'' metrics are fully justified, and the ''claimed_type'' of Tangled Rope (implying some coordination) would be challenged, potentially reclassifying it as a Snare. If legitimate, the extraction is seen as a necessary cost of democratic coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_exclusion, preference, 'Normative judgment on the justification for excluding political Islam from the democratic process.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__secular_democratic_reading, 1971, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(july_tr_t1971, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 1971, 0.2).
narrative_ontology:measurement(july_tr_t1985, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 1985, 0.3).
narrative_ontology:measurement(july_tr_t1998, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 1998, 0.35).
narrative_ontology:measurement(july_tr_t2011, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 2011, 0.45).
narrative_ontology:measurement(july_tr_t2024, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(july_be_t1971, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 1971, 0.55).
narrative_ontology:measurement(july_be_t1985, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 1985, 0.62).
narrative_ontology:measurement(july_be_t1998, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 1998, 0.68).
narrative_ontology:measurement(july_be_t2011, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 2011, 0.72).
narrative_ontology:measurement(july_be_t2024, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 2024, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(july_su_t1971, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 1971, 0.6).
narrative_ontology:measurement(july_su_t1985, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 1985, 0.7).
narrative_ontology:measurement(july_su_t1998, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 1998, 0.78).
narrative_ontology:measurement(july_su_t2011, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 2011, 0.85).
narrative_ontology:measurement(july_su_t2024, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 2024, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__secular_democratic_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
