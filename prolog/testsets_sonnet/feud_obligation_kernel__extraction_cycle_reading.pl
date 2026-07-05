% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__extraction_cycle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feud_obligation_kernel__extraction_cycle_reading, []).

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
 *   constraint_id: feud_obligation_kernel__extraction_cycle_reading
 *   human_readable: Blood-Feud Kinship Obligation as Destructive Extraction Cycle
 *   domain: legal_anthropology/medieval_history/comparative_political_systems
 *
 * SUMMARY:
 *   This story reads the blood-feud obligation kernel as a destructive
 *   extraction cycle: kinship honor codes obligate retaliatory violence or
 *   compensation payment whenever a member is killed, and each settlement or
 *   retaliation renews the conditions for the next round. Rather than
 *   functioning as effective deterrence or self-enforcing justice, the cycle
 *   recurrently depletes the labor, land, livestock, and marriageable women
 *   of the feuding lineages themselves, while an emergent royal authority
 *   benefits doubly: first, by offering court-based wergild adjudication as
 *   the visible alternative to a self-destroying kin order, and second, by
 *   inheriting weakened, court-dependent populations that are easier to tax
 *   and conscript than intact, feud-capable kin networks. Rising suppression
 *   over the interval tracks the crown's growing capacity to criminalize
 *   private vengeance and compel disputes into royal courts — the extraction,
 *   in this reading, runs through the persistence of the kin obligation
 *   itself, not through the crown's suppression of it (the crown is a
 *   downstream beneficiary of attrition, not the source of the feud
 *   obligation).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__extraction_cycle_reading, 0.71).
domain_priors:suppression_score(feud_obligation_kernel__extraction_cycle_reading, 0.62).
domain_priors:theater_ratio(feud_obligation_kernel__extraction_cycle_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__extraction_cycle_reading, snare).
narrative_ontology:human_readable(feud_obligation_kernel__extraction_cycle_reading, "Blood-Feud Kinship Obligation as Destructive Extraction Cycle").
narrative_ontology:topic_domain(feud_obligation_kernel__extraction_cycle_reading, "legal_anthropology/medieval_history/comparative_political_systems").

domain_priors:requires_active_enforcement(feud_obligation_kernel__extraction_cycle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__extraction_cycle_reading, 'e4ce7a4a-2138-4d53-815b-c975b5e7d54b').
narrative_ontology:cs_kernel_codification('e4ce7a4a-2138-4d53-815b-c975b5e7d54b', distributed).
narrative_ontology:cs_authority_grounding('e4ce7a4a-2138-4d53-815b-c975b5e7d54b', practice).
narrative_ontology:cs_interpretation_layer_present('e4ce7a4a-2138-4d53-815b-c975b5e7d54b').
narrative_ontology:cs_reading_relation('e4ce7a4a-2138-4d53-815b-c975b5e7d54b', feud_obligation_kernel__stateless_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('e4ce7a4a-2138-4d53-815b-c975b5e7d54b', feud_obligation_kernel__christianized_pacification_reading, influences).
narrative_ontology:cs_axiom('e4ce7a4a-2138-4d53-815b-c975b5e7d54b', foundational, feud_cycle_net_destroys_productive_capacity).
narrative_ontology:cs_axiom_status(feud_cycle_net_destroys_productive_capacity, holdable).
narrative_ontology:cs_axiom_grounding('e4ce7a4a-2138-4d53-815b-c975b5e7d54b', feud_cycle_net_destroys_productive_capacity, empirically_contingent).
narrative_ontology:cs_axiom('e4ce7a4a-2138-4d53-815b-c975b5e7d54b', secondary, royal_monopoly_on_violence_is_fiscally_motivated_not_merely_pacifying).
narrative_ontology:cs_axiom_status(royal_monopoly_on_violence_is_fiscally_motivated_not_merely_pacifying, holdable).
narrative_ontology:cs_axiom_grounding('e4ce7a4a-2138-4d53-815b-c975b5e7d54b', royal_monopoly_on_violence_is_fiscally_motivated_not_merely_pacifying, empirically_contingent).
narrative_ontology:cs_reference_frame('e4ce7a4a-2138-4d53-815b-c975b5e7d54b', kin_group_self_enforcement_baseline).
narrative_ontology:cs_drift_state('e4ce7a4a-2138-4d53-815b-c975b5e7d54b', royal_court_consolidation_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('e4ce7a4a-2138-4d53-815b-c975b5e7d54b', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__extraction_cycle_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__extraction_cycle_reading, emergent_royal_authority).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__extraction_cycle_reading, rival_kin_leaders_who_survive_and_consolidate).
narrative_ontology:constraint_victim(feud_obligation_kernel__extraction_cycle_reading, feuding_kin_groups).
narrative_ontology:constraint_victim(feud_obligation_kernel__extraction_cycle_reading, agricultural_households_of_involved_lineages).
narrative_ontology:constraint_victim(feud_obligation_kernel__extraction_cycle_reading, unmarried_daughters_used_as_compensation_currency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bound by kinship honor codes to pursue vengeance or wergild for a slain relative; each retaliatory killing obligates a countervailing claim from the other side. Refusing to act marks the lineage as dishonorable and invites further predation, so the obligation self-renews across generations, consuming able-bodied men, livestock, and land in compensation payments and burying productive-age adults faster than the group can replace them.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, feuding_kin_groups, payer,
    moderate, generational, identity_locked, regional).

% Farm the land the feuding lineages hold; when feud violence removes working-age men or when compensation payments strip the household of cattle and land, the harvest, herd, and household labor pool shrink. They cannot leave the kin territory without abandoning land rights and kin protection entirely, so they absorb the cost of a conflict decided by lineage elders.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, agricultural_households_of_involved_lineages, payer,
    powerless, biographical, trapped, local).

% Offered in marriage to the aggrieved lineage as part of wergild settlements to close a feud cycle, converting a person into a peace-payment instrument. They have no voice in the negotiation and no ability to refuse the arrangement without reigniting the violence their marriage is meant to end.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, unmarried_daughters_used_as_compensation_currency, payer,
    powerless, biographical, trapped, local).

% Positions itself as the alternative to endless private vengeance, offering royal courts and fixed wergild schedules in exchange for recognizing the king's monopoly on legitimate violence. Every feud cycle that depletes rival kin-group manpower and wealth makes royal arbitration more attractive by comparison, and every household that submits disputes to royal courts becomes taxable and countable in a way a feuding kin network is not. The crown's fiscal and territorial consolidation depends on feud attrition weakening the kin groups that would otherwise resist central taxation and conscription.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, emergent_royal_authority, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__extraction_cycle_reading, emergent_royal_authority, agenda_setter).

% A minority of lineage heads who successfully absorb rivals' depleted lands and dependents after a feud cycle exhausts the opposing kin group, converting the attrition of others into their own territorial gain. Their gain is contingent and small relative to the aggregate destruction, but it is real and it is why some elders keep the cycle alive rather than settling.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, rival_kin_leaders_who_survive_and_consolidate, beneficiary,
    moderate, generational, mobile, regional).

% Draft and administer fixed compensation schedules, adjudicate disputes when parties bring them to court rather than to the sword, and collect fees or fines for doing so. Their institutional survival depends on feud persisting as a live alternative that royal justice is offered against — a fully resolved kin-based order would remove their reason for existing, but a fully unrestrained feud order would remove their capacity to tax and conscript.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, royal_courts_and_wergild_administrators, agenda_setter,
    institutional, civilizational, analytical, national).

% Object to feud violence on doctrinal grounds and press for peace-oaths and sanctuary law, but their framing is theological rather than fiscal; in this extraction-cycle reading their voice is present in the historical record but not part of the resource-transfer accounting this constraint measures.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, regional_ecclesiastical_authorities, excluded,
    organized, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feud_obligation_kernel__extraction_cycle_reading, emergent_royal_authority).
narrative_ontology:fixing_cost_class(feud_obligation_kernel__extraction_cycle_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None genuine at the level this reading measures: the feud cycle does not efficiently allocate justice or deter aggression net of its costs — it recurrently destroys the productive capital (labor, land, livestock) of the very lineages it claims to protect, and the surviving coordination benefit (order restored between two specific households after settlement) is smaller than the aggregate depletion across the region.
% TRANSFER_FUNCTION: Moves able-bodied labor, livestock, land, and marriageable women out of feuding households and into either graves, rival lineages' hands, or royal fiscal visibility; over successive cycles it moves relative bargaining power away from kin networks and toward the emergent crown, which inherits weakened, court-dependent populations.
% ABSENT_VOICES: The daughters exchanged as wergild currency and the tenant households whose harvests are stripped to fund compensation payments have no seat in the feud councils that decide whether to escalate or settle; ecclesiastical authorities object on separate doctrinal grounds but are excluded from this reading's resource-accounting frame by design (see kernel_context).
% DISAPPEARANCE_RATIONALE: If the feud-obligation cycle vanished overnight, kin groups would retain their labor, land, and livestock across generations rather than losing them recurrently to compensation and mortality; regional power balances would stabilize around productive capacity rather than surviving-lineage attrition, and the royal court's specific fiscal leverage — offering an alternative to a self-destructive kin cycle — would lose its comparative advantage, forcing the crown to find another route to the same consolidation.
% FOUNDING_PROBLEM: In the absence of centralized courts, kin groups needed SOME mechanism to deter killing and secure restitution when a member was harmed; the vengeance-or-wergild obligation was the available lever.
% FOUNDING_PROBLEM_CORROBORATION: Royal chroniclers and later legal historians (outside the feuding lineages themselves) attest that by the period this reading covers, the deterrence function was demonstrably failing — feud cycles were self-perpetuating rather than self-limiting in the documented case clusters, and royal court records from the same period show the crown explicitly citing feud attrition as justification for expanding taxation and conscription authority over the weakened kin territories.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__extraction_cycle_reading, world_rearranges).
narrative_ontology:founding_problem_status(feud_obligation_kernel__extraction_cycle_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__extraction_cycle_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(feud_obligation_kernel__extraction_cycle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feud_obligation_kernel__extraction_cycle_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feud_obligation_kernel__extraction_cycle_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feud_obligation_kernel__extraction_cycle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feud_obligation_kernel__extraction_cycle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises across the interval (0.48 to 0.71) as feud cycles compound: each generation's retaliatory killings and compensation payments deplete the same finite pool of productive-age men, land, and livestock, with recovery time between rounds shrinking as lineages grow leaner and more vulnerable. Suppression is authored as substantial but not maximal (0.62 at interval end) because kinship-based enforcement of feud obligation is not fully extinguished by external coercion within this window — it persists partly through internalized honor codes that outlast any single royal edict, which is itself the suppression-mechanism ambiguity flagged in the omega below. Theater ratio stays low (0.1 to 0.22) because the destructive function is real and largely undisguised in this reading — feud violence is not performative, it is substantively resource-consuming; the modest rise reflects growing ceremonial elements (formalized peace-oaths, staged settlement rituals) layered on as royal courts begin absorbing part of the dispute-resolution function.
 *
 * PERSPECTIVAL GAP:
 *   A feuding lineage elder inside the honor code sees the obligation as the only available mechanism for redress and deterrence — refusing to act is unthinkable, not merely costly. Royal court administrators and later historians, positioned outside the kin structure with visibility into aggregate attrition across many lineages, see the same cycle as a net destroyer of the resource base the kin groups depend on. The engine computes these as structurally different seats from the same authored data; this reading deliberately measures the destruction, not the internal honor-logic (that logic is the subject of the sibling stateless_coordination_reading).
 *
 * DIRECTIONALITY LOGIC:
 *   Feuding kin groups, their dependent agricultural households, and the daughters exchanged as compensation currency are declared victims: they bear mortality, resource depletion, and loss of personal agency directly and cannot exit the kin structure without forfeiting land rights and protection entirely (trapped/identity_locked exit options push their directionality toward the full-target end). Emergent royal authority and the minority of surviving/consolidating kin leaders are declared beneficiaries: the crown's institutional exit options are near-arbitrage (it can reshape the legal environment it operates in) and its time horizon is generational-to-civilizational, giving it a structurally different relationship to the same cycle that destroys the payer seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (deterrence and restitution absent centralized courts) is authored as contested rather than flatly dead, because feud obligation's origin as A coordination mechanism is real even in this extractive reading — the mandatrophy concern is that a self-renewing obligation can persist and intensify long after its deterrence function has degraded into a self-perpetuating depletion cycle, with royal authority actively benefiting from that degradation rather than moving to correct it. Corroboration from royal court records naming feud attrition as a fiscal opportunity is exactly the kind of outside-the-beneficiary-set evidence that distinguishes a genuine mandatrophy diagnosis from a self-serving crown narrative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_choice_extraction_vs_coordination,
    'Is blood-feud obligation better modeled as a destructive extraction cycle (this reading) or as a genuinely functional coordination mechanism whose deterrence value exceeds its resource cost (the stateless_coordination_reading)?',
    'Comparative case-study data on feud frequency, settlement rates, and homicide recurrence across kin networks with strong vs. weak wergild schedules; if settlement rates are high and recurrence low, the coordination reading gains support; if cycles recur and escalate net of settlement attempts, this extraction reading gains support.',
    'If the coordination reading is empirically dominant for a given region/period, the correct classification there shifts toward rope or tangled_rope rather than snare, and royal authority''s beneficiary status weakens correspondingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_choice_extraction_vs_coordination, conceptual, 'Whether feud obligation is dominantly extractive or dominantly coordinative is a framing choice this reading takes one side of.').

omega_variable(
    royal_beneficiary_causal_priority,
    'Did royal authority actively cultivate feud persistence to weaken rival kin networks, or did it merely benefit passively from an attrition dynamic it did not cause and could not have prevented?',
    'Archival evidence of royal policy explicitly discouraging early settlement, versus evidence of consistent royal peace-enforcement efforts that simply failed due to capacity limits.',
    'Active cultivation supports the snare classification with royal authority as a structural architect; passive benefit without cultivation would push the constraint toward tangled_rope, since the coordination function (feud as available justice mechanism) would be more clearly separable from the extraction that rides on it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(royal_beneficiary_causal_priority, empirical, 'Whether the crown''s beneficiary status reflects design or opportunism.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of kinship-based enforcement structural (royal military/legal coercion) or internalized (honor-code erosion as royal courts offer a face-saving alternative to vengeance)?',
    'Track settlement patterns after royal court capacity is locally removed (e.g., during succession crises or frontier gaps in royal reach): if feud violence resumes immediately, suppression was structural; if kin groups continue preferring court settlement even without enforcement capacity, suppression has partly internalized as a shift in normative expectation.',
    'If internalized, the effective suppression carried forward by kin groups is higher than the structural suppression metric alone suggests, and the transition away from feud obligation is more durable than raw enforcement capacity would predict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of feud-based enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__extraction_cycle_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(feud_tr_t40, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 40, 0.13).
narrative_ontology:measurement(feud_tr_t80, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 80, 0.16).
narrative_ontology:measurement(feud_tr_t120, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 120, 0.18).
narrative_ontology:measurement(feud_tr_t160, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 160, 0.2).
narrative_ontology:measurement(feud_tr_t200, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 200, 0.22).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(feud_be_t40, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(feud_be_t80, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 80, 0.62).
narrative_ontology:measurement(feud_be_t120, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 120, 0.67).
narrative_ontology:measurement(feud_be_t160, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 160, 0.7).
narrative_ontology:measurement(feud_be_t200, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 200, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t0, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(feud_su_t40, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 40, 0.42).
narrative_ontology:measurement(feud_su_t80, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 80, 0.49).
narrative_ontology:measurement(feud_su_t120, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 120, 0.55).
narrative_ontology:measurement(feud_su_t160, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 160, 0.6).
narrative_ontology:measurement(feud_su_t200, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 200, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feud_obligation_kernel__extraction_cycle_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(feud_obligation_kernel__extraction_cycle_reading, feud_obligation_kernel__stateless_coordination_reading).
narrative_ontology:affects_constraint(feud_obligation_kernel__extraction_cycle_reading, feud_obligation_kernel__christianized_pacification_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the feud_obligation_kernel. extraction_cycle_reading (this file) treats the arrangement as resource-depleting extraction that structurally benefits emergent royal authority. stateless_coordination_reading treats the same nominal arrangement as a functional deterrence-and-restitution mechanism with no crown beneficiary. christianized_pacification_reading treats it as a violation of divinely-grounded prohibition on private vengeance, with ecclesiastical and royal authority as joint beneficiaries via a doctrinal rather than fiscal transfer function. Each carries its own epsilon, beneficiary/victim set, and classification per the epsilon-invariance principle; they are not three views of one constraint but three distinct constraints sharing a contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
