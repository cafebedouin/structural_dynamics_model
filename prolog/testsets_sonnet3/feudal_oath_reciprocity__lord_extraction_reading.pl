% ============================================================================
% CONSTRAINT STORY: feudal_oath_reciprocity__lord_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: feudal_oath_reciprocity__lord_extraction_reading
 *   human_readable: Feudal Oath as Unbounded Lordly Extraction Authority
 *   domain: medieval_political_economy/legal_history
 *
 * SUMMARY:
 *   This story instantiates the lord-extraction reading of the feudal oath
 *   kernel: the oath of fealty as an open license for the lord to demand
 *   whatever service or payment the vassal's estate can bear, with
 *   'reciprocity' functioning as rhetorical cover rather than a binding
 *   limit. The referent is the standing arrangement as this reading's own
 *   lights see it — the historical practice of escalating scutage, relief,
 *   wardship exploitation, and extraordinary aids that provoked baronial
 *   rebellion and eventually forced charter concessions (e.g., the grievances
 *   catalogued in Magna Carta's tenurial clauses). This is one of three
 *   sibling readings of the same kernel text; the vassal-coordination reading
 *   treats the same oath as fixing bounded obligations enforceable by
 *   charter, and the ecclesiastical-mediation reading treats it as bounded by
 *   sacramental conscience. Each sibling is a separate constraint with its
 *   own ε; this file speaks only for the extraction reading.
 *
 * KEY AGENTS:
 *   - landholding_lords: agenda_setter/beneficiary (institutional/arbitrage) — sets and escalates extraction terms under cover of the oath
 *   - liege_overlords: beneficiary (institutional/arbitrage) — collects pass-through revenue from the extraction cascade
 *   - enfeoffed_vassals: payer (moderate/trapped) — bears escalating demands with fief forfeiture as the only formal exit
 *   - vassal_tenant_peasantry: payer (powerless/trapped) — absorbs cascading extraction with no oath standing at all
 *   - minor_knights: payer (moderate/constrained) — provides the actual service the oath nominally trades for land
 *   - ecclesiastical_courts: excluded (organized/constrained) — holds a rival conscience-based limiting theory with no secular jurisdiction
 *   - royal_justices: observer (institutional/analytical) — intervenes only episodically, sharing class interest with lords
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__lord_extraction_reading, 0.87).
domain_priors:suppression_score(feudal_oath_reciprocity__lord_extraction_reading, 0.81).
domain_priors:theater_ratio(feudal_oath_reciprocity__lord_extraction_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, extractiveness, 0.87).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__lord_extraction_reading, snare).
narrative_ontology:human_readable(feudal_oath_reciprocity__lord_extraction_reading, "Feudal Oath as Unbounded Lordly Extraction Authority").
narrative_ontology:topic_domain(feudal_oath_reciprocity__lord_extraction_reading, "medieval_political_economy/legal_history").

domain_priors:requires_active_enforcement(feudal_oath_reciprocity__lord_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__lord_extraction_reading, '717de4c7-0fa9-40a2-8ce5-26edafdc1f84').
narrative_ontology:cs_kernel_codification('717de4c7-0fa9-40a2-8ce5-26edafdc1f84', distributed).
narrative_ontology:cs_authority_grounding('717de4c7-0fa9-40a2-8ce5-26edafdc1f84', extraction).
narrative_ontology:cs_interpretation_layer_present('717de4c7-0fa9-40a2-8ce5-26edafdc1f84').
narrative_ontology:cs_reading_relation('717de4c7-0fa9-40a2-8ce5-26edafdc1f84', feudal_oath_reciprocity__vassal_coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('717de4c7-0fa9-40a2-8ce5-26edafdc1f84', feudal_oath_reciprocity__ecclesiastical_mediation_reading, coexists_with).
narrative_ontology:cs_axiom('717de4c7-0fa9-40a2-8ce5-26edafdc1f84', foundational, fealty_authorizes_discretionary_maximal_demand).
narrative_ontology:cs_axiom_status(fealty_authorizes_discretionary_maximal_demand, holdable).
narrative_ontology:cs_axiom_grounding('717de4c7-0fa9-40a2-8ce5-26edafdc1f84', fealty_authorizes_discretionary_maximal_demand, conventional).
narrative_ontology:cs_axiom('717de4c7-0fa9-40a2-8ce5-26edafdc1f84', secondary, rebellion_risk_is_the_only_effective_ceiling).
narrative_ontology:cs_axiom_status(rebellion_risk_is_the_only_effective_ceiling, holdable).
narrative_ontology:cs_axiom_grounding('717de4c7-0fa9-40a2-8ce5-26edafdc1f84', rebellion_risk_is_the_only_effective_ceiling, empirically_contingent).
narrative_ontology:cs_reference_frame('717de4c7-0fa9-40a2-8ce5-26edafdc1f84', unbounded_lordly_prerogative_over_fealty).
narrative_ontology:cs_drift_state('717de4c7-0fa9-40a2-8ce5-26edafdc1f84', post_magna_carta_charter_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('717de4c7-0fa9-40a2-8ce5-26edafdc1f84', '').
narrative_ontology:cs_kernel_id(feudal_oath_reciprocity__lord_extraction_reading, feudal_oath_reciprocity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__lord_extraction_reading, landholding_lords).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__lord_extraction_reading, liege_overlords).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__lord_extraction_reading, enfeoffed_vassals).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__lord_extraction_reading, vassal_tenant_peasantry).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__lord_extraction_reading, minor_knights).
narrative_ontology:constraint_vindicates(feudal_oath_reciprocity__lord_extraction_reading, lordly_prerogative_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Grants land (the fief) in exchange for the oath of homage, then sets the terms of military service, scutage payments, relief on inheritance, wardship, and aid demands unilaterally, citing the vassal's sworn fealty as authorizing whatever the lord deems necessary. Can escalate demands year to year and reinterpret customary obligation upward; holds the courts that adjudicate disputes over what was 'owed.'
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, landholding_lords, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__lord_extraction_reading, landholding_lords, beneficiary).

% Sit above the landholding lords in the tenurial chain and receive escalating aids, feudal incidents, and military levies passed upward through the same extraction logic, benefiting from the lords' unbounded reading of the oath without bearing the direct enforcement cost against vassals themselves.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, liege_overlords, beneficiary,
    institutional, civilizational, arbitrage, national).

% Hold land under the lord's grant and swore homage in exchange for protection and defined service, but face demands for scutage, extraordinary aids, wardship fees, and military levies that expand beyond what custom or charter specified. Leaving means forfeiting the fief, kin, and social standing built on it; appealing to the lord's own court for redress is structurally compromised.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, enfeoffed_vassals, payer,
    moderate, biographical, trapped, local).

% Work the land under the vassal's tenancy and bear the pass-through cost when the vassal, squeezed by the lord's rising demands, extracts more in labor dues, rents, and levies to meet obligations upward. Have no oath relationship with the lord at all yet absorb the shock of the extraction cascade with the least capacity to resist or relocate.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, vassal_tenant_peasantry, payer,
    powerless, biographical, trapped, local).

% Provide the actual military service the oath nominally trades for the fief, called to campaign, garrison duty, and escort at the lord's discretion beyond agreed terms of service (knight's fee obligations stretched past the customary forty days). Their only leverage is collective refusal, which risks charges of felony and forfeiture.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, minor_knights, payer,
    moderate, biographical, constrained, local).

% Would assert that the oath is a sacramental commitment bound by conscience and charity, capping what a lord may righteously demand — but have no direct jurisdiction over secular tenurial disputes and are excluded from the lord's own courts where feudal incidents are actually adjudicated.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, ecclesiastical_courts, excluded,
    organized, civilizational, constrained, national).

% Occasionally hear appeals when vassal grievances escalate to rebellion or petition the crown, and can in principle constrain lordly overreach through royal writ, but intervene only episodically and often share the same class interest as the lords they would be checking.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, royal_justices, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feudal_oath_reciprocity__lord_extraction_reading, landholding_lords).
narrative_ontology:fixing_cost_class(feudal_oath_reciprocity__lord_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The oath of homage nominally coordinates land tenure with military and administrative service: the lord grants productive land and protection, the vassal returns defined service and loyalty, solving the problem of maintaining an armed retinue and administered territory without a cash economy.
% TRANSFER_FUNCTION: Moves labor, military service, and increasingly cash payments (scutage, relief, aids, wardship revenue) from vassals and the peasantry beneath them upward to the lord and the liege overlord above him, with the flow's magnitude set unilaterally by the lord invoking the oath's open-ended language of fealty.
% ABSENT_VOICES: Ecclesiastical courts, which hold a competing theory that the oath is bound by sacramental conscience, are structurally excluded from the secular tenurial forum where extraction disputes are actually decided; the vassal-tenant peasantry, who bear the cascading cost, have no oath relationship or standing at all.
% DISAPPEARANCE_RATIONALE: If the lord's unbounded reading of the oath vanished overnight and were replaced by fixed, enforceable obligations, vassals would retain far more of their fief's yield, military service would revert to customary limits, scutage and aid demands would require renegotiated consent, and the entire tenurial hierarchy's revenue structure would have to reorganize around bounded, charter-defined terms.
% FOUNDING_PROBLEM: Early medieval lords needed a durable way to raise armed retainers and administer scattered territory without a monetized state apparatus; granting land in exchange for a personal oath of service solved the problem of maintaining loyalty and military capacity across generations.
% FOUNDING_PROBLEM_CORROBORATION: Lords and liege overlords attest the broad reading of the oath remains necessary to fund an active military and administrative apparatus. Vassals, minor knights, and later royal justices (in charters like Magna Carta's constraints on relief, wardship, and scutage) attest from outside the beneficiary class that the founding military-coordination problem no longer justifies open-ended extraction and that the oath's elastic interpretation has become a revenue mechanism detached from its original service rationale.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__lord_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__lord_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__lord_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(feudal_oath_reciprocity__lord_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feudal_oath_reciprocity__lord_extraction_reading, 0.87, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is authored high (0.87 at interval end) because, under this reading, the oath's language ('fealty,' 'aid,' 'counsel') places no textual ceiling on what may be demanded — the only practical bound is the vassal's capacity to pay or fight, which is precisely the delta this reading is meant to model. Suppression is high (0.81) because the lord controls the adjudicating court, so a vassal contesting an 'excessive' demand petitions the very party who benefits from the escalation. Theater rises over the interval (0.20→0.42) as lords increasingly invoke ceremonial language of mutual obligation and ancestral custom precisely as the material relationship becomes more extractive — the rhetoric of reciprocity intensifies as the reciprocity itself hollows out.
 *
 * DIRECTIONALITY LOGIC:
 *   Landholding lords and liege overlords are declared beneficiaries: they set terms and collect the transfer without bearing its enforcement cost against their own persons. Enfeoffed vassals, minor knights, and vassal-tenant peasantry are declared victims with directionality pushed toward the full-target end — vassals are trapped by fief forfeiture, knights are constrained by felony risk for refusal, and peasants have no standing at all and simply absorb the cascade. Ecclesiastical courts are excluded rather than beneficiary or payer: they hold a rival theory of the same kernel but have no jurisdictional purchase in the forum where this reading's extraction is actually enacted.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (raising an armed, administered retinue without a cash economy) was genuinely live in the early medieval period; under this reading it has since become pretextual — the military-coordination need persists in attenuated form, but the extraction machinery built atop it has decoupled from any bounded service calculus. Classifying this reading as snare rather than tangled_rope reflects that, on THIS reading, no genuine bounded coordination function survives to be balanced against the extraction; the coordination story is read here as cover, which is exactly why the sibling vassal-coordination reading exists as a separate constraint asserting the opposite structural claim from the same text.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_ceiling_indeterminacy,
    'Does the oath''s language of fealty and aid contain an implicit customary ceiling that this reading denies, or is the extraction reading correct that no textual limit exists absent charter codification?',
    'Comparative analysis of surviving fealty oath texts and contemporaneous custumals across regions/periods: consistent customary caps across independent jurisdictions would favor the vassal-coordination reading; wide, lord-specific variance in what was extracted would favor this reading''s unbounded-in-practice claim.',
    'If a genuine customary ceiling is established as historically operative and enforced, this reading''s snare classification would be undermined in favor of the sibling tangled_rope or rope readings for the periods/regions where the ceiling held.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_ceiling_indeterminacy, empirical, 'Whether extraction under the oath was ever textually or customarily bounded, or only bounded by rebellion risk.').

omega_variable(
    rebellion_threshold_as_the_real_ceiling,
    'Is the vassal rebellion threshold (baronial revolt, refusal of service, flight) the actual and only effective limit on lordly extraction under this reading, and how would that threshold be measured historically?',
    'Track documented instances of baronial revolt, coalition refusal of scutage, and charter concessions (e.g., 1215) as a time series against escalating feudal incident demands; a tight correlation between demand spikes and revolt events supports the rebellion-threshold-as-ceiling reading central to this constraint''s structural delta.',
    'Confirms or disconfirms the specific mechanism this reading claims bounds extraction — not custom, not conscience, but the practical cost of provoking armed resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rebellion_threshold_as_the_real_ceiling, empirical, 'Whether rebellion risk, rather than any normative limit, is the operative ceiling on extraction.').

omega_variable(
    reading_selection_grounds,
    'What in the source material or historiographical tradition licenses treating the extraction reading, rather than the coordination or ecclesiastical readings, as the dominant lens for a given lord-vassal relationship at a given time and place?',
    'Case-level historical evidence: charter specificity, frequency and magnitude of ''extraordinary'' aid demands relative to custom, and outcomes of vassal grievance petitions would indicate which reading fits a particular documented relationship best.',
    'Without case-level grounding, the choice among the three sibling readings for any specific historical lord-vassal pair remains an interpretive commitment rather than a settled empirical fact; this affects how confidently the family of constraints should be applied to any single documented fief.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_grounds, conceptual, 'Whether the three kernel readings can be empirically assigned to specific historical relationships or remain interpretive framings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__lord_extraction_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(feud_tr_t8, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement(feud_tr_t16, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 16, 0.29).
narrative_ontology:measurement(feud_tr_t24, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 24, 0.34).
narrative_ontology:measurement(feud_tr_t32, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 32, 0.38).
narrative_ontology:measurement(feud_tr_t40, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(feud_be_t8, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 8, 0.63).
narrative_ontology:measurement(feud_be_t16, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 16, 0.71).
narrative_ontology:measurement(feud_be_t24, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 24, 0.78).
narrative_ontology:measurement(feud_be_t32, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 32, 0.83).
narrative_ontology:measurement(feud_be_t40, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 40, 0.87).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t0, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(feud_su_t8, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 8, 0.64).
narrative_ontology:measurement(feud_su_t16, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 16, 0.69).
narrative_ontology:measurement(feud_su_t24, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 24, 0.74).
narrative_ontology:measurement(feud_su_t32, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 32, 0.78).
narrative_ontology:measurement(feud_su_t40, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 40, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__lord_extraction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__lord_extraction_reading, vassal_coordination_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__lord_extraction_reading, ecclesiastical_mediation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the feudal_oath_reciprocity kernel, decomposed per the ε-invariance principle because the natural-language label 'the feudal oath' conflates structurally distinct claims with materially different ε. lord_extraction_reading (this file, ε=0.87, snare) treats the oath as authorizing unbounded lordly demand capped only by rebellion risk. vassal_coordination_reading (ε expected low, rope-flavored) treats the identical oath as establishing fixed, charter-enforceable reciprocal obligations. ecclesiastical_mediation_reading (ε expected moderate, tangled_rope-flavored) treats the oath as bound by sacramental conscience limiting secular extraction, contested and imperfectly enforced. All three share the same underlying oath text and historical institution but diverge on what the text is taken to authorize, which is exactly the ε-invariance test: measuring 'the feudal oath' one way yields high extraction, another way yields low extraction — hence three linked files rather than one hedged story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
