% ============================================================================
% CONSTRAINT STORY: feudal_oath_reciprocity__lord_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
 *   human_readable: Feudal Oath as Lord's Extraction Authorization (Lord Extraction Reading)
 *   domain: medieval_political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the lord_extraction_reading of the
 *   feudal_oath_reciprocity kernel. The oath — a personal bond of homage and
 *   fealty sworn by a vassal to a lord — is read here as authorizing the lord
 *   to demand maximal service and revenue from the vassal, bounded only by
 *   the vassal's capacity to fulfill the demand without collapsing into
 *   rebellion or flight. The coordination story (mutual protection and land
 *   tenure) is the cover; the operational reality is a snare where the lord
 *   extracts labor, military service, and surplus from vassals (and through
 *   them, from peasant producers), enforced by the lord's military power and
 *   the vassal's identity-locked position (homage makes exit a betrayal of
 *   self). The vassal_coordination_reading and
 *   ecclesiastical_mediation_reading are sibling constraints from the same
 *   kernel; they are not described here — each gets its own story.
 *
 * KEY AGENTS:
 *   - secular_lord: Agenda setter (institutional/arbitrage) — sets extraction terms, holds military enforcement, can exit to rival lordships or crown
 *   - vassals: Payers (powerful/identity_locked) — owe service/revenue, exit is betrayal of oath and loss of fief, rebellion is the only exit
 *   - peasant_producers: Payers (powerless/trapped) — bear ultimate incidence via vassal's extraction, no exit from manorial system
 *   - ecclesiastical_authorities: Excluded/Observer (institutional/analytical) — claim sacramental bound on extraction, structural force contested
 *   - rival_lords: Observer (powerful/mobile) — alternative power centers vassals could defect to
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__lord_extraction_reading, 0.82).
domain_priors:suppression_score(feudal_oath_reciprocity__lord_extraction_reading, 0.78).
domain_priors:theater_ratio(feudal_oath_reciprocity__lord_extraction_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__lord_extraction_reading, snare).
narrative_ontology:human_readable(feudal_oath_reciprocity__lord_extraction_reading, "Feudal Oath as Lord's Extraction Authorization (Lord Extraction Reading)").
narrative_ontology:topic_domain(feudal_oath_reciprocity__lord_extraction_reading, "medieval_political_economy").

domain_priors:requires_active_enforcement(feudal_oath_reciprocity__lord_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__lord_extraction_reading, 'e358321f-4b09-4a76-be50-7a42687dbc0a').
narrative_ontology:cs_kernel_codification('e358321f-4b09-4a76-be50-7a42687dbc0a', fixed_text).
narrative_ontology:cs_authority_grounding('e358321f-4b09-4a76-be50-7a42687dbc0a', lineage).
narrative_ontology:cs_interpretation_layer_present('e358321f-4b09-4a76-be50-7a42687dbc0a').
narrative_ontology:cs_reading_relation('e358321f-4b09-4a76-be50-7a42687dbc0a', feudal_oath_reciprocity__vassal_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('e358321f-4b09-4a76-be50-7a42687dbc0a', feudal_oath_reciprocity__ecclesiastical_mediation_reading, influences).
narrative_ontology:cs_axiom('e358321f-4b09-4a76-be50-7a42687dbc0a', foundational, lord_defines_vassal_capacity).
narrative_ontology:cs_axiom_status(lord_defines_vassal_capacity, holdable).
narrative_ontology:cs_axiom_grounding('e358321f-4b09-4a76-be50-7a42687dbc0a', lord_defines_vassal_capacity, conventional).
narrative_ontology:cs_axiom('e358321f-4b09-4a76-be50-7a42687dbc0a', foundational, homage_fuses_identity_to_bond).
narrative_ontology:cs_axiom_status(homage_fuses_identity_to_bond, holdable).
narrative_ontology:cs_axiom_grounding('e358321f-4b09-4a76-be50-7a42687dbc0a', homage_fuses_identity_to_bond, conventional).
narrative_ontology:cs_axiom('e358321f-4b09-4a76-be50-7a42687dbc0a', secondary, founding_problem_dead).
narrative_ontology:cs_axiom_status(founding_problem_dead, holdable).
narrative_ontology:cs_axiom_grounding('e358321f-4b09-4a76-be50-7a42687dbc0a', founding_problem_dead, empirically_contingent).
narrative_ontology:cs_reference_frame('e358321f-4b09-4a76-be50-7a42687dbc0a', carolingian_oath_reciprocity).
narrative_ontology:cs_drift_state('e358321f-4b09-4a76-be50-7a42687dbc0a', high_medieval_lordship, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e358321f-4b09-4a76-be50-7a42687dbc0a', '').
narrative_ontology:cs_kernel_id(feudal_oath_reciprocity__lord_extraction_reading, feudal_oath_reciprocity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__lord_extraction_reading, secular_lord).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__lord_extraction_reading, vassals).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__lord_extraction_reading, peasant_producers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the fief-granting power and defines the terms of the oath. Demands military service, court attendance, and financial aids (reliefs, scutages, arbitrary tallages) up to the vassal's capacity. Enforces via distraint, escheat, and military force. Can exit by commending to a stronger lord or the crown; holds arbitrage-grade exit because lordship is a portable status.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, secular_lord, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__lord_extraction_reading, secular_lord, beneficiary).

% Swear homage and fealty, receiving a fief in return. The oath fuses their identity to the bond — to leave is to betray oneself and forfeit the fief. They bear the extraction: military service (40 days/year plus extras), financial aids at the lord's discretion, court service. Their 'capacity' is assessed by the lord. Exit options: rebellion (high cost, high risk), negotiation (delay/reduce specific demands), or commendation to a rival lord (treasonous, rarely viable).
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, vassals, payer,
    powerful, biographical, identity_locked, regional).

% Work the demesne and render labor/rent to the vassal (or directly to the lord where the lord holds demesne). The vassal's extraction from the lord is passed down as increased labor dues, tallerages, and merchet fees. No exit from the manorial system: flight is pursued, urban refuge is time-limited (year-and-a-day), and the village community enforces collective liability. They are not party to the oath but bear its ultimate incidence.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, peasant_producers, payer,
    powerless, biographical, trapped, local).

% Claim the oath is a sacrament binding the lord to Christian charity and just measure. Bishops intervene in specific disputes, threatening excommunication for excessive extraction. Their structural force varies: strong where the church has independent military/political power (e.g., prince-bishoprics), weak where lords control episcopal appointments. They are excluded from the lord-vassal bargain but claim authority over its moral bounds.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, ecclesiastical_authorities, excluded,
    institutional, generational, analytical, continental).

% Alternative power centers a vassal could theoretically defect to. In practice, defection is rare — the new lord demands the same oath, and the old lord pursues the defector. Their presence sets a theoretical ceiling on extraction (if extraction exceeds the cost of defection + pursuit risk, vassals flee). They do not participate in the constraint but bound its practical operation.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, rival_lords, observer,
    powerful, biographical, mobile, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feudal_oath_reciprocity__lord_extraction_reading, secular_lord).
narrative_ontology:fixing_cost_class(feudal_oath_reciprocity__lord_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Mutual protection and land tenure in a stateless landscape: the lord provides military protection and justice; the vassal provides military service and counsel. The oath solves the credible commitment problem of exchanging land for service in a world without contract enforcement.
% TRANSFER_FUNCTION: Moves military service, labor, and surplus revenue from vassals (and through them from peasant producers) to the secular lord, as the price of holding a fief and the lord's 'protection.' The transfer is open-ended: the lord defines what 'aid and counsel' requires in each instance.
% ABSENT_VOICES: Peasant producers are the ultimate bearers of the extraction but have no voice in the oath. Rival lords would offer alternative terms but are structurally excluded by the oath's exclusivity. The ecclesiastical_mediation_reading's voice (sacramental bound) is present in discourse but excluded from the lord's operational calculus.
% DISAPPEARANCE_RATIONALE: If the lord's extraction authorization vanished overnight, vassals would retain their fiefs without open-ended service obligations; peasant labor dues would fall to the vassal's subsistence needs only; the military household system would collapse; lords would lose their primary revenue base. The feudal political economy would reorganize around fixed rents, monetary contracts, or allodial tenure — a fundamental rearrangement.
% FOUNDING_PROBLEM: Credible commitment of mutual protection and land tenure in a fragmented, post-Carolingian landscape where no central authority could enforce contracts. The oath bound lord and vassal personally, substituting personal bond for institutional enforcement.
% FOUNDING_PROBLEM_CORROBORATION: Royal chroniclers (e.g., Orderic Vitalis) document lords extracting far beyond protection costs by 1100. Magna Carta (1215) — forced by vassals, not lords — explicitly caps reliefs and aids, corroborating that the founding problem (mutual protection) had been displaced by extraction. Ecclesiastical reformers (e.g., Gratian, Decretum) cite the oath's corruption as evidence the original reciprocal balance was lost. No corroborating source outside the lordly class attests the founding problem remains live.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__lord_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__lord_extraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__lord_extraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(feudal_oath_reciprocity__lord_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feudal_oath_reciprocity__lord_extraction_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness 0.82: the lord claims the full surplus of the vassal's holding above subsistence/service capacity; the oath text ('aid and counsel') is open-ended and the lord defines what 'capacity' means. Suppression 0.78: exit is structurally blocked by the oath's identity-binding nature (homage fuses vassal identity to the bond) and by the military reality that defection = forfeiture + pursuit. Theater 0.15: low — the constraint makes little pretense of reciprocity beyond the vassal's survival; the 'mutual obligation' frame is thin and widely understood as lord's prerogative. Accessibility collapse 0.65: alternatives (free tenure, allodial hold, urban commune) exist but are geographically limited and politically dangerous. Resistance 0.55: vassals resist via negotiation, delay, and occasional rebellion; peasants resist via flight and covert resistance — real but contained.
 *
 * PERSPECTIVAL GAP:
 *   The lord's seat computes as beneficiary (d near 0): the constraint subsidizes the lord's military household and status. The vassal's seat computes as full target (d near 1): identity-locked, bears the full extraction, exit = rebellion. The peasant seat computes as target (d ~0.8): trapped, bears downstream incidence, no voice in the oath. The ecclesiastical seat computes as observer/excluded: claims authority to bound extraction but lacks enforcement capacity against determined lords. The engine computes these per-seat types from the structural data authored here.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary: secular_lord — collects service/revenue, defines the terms, holds enforcement monopoly. Victims: vassals (primary, identity-locked by homage) and peasant_producers (secondary, trapped by manorial dependency). The extraction limit 'vassal service capacity' is defined by the lord; the rebellion threshold is the only external check. Directionality overrides not needed — beneficiary/victim declarations + power/exit produce the correct d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (kernel-level) was mutual protection and land tenure in a stateless landscape. This reading declares that problem dead or contested — the lord no longer provides protection proportional to extraction; the oath persists as an extraction vehicle. Mandatrophy resolved = true for this reading: the arrangement's original coordination function has atrophied, leaving a snare. The vassal_coordination_reading claims the problem is live; the ecclesiastical_mediation_reading claims it is transformed by sacramental obligation. This reading's axioms declare the founding problem dead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a reading of the feudal_oath_reciprocity kernel, specifically the lord_extraction_reading?',
    'Structural analysis of the commitment system: if the kernel''s stabilized commitment (the oath text and its interpretive tradition) admits this reading as a coherent instantiation, the reading is valid. Compare sibling readings'' axioms and reference frames.',
    'If this is a valid reading of the kernel, its ε and victim structure are indexed to this reading''s frame; sibling readings instantiate distinct constraints with their own ε values. If not, this story describes a different constraint entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment to kernel/reading identity').

omega_variable(
    extraction_limit_mechanism,
    'Is the extraction ceiling (vassal service capacity/rebellion threshold) a structural feature of the oath itself, or an external constraint from military/political reality?',
    'Comparative analysis of oath texts across regions/periods: if oaths explicitly articulate a ''service capacity'' limit, the limit is internal to the commitment; if all oaths authorize open-ended service and the limit appears only in practice, the limit is external.',
    'If internal, the constraint''s extractiveness is structurally bounded (still high ε but with a built-in ceiling). If external, the constraint is an unbounded snare whose practical ceiling depends on contingent power balances — higher effective extraction when lords are strong.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_limit_mechanism, empirical, 'Whether the extraction limit is internal to the commitment or external to it').

omega_variable(
    ecclesiastical_constraint_ambiguity,
    'Does the ecclesiastical_mediation_reading''s charity/sacramental bound structurally constrain this reading''s extraction, or does this reading simply ignore/override it?',
    'Historical record of church interventions in specific lord-vassal extraction disputes: if bishops successfully compelled restitution citing sacramental oath, the ecclesiastical reading has structural force. If interventions were symbolic or ignored, this reading operates autonomously.',
    'If ecclesiastical bounds have structural force, this reading''s ε is lower than its declared 0.82 when the church is active — a contextual modulation. If they are ignorable, this reading''s extraction is unconstrained by the sibling.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ecclesiastical_constraint_ambiguity, conceptual, 'Whether the sibling ecclesiastical reading structurally limits this reading''s extraction').

omega_variable(
    peasant_producer_victim_status,
    'Are peasant producers direct victims of the lord''s extraction via the vassal intermediary, or are they indirect bearers of costs the vassal passes down?',
    'Manorial court records and tax/rent rolls showing whether lords extracted directly from peasant production (via tallage, merchet, etc.) or only via vassal service quotas. Distinguishes direct victim (extraction relationship) from indirect cost-bearer (downstream incidence).',
    'If direct victims, the victim set is broader and the constraint''s extraction operates across two levels (lord→vassal→peasant). If indirect, the constraint''s primary victim is the vassal; peasant burden is a secondary incidence not counted in this constraint''s ε.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(peasant_producer_victim_status, empirical, 'Whether peasant producers are direct or indirect victims of the lord''s extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__lord_extraction_reading, 1000, 1300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feudal_oath_reciprocity__lord_extraction_reading_tr_t1000, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 1000, 0.08).
narrative_ontology:measurement(feudal_oath_reciprocity__lord_extraction_reading_tr_t1050, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 1050, 0.1).
narrative_ontology:measurement(feudal_oath_reciprocity__lord_extraction_reading_tr_t1100, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 1100, 0.12).
narrative_ontology:measurement(feudal_oath_reciprocity__lord_extraction_reading_tr_t1150, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 1150, 0.13).
narrative_ontology:measurement(feudal_oath_reciprocity__lord_extraction_reading_tr_t1200, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 1200, 0.14).
narrative_ontology:measurement(feudal_oath_reciprocity__lord_extraction_reading_tr_t1250, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 1250, 0.15).
narrative_ontology:measurement(feudal_oath_reciprocity__lord_extraction_reading_tr_t1300, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 1300, 0.15).

% Extraction over time
narrative_ontology:measurement(feudal_oath_reciprocity__lord_extraction_reading_be_t1000, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 1000, 0.55).
narrative_ontology:measurement(feudal_oath_reciprocity__lord_extraction_reading_be_t1050, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 1050, 0.62).
narrative_ontology:measurement(feudal_oath_reciprocity__lord_extraction_reading_be_t1100, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 1100, 0.68).
narrative_ontology:measurement(feudal_oath_reciprocity__lord_extraction_reading_be_t1150, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 1150, 0.73).
narrative_ontology:measurement(feudal_oath_reciprocity__lord_extraction_reading_be_t1200, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 1200, 0.77).
narrative_ontology:measurement(feudal_oath_reciprocity__lord_extraction_reading_be_t1250, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 1250, 0.8).
narrative_ontology:measurement(feudal_oath_reciprocity__lord_extraction_reading_be_t1300, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 1300, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(feudal_oath_reciprocity__lord_extraction_reading_su_t1000, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 1000, 0.6).
narrative_ontology:measurement(feudal_oath_reciprocity__lord_extraction_reading_su_t1050, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 1050, 0.65).
narrative_ontology:measurement(feudal_oath_reciprocity__lord_extraction_reading_su_t1100, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 1100, 0.7).
narrative_ontology:measurement(feudal_oath_reciprocity__lord_extraction_reading_su_t1150, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 1150, 0.74).
narrative_ontology:measurement(feudal_oath_reciprocity__lord_extraction_reading_su_t1200, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 1200, 0.76).
narrative_ontology:measurement(feudal_oath_reciprocity__lord_extraction_reading_su_t1250, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 1250, 0.77).
narrative_ontology:measurement(feudal_oath_reciprocity__lord_extraction_reading_su_t1300, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 1300, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__lord_extraction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(feudal_oath_reciprocity__lord_extraction_reading, 0.12).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__lord_extraction_reading, feudal_oath_reciprocity__vassal_coordination_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__lord_extraction_reading, feudal_oath_reciprocity__ecclesiastical_mediation_reading).

% DUAL FORMULATION NOTE:
% This constraint (lord_extraction_reading) and its siblings (vassal_coordination_reading, ecclesiastical_mediation_reading) decompose the 'feudal oath' label into structurally distinct claims. This reading has high ε (0.82) and snare classification; vassal_coordination_reading claims low ε and rope/tangled_rope; ecclesiastical_mediation_reading claims bounded extraction via sacramental limits. They share the kernel but instantiate different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
