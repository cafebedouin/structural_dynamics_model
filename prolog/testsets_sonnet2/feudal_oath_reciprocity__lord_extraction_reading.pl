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
 *   constraint_id: feudal_oath_reciprocity__lord_extraction_reading
 *   human_readable: Feudal Oath as Unbounded Lord Extraction (Rebellion-Threshold Reading)
 *   domain: Medieval Political Economy / Legal History / Institutional Analysis
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested feudal-oath kernel:
 *   the reading under which the mutual oath of homage and fealty functions
 *   primarily as a license for the lord to extract whatever the vassal
 *   hierarchy can be made to bear, bounded not by any fixed charter term but
 *   by the practical ceiling of rebellion capacity. Under this reading, the
 *   coordination story (mutual defense, land security) is real at the
 *   founding moment but decays into cover as lords learn to interpret
 *   ambiguous fealty obligations expansively, escalate scutage and aids, and
 *   use wardship/marriage rights as an additional extraction channel over
 *   lineages too weak to resist. This is a distinct constraint from the
 *   vassal_coordination_reading (which holds the same oath text produces
 *   fixed, charter-bounded obligations) and from the
 *   ecclesiastical_mediation_reading (which holds Christian charity doctrine
 *   caps secular extraction) — the epsilon here is high and stable-to-rising
 *   because, on this reading's own terms, no textual or doctrinal ceiling
 *   actually binds the lord; only the empirical threshold of coordinated
 *   violent resistance does.
 *
 * KEY AGENTS:
 *   - landholding_liege_lords: primary beneficiary and agenda-setter (institutional/arbitrage) — sets and reinterprets extraction demands, controls adjudication
 *   - enfeoffed_knights: primary target (moderate/constrained) — bears escalating unscheduled demands, exit costs forfeiture of status and land
 *   - sub_tenant_peasant_households: downstream target (powerless/trapped) — absorbs extraction passed down the chain, has no standing under the oath at all
 *   - minor_vassal_lineages: structurally weakest target (powerless/trapped) — absorbs wardship/marriage extraction with no bargaining leverage
 *   - royal_overlord: secondary beneficiary and systemic observer (institutional/analytical) — shares the extraction logic but intervenes when destabilization threatens the crown
 *   - rebellious_baronial_coalition: excluded voice with power (organized/constrained) — the only real check, activating post-hoc via revolt rather than through the oath's ordinary machinery
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__lord_extraction_reading, 0.86).
domain_priors:suppression_score(feudal_oath_reciprocity__lord_extraction_reading, 0.79).
domain_priors:theater_ratio(feudal_oath_reciprocity__lord_extraction_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, extractiveness, 0.86).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__lord_extraction_reading, snare).
narrative_ontology:human_readable(feudal_oath_reciprocity__lord_extraction_reading, "Feudal Oath as Unbounded Lord Extraction (Rebellion-Threshold Reading)").
narrative_ontology:topic_domain(feudal_oath_reciprocity__lord_extraction_reading, "Medieval Political Economy / Legal History / Institutional Analysis").

domain_priors:requires_active_enforcement(feudal_oath_reciprocity__lord_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__lord_extraction_reading, '4a18d89b-6a7a-405a-acf2-8bf48506fcb4').
narrative_ontology:cs_kernel_codification('4a18d89b-6a7a-405a-acf2-8bf48506fcb4', distributed).
narrative_ontology:cs_authority_grounding('4a18d89b-6a7a-405a-acf2-8bf48506fcb4', extraction).
narrative_ontology:cs_interpretation_layer_present('4a18d89b-6a7a-405a-acf2-8bf48506fcb4').
narrative_ontology:cs_reading_relation('4a18d89b-6a7a-405a-acf2-8bf48506fcb4', feudal_oath_reciprocity__vassal_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('4a18d89b-6a7a-405a-acf2-8bf48506fcb4', feudal_oath_reciprocity__ecclesiastical_mediation_reading, coexists_with).
narrative_ontology:cs_axiom('4a18d89b-6a7a-405a-acf2-8bf48506fcb4', foundational, fealty_grants_unbounded_interpretive_discretion_to_lord).
narrative_ontology:cs_axiom_status(fealty_grants_unbounded_interpretive_discretion_to_lord, holdable).
narrative_ontology:cs_axiom_grounding('4a18d89b-6a7a-405a-acf2-8bf48506fcb4', fealty_grants_unbounded_interpretive_discretion_to_lord, conventional).
narrative_ontology:cs_axiom('4a18d89b-6a7a-405a-acf2-8bf48506fcb4', secondary, rebellion_capacity_is_the_only_operative_limit_on_extraction).
narrative_ontology:cs_axiom_status(rebellion_capacity_is_the_only_operative_limit_on_extraction, holdable).
narrative_ontology:cs_axiom_grounding('4a18d89b-6a7a-405a-acf2-8bf48506fcb4', rebellion_capacity_is_the_only_operative_limit_on_extraction, empirically_contingent).
narrative_ontology:cs_reference_frame('4a18d89b-6a7a-405a-acf2-8bf48506fcb4', unilateral_lordly_discretion_over_fealty).
narrative_ontology:cs_drift_state('4a18d89b-6a7a-405a-acf2-8bf48506fcb4', high_medieval_baronial_crisis, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('4a18d89b-6a7a-405a-acf2-8bf48506fcb4', '').
narrative_ontology:cs_kernel_id(feudal_oath_reciprocity__lord_extraction_reading, feudal_oath_reciprocity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__lord_extraction_reading, landholding_liege_lords).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__lord_extraction_reading, enfeoffed_knights).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__lord_extraction_reading, sub_tenant_peasant_households).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__lord_extraction_reading, minor_vassal_lineages).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__lord_extraction_reading, royal_overlord).
narrative_ontology:constraint_vindicates(feudal_oath_reciprocity__lord_extraction_reading, doctrine_of_fealty_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Grants fiefs in exchange for the oath of homage and fealty, then sets the actual demands for military service, scutage, aids, wardship revenue, and labor dues each year according to need and opportunity rather than fixed schedule. Interprets the oath's ambiguity in his own favor when demands are challenged, and can escheat a vassal's holding for perceived breach of fealty, which functions as the ultimate enforcement lever. Holds the courts that adjudicate disputes about the oath's meaning.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, landholding_liege_lords, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__lord_extraction_reading, landholding_liege_lords, beneficiary).

% Hold land conditionally on service and owe whatever the lord construes fealty to require in a given year — military levy beyond customary terms, extraordinary aids, or forced attendance at court at personal cost. Formal exit means forfeiting the fief and often social standing; appeal runs through the lord's own court or, rarely, upward to a king who has identical incentives. Some retain enough retinue to negotiate terms informally, but cannot refuse outright without risking charges of breach of fealty.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, enfeoffed_knights, payer,
    moderate, biographical, constrained, regional).

% Bear the downstream weight of the lord's extraction as it passes through the knight's own need to meet obligations upward: labor services, tallage, and in-kind renders are increased whenever the knight himself is squeezed. Cannot invoke the oath at all, since they are not party to it, and have no court of appeal within the system that produced their burden. Flight to another lord's land or to a town is the only exit, and it is dangerous and often blocked by lords acting in mutual interest to prevent poaching of labor.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, sub_tenant_peasant_households, payer,
    powerless, biographical, trapped, local).

% Hold small or marginal fiefs with little military value to offer in bargaining, and so absorb wardship and marriage-right extraction most heavily — the lord's right to control heirs' marriages and manage estates during minority is used to extract fees and favorable matches without effective resistance. Generational continuity of the lineage's landholding depends entirely on the lord's forbearance.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, minor_vassal_lineages, payer,
    powerless, generational, trapped, local).

% Sits atop the same structure as ultimate liege and shares the extraction logic when acting as a lord of lords, but also has an interest in restraining the worst extraction by great lords to prevent baronial rebellion from threatening the crown. Intervenes selectively — through charters, inquests, or adjudication — when extraction destabilizes the wider order rather than out of concern for individual vassals.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, royal_overlord, observer,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__lord_extraction_reading, royal_overlord, beneficiary).

% Represents the only structural check this reading recognizes: when extraction crosses a threshold, vassals with sufficient coordinated military capacity can withhold service or rise in arms, as at Runnymede. This is not a voice built into the system's ordinary operation — it is an extra-legal correction that only activates after extraction has already occurred and only for vassals wealthy or numerous enough to coordinate; smaller vassals and peasants below them have no equivalent recourse.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, rebellious_baronial_coalition, excluded,
    organized, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feudal_oath_reciprocity__lord_extraction_reading, landholding_liege_lords).
narrative_ontology:fixing_cost_class(feudal_oath_reciprocity__lord_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The oath nominally coordinates a mutual-defense and land-tenure system: the lord provides protection and land tenure security, the vassal provides military service and loyalty. Under this reading, that coordination function is real in origin but has become the pretext under which extraction operates, since the lord alone interprets what fealty requires.
% TRANSFER_FUNCTION: Moves military service, cash renders (scutage, aids, reliefs), labor, and control over marriage/wardship from vassals and their dependents upward to the lord, in quantities the lord sets unilaterally within the elastic bound of what the vassal can be made to bear before open rebellion becomes viable.
% ABSENT_VOICES: Sub-tenant peasants and minor lineages have no standing to invoke the oath at all — they are not parties to it, yet bear its downstream costs. The rebellious baronial coalition is the closest thing to a voice with power, but it speaks only for vassals wealthy enough to organize; it is excluded from the ordinary operation of the constraint and appears only as a rupture.
% DISAPPEARANCE_RATIONALE: If the oath's extractive authority vanished overnight, the entire upward flow of scutage, aids, unscheduled levies, and wardship revenue would stop; lords would lose their principal revenue and coercive lever over vassals, knights would retain land without unpredictable liability, and the elaborate court and escheat apparatus that exists to enforce compliance would become unnecessary almost immediately.
% FOUNDING_PROBLEM: Post-Carolingian collapse left no reliable central military or administrative capacity; the oath structure was built to bind mounted warriors to land and to a lord in exchange for protection, solving a genuine problem of decentralized defense and administration in the absence of a functioning state.
% FOUNDING_PROBLEM_CORROBORATION: Lords and their court clerks attest that customary service remains a live necessity requiring flexible interpretation of the oath. Independent evidence — chronicle accounts of baronial revolt (1215 and its antecedents), ecclesiastical commentary condemning 'unjust exactions,' and the drafting of charters explicitly to fix obligations against arbitrary lordly demand — corroborates from outside the lords' own ranks that the original defense-and-administration problem had, by the practices this reading describes, been substantially superseded by rent-extraction untethered to any defensive necessity.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__lord_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__lord_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__lord_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(feudal_oath_reciprocity__lord_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feudal_oath_reciprocity__lord_extraction_reading, 0.86, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored high (0.86 at interval end) and rising because, under this reading, nothing textually or doctrinally caps what the lord can demand under the fealty relationship — the only real ceiling is the vassal's or coalition's capacity to withhold service or revolt, which is a threshold event, not a routine constraint. Suppression is high (0.79) because escheat, forfeiture, and the lord's own courts function as active enforcement against noncompliance; it is not scaled by scope per the framework's rule — it is authored directly as the raw coercive apparatus available to enforce compliance. Theater ratio is moderate (0.42) and rising, reflecting that as extraction intensifies, more of the apparatus (court ritual, homage ceremony, formal record-keeping of 'customary' dues that are no longer actually customary) becomes performative legitimation layered atop what is functionally unilateral extraction. accessibility_collapse (0.62) and resistance (0.71) reflect that alternatives are not fully foreclosed — flight, revolt, and appeal to the crown remain theoretically available — but are costly and rare enough that most vassals experience the arrangement as a closed system most of the time.
 *
 * PERSPECTIVAL GAP:
 *   The lord's seat and the knight's/peasant's seats diverge sharply on this reading: from the lord's position, exercising discretion over fealty's meaning is simply governing — a prerogative inherent to lordship, exercised because circumstances (war, marriage, minority) genuinely require flexible response. From the payer seats, the identical discretion is experienced as an unbounded extraction license whose only real limit is what will trigger armed resistance. The royal overlord occupies a third position: benefiting from the same logic upward while needing to cap its excesses downward to prevent the coalition problem from reaching him.
 *
 * DIRECTIONALITY LOGIC:
 *   landholding_liege_lords sit at the full-beneficiary end: they set demands, adjudicate disputes over those demands in their own courts, and hold the ultimate escheat lever — d is derived near 0 (subsidized) from the beneficiary declaration and their arbitrage-grade exit (they can always find another vassal to enfeoff). enfeoffed_knights and sub_tenant_peasant_households and minor_vassal_lineages are declared victims with trapped or constrained exit, driving d toward the full-target end — amplified further for the peasants and minor lineages because their exit options (trapped, local scope) leave them with no arbitrage at all. The royal_overlord is given a directionality_override below because the default derivation (from beneficiary declaration alone) would treat him identically to the primary lords, when in fact his structural position — needing to prevent baronial coalitions from threatening the crown — makes him a partial, self-interested check rather than a pure co-beneficiary.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists conflating founding coordination function with present extraction: the founding_problem (decentralized defense in a collapsed administrative order) is authored as historically real, but founding_problem_status is marked contested because, on this reading's own account, the defense function had been substantially superseded by rent-extraction well before the practices described here stabilized — corroborated by chronicle and charter evidence external to the lords' own self-justification. Treating the constraint as still purely functional coordination (as the vassal_coordination_reading does) would mislabel entrenched extraction as bounded reciprocity; treating it as having never had a coordination function would erase the genuine founding problem. The engine's per-seat computation is what should surface this: the lord's seat may compute as something closer to a legitimate hierarchy, while the payer seats compute as a snare — that divergence is the finding, not an error to reconcile.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reciprocity_kernel_reading_indeterminacy,
    'Does the feudal oath''s actual historical operation match the lord_extraction_reading (unbounded, rebellion-threshold-limited extraction), the vassal_coordination_reading (fixed charter-bounded obligations), or the ecclesiastical_mediation_reading (doctrine-capped extraction) — or did its operation vary substantially by region, period, and individual lord?',
    'Comparative analysis of manorial and court rolls across regions and centuries: frequency and magnitude of extraordinary levies relative to customary baseline, frequency of successful vassal appeal to church or crown courts against lordly demands, and documented instances of demands scaling with lord''s need versus staying fixed by charter.',
    'If the historical record shows extraction consistently tracking the lord''s unilateral need rather than fixed terms, this reading is well-supported as the dominant operative pattern; if charter terms were generally honored and appeal mechanisms were regularly effective, the vassal_coordination_reading better describes the actual arrangement and this story''s high epsilon would be reading-specific rather than descriptive of typical practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_kernel_reading_indeterminacy, empirical, 'Which kernel reading best matches actual historical operation, and whether operation varied by region/period.').

omega_variable(
    rebellion_threshold_as_bound_or_absence_of_bound,
    'Is ''bounded only by rebellion threshold'' itself a meaningful structural limit (a real, if crude, ceiling) or is it functionally equivalent to no bound at all, given how rarely coordinated rebellion was actually achievable?',
    'Historical frequency analysis of successful baronial coalitions relative to total instances of escalating lordly demand — if successful checks were rare relative to demand escalation events, the ''bound'' is closer to theoretical than operative.',
    'A rare, hard-to-coordinate check functions closer to unbounded extraction for classification purposes; a frequently-exercised check would push this reading''s extraction ceiling downward and closer to the tangled_rope range rather than pure snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rebellion_threshold_as_bound_or_absence_of_bound, conceptual, 'Whether the rebellion threshold is a real structural bound or a nominal one.').

omega_variable(
    kernel_disagreement_location,
    'Where, precisely, do the three sibling readings of the feudal oath disagree — is it about what the oath TEXT says, about how courts actually enforced it, or about what background doctrine (ecclesiastical, customary, or royal) was understood to constrain it?',
    'Textual analysis of surviving oath formulas and charters compared against court records and canonical/ecclesiastical commentary from the same period and region, to locate whether the disagreement is textual, enforcement-practical, or doctrinal.',
    'If the disagreement is primarily doctrinal (whether church teaching effectively capped lordly demand), the ecclesiastical_mediation_reading and this reading are compatible at the textual level and diverge only on doctrine''s practical force. If the disagreement is enforcement-practical (whether charter terms were actually honored), this reading and the vassal_coordination_reading are the ones in direct tension.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_disagreement_location, conceptual, 'Locating the specific structural element the three kernel readings disagree about.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__lord_extraction_reading, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(feud_tr_t50, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 50, 0.25).
narrative_ontology:measurement(feud_tr_t100, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 100, 0.3).
narrative_ontology:measurement(feud_tr_t150, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 150, 0.34).
narrative_ontology:measurement(feud_tr_t200, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 200, 0.38).
narrative_ontology:measurement(feud_tr_t250, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 250, 0.4).
narrative_ontology:measurement(feud_tr_t300, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 300, 0.42).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(feud_be_t50, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 50, 0.63).
narrative_ontology:measurement(feud_be_t100, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 100, 0.71).
narrative_ontology:measurement(feud_be_t150, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 150, 0.79).
narrative_ontology:measurement(feud_be_t200, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 200, 0.83).
narrative_ontology:measurement(feud_be_t250, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 250, 0.85).
narrative_ontology:measurement(feud_be_t300, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 300, 0.86).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t0, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(feud_su_t50, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 50, 0.58).
narrative_ontology:measurement(feud_su_t100, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 100, 0.65).
narrative_ontology:measurement(feud_su_t150, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 150, 0.72).
narrative_ontology:measurement(feud_su_t200, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 200, 0.76).
narrative_ontology:measurement(feud_su_t250, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 250, 0.78).
narrative_ontology:measurement(feud_su_t300, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 300, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__lord_extraction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(feudal_oath_reciprocity__lord_extraction_reading, 0.1).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__lord_extraction_reading, feudal_oath_reciprocity__vassal_coordination_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__lord_extraction_reading, feudal_oath_reciprocity__ecclesiastical_mediation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the feudal_oath_reciprocity kernel. vassal_coordination_reading treats the same oath as producing fixed, charter-enforced reciprocal obligations (low-to-moderate epsilon, closer to tangled_rope or rope). ecclesiastical_mediation_reading treats the oath as bound by sacramental/charity doctrine that caps secular extraction (moderate epsilon). This story (lord_extraction_reading) authors the highest epsilon of the three, on the premise that no textual or doctrinal limit actually binds the lord's discretion — only the empirical threshold of coordinated resistance does. All three share the same underlying kernel text and historical institution but instantiate structurally distinct constraints per the epsilon-invariance principle; they must not be averaged or treated as observation angles on one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(feudal_oath_reciprocity__lord_extraction_reading, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
