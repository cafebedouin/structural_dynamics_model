% ============================================================================
% CONSTRAINT STORY: feudal_oath_reciprocity__lord_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   kernel: the oath of homage and fealty is treated as authorizing the lord
 *   to extract military service, aids, and dues from vassals without a fixed
 *   ceiling, bounded in practice only by the vassal's physical capacity to
 *   render service or by the threat of coalition rebellion. This is one of
 *   three structurally distinct constraints sharing the same textual kernel
 *   (the oath ceremony and its formulaic language) — the vassal-coordination
 *   reading treats the same oath as fixing bounded, charter-enforced
 *   reciprocal obligations, and the ecclesiastical-mediation reading treats
 *   it as bound by sacramental conscience and charity. Under ε-invariance,
 *   these are not the same constraint measured three ways; they are three
 *   constraints because their extraction, victim sets, and enforcement
 *   mechanisms differ sharply. This reading alone is authored here; the
 *   siblings are separate constraint_ids linked via
 *   network.affects_constraints, per Rule 1.
 *
 * KEY AGENTS:
 *   - landed_lords: agenda_setter (institutional/arbitrage) — administers and escalates extraction under the oath
 *   - lordly_household_retinue: beneficiary (powerful/constrained) — provisioned by the upward flow
 *   - enfeoffed_knights: payer (moderate/constrained) — bears expanding military and financial obligations
 *   - tenant_vassals: payer (powerless/trapped) — no coalition leverage, no meaningful exit
 *   - vassal_peasant_dependents: payer (powerless/trapped) — bears downstream squeeze with no oath standing at all
 *   - ecclesiastical_courts: excluded (institutional/analytical) — asserts a rival limiting claim with no binding force here
 *   - royal_or_overlord_authority: observer (institutional/analytical) — weak enforcement reach, shares the same extraction logic downward
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__lord_extraction_reading, 0.87).
domain_priors:suppression_score(feudal_oath_reciprocity__lord_extraction_reading, 0.81).
domain_priors:theater_ratio(feudal_oath_reciprocity__lord_extraction_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, extractiveness, 0.87).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__lord_extraction_reading, snare).
narrative_ontology:human_readable(feudal_oath_reciprocity__lord_extraction_reading, "Feudal Oath as Unbounded Lordly Extraction Authority").
narrative_ontology:topic_domain(feudal_oath_reciprocity__lord_extraction_reading, "medieval_political_economy/legal_history").

domain_priors:requires_active_enforcement(feudal_oath_reciprocity__lord_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__lord_extraction_reading, 'a4af6110-2cd7-4a65-810e-16852dff0d2d').
narrative_ontology:cs_kernel_codification('a4af6110-2cd7-4a65-810e-16852dff0d2d', formalized).
narrative_ontology:cs_authority_grounding('a4af6110-2cd7-4a65-810e-16852dff0d2d', extraction).
narrative_ontology:cs_interpretation_layer_present('a4af6110-2cd7-4a65-810e-16852dff0d2d').
narrative_ontology:cs_reading_relation('a4af6110-2cd7-4a65-810e-16852dff0d2d', feudal_oath_reciprocity__vassal_coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('a4af6110-2cd7-4a65-810e-16852dff0d2d', feudal_oath_reciprocity__ecclesiastical_mediation_reading, forecloses).
narrative_ontology:cs_axiom('a4af6110-2cd7-4a65-810e-16852dff0d2d', foundational, oath_language_admits_no_implicit_ceiling).
narrative_ontology:cs_axiom_status(oath_language_admits_no_implicit_ceiling, holdable).
narrative_ontology:cs_axiom_grounding('a4af6110-2cd7-4a65-810e-16852dff0d2d', oath_language_admits_no_implicit_ceiling, conventional).
narrative_ontology:cs_axiom('a4af6110-2cd7-4a65-810e-16852dff0d2d', foundational, lordly_discretion_supersedes_written_custumal_terms).
narrative_ontology:cs_axiom_status(lordly_discretion_supersedes_written_custumal_terms, holdable).
narrative_ontology:cs_axiom_grounding('a4af6110-2cd7-4a65-810e-16852dff0d2d', lordly_discretion_supersedes_written_custumal_terms, conventional).
narrative_ontology:cs_axiom('a4af6110-2cd7-4a65-810e-16852dff0d2d', secondary, secular_fief_law_unreviewable_by_ecclesiastical_courts).
narrative_ontology:cs_axiom_status(secular_fief_law_unreviewable_by_ecclesiastical_courts, holdable).
narrative_ontology:cs_axiom_grounding('a4af6110-2cd7-4a65-810e-16852dff0d2d', secular_fief_law_unreviewable_by_ecclesiastical_courts, conventional).
narrative_ontology:cs_reference_frame('a4af6110-2cd7-4a65-810e-16852dff0d2d', customary_military_tenure_baseline).
narrative_ontology:cs_drift_state('a4af6110-2cd7-4a65-810e-16852dff0d2d', high_medieval_administrative_intensification, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a4af6110-2cd7-4a65-810e-16852dff0d2d', '').
narrative_ontology:cs_kernel_id(feudal_oath_reciprocity__lord_extraction_reading, feudal_oath_reciprocity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__lord_extraction_reading, landed_lords).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__lord_extraction_reading, lordly_household_retinue).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__lord_extraction_reading, enfeoffed_knights).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__lord_extraction_reading, tenant_vassals).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__lord_extraction_reading, vassal_peasant_dependents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the oath of homage and fealty, sets the schedule and quantum of military service, aids (relief, wardship, marriage fines, scutage), and hospitality obligations owed by vassals. Adjusts demands upward as vassal capacity allows and invokes the oath's language of loyalty to justify escalation. Faces no fixed ceiling on extraction other than what the vassal can physically render or what provokes open revolt.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, landed_lords, agenda_setter,
    institutional, generational, arbitrage, regional).

% Household knights, stewards, and officials who administer the lord's demands and are provisioned from the extracted service and dues. They benefit from the flow of resources upward and have professional incentive to enforce, not moderate, the lord's claims.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, lordly_household_retinue, beneficiary,
    powerful, biographical, constrained, regional).

% Hold land in exchange for military service but face demands that expand beyond the original quantum — extended campaign duration, additional aids, arbitrary wardship and marriage fines when heirs are minors. Renouncing the fief means losing land and status; the only recourse beyond compliance is joining an armed coalition of fellow vassals, which is slow to form and risky to individuals.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, enfeoffed_knights, payer,
    moderate, biographical, constrained, local).

% Lesser vassals and sub-tenants with no independent armed following, who render labor service, produce-in-kind, and occasional military levy. They have no meaningful exit — abandoning the tenancy means destitution — and no coalition leverage since they lack the retinues that make knightly rebellion possible.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, tenant_vassals, payer,
    powerless, biographical, trapped, local).

% Peasants bound to vassal-held land who ultimately bear the downstream cost when a vassal, squeezed by the lord, intensifies demands on his own dependents to meet obligations upward. They have no oath relationship to the lord at all and no voice in a structure that nonetheless determines their burden.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, vassal_peasant_dependents, payer,
    powerless, biographical, trapped, local).

% Church courts and clergy who, under the rival ecclesiastical reading, would assert that the oath is bound by sacramental conscience and charity limiting extraction — but under this extraction-authorizing reading, their claims are treated as advisory sermonizing with no binding force on secular fief administration, and they are structurally excluded from adjudicating the lord's demands.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, ecclesiastical_courts, excluded,
    institutional, generational, analytical, regional).

% A king or paramount lord above the lord in question, who may intervene if extraction destabilizes the broader hierarchy (mass vassal flight, coalition rebellion) but otherwise has weak enforcement reach into local lord-vassal relations and often shares the same extraction logic downward from his own vassals.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, royal_or_overlord_authority, observer,
    institutional, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feudal_oath_reciprocity__lord_extraction_reading, landed_lords).
narrative_ontology:fixing_cost_class(feudal_oath_reciprocity__lord_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In its residual coordination aspect, the oath still identifies who owes military and material support to whom in a decentralized polity lacking a standing bureaucracy or currency economy — it names a chain of obligation where none would otherwise exist.
% TRANSFER_FUNCTION: Moves military service, agricultural surplus, labor, and cash substitutes (scutage, aids, relief, wardship revenue, marriage fines) from vassals and their dependents upward to the lord and his household, with no fixed cap — the ceiling is set by what the vassal can render before collapse or revolt, not by the oath's original terms.
% ABSENT_VOICES: Tenant vassals and vassal-peasant dependents have no forum to contest escalating demands; ecclesiastical courts assert a competing charity-based limit but are excluded from binding secular fief adjudication under this reading. Lesser vassals lack the armed retinues that let knightly tenants threaten coalition resistance.
% DISAPPEARANCE_RATIONALE: If the lord's unbounded extraction authority vanished overnight, vassals would retain land under fixed, negotiated terms (as the coordination reading holds), household retinues would lose their provisioning surplus, and the entire upward flow of aids, relief, and unscheduled levies would collapse to whatever bounded service the original charter specified — a fundamentally different political economy.
% FOUNDING_PROBLEM: In the absence of centralized taxation, standing armies, or reliable currency, a lord needed a durable mechanism to raise military force and revenue from land he could not personally administer — the oath solved the problem of converting land grants into recurring, enforceable service.
% FOUNDING_PROBLEM_CORROBORATION: Lords and their chroniclers attest the oath's continuing necessity to maintain military readiness. Independent evidence — vassal petitions to overlords, chronicled rebellions (e.g., baronial revolts against escalating relief and wardship abuses), and ecclesiastical writings condemning lordly avarice — corroborates from outside the beneficiary set that the founding military-coordination problem had, by the high medieval period, become a pretext for extraction well beyond what raising an army required.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__lord_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__lord_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__lord_extraction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness rises across the interval (0.48 to 0.87) reflecting the historically documented pattern of lords converting fixed feudal incidents (relief, wardship, marriage fines, scutage) into escalating, arbitrarily-set impositions as administrative capacity to enforce and record demands grew. Suppression is high (0.81) because maintaining the unbounded reading requires active military and legal coercion — courts controlled by the lord, garrisoned castles, and the credible threat of forfeiture. Theater ratio remains comparatively low (0.28): the extraction is substantially real, not performative, though a rising theater component reflects increasing ceremonial reaffirmation of fealty used to paper over widening gaps between oath language and actual demand.
 *
 * PERSPECTIVAL GAP:
 *   From the lord's seat, the arrangement is a continuation of legitimate, ceremonially-grounded lordship — the oath's language of loyalty and service is read expansively because nothing in it names a ceiling. From the tenant vassal and peasant-dependent seats, the same structure is naked extraction with no meaningful recourse. The engine computes this divergence from the structural power/exit data; the two seats are not disagreeing about facts but experiencing genuinely different structural positions relative to the same enforcement apparatus.
 *
 * DIRECTIONALITY LOGIC:
 *   Landed lords and their retinues sit at the beneficiary end: they administer the demand schedule and consume its proceeds. Enfeoffed knights sit closer to target but retain some leverage (moderate power, constrained rather than trapped exit) via potential coalition threat — this is why baronial revolts are a recurring historical check on this reading. Tenant vassals and vassal-peasant dependents sit at the full-target end: powerless, trapped, and doubly removed from any oath-based standing to contest demands, since the peasants were never party to the oath at all yet bear its downstream cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding military-coordination problem (raising force without a standing army or currency) was genuinely live in the arrangement's origin. Under this reading, that founding problem has become substantially dead as a justification even where it remains rhetorically invoked — administrative and financial extraction (relief, wardship, marriage fines) exceeds what raw military coordination requires, and the persistence of the unbounded-extraction reading past the point of military necessity is exactly what the founding_problem_status: contested and the corroboration from independent baronial-revolt evidence are meant to surface, rather than accepting the lord's own military-necessity narrative uncritically.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'The feudal oath kernel (feudal_oath_reciprocity) admits at least three structurally distinct readings: this lord-extraction reading (unbounded, bounded only by vassal capacity/rebellion threshold), the vassal-coordination reading (fixed obligations enforced by charter text), and the ecclesiastical-mediation reading (bounded by sacramental charity). Which reading governed in a given lordship at a given moment was itself a site of active contest, not a settled fact.',
    'Charter and custumal analysis across regions and periods: where written charters specified fixed quanta of service and lords were shown in court rolls to be held to them, the coordination reading better fits; where relief/wardship/marriage fines varied lord-to-lord with no textual ceiling and escalated with lordly power, the extraction reading fits; where ecclesiastical courts successfully intervened to cap secular demands, the mediation reading fits.',
    'If regional evidence shows charter enforcement was typically effective, this extraction reading over-generalizes from exceptional/crisis cases; if charter enforcement was rare and toothless, this reading captures the modal historical reality and the coordination reading is the exceptional case.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Which of the three sibling readings of the feudal oath kernel actually predominated is contested and period/region-dependent.').

omega_variable(
    rebellion_threshold_as_ceiling,
    'Is the ''rebellion threshold'' bounding extraction in this reading a genuine structural ceiling (comparable to a legal limit) or merely a description of when the system breaks down entirely rather than self-correcting?',
    'Compare documented cases of successful vassal coalition resistance (e.g., Magna Carta-type baronial confrontations) against cases of extraction continuing unchecked until total institutional collapse or conquest — a genuine ceiling would show recurring successful moderation; a breakdown-only pattern would show extraction continuing until catastrophic rupture with no intermediate correction.',
    'If rebellion functions as a genuine recurring ceiling, effective extraction is somewhat self-limiting and the snare classification should account for periodic correction; if it functions only as catastrophic collapse, the unbounded character of this reading is even starker than the base metrics suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rebellion_threshold_as_ceiling, empirical, 'Whether vassal rebellion functioned as a recurring corrective mechanism or only as terminal system failure.').

omega_variable(
    ecclesiastical_claim_enforceability,
    'Under this extraction reading, ecclesiastical courts'' charity-based limits are treated as non-binding — but was this truly the case, or did church courts and excommunication threats sometimes exercise real practical leverage over lordly behavior even absent formal jurisdiction over fief law?',
    'Survey documented instances where lords moderated demands following ecclesiastical censure, penance requirements, or threatened excommunication, versus instances where such threats were ignored without consequence.',
    'If ecclesiastical pressure had real practical bite, the pure lord-extraction reading overstates the absence of any binding limit and some hybrid with the ecclesiastical-mediation reading is closer to historical fact in those cases; if ecclesiastical pressure was consistently ignored, this reading''s exclusion of church authority is well-founded.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ecclesiastical_claim_enforceability, empirical, 'Whether the exclusion of ecclesiastical authority from binding force on secular extraction is historically accurate or an artifact of this reading''s framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__lord_extraction_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(feud_tr_t40, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement(feud_tr_t80, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 80, 0.18).
narrative_ontology:measurement(feud_tr_t120, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 120, 0.22).
narrative_ontology:measurement(feud_tr_t160, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 160, 0.25).
narrative_ontology:measurement(feud_tr_t200, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 200, 0.28).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(feud_be_t40, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(feud_be_t80, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 80, 0.68).
narrative_ontology:measurement(feud_be_t120, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 120, 0.76).
narrative_ontology:measurement(feud_be_t160, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 160, 0.83).
narrative_ontology:measurement(feud_be_t200, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 200, 0.87).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t0, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(feud_su_t40, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement(feud_su_t80, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 80, 0.68).
narrative_ontology:measurement(feud_su_t120, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 120, 0.73).
narrative_ontology:measurement(feud_su_t160, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 160, 0.78).
narrative_ontology:measurement(feud_su_t200, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 200, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__lord_extraction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(feudal_oath_reciprocity__lord_extraction_reading, 0.1).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__lord_extraction_reading, feudal_oath_reciprocity__vassal_coordination_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__lord_extraction_reading, feudal_oath_reciprocity__ecclesiastical_mediation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the feudal_oath_reciprocity kernel. The vassal_coordination_reading treats the same oath ceremony and formulaic language as establishing fixed, charter-bounded reciprocal obligations — a rope-like coordination mechanism with negligible extraction beyond agreed terms. The ecclesiastical_mediation_reading treats the oath as bound by sacramental conscience and charity, limiting secular extraction through church court intervention. This story (lord_extraction_reading) treats the oath as authorizing extraction bounded only by vassal capacity and rebellion risk — a high-ε snare. All three share the identical textual kernel (the homage/fealty ceremony) but diverge sharply in claimed enforceability, victim set, and ε; per the ε-invariance principle they are authored as three separate constraint_ids rather than one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(feudal_oath_reciprocity__lord_extraction_reading, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
