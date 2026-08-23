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
 *   human_readable: Feudal Oath as Lord Extraction Mechanism
 *   domain: medieval_political_economy
 *
 * SUMMARY:
 *   This constraint instantiates the lord_extraction_reading of the contested
 *   kernel feudal_oath_reciprocity. Under this reading, the feudal oath is
 *   not a reciprocal coordination device but a structurally extractive bond
 *   that authorizes the lord to extract surplus up to the vassal's rebellion
 *   threshold. The reciprocal rhetoric of homage and protection operates as
 *   legitimizing theater for a relationship whose effective limit is force,
 *   not charter. The sibling readingsâvassal_coordination_reading and
 *   ecclesiastical_mediation_readingâtreat the same oath as a bounded,
 *   reciprocal, or sacramentally limited obligation. This JSON isolates the
 *   lord-extraction reading as a clean, Îµ-invariant constraint per DP-001.
 *
 * KEY AGENTS:
 *   - feudal_lords: Primary beneficiary and agenda-setter (powerful/mobile) â sets oath terms, controls manorial courts, and captures the extracted surplus
 *   - vassals: Primary payer (moderate/constrained) â bears extraction under tenurial bond; exit means forfeiture
 *   - peasant_cultivators: Secondary payer (powerless/trapped) â supplies actual labor and surplus; structurally invisible to the oath text
 *   - ecclesiastical_authorities: Excluded voice (institutional/constrained) â would impose sacramental limits but is sidelined in this reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__lord_extraction_reading, 0.85).
domain_priors:suppression_score(feudal_oath_reciprocity__lord_extraction_reading, 0.78).
domain_priors:theater_ratio(feudal_oath_reciprocity__lord_extraction_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__lord_extraction_reading, snare).
narrative_ontology:human_readable(feudal_oath_reciprocity__lord_extraction_reading, "Feudal Oath as Lord Extraction Mechanism").
narrative_ontology:topic_domain(feudal_oath_reciprocity__lord_extraction_reading, "medieval_political_economy").

domain_priors:requires_active_enforcement(feudal_oath_reciprocity__lord_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__lord_extraction_reading, 'ba16182f-ac65-49c4-b642-1e346b657320').
narrative_ontology:cs_kernel_codification('ba16182f-ac65-49c4-b642-1e346b657320', fixed_text).
narrative_ontology:cs_authority_grounding('ba16182f-ac65-49c4-b642-1e346b657320', extraction).
narrative_ontology:cs_interpretation_layer_present('ba16182f-ac65-49c4-b642-1e346b657320').
narrative_ontology:cs_reading_relation('ba16182f-ac65-49c4-b642-1e346b657320', feudal_oath_reciprocity__vassal_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('ba16182f-ac65-49c4-b642-1e346b657320', feudal_oath_reciprocity__ecclesiastical_mediation_reading, influences).
narrative_ontology:cs_axiom('ba16182f-ac65-49c4-b642-1e346b657320', foundational, oath_as_lordly_extraction_instrument).
narrative_ontology:cs_axiom_status(oath_as_lordly_extraction_instrument, holdable).
narrative_ontology:cs_axiom_grounding('ba16182f-ac65-49c4-b642-1e346b657320', oath_as_lordly_extraction_instrument, conventional).
narrative_ontology:cs_axiom('ba16182f-ac65-49c4-b642-1e346b657320', foundational, service_capacity_as_only_bound).
narrative_ontology:cs_axiom_status(service_capacity_as_only_bound, holdable).
narrative_ontology:cs_axiom_grounding('ba16182f-ac65-49c4-b642-1e346b657320', service_capacity_as_only_bound, empirically_contingent).
narrative_ontology:cs_reference_frame('ba16182f-ac65-49c4-b642-1e346b657320', lordly_dominion_framework).
narrative_ontology:cs_drift_state('ba16182f-ac65-49c4-b642-1e346b657320', high_medieval_customary_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ba16182f-ac65-49c4-b642-1e346b657320', '').
narrative_ontology:cs_kernel_id(feudal_oath_reciprocity__lord_extraction_reading, feudal_oath_reciprocity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__lord_extraction_reading, feudal_lords).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__lord_extraction_reading, vassals).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__lord_extraction_reading, peasant_cultivators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the terms of the oath, controls land tenure and the manor court, and directs the extraction of agricultural surplus, labor services, and military duty from those below. Their wealth and political power depend on maintaining the oath bond as a binding, hierarchal duty rather than a bargained exchange. Exit from the role is possible through dynastic failure or royal attainder, but the position itself is the source of extraction.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, feudal_lords, agenda_setter,
    powerful, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__lord_extraction_reading, feudal_lords, beneficiary).

% Swear homage and fealty in exchange for a fief, then render military service, counsel, and material surplus to the lord. Their practical exit is forfeiture of the fief and loss of status; commutation of service is occasional but set by the lord. They experience the oath as a one-sided dependency masked by reciprocal ritual.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, vassals, payer,
    moderate, biographical, constrained, regional).

% Perform labor services, pay rents in kind or cash, and submit to manorial jurisdiction. They are not party to the noble oath but bear its material weight. Flight to towns is possible but risky and legally penalized; most are bound to the land by custom and force. Their surplus is the ultimate source of what moves upward through the tenurial chain.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, peasant_cultivators, payer,
    powerless, immediate, trapped, local).

% Possess doctrinal authority to bind oaths sacramentally and to limit secular extraction through appeals to Christian charity and Truce of God movements. In this reading, their limiting voice is structurally sidelined by the secular lordship system; they remain in the domain but are not the operative check on extraction.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, ecclesiastical_authorities, excluded,
    institutional, generational, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feudal_oath_reciprocity__lord_extraction_reading, feudal_lords).
narrative_ontology:fixing_cost_class(feudal_oath_reciprocity__lord_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes localized military force and land tenure in a fragmented post-imperial landscape by binding armed followers to territorial lords through a personal oath, substituting for absent centralized state capacity.
% TRANSFER_FUNCTION: Moves agricultural surplus, labor service, and military duty from vassals and peasant cultivators to feudal lords, with the transfer volume set by the lord's assessment of the maximum sustainable extraction before flight or rebellion.
% ABSENT_VOICES: Peasant cultivators, who have no seat in the oath ceremony but supply the extracted surplus; ecclesiastical authorities, who would impose sacramental limits on secular extraction; and royal justiciars, who would enforce written charter bounds against arbitrary lordship.
% DISAPPEARANCE_RATIONALE: Without the oath-bond, localized military power dissolves into fragmented households, land tenure loses its enforcement backbone, and the flow of surplus to the noble class collapses; the political economy would reorganize around centralized taxation, wage labor, or slave estate production.
% FOUNDING_PROBLEM: Collapse of centralized state protection and revenue extraction in the post-Carolingian West; the need for localized military force and agrarian management in a politically fragmented landscape.
% FOUNDING_PROBLEM_CORROBORATION: Ecclesiastical chroniclers and royal jurists from outside the lordly beneficiary class attest that the protective function had atrophied by the high medieval period. Modern historiography (e.g., Bloch, Duby) corroborates that the oath had shifted from a coordination device for defense to a rent-extraction mechanism sustained by manorial enforcement.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__lord_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__lord_extraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__lord_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(feudal_oath_reciprocity__lord_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feudal_oath_reciprocity__lord_extraction_reading, 0.85, 'kimi-k2.6', 'none', direct).

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
 *   Base extractiveness is high (0.85) because historical render levels and labor dues approached the maximum sustainable before flight or revolt. Suppression (0.78) is high because the constraint persists only through manorial enforcement, legal penalties for flight, and military household coercion. Theater ratio (0.48) reflects the growing gap between reciprocal ritual language and actual extraction: the oath ceremony becomes performative cover. Accessibility collapse (0.62) captures the difficulty of exiting serfdom or tenurial bond, though not absolute. Resistance (0.55) reflects chronic but fragmented peasant and vassal resistance that rarely achieves systemic change. The temporal series show extraction and theater rising together as the founding protective function atrophies.
 *
 * PERSPECTIVAL GAP:
 *   The lordly seat experiences the oath as legitimate dominion and necessary order; the vassal and peasant seats experience it as an extractive ceiling on their productive capacity. The engine computes this divergence from the structural asymmetry in power, exit options, and beneficiary/victim roles. The authored claim (snare) does not adjudicate the dispute but names the structural reading from which the metrics are authored.
 *
 * DIRECTIONALITY LOGIC:
 *   Feudal lords are declared beneficiaries and agenda-setters with mobile exit options, placing them near the full-beneficiary end (low d). Vassals are declared payers with constrained exit, placing them in the mid-to-high target range. Peasant cultivators are payers with trapped exit and powerless status, placing them nearest the full-target end (high d). The directionality derivation chain produces strong seat divergence without override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâlocalized defense in a post-imperial vacuumâis dead by the high medieval period, while the arrangement persists as a rent-collection mechanism. The snare classification prevents the mandatrophy error of reading the oath as a scaffold (transitional support) or rope (genuine coordination): the protective justification is theater, and the actual function is extraction sustained by coercion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    lord_extraction_kernel_location,
    'Does the lord_extraction_reading capture the operative structural core of the feudal oath, or does the vassal_coordination_reading''s bounded reciprocity describe the constraint that actually governed behavior on the ground?',
    'Systematic comparison of charter render limits against actual manorial account render levels; archaeological and court-roll evidence of lordly violations of customary bounds.',
    'If actual behavior tracked the lord reading, the kernel''s effective classification is snare/tangled_rope; if it tracked the vassal reading, the lord reading is ideological cover and the operative constraint is coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lord_extraction_kernel_location, empirical, 'Whether the lord-extraction reading or the vassal-coordination reading describes the operative constraint').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is vassal and peasant compliance driven primarily by structural coercion (military force, legal penalties, land bondage) or by internalized ideological commitment to the honor-bound, hierarchical order?',
    'Comparative analysis of exit behavior when structural barriers are removed (e.g., flight to towns after legal enfranchisement) versus persistence of deference patterns.',
    'If internalized suppression dominates, effective extraction exceeds the structural measure because targets carry the constraint after exit; if structural, extraction is bounded by enforcement capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural versus internalized suppression in feudal compliance').

omega_variable(
    ecclesiastical_limit_bindingness,
    'Did ecclesiastical mediation structurally limit lordly extraction, or did it merely provide a theatrical legitimacy layer that left secular extraction unchanged?',
    'Review of episcopal court interventions in feudal disputes; correlation between Truce of God movements and measurable render fluctuations.',
    'If church limits were binding, the lord reading overstates extraction and the kernel may be a tangled rope rather than a pure snare; if theatrical, the lord reading is confirmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ecclesiastical_limit_bindingness, empirical, 'Whether ecclesiastical limits were structurally binding or performative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__lord_extraction_reading, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(feud_tr_t100, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 100, 0.26).
narrative_ontology:measurement(feud_tr_t200, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 200, 0.34).
narrative_ontology:measurement(feud_tr_t300, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 300, 0.42).
narrative_ontology:measurement(feud_tr_t400, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 400, 0.46).
narrative_ontology:measurement(feud_tr_t500, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 500, 0.48).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(feud_be_t100, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 100, 0.62).
narrative_ontology:measurement(feud_be_t200, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 200, 0.7).
narrative_ontology:measurement(feud_be_t300, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 300, 0.78).
narrative_ontology:measurement(feud_be_t400, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 400, 0.82).
narrative_ontology:measurement(feud_be_t500, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 500, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t0, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(feud_su_t100, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 100, 0.52).
narrative_ontology:measurement(feud_su_t200, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 200, 0.61).
narrative_ontology:measurement(feud_su_t300, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 300, 0.69).
narrative_ontology:measurement(feud_su_t400, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 400, 0.74).
narrative_ontology:measurement(feud_su_t500, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 500, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(feudal_oath_reciprocity__lord_extraction_reading, vassal_coordination_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__lord_extraction_reading, ecclesiastical_mediation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel feudal_oath_reciprocity. Sibling readings instantiate structurally distinct claims from the same natural-language label. The lord-extraction reading isolates the high-epsilon, asymmetric-extraction claim; the vassal-coordination reading isolates the bounded-reciprocity claim; the ecclesiastical-mediation reading isolates the sacramental-limit claim. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
