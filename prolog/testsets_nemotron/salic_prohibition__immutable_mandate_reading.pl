% ============================================================================
% CONSTRAINT STORY: salic_prohibition__immutable_mandate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_salic_prohibition__immutable_mandate_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: salic_prohibition__immutable_mandate_reading
 *   human_readable: Salic Law as Irrevocable Natural/Divine Law Embedded in Dynastic Constitution
 *   domain: constitutional_law/dynastic_succession/political_history
 *
 * SUMMARY:
 *   This constraint story captures the 'immutable mandate' reading of the
 *   Salic Law — the interpretation that the prohibition on female succession
 *   and succession through female lines is not merely a Frankish tribal
 *   custom but an irrevocable natural and divine law embedded in the
 *   fundamental constitution of certain dynasties (most prominently the
 *   French Valois and Bourbon lines). Under this reading, the exclusion of
 *   female heirs is categorical and metaphysically grounded; challengers to
 *   female succession are legitimate; preventive war to enforce agnatic
 *   priority is justifiable. The story covers the period from the 1316 Valois
 *   succession crisis (where the law was 'discovered' to exclude Jeanne de
 *   Navarre) through the height of its doctrinal enforcement in the 15th-16th
 *   centuries. The constraint is claimed as a mountain (natural law,
 *   emerges_naturally: true) but declares beneficiaries (agnatic heirs,
 *   agnatic nobility) and victims (cognatic heirs), making it a false summit
 *   mountain candidate. The measurement series tracks rising extractiveness,
 *   theater, and suppression as the prohibition hardens from a contested
 *   custom into an enforced constitutional doctrine.
 *
 * KEY AGENTS:
 *   - agnatic_dynastic_heirs: Primary beneficiary (institutional/arbitrage) — inherit throne and legitimize rule through agnatic purity
 *   - agnatic_nobility: Secondary beneficiary (organized/arbitrage) — their position secured by agnatic succession stability
 *   - agnatic_cadets: Tertiary beneficiary (powerful/constrained) — cadet branches gain succession prospects under strict agnatic rules
 *   - cognatic_heirs_female: Primary victim (powerless/trapped) — categorically excluded from succession regardless of proximity
 *   - cognatic_heirs_through_females: Secondary victim (moderate/trapped) — males descended through female lines excluded (e.g., English kings, Habsburgs)
 *   - dynastic_jurists: Agenda setter (institutional/analytical) — formulate, codify, and defend the immutable mandate interpretation
 *   - foreign_powers: Observer/beneficiary (institutional/arbitrage) — exploit succession disputes triggered by the prohibition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__immutable_mandate_reading, 0.68).
domain_priors:suppression_score(salic_prohibition__immutable_mandate_reading, 0.85).
domain_priors:theater_ratio(salic_prohibition__immutable_mandate_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__immutable_mandate_reading, mountain).
narrative_ontology:human_readable(salic_prohibition__immutable_mandate_reading, "Salic Law as Irrevocable Natural/Divine Law Embedded in Dynastic Constitution").
narrative_ontology:topic_domain(salic_prohibition__immutable_mandate_reading, "constitutional_law/dynastic_succession/political_history").

domain_priors:emerges_naturally(salic_prohibition__immutable_mandate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__immutable_mandate_reading, 'a2cc79ea-e35c-43eb-9371-eb0ea64fa7be').
narrative_ontology:cs_kernel_codification('a2cc79ea-e35c-43eb-9371-eb0ea64fa7be', fixed_text).
narrative_ontology:cs_authority_grounding('a2cc79ea-e35c-43eb-9371-eb0ea64fa7be', lineage).
narrative_ontology:cs_interpretation_layer_present('a2cc79ea-e35c-43eb-9371-eb0ea64fa7be').
narrative_ontology:cs_reading_relation('a2cc79ea-e35c-43eb-9371-eb0ea64fa7be', salic_prohibition__sovereign_override_reading, forecloses).
narrative_ontology:cs_reading_relation('a2cc79ea-e35c-43eb-9371-eb0ea64fa7be', salic_prohibition__cognatic_reversion_reading, coexists_with).
narrative_ontology:cs_axiom('a2cc79ea-e35c-43eb-9371-eb0ea64fa7be', foundational, salic_law_as_divine_natural_law).
narrative_ontology:cs_axiom_status(salic_law_as_divine_natural_law, holdable).
narrative_ontology:cs_axiom_grounding('a2cc79ea-e35c-43eb-9371-eb0ea64fa7be', salic_law_as_divine_natural_law, theological).
narrative_ontology:cs_axiom('a2cc79ea-e35c-43eb-9371-eb0ea64fa7be', foundational, agnatic_exclusivity_as_immutable_constitutional_principle).
narrative_ontology:cs_axiom_status(agnatic_exclusivity_as_immutable_constitutional_principle, holdable).
narrative_ontology:cs_axiom_grounding('a2cc79ea-e35c-43eb-9371-eb0ea64fa7be', agnatic_exclusivity_as_immutable_constitutional_principle, deontological).
narrative_ontology:cs_axiom('a2cc79ea-e35c-43eb-9371-eb0ea64fa7be', secondary, preventive_war_justified_for_agnatic_enforcement).
narrative_ontology:cs_axiom_status(preventive_war_justified_for_agnatic_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('a2cc79ea-e35c-43eb-9371-eb0ea64fa7be', preventive_war_justified_for_agnatic_enforcement, instrumental).
narrative_ontology:cs_reference_frame('a2cc79ea-e35c-43eb-9371-eb0ea64fa7be', capetian_agnatic_continuity).
narrative_ontology:cs_drift_state('a2cc79ea-e35c-43eb-9371-eb0ea64fa7be', valois_bourbon_codification_era, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('a2cc79ea-e35c-43eb-9371-eb0ea64fa7be', '').
narrative_ontology:cs_kernel_id(salic_prohibition__immutable_mandate_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, agnatic_dynastic_heirs).
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, agnatic_nobility).
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, agnatic_cadets).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, cognatic_heirs_female).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, cognatic_heirs_through_females).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, agnatic_cadets).
narrative_ontology:constraint_vindicates(salic_prohibition__immutable_mandate_reading, agnatic_succession_as_divine_order).
narrative_ontology:constraint_vindicates(salic_prohibition__immutable_mandate_reading, salic_law_as_natural_law).
narrative_ontology:constraint_vindicates(salic_prohibition__immutable_mandate_reading, male_line_exclusivity_as_immutable).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Direct male-line heirs to the throne whose succession is guaranteed by the Salic prohibition. They inherit the crown without competition from female relatives or their descendants. The constraint secures their position as natural/divine right. Exit is arbitrage-grade — they could accept a cognatic succession but would lose the theological legitimating framework that makes their rule sacral.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, agnatic_dynastic_heirs, beneficiary,
    institutional, generational, arbitrage, national).

% High nobility whose feudal positions and court influence are stabilized by agnatic succession certainty. Female succession historically brought foreign husbands and partition; the Salic Law prevents both. They benefit from the constraint's exclusion of foreign dynasties. Exit is arbitrage — they could support a cognatic heir but would risk their institutional position.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, agnatic_nobility, beneficiary,
    organized, generational, arbitrage, national).

% Cadet branches of the royal house (e.g., Valois-Orléans, Valois-Angoulême, Bourbon-Vendôme) who gain succession prospects under strict agnatic rules but pay through exclusion from regency roles and subordination to senior line. They benefit from the rule's rigidity (their claims are protected) but bear costs of dynastic discipline. Exit is constrained — leaving the royal orbit means losing princely status.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, agnatic_cadets, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(salic_prohibition__immutable_mandate_reading, agnatic_cadets, payer).

% Daughters and female relatives of monarchs (e.g., Jeanne de Navarre 1316, daughters of Louis X, Charles IV) categorically excluded from succession regardless of proximity. Their hereditary rights are extinguished by the prohibition. No exit exists within the dynastic system — marriage to foreign princes is the only path, which the Salic Law was partly designed to prevent. They bear the full extractive weight of the constraint.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, cognatic_heirs_female, payer,
    powerless, biographical, trapped, national).

% Male heirs descended through female lines (e.g., Edward III of England through Isabella of France, Habsburg claimants through Burgundian inheritance) excluded from French succession. They possess military and diplomatic power to press claims but are structurally delegitimized by the Salic Law. Their exclusion justifies wars (Hundred Years' War) and diplomatic isolation. Exit is trapped — they cannot renounce their dynastic claims without losing their own thrones' legitimacy.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, cognatic_heirs_through_females, payer,
    moderate, biographical, trapped, continental).

% Royal jurists, parlementaires, and theologians (e.g., Jean de Montreuil, Christine de Pizan's opponents, later Bodin) who formulate, codify, and defend the immutable mandate interpretation. They produce the legal theology that makes the prohibition appear as natural law. Their professional authority depends on the constraint's natural-law status. Exit is analytical — they can change interpretation but only from within the scholastic framework.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, dynastic_jurists, agenda_setter,
    institutional, generational, analytical, national).

% Neighboring monarchies (England, Burgundy, Habsburgs, Navarre) who observe the French succession constraint and exploit its rigidities. They benefit when Salic Law creates succession crises they can intervene in (e.g., English claim to French throne). They are not bound by the constraint but their dynastic strategies are shaped by it. Exit is arbitrage — they choose whether to press cognatic claims based on cost-benefit.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, foreign_powers, observer,
    institutional, biographical, arbitrage, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents dynastic partition and foreign domination through female marriage alliances; provides a clear, non-negotiable succession rule that avoids contested regencies and civil war over competing claims.
% TRANSFER_FUNCTION: Moves the throne and its attendant lands, titles, and legitimating authority from cognatic heirs (female and male-through-female) to agnatic heirs (male-only line). The transfer is justified as preserving the 'natural order' but functionally concentrates dynastic power in the agnatic line.
% ABSENT_VOICES: The excluded female heirs themselves (Jeanne de Navarre, daughters of Louis X, Charles IV) had no formal voice in the 1316-1328 determinations. Their potential supporters among the nobility and clergy were silenced by the rapide jurisprudential closure. Foreign female sovereigns (e.g., Isabella of Castile later) demonstrate the excluded capacity but were structurally absent from the French deliberation.
% DISAPPEARANCE_RATIONALE: If the Salic prohibition vanished overnight, the French succession would immediately open to cognatic claimants: English kings (through Isabella), Habsburgs (through Burgundian marriages), Navarrese heirs. The Valois and Bourbon dynasties would lose their legitimating framework. The Hundred Years' War's legal basis would collapse. The European diplomatic order built on French agnatic certainty would reorganize around competing cognatic claims.
% FOUNDING_PROBLEM: The 1316 succession crisis: Louis X died leaving a pregnant queen and a daughter (Jeanne). The nobility feared partition if Jeanne inherited (her husband was a foreign count) or regency instability. The 'rediscovery' of Salic Law provided a ready-made exclusion of female heirs, securing the throne for Louis's brother Philip V and establishing a clear agnatic line.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (preventing partition/foreign domination through female marriage) is attested as dead by: (1) the French state's centralization under Louis XI and Francis I made partition obsolete; (2) foreign marriages continued under agnatic rules (e.g., French kings marrying Habsburgs, English princesses) showing the original fear was manageable; (3) contemporary jurists (Commines, later Bodin) noted the law's utility had shifted from crisis management to dynastic entrenchment. The Valois and Bourbon dynasties themselves never claimed the original crisis persisted — they invoked the law's 'immemorial' and 'divine' character instead.
narrative_ontology:disappearance_verdict(salic_prohibition__immutable_mandate_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__immutable_mandate_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__immutable_mandate_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(salic_prohibition__immutable_mandate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(salic_prohibition__immutable_mandate_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_prohibition__immutable_mandate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(salic_prohibition__immutable_mandate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, ExtMetricName, E),
    domain_priors:suppression_score(salic_prohibition__immutable_mandate_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(salic_prohibition__immutable_mandate_reading),
    narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(salic_prohibition__immutable_mandate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint claims mountain status (natural/divine law, emerges_naturally: true) with high accessibility_collapse (0.92) and low resistance (0.35) — the prohibition appears as an immutable structural feature of the dynastic order. However, extractiveness (0.68) and suppression (0.85) are substantial and rising over the interval, while theater_ratio (0.42) indicates significant performative enforcement activity. The beneficiary/victim declarations reveal asymmetric extraction: agnatic heirs and nobility benefit from the exclusion of cognatic claimants, while female heirs and their descendants are categorically trapped. The temporal trajectory shows the constraint hardening: initial ambiguity in 1316 (extractiveness 0.35) becomes doctrinal rigidity by 1500 (extractiveness 0.71), with suppression rising from 0.5 to 0.85 as alternative successions are actively prevented (e.g., Hundred Years' War justified by Salic Law, exclusion of Habsburg and English claims). This pattern suggests a false summit mountain — presented as natural law but operating as constructed exclusion benefiting identifiable agnatic interests.
 *
 * PERSPECTIVAL GAP:
 *   From the agnatic heir's seat (beneficiary, d ≈ 0.1), the constraint appears as legitimate natural order securing their inheritance — coordination function dominates. From the cognatic heir's seat (victim, d ≈ 0.95), the same constraint appears as pure extraction enforced by sovereign violence — no coordination function experienced, only categorical exclusion. The dynastic jurists (agenda_setter, d ≈ 0.2) experience it as their professional mandate to defend the natural law interpretation. Foreign powers (observer, d ≈ 0.3) see it as a structural vulnerability to exploit. The engine computes these divergent effective extractions from the same base ε = 0.68.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: agnatic_dynastic_heirs (direct succession capture), agnatic_nobility (political stability under agnatic rule), agnatic_cadets (expanded succession pool). Victims declared: cognatic_heirs_female (categorical exclusion from throne), cognatic_heirs_through_females (exclusion of male lines descended through females). The directionality derivation assigns low d to beneficiaries (constraint subsidizes their position), high d to victims (constraint extracts their hereditary rights). Agnatic cadets have constrained exit (cannot easily leave the dynastic system) but benefit from the rule's rigidity. Foreign powers have arbitrage exit (can intervene or not) but are not direct subjects.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1316: preventing partition and foreign domination through female marriage) was arguably live initially but became dead as the French state centralized and the original threat receded. Yet the prohibition persisted and hardened (extraction accumulation). The arrangement now serves agnatic interests without solving its founding problem — classic mandatrophy. The 'natural law' framing prevents acknowledgment of obsolescence: to admit the law is positive would undermine the theological legitimacy of the dynasty itself. The theater_ratio rise (0.15→0.42) tracks the increasing performative defense of a constraint whose functional justification has atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'Is the Salic prohibition a genuine natural/divine law or a constructed positive law that benefits agnatic dynasties?',
    'Comparative historical analysis of whether the prohibition operates identically across Frankish and non-Frankish territories, and whether sovereign override mechanisms existed and were exercised in the same dynastic tradition.',
    'If constructed positive law, the constraint is a false summit mountain masking tangled rope or snare dynamics; if genuine natural law, mountain classification holds but beneficiary presence triggers FSM evaluation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Whether the immutable mandate reading reflects ontological reality or dynastic interest.').

omega_variable(
    natural_law_vs_constructed_ambiguity,
    'Does the ''natural law'' framing of Salic Law represent an irreducible structural feature of political legitimacy, or a theological-political construction that serves agnatic interests?',
    'Examine whether the prohibition persists when the theological framework is removed (secular dynastic states), and whether cognatic succession functions stably in parallel traditions without the claimed ''natural'' collapse.',
    'If theological-political construction, the accessibility_collapse metric (0.92) measures enforced closure not natural necessity; the mountain claim dissolves into a snare or tangled rope maintained by sovereign power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_ambiguity, conceptual, 'Natural law status vs. constructed prohibition serving agnatic beneficiaries.').

omega_variable(
    sibling_reading_foreclosure_boundary,
    'Does the immutable mandate reading''s core premise (Salic Law as irrevocable natural/divine law) logically foreclose the sovereign override reading within any single constitutional framework?',
    'Analyze whether a single dynastic constitution can simultaneously hold that succession law is (a) irrevocable natural law and (b) revocable positive law subject to sovereign authority, or whether these are mutually exclusive commitments.',
    'If forecloses, the two readings cannot coexist in one legal framework — choosing one eliminates the other structurally. If coexists_with, different parties within the same polity can hold both readings simultaneously.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_boundary, conceptual, 'Logical foreclosure between immutable mandate and sovereign override readings.').

omega_variable(
    cognatic_reversion_empirical_challenge,
    'Do stable cognatic successions in non-Frankish territories (e.g., England, Iberia, later Austria) empirically falsify the claim that agnatic exclusivity is a natural law of dynastic stability?',
    'Historical survey of dynastic longevity and stability under cognatic vs. agnatic rules across European monarchies 1300-1800, controlling for exogenous factors.',
    'If cognatic successions are empirically stable, the natural law claim loses its empirical warrant and the prohibition''s extraction from female heirs lacks even coordination justification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cognatic_reversion_empirical_challenge, empirical, 'Whether empirical evidence of stable cognatic succession falsifies the natural law claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__immutable_mandate_reading, 1316, 1500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sali_tr_t1316, salic_prohibition__immutable_mandate_reading, theater_ratio, 1316, 0.15).
narrative_ontology:measurement(sali_tr_t1340, salic_prohibition__immutable_mandate_reading, theater_ratio, 1340, 0.22).
narrative_ontology:measurement(sali_tr_t1370, salic_prohibition__immutable_mandate_reading, theater_ratio, 1370, 0.3).
narrative_ontology:measurement(sali_tr_t1400, salic_prohibition__immutable_mandate_reading, theater_ratio, 1400, 0.35).
narrative_ontology:measurement(sali_tr_t1450, salic_prohibition__immutable_mandate_reading, theater_ratio, 1450, 0.4).
narrative_ontology:measurement(sali_tr_t1500, salic_prohibition__immutable_mandate_reading, theater_ratio, 1500, 0.42).

% Extraction over time
narrative_ontology:measurement(sali_be_t1316, salic_prohibition__immutable_mandate_reading, base_extractiveness, 1316, 0.35).
narrative_ontology:measurement(sali_be_t1340, salic_prohibition__immutable_mandate_reading, base_extractiveness, 1340, 0.42).
narrative_ontology:measurement(sali_be_t1370, salic_prohibition__immutable_mandate_reading, base_extractiveness, 1370, 0.55).
narrative_ontology:measurement(sali_be_t1400, salic_prohibition__immutable_mandate_reading, base_extractiveness, 1400, 0.62).
narrative_ontology:measurement(sali_be_t1450, salic_prohibition__immutable_mandate_reading, base_extractiveness, 1450, 0.68).
narrative_ontology:measurement(sali_be_t1500, salic_prohibition__immutable_mandate_reading, base_extractiveness, 1500, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(sali_su_t1316, salic_prohibition__immutable_mandate_reading, suppression_requirement, 1316, 0.5).
narrative_ontology:measurement(sali_su_t1340, salic_prohibition__immutable_mandate_reading, suppression_requirement, 1340, 0.6).
narrative_ontology:measurement(sali_su_t1370, salic_prohibition__immutable_mandate_reading, suppression_requirement, 1370, 0.72).
narrative_ontology:measurement(sali_su_t1400, salic_prohibition__immutable_mandate_reading, suppression_requirement, 1400, 0.78).
narrative_ontology:measurement(sali_su_t1450, salic_prohibition__immutable_mandate_reading, suppression_requirement, 1450, 0.82).
narrative_ontology:measurement(sali_su_t1500, salic_prohibition__immutable_mandate_reading, suppression_requirement, 1500, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_prohibition__immutable_mandate_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(salic_prohibition__immutable_mandate_reading, 0.1).
narrative_ontology:affects_constraint(salic_prohibition__immutable_mandate_reading, salic_prohibition__sovereign_override_reading).
narrative_ontology:affects_constraint(salic_prohibition__immutable_mandate_reading, salic_prohibition__cognatic_reversion_reading).
narrative_ontology:affects_constraint(salic_prohibition__immutable_mandate_reading, valois_succession_crisis_1316).
narrative_ontology:affects_constraint(salic_prohibition__immutable_mandate_reading, hundred_years_war_succession_justification).
narrative_ontology:affects_constraint(salic_prohibition__immutable_mandate_reading, pragmatic_sanction_1713_habsburg).

% DUAL FORMULATION NOTE:
% Part of the Salic Prohibition constraint family (kernel: salic_prohibition). This reading (immutable_mandate_reading) claims the prohibition is natural/divine law with ε=0.68 (substantial extraction masked as mountain). The sovereign_override_reading claims revocable positive law with lower ε (coordination function acknowledged). The cognatic_reversion_reading claims Frankish-only binding with near-zero ε for non-Frankish territories. The three readings have different beneficiary/victim structures and different claimed types — they are structurally distinct constraints linked by the kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(salic_prohibition__immutable_mandate_reading, institutional, 0.15).
constraint_indexing:directionality_override(salic_prohibition__immutable_mandate_reading, powerless, 0.95).
constraint_indexing:directionality_override(salic_prohibition__immutable_mandate_reading, moderate, 0.85).
constraint_indexing:directionality_override(salic_prohibition__immutable_mandate_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
