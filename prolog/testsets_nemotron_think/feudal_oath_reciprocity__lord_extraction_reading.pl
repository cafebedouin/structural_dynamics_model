% ============================================================================
% CONSTRAINT STORY: feudal_oath_reciprocity__lord_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   human_readable: Feudal Oath — Lord's Extraction Reading
 *   domain: political/historical/institutional
 *
 * SUMMARY:
 *   The feudal oath of fealty and homage is read by lords as authorizing
 *   maximal extraction from vassals — military service, fiscal incidents,
 *   judicial profits, and the peasant surplus that funds it all — bounded
 *   only by the vassal's capacity to resist or rebel. This reading treats the
 *   reciprocal language of the oath ('protection and justice' for 'service
 *   and counsel') as ceremonial theater; the lord's courts enforce the
 *   extraction side while the protection side is unenforceable. The
 *   constraint is a snare: vassals and peasantry are identifiable victims,
 *   extraction is actively enforced by military power, and alternatives
 *   (allodial tenure, royal protection, urban commune) are suppressed. The
 *   claimed type is snare; the metrics reflect high extraction (0.85), high
 *   suppression (0.80), moderate theater (0.30), high accessibility collapse
 *   (0.82), and moderate resistance (0.50) — the rebellion threshold is real
 *   but costly.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__lord_extraction_reading, 0.85).
domain_priors:suppression_score(feudal_oath_reciprocity__lord_extraction_reading, 0.8).
domain_priors:theater_ratio(feudal_oath_reciprocity__lord_extraction_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__lord_extraction_reading, snare).
narrative_ontology:human_readable(feudal_oath_reciprocity__lord_extraction_reading, "Feudal Oath — Lord's Extraction Reading").
narrative_ontology:topic_domain(feudal_oath_reciprocity__lord_extraction_reading, "political/historical/institutional").

domain_priors:requires_active_enforcement(feudal_oath_reciprocity__lord_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__lord_extraction_reading, '2e9981b3-9489-4b44-b40c-9f54bb54e085').
narrative_ontology:cs_kernel_codification('2e9981b3-9489-4b44-b40c-9f54bb54e085', implicit).
narrative_ontology:cs_authority_grounding('2e9981b3-9489-4b44-b40c-9f54bb54e085', extraction).
narrative_ontology:cs_interpretation_layer_present('2e9981b3-9489-4b44-b40c-9f54bb54e085').
narrative_ontology:cs_reading_relation('2e9981b3-9489-4b44-b40c-9f54bb54e085', feudal_oath_reciprocity__vassal_coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('2e9981b3-9489-4b44-b40c-9f54bb54e085', feudal_oath_reciprocity__ecclesiastical_mediation_reading, influences).
narrative_ontology:cs_axiom('2e9981b3-9489-4b44-b40c-9f54bb54e085', foundational, oath_authorizes_unbounded_secular_extraction).
narrative_ontology:cs_axiom_status(oath_authorizes_unbounded_secular_extraction, holdable).
narrative_ontology:cs_axiom_grounding('2e9981b3-9489-4b44-b40c-9f54bb54e085', oath_authorizes_unbounded_secular_extraction, conventional).
narrative_ontology:cs_axiom('2e9981b3-9489-4b44-b40c-9f54bb54e085', secondary, vassal_capacity_is_only_limit).
narrative_ontology:cs_axiom_status(vassal_capacity_is_only_limit, holdable).
narrative_ontology:cs_axiom_grounding('2e9981b3-9489-4b44-b40c-9f54bb54e085', vassal_capacity_is_only_limit, instrumental).
narrative_ontology:cs_reference_frame('2e9981b3-9489-4b44-b40c-9f54bb54e085', primordial_feudal_contract).
narrative_ontology:cs_drift_state('2e9981b3-9489-4b44-b40c-9f54bb54e085', high_medieval_centralization, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2e9981b3-9489-4b44-b40c-9f54bb54e085', '2026-06-15T14:30:00Z').
narrative_ontology:cs_kernel_id(feudal_oath_reciprocity__lord_extraction_reading, feudal_oath_reciprocity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__lord_extraction_reading, lord).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__lord_extraction_reading, lord_household).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__lord_extraction_reading, vassals).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__lord_extraction_reading, vassal_households).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__lord_extraction_reading, peasantry).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__lord_extraction_reading, lord_household).
narrative_ontology:constraint_vindicates(feudal_oath_reciprocity__lord_extraction_reading, feudal_hierarchy_legitimacy).
narrative_ontology:constraint_vindicates(feudal_oath_reciprocity__lord_extraction_reading, lord_sovereign_authority).
narrative_ontology:constraint_vindicates(feudal_oath_reciprocity__lord_extraction_reading, personal_loyalty_as_primary_obligation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the feudal oath as authorization to demand maximal service and surplus from vassals. Sets the terms of the relationship, defines what constitutes 'service capacity,' and controls the military enforcement that backs extraction. Can alienate vassals to other lords or the crown, but rarely needs to — the oath binds the vassal to him personally. Collects the surplus directly through labor services, renders, and military obligations.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, lord, agenda_setter,
    institutional, generational, arbitrage, regional).

% Receives the extracted surplus — food renders, labor, military service, judicial profits. The household's status and capacity for patronage depend on this flow. Also bears costs: maintaining the military retinue that enforces the oath, defending the tenure against rival lords, and performing service to the lord's own superior (king or higher lord).
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, lord_household, beneficiary,
    powerful, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__lord_extraction_reading, lord_household, payer).

% Bound by the oath to provide military service, counsel, and financial aids (relief, scutage, marriage portions). Their 'service capacity' is the only structural limit — the lord reads the oath as authorizing everything up to the rebellion threshold. Exit means forfeiting the fief (loss of status and livelihood) or rebellion (risk of death and attainder). Some negotiate written charters to bound obligations, but the lord's reading treats charters as revocable concessions.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, vassals, payer,
    organized, biographical, constrained, regional).

% Bear the intergenerational burden: heirs pay relief to inherit, widows pay for dower or remarriage rights, younger sons are squeezed by primogeniture. The household's survival depends on extracting from the peasantry below to meet the lord's demands above. Their exit options are the same as the vassal's — forfeiture or rebellion — but with the added weight of dynastic continuity.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, vassal_households, payer,
    organized, generational, constrained, regional).

% Not party to the oath but bear its ultimate extraction. The vassal's service capacity is filled by peasant labor — week-work, tallage, merchet, heriot, mill and oven monopolies. They have no voice in the oath, no contractual relation with the lord, and no exit but flight (itself punished as fugitive serfdom). Their resistance is sporadic and localized — revolt is the only leverage, and it is crushed by the same military service the oath commands.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, peasantry, payer,
    powerless, biographical, trapped, local).

% Claim the oath is bound by Christian charity, sacramental obligation, and canon law — extraction must not exceed what preserves the vassal's soul and household. They are structurally excluded from secular enforcement: the lord's courts do not admit canon law as a limit on feudal exactions. Their leverage is spiritual (excommunication, interdict) which works only when the lord fears for his soul or needs ecclesiastical legitimacy.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, ecclesiastical_authorities, excluded,
    institutional, civilizational, analytical, continental).

% The king is notionally the supreme lord, but his capacity to limit baronial extraction depends on his own military and fiscal resources. He benefits from the oath as a mechanism to raise armies without a bureaucracy, but suffers when great lords use their extracted surplus to challenge the crown. Royal courts slowly develop protections (Magna Carta, common law tenure) but these are contested and partial.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, royal_authority, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates military service and local governance in a fragmented polity where central authority cannot directly administer territory or raise armies; the oath binds vassals to provide knights, castle-guard, and counsel in exchange for land tenure.
% TRANSFER_FUNCTION: Moves agricultural surplus (food renders, labor services), military capacity (knight service, castle-guard), and fiscal incidents (relief, scutage, wardship, marriage) from vassals and peasantry upward to lords, bounded only by the vassal's capacity to resist or rebel.
% ABSENT_VOICES: Peasantry who bear the ultimate extraction but have no voice in the oath; ecclesiastical authorities who claim moral limitation on extraction but are excluded from secular enforcement; urban communities and merchant classes who develop outside the feudal nexus but are taxed by lords claiming feudal prerogative.
% DISAPPEARANCE_RATIONALE: If the feudal oath vanished overnight, the entire structure of land tenure, military obligation, and surplus extraction would collapse. Vassals would become either independent landholders or royal tenants; peasantry would face new lords or direct royal administration; the military system would require a fiscal-military state to replace knight service. The world rearranges completely — the oath is the keystone of the feudal order.
% FOUNDING_PROBLEM: Establish reliable military service and local governance in a fragmented post-Carolingian landscape where central authority could not directly administer territory or raise armies; the personal bond of vassalage substituted for bureaucratic administration.
% FOUNDING_PROBLEM_CORROBORATION: Medieval historians outside the lordly beneficiary class (Marc Bloch, Georges Duby, Chris Wickham, Susan Reynolds) document that feudal obligations originated as coordination for defense and governance in a stateless landscape, but the extraction mechanisms persisted and intensified after royal administration and fiscal-military states solved the original coordination problem. The 'feudal revolution' thesis (Bouchard, Barthélemy) shows the mutation from coordination to extraction in the 10th-11th centuries.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__lord_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__lord_extraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__lord_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(feudal_oath_reciprocity__lord_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feudal_oath_reciprocity__lord_extraction_reading, 0.85, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extraction is very high because the oath's reciprocity is structurally one-sided: the lord defines 'service capacity' and enforces it militarily, while the vassal has no equivalent power to define 'protection.' Suppression is high because the constraint persists through active exclusion of alternatives — allodial land is rare and vulnerable, royal courts are distant, urban charters are exceptional. Theater ratio is moderate-low: the ceremonial reciprocity (homage ceremony, oath on relics) is real but a shrinking share of the constraint's operation; most activity is extraction enforcement. Accessibility collapse is high because the vassal's identity, status, and livelihood are fused to the fief — exit means social death. Resistance is moderate because the rebellion threshold is the only limit, and lords calibrate extraction to stay just below it.
 *
 * PERSPECTIVAL GAP:
 *   From the lord's seat, the oath is a coordination mechanism he maintains at personal cost (military retinue, defense of tenure) — genuine reciprocity. From the vassal's seat, the same structure is enforced extraction — the lord's 'protection' is the absence of the lord's own predation. From the peasant's seat, the oath is invisible; they experience only the vassal's extraction, which the oath authorizes. The engine computes these divergent seat types from the structural data; the authored claim (snare) reflects the vassal/peasantry perspective as the structural truth.
 *
 * DIRECTIONALITY LOGIC:
 *   The lord is the structural beneficiary (d near 0.0) — collects the surplus, sets the terms, controls enforcement. The lord_household is a secondary beneficiary (d ~0.15) — receives the flow but bears some enforcement costs. Vassals and vassal_households are targets (d near 0.9) — pay the transfer, constrained exit, identity-locked to the fief. Peasantry are extreme targets (d ~0.95) — trapped, no voice, bear the ultimate extraction. Ecclesiastical authorities are excluded (d undefined) — they would limit extraction but are kept out of enforcement. Royal authority is an observer (d ~0.5) — benefits from the military system but threatened by the extraction concentration.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (military coordination in a stateless landscape) is dead — royal administration, standing armies, and fiscal-military states solved it by 1300. But the extraction machinery persisted and intensified (rising base_extractiveness in measurements). This is classic mandatrophy: the coordination function atrophied, the extraction function captured the structure, and the ceremonial reciprocity (theater) masks the drift. The 'rebellion threshold' limit is not a coordination boundary — it is the extraction calibration point.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_hierarchy_vs_constructed_extraction,
    'Is the feudal hierarchy a natural law of human society (as lords and some theorists claimed) or a constructed extraction mechanism that benefits identifiable agents?',
    'Comparative historical analysis: if societies without feudal oaths (Icelandic Commonwealth, early medieval Frisia, segments of Slavic Europe) achieved comparable military coordination without the extraction profile, the hierarchy is constructed. If all polities at this scale and technology converge on feudal extraction, natural-law plausibility increases.',
    'If constructed, the false_summit_mountain signature could apply to any ''natural hierarchy'' claim; the snare classification stands. If natural, the high extraction might be reinterpreted as the coordination cost of the only viable military system — but the peasantry''s trapped status and the lord''s arbitrage exit would still make it extractive for those seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_hierarchy_vs_constructed_extraction, conceptual, 'Natural-law vs. constructed ambiguity for the feudal hierarchy claim').

omega_variable(
    rebellion_threshold_as_coordination_or_extraction_calibration,
    'Is the ''vassal service capacity'' limit a genuine coordination boundary (the point where the system breaks) or the lord''s extraction calibration (take everything up to but not including revolt)?',
    'Case studies of lord-vassal conflict: when vassals rebel, do lords concede substantive limits (charters, fixed customs) or merely tactical pauses? Longitudinal data on whether extraction ratchets up after each suppressed revolt.',
    'If calibration, the constraint is a pure snare with no coordination residue — the limit is the extraction frontier. If genuine coordination boundary, a tangled_rope element exists: the oath solves a real collective-action problem (mutual defense) at the cost of asymmetric extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rebellion_threshold_as_coordination_or_extraction_calibration, empirical, 'Whether the rebellion threshold is a structural coordination limit or an extraction tuning parameter').

omega_variable(
    committer_kernel_reading_identity,
    'This constraint is one reading (lord_extraction_reading) of the contested kernel ''feudal_oath_reciprocity''. What structural elements do the sibling readings (vassal_coordination_reading, ecclesiastical_mediation_reading) change, and where is the disagreement located?',
    'Structural comparison of the three readings'' beneficiary/victim sets, enforcement mechanisms, and claimed coordination functions. The disagreement is located in: (1) whether the oath''s reciprocity is fixed/textual (vassal_coordination) or open-ended/lord-defined (this reading); (2) whether ecclesiastical authority binds secular extraction (ecclesiastical_mediation) or is excluded (this reading).',
    'If vassal_coordination_reading is structurally valid (charters enforce fixed obligations), this reading''s snare classification applies only where charters are absent or unenforced — a spatial/temporal split. If ecclesiastical_mediation_reading has enforcement teeth (excommunication that lords fear), the extraction ceiling is real and this reading''s ε is overstated for pious lords.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_reading_identity, conceptual, 'Committer frame: this reading''s structural delta from sibling readings of the same kernel').

omega_variable(
    peasantry_as_direct_victims_or_incidental,
    'Are the peasantry direct victims of the feudal oath (the oath authorizes their extraction) or incidental victims (the oath binds lords and vassals; peasant extraction is the vassal''s separate choice)?',
    'Legal history: do feudal customals and court records treat peasant obligations as incidents of the fief (owed to the lord through the vassal) or as the vassal''s private domain? The distinction determines whether peasantry belongs in this constraint''s victim set or a separate constraint (manorial_extraction).',
    'If direct victims, this constraint''s ε and victim set are larger; the snare classification strengthens. If incidental, this constraint is lord-vassal only (still snare but narrower), and manorial_extraction is a separate constraint linked by network.affects_constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(peasantry_as_direct_victims_or_incidental, empirical, 'Whether peasantry are in this constraint''s victim set or a downstream constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__lord_extraction_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feudal_oath_lord_extraction_tr_t0, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(feudal_oath_lord_extraction_tr_t10, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(feudal_oath_lord_extraction_tr_t20, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(feudal_oath_lord_extraction_tr_t30, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(feudal_oath_lord_extraction_tr_t40, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(feudal_oath_lord_extraction_tr_t50, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 50, 0.3).

% Extraction over time
narrative_ontology:measurement(feudal_oath_lord_extraction_be_t0, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(feudal_oath_lord_extraction_be_t10, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(feudal_oath_lord_extraction_be_t20, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 20, 0.75).
narrative_ontology:measurement(feudal_oath_lord_extraction_be_t30, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 30, 0.82).
narrative_ontology:measurement(feudal_oath_lord_extraction_be_t40, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 40, 0.85).
narrative_ontology:measurement(feudal_oath_lord_extraction_be_t50, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 50, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(feudal_oath_lord_extraction_su_t0, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(feudal_oath_lord_extraction_su_t10, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(feudal_oath_lord_extraction_su_t20, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(feudal_oath_lord_extraction_su_t30, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 30, 0.78).
narrative_ontology:measurement(feudal_oath_lord_extraction_su_t40, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 40, 0.8).
narrative_ontology:measurement(feudal_oath_lord_extraction_su_t50, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 50, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__lord_extraction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(feudal_oath_reciprocity__lord_extraction_reading, 0.12).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__lord_extraction_reading, manorial_extraction).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__lord_extraction_reading, royal_feudal_incidents).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__lord_extraction_reading, ecclesiastical_immunities).

% DUAL FORMULATION NOTE:
% This reading (lord_extraction) and vassal_coordination_reading decompose the single label 'feudal oath' into two constraints with different ε: the vassal_coordination reading has fixed obligations (lower ε, tangled_rope); this reading has open-ended extraction (higher ε, snare). The ecclesiastical_mediation_reading adds a third constraint with moral ceiling on extraction. All three link via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(feudal_oath_reciprocity__lord_extraction_reading, institutional, 0.05).
constraint_indexing:directionality_override(feudal_oath_reciprocity__lord_extraction_reading, powerful, 0.15).
constraint_indexing:directionality_override(feudal_oath_reciprocity__lord_extraction_reading, organized, 0.88).
constraint_indexing:directionality_override(feudal_oath_reciprocity__lord_extraction_reading, powerless, 0.95).
constraint_indexing:directionality_override(feudal_oath_reciprocity__lord_extraction_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
