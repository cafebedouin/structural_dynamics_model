% ============================================================================
% CONSTRAINT STORY: feudal_oath_reciprocity__vassal_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feudal_oath_reciprocity__vassal_coordination_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: feudal_oath_reciprocity__vassal_coordination_reading
 *   human_readable: Feudal Oath Reciprocity (Vassal Coordination Reading)
 *   domain: medieval_political_economy
 *
 * SUMMARY:
 *   This story instantiates the vassal_coordination_reading of the feudal
 *   oath kernel. Under this reading, the oath-charter is a genuine
 *   mutual-benefit coordination mechanism: both lord and vassal bind
 *   themselves to fixed, bounded obligations specified in writing. The vassal
 *   gains land tenure security and collective defense; the lord gains
 *   organized military service and renders. Extraction is limited because the
 *   charter text specifies the obligations — the vassal can invoke the text
 *   against extraction beyond its terms. This reading emphasizes the
 *   reciprocity and textual enforceability. It is contested by the
 *   lord_extraction_reading (which reads the same charter as authorizing
 *   maximal extraction bounded only by 'custom' and 'capacity,' terms the
 *   lord interprets) and the ecclesiastical_mediation_reading (which
 *   emphasizes Christian charity limits). This story measures the constraint
 *   as a true rope: low extractiveness (0.35), low suppression (0.28),
 *   genuine coordination benefit to both parties, and relatively flat
 *   trajectory over 36 time units. The claim and metrics are aligned because
 *   the reading itself asserts coordination without extraction.
 *
 * KEY AGENTS:
 *   - vassal_class: seeks tenure security and collective protection through oath-bound reciprocity
 *   - lord_class: organizes military service and territorial security through oath administration; benefits from organized vassals but constrained by charter reciprocity
 *   - ecclesiastical_authority: witnesses and adds sacramental force to the oath; observes from institutional seat with potential to mediate disputes
 *   - peer_vassal_network: excluded from oath-drafting but forms the implicit enforcement coalition that makes reciprocity binding
 *   - secular_royal_authority: observes from above; provides appellate recourse if oath breach is too egregious
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__vassal_coordination_reading, 0.35).
domain_priors:suppression_score(feudal_oath_reciprocity__vassal_coordination_reading, 0.28).
domain_priors:theater_ratio(feudal_oath_reciprocity__vassal_coordination_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__vassal_coordination_reading, rope).
narrative_ontology:human_readable(feudal_oath_reciprocity__vassal_coordination_reading, "Feudal Oath Reciprocity (Vassal Coordination Reading)").
narrative_ontology:topic_domain(feudal_oath_reciprocity__vassal_coordination_reading, "medieval_political_economy").

domain_priors:requires_active_enforcement(feudal_oath_reciprocity__vassal_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__vassal_coordination_reading, '414215da-a74a-4087-a5b8-4de5568bab8c').
narrative_ontology:cs_kernel_codification('414215da-a74a-4087-a5b8-4de5568bab8c', fixed_text).
narrative_ontology:cs_authority_grounding('414215da-a74a-4087-a5b8-4de5568bab8c', lineage).
narrative_ontology:cs_interpretation_layer_present('414215da-a74a-4087-a5b8-4de5568bab8c').
narrative_ontology:cs_reading_relation('414215da-a74a-4087-a5b8-4de5568bab8c', feudal_oath_reciprocity__lord_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('414215da-a74a-4087-a5b8-4de5568bab8c', feudal_oath_reciprocity__ecclesiastical_mediation_reading, coexists_with).
narrative_ontology:cs_axiom('414215da-a74a-4087-a5b8-4de5568bab8c', foundational, reciprocal_text_binding).
narrative_ontology:cs_axiom_status(reciprocal_text_binding, holdable).
narrative_ontology:cs_axiom_grounding('414215da-a74a-4087-a5b8-4de5568bab8c', reciprocal_text_binding, conventional).
narrative_ontology:cs_axiom('414215da-a74a-4087-a5b8-4de5568bab8c', foundational, mutual_invocation_enforceability).
narrative_ontology:cs_axiom_status(mutual_invocation_enforceability, holdable).
narrative_ontology:cs_axiom_grounding('414215da-a74a-4087-a5b8-4de5568bab8c', mutual_invocation_enforceability, conventional).
narrative_ontology:cs_reference_frame('414215da-a74a-4087-a5b8-4de5568bab8c', oath_text_mutual_binding).
narrative_ontology:cs_drift_state('414215da-a74a-4087-a5b8-4de5568bab8c', late_medieval_royal_centralization, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('414215da-a74a-4087-a5b8-4de5568bab8c', '').
narrative_ontology:cs_kernel_id(feudal_oath_reciprocity__vassal_coordination_reading, feudal_oath_reciprocity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__vassal_coordination_reading, vassal_class).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__vassal_coordination_reading, lord_class).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__vassal_coordination_reading, vassal_class).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Swears homage to a lord in exchange for land tenure (the fief) and military protection. The oath text specifies fixed obligations: military service for a set number of days per year, customary renders (grain, livestock, labor), and counsel on military campaigns. In return, the lord guarantees protection against external enemies and recognition of the vassal's hereditary claim to the fief. The vassal benefits from land tenure security and collective defense; the vassal pays through service obligations. Exit is constrained: breaking the oath forfeits the fief and invites military retaliation, but the oath itself defines what constitutes breach — the text is the mutual enforcement mechanism.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, vassal_class, beneficiary,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__vassal_coordination_reading, vassal_class, payer).

% Sets the oath's terms and witnesses the vassals' swearing. Receives military service, renders, and counsel. Provides protection and recognition of the vassal's title. The lord benefits from organized vassal service and loyalty; the lord pays through the obligation to defend and respect the oath's terms. The charter text binds the lord as well: a lord who demands service beyond the customary amount or seizes the fief without cause violates the oath and can be lawfully resisted by the vassal's peers.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, lord_class, agenda_setter,
    powerful, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__vassal_coordination_reading, lord_class, beneficiary).

% Witnesses and blesses the oath, adds sacramental weight through the oath's status as a solemn act before God. The church does not adjudicate disputes but its authority undergirds the oath's perceived binding force. Bishops and abbots hold vassal positions themselves, creating overlapping jurisdictions; ecclesiastical judgment can be invoked as an outside arbiter when oath disputes arise.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, ecclesiastical_authority, observer,
    institutional, generational, analytical, regional).

% Other vassals of the same lord form an informal enforcement coalition. If one vassal perceives a lord is violating the oath (extracting beyond the text), peer vassals can collectively withhold service or renounce their oaths as a lawful response — the charter's reciprocity clause is the ground for this collective action. The peer network is excluded from formal oath-drafting but their solidarity is the mechanism that keeps the charter text binding.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, peer_vassal_network, excluded,
    organized, generational, constrained, regional).

% A king or equivalent higher lord to whom some of the lords themselves are vassals. Royal authority can be invoked as an appellate arbiter in oath disputes, though appeal is costly and requires a sufficiently grave breach. The presence of a higher authority makes it risky for a lord to blatantly violate the oath, since the vassal can seek royal judgment.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, secular_royal_authority, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feudal_oath_reciprocity__vassal_coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(feudal_oath_reciprocity__vassal_coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of how to organize military service, render obligations, and protective reciprocity across dispersed landholdings without a standing army or central tax apparatus. The oath text specifies the terms, duration, and amount of service in advance, reducing bargaining frictions and preventing arbitrary exaction.
% TRANSFER_FUNCTION: Moves military labor (fixed number of service days per year), agricultural renders (grain, animals, labor rents), and counsel-participation from vassal to lord; moves land tenure security and military protection from lord to vassal. The transfer is bounded and explicit in the charter text.
% ABSENT_VOICES: Peasantry bound to the land (unfree serfs and free tenants) have no voice in the oath. They bear the extraction through render obligations and labor dues that their lord demands to meet his vassal duties. They would object to both the vassal's and lord's claims on their labor if present, but they are structurally excluded from the covenant.
% DISAPPEARANCE_RATIONALE: If the oath-charter system vanished, the mechanism for organizing cross-regional military obligation would collapse. Lords would face coordination costs in raising service; vassals would lose tenure security. The result would be either a return to scattered private wars (fragmentation) or the rise of an alternative centralized enforcement mechanism (royal absolutism, standing army). The feudal system as a practical arrangement depends on oath-reciprocity being the coordination solution.
% FOUNDING_PROBLEM: In the early medieval period, Roman administrative apparatus had collapsed. How do you organize military defense, land tenure, and obligation without a centralized state? The oath-charter system emerged as a solution: a localized reciprocal covenant text that both parties could invoke.
% FOUNDING_PROBLEM_CORROBORATION: Medieval chroniclers, charter archives, and the surviving oath texts themselves attest the founding problem. Modern historians (Bloch, Hittable, Reynolds) corroborate that feudal oaths emerged to solve coordination problems of fragmented military and territorial organization. The problem is 'live' in the sense that the solution continued to adapt to changing circumstances (urbanization, royal centralization) for centuries — it was not a fixed response to a solved problem, but an evolving institutional practice.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__vassal_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__vassal_coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__vassal_coordination_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(feudal_oath_reciprocity__vassal_coordination_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feudal_oath_reciprocity__vassal_coordination_reading_tests).
:- end_tests(feudal_oath_reciprocity__vassal_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.35) because the covenant is genuinely reciprocal: both parties receive and pay. The vassal's obligation is fixed (customary days of service, standard renders), not arbitrary — the charter text is the constraint. Suppression is low (0.28) because compliance is not primarily coerced; it flows from mutual interest and the mutual invocation of the text. Theater ratio is low (0.18) because the oath's coordination function is genuine and primary — most activity is performing the actual obligations (service, renders, protection), not maintaining the appearance of legitimacy. Accessibility collapse is moderate (0.42): alternatives to feudal oath exist (private wars, local strongmen), but the oath is the most effective known coordination solution in the medieval period, so it becomes institutionally widespread. Resistance is moderate (0.38): some vassals resist specific obligations or lords resist specific reciprocity claims, but resistance is not systemic — both parties generally accept the framework. The measurement series is largely flat (0.32→0.35 extractiveness over 36 units, theater 0.12→0.18 with a slight dip at end) because the constraint is stable: no drift toward pure extraction (which would be the lord_extraction_reading), no rise in theater to indicate functional decay. The small rise in theater in the middle (to 0.19 at t=30) and dip at t=36 (0.18) reflects the normal oscillation of oath-keeping and breach-dispute cycles: some periods require more reassertion of reciprocal obligation, then equilibrium restores.
 *
 * PERSPECTIVAL GAP:
 *   The lord and vassal seats should compute differently from this story's authored metrics: the lord, as agenda-setter, experiences the oath as a constraint on extractiveness (the charter prevents unlimited seizure); the vassal, as a bound party, experiences the oath as a guarantee of tenure. Both benefit, but from different structural positions. The engine computes this divergence from power atom and role: the lord sits at institutional power and agenda-setter role (lower d because the lord sets terms), the vassal sits at moderate power and beneficiary-payer dual role (d near symmetric). Both seats should classify the constraint as rope (genuine coordination with no victim), but from different motivational angles. The ecclesiastical observer and royal observer seats compute as analytical (external perspective, not collecting or paying directly).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for vassal_class is near 0.5 (symmetric): the vassal benefits from tenure security and protection (low-d direction) but pays in service and renders (high-d direction). The offsetting obligations keep d symmetric. Directionality for lord_class is also near 0.5 (symmetric): the lord benefits from organized vassals (low-d) but pays in providing defense and respecting reciprocity (high-d). Exit options are constrained for both: a vassal breaking the oath loses the fief and faces military retaliation, but the oath text itself is the exit limit — if the lord violates the oath, the vassal can lawfully renounce it (it is written). A lord abandoning vassal oaths loses organized service and faces peer defection. The constraint is enforceable because both parties gain from mutual invocation of the text.
 *
 * MANDATROPHY ANALYSIS:
 *   The vassal_coordination_reading avoids mandatrophy by maintaining alignment between the founding problem (how to organize military coordination without a state) and the constraint's operation (oath-text specifies and binds obligations). The founding problem is live because feudalism continues to evolve as the medieval period progresses — oaths adapt to urbanization, crusades, and royal centralization. The constraint persists because both parties continue to benefit from the coordination. This reading explicitly rejects the claim that the oath is a theater: the 0.18 theater_ratio reflects normal oscillation, not degraded function. The sibling lord_extraction_reading, by contrast, would have much higher theater: a constraint that claims reciprocity but operates as one-way extraction would show high theater_ratio as the coordination story becomes increasingly divorced from actual transfers. That reading would trigger mandatrophy warning (founding problem = coordination, but actual operation = extraction; disappearance_verdict = world_rearranges but only because extraction ends, not because coordination ceases).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    charter_text_enforceability,
    'Is the oath''s reciprocal binding force grounded in the text itself (mutual written commitments that both parties invoke), or in the implicit threat of peer vassal coalition and royal appeal?',
    'Historical analysis of oath-breach disputes: how often did vassals successfully invoke the charter text against lords? How often did disputes resolve through peer solidarity versus royal intervention versus military coercion?',
    'If enforceability is primarily textual and mutual (vassal can hold lord to the charter''s terms), the reading as pure coordination rope is strengthened. If enforceability depends on external threat (peer coalition or royal force), the constraint drifts toward tangled_rope with active enforcement by non-parties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(charter_text_enforceability, empirical, 'Whether oath reciprocity is self-enforcing through text or depends on external enforcement coalition.').

omega_variable(
    lord_extraction_reading_contest,
    'Does the same oath-charter kernel instantiate a different constraint under the lord_extraction_reading, where ''custom'' and ''capacity'' are interpreted maximally by the lord, turning the apparent reciprocity into one-way extraction?',
    'Comparison of this vassal_coordination_reading''s measurement series (low extractiveness, flat trajectory) against the lord_extraction_reading''s measurements. If the lord reading measures higher extractiveness and rising theater_ratio (custom claims becoming pretexts), the readings are genuinely distinct constraints on the same kernel.',
    'If the readings diverge in measured extractiveness, the kernel contest is a structural fact — different actors read the same text as producing different constraints. This supports the ε-invariance principle: the kernel is one, but the constraints are two.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(lord_extraction_reading_contest, conceptual, 'Whether the oath-reciprocity kernel admits two structurally distinct constraint readings.').

omega_variable(
    peasant_extraction_externality,
    'The measurable extractiveness of the oath constraint (0.35) applies to lord-vassal reciprocity. But the oath''s operation cascades extraction downward to the peasantry who actually produce the renders. Is the peasant layer a separate constraint, or a structural externality of the vassal-level rope?',
    'Conceptual decomposition: if the oath-charter between lord and vassal is separately analyzable (its own ε, its own beneficiaries/victims), then peasant extraction belongs in a second story (peasant_serfdom_or_tenancy constraint). The network link between the two stories (peasant_constraint affects vassal_constraint) records the structural hierarchy.',
    'If decomposed, the vassal_coordination_reading remains a true low-ε rope at its level of analysis. The overall extraction from peasantry to lord is the sum of two constraints plus their interaction. If not decomposed, the claimed type must adjust for the fact that the oath''s operation enables further downward extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(peasant_extraction_externality, conceptual, 'Whether peasant-layer extraction is part of the feudal_oath constraint or a separate constraint linked by network dependency.').

omega_variable(
    sibling_reading_mutual_foreclosure,
    'The ecclesiastical_mediation_reading introduces Christian charity and sacramental limits, while the lord_extraction_reading asserts maximal secular extraction. Can all three readings (ecclesiastical, extraction, coordination) coexist as held by different medieval actors, or does one foreclosure relation hold (e.g., does ecclesiastical mediation structurally foreclose unlimited extraction)?',
    'Historical investigation: did ecclesiastical authority genuinely constrain lord extraction, or did ecclesiastical mediators themselves claim benefit from the arrangement? Did the three framings represent distinct institutional viewpoints (church vs. lords vs. vassals), or did individual actors hold contradictory framings simultaneously?',
    'If the readings coexist (different actors, different institutional seats), all three remain live — the kernel admits three constraint instantiations. If one reading forecloses another (e.g., if ecclesiastical authority''s actual practice was to preserve lord extraction), the reading_relations should reflect foreclosure rather than coexistence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_mutual_foreclosure, empirical, 'Whether sibling readings of the feudal oath kernel coexist or foreclose one another.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__vassal_coordination_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(feud_tr_t0, observed).
narrative_ontology:measurement(feud_tr_t6, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 6, 0.13).
narrative_ontology:measurement_basis(feud_tr_t6, observed).
narrative_ontology:measurement(feud_tr_t12, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 12, 0.15).
narrative_ontology:measurement_basis(feud_tr_t12, observed).
narrative_ontology:measurement(feud_tr_t18, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 18, 0.17).
narrative_ontology:measurement_basis(feud_tr_t18, observed).
narrative_ontology:measurement(feud_tr_t24, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 24, 0.18).
narrative_ontology:measurement_basis(feud_tr_t24, observed).
narrative_ontology:measurement(feud_tr_t30, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 30, 0.19).
narrative_ontology:measurement_basis(feud_tr_t30, observed).
narrative_ontology:measurement(feud_tr_t36, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 36, 0.18).
narrative_ontology:measurement_basis(feud_tr_t36, observed).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(feud_be_t0, observed).
narrative_ontology:measurement(feud_be_t6, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 6, 0.33).
narrative_ontology:measurement_basis(feud_be_t6, observed).
narrative_ontology:measurement(feud_be_t12, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 12, 0.34).
narrative_ontology:measurement_basis(feud_be_t12, observed).
narrative_ontology:measurement(feud_be_t18, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 18, 0.35).
narrative_ontology:measurement_basis(feud_be_t18, observed).
narrative_ontology:measurement(feud_be_t24, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 24, 0.36).
narrative_ontology:measurement_basis(feud_be_t24, observed).
narrative_ontology:measurement(feud_be_t30, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 30, 0.35).
narrative_ontology:measurement_basis(feud_be_t30, observed).
narrative_ontology:measurement(feud_be_t36, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 36, 0.35).
narrative_ontology:measurement_basis(feud_be_t36, observed).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t0, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 0, 0.24).
narrative_ontology:measurement_basis(feud_su_t0, observed).
narrative_ontology:measurement(feud_su_t6, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 6, 0.25).
narrative_ontology:measurement_basis(feud_su_t6, observed).
narrative_ontology:measurement(feud_su_t12, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 12, 0.26).
narrative_ontology:measurement_basis(feud_su_t12, observed).
narrative_ontology:measurement(feud_su_t18, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 18, 0.27).
narrative_ontology:measurement_basis(feud_su_t18, observed).
narrative_ontology:measurement(feud_su_t24, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 24, 0.28).
narrative_ontology:measurement_basis(feud_su_t24, observed).
narrative_ontology:measurement(feud_su_t30, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 30, 0.28).
narrative_ontology:measurement_basis(feud_su_t30, observed).
narrative_ontology:measurement(feud_su_t36, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 36, 0.28).
narrative_ontology:measurement_basis(feud_su_t36, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__vassal_coordination_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(feudal_oath_reciprocity__vassal_coordination_reading, 0.12).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__vassal_coordination_reading, feudal_oath_reciprocity__lord_extraction_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__vassal_coordination_reading, feudal_oath_reciprocity__ecclesiastical_mediation_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__vassal_coordination_reading, medieval_peasant_serfdom_extraction).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the feudal_oath_reciprocity kernel. The lord_extraction_reading and ecclesiastical_mediation_reading are sibling constraints on the same kernel, each with its own ε, beneficiary/victim structure, and measurement series. They are not alternative measurements of this constraint — they are structurally distinct constraints instantiated by different readings of the same oath-charter text. Network links show that all three readings affect each other (a shift in one reading's authority or credibility changes the contest across readings) and that the medieval_peasant_serfdom constraint operates at a lower institutional level, with feudal oath serving as the upper-level coordination that enables peasant extraction at the lower level.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
