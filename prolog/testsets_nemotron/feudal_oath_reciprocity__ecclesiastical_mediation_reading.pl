% ============================================================================
% CONSTRAINT STORY: feudal_oath_reciprocity__ecclesiastical_mediation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feudal_oath_reciprocity__ecclesiastical_mediation_reading, []).

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
 *   constraint_id: feudal_oath_reciprocity__ecclesiastical_mediation_reading
 *   human_readable: Feudal Oath Bound by Christian Charity and Sacramental Obligations
 *   domain: medieval/political_economy/legal_history/institutional_analysis
 *
 * SUMMARY:
 *   This constraint story instantiates the ecclesiastical_mediation_reading
 *   of the feudal_oath_reciprocity kernel. The kernel is the feudal oath
 *   itself — a stabilized commitment structure persisting from roughly
 *   900–1300 across Latin Christendom. This reading holds that the oath's
 *   primary structural function is to limit secular extraction through
 *   Christian charity and sacramental obligation, mediated by ecclesiastical
 *   authority. The church gains interpretive authority; lords face
 *   theological constraints on their extraction rights. The sibling readings
 *   — lord_extraction_reading (oath authorizes maximal extraction) and
 *   vassal_coordination_reading (oath establishes fixed reciprocal
 *   obligations by charter) — are different constraints instantiated from the
 *   same kernel, not variations of this one. This story follows Rule 1: it
 *   generates only this reading as a clean, ε-invariant constraint with its
 *   own stable ε (0.42), beneficiary/victim structure, and type
 *   (tangled_rope). The committer structure (kernel_id, reading_id, sibling
 *   relations) is routed to omega variables per Rule 2.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.42).
domain_priors:suppression_score(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.38).
domain_priors:theater_ratio(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, tangled_rope).
narrative_ontology:human_readable(feudal_oath_reciprocity__ecclesiastical_mediation_reading, "Feudal Oath Bound by Christian Charity and Sacramental Obligations").
narrative_ontology:topic_domain(feudal_oath_reciprocity__ecclesiastical_mediation_reading, "medieval/political_economy/legal_history/institutional_analysis").

domain_priors:requires_active_enforcement(feudal_oath_reciprocity__ecclesiastical_mediation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 'db72a515-d08c-4832-a2c5-0603807731b0').
narrative_ontology:cs_kernel_codification('db72a515-d08c-4832-a2c5-0603807731b0', distributed).
narrative_ontology:cs_authority_grounding('db72a515-d08c-4832-a2c5-0603807731b0', lineage).
narrative_ontology:cs_interpretation_layer_present('db72a515-d08c-4832-a2c5-0603807731b0').
narrative_ontology:cs_reading_relation('db72a515-d08c-4832-a2c5-0603807731b0', feudal_oath_reciprocity__lord_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('db72a515-d08c-4832-a2c5-0603807731b0', feudal_oath_reciprocity__vassal_coordination_reading, influences).
narrative_ontology:cs_axiom('db72a515-d08c-4832-a2c5-0603807731b0', foundational, sacramental_oath_binds_conscience_beyond_law).
narrative_ontology:cs_axiom_status(sacramental_oath_binds_conscience_beyond_law, holdable).
narrative_ontology:cs_axiom_grounding('db72a515-d08c-4832-a2c5-0603807731b0', sacramental_oath_binds_conscience_beyond_law, deontological).
narrative_ontology:cs_axiom('db72a515-d08c-4832-a2c5-0603807731b0', foundational, christian_charity_limits_secular_extraction_rights).
narrative_ontology:cs_axiom_status(christian_charity_limits_secular_extraction_rights, holdable).
narrative_ontology:cs_axiom_grounding('db72a515-d08c-4832-a2c5-0603807731b0', christian_charity_limits_secular_extraction_rights, deontological).
narrative_ontology:cs_axiom('db72a515-d08c-4832-a2c5-0603807731b0', secondary, ecclesiastical_authority_mediates_temporal_obligations).
narrative_ontology:cs_axiom_status(ecclesiastical_authority_mediates_temporal_obligations, holdable).
narrative_ontology:cs_axiom_grounding('db72a515-d08c-4832-a2c5-0603807731b0', ecclesiastical_authority_mediates_temporal_obligations, conventional).
narrative_ontology:cs_reference_frame('db72a515-d08c-4832-a2c5-0603807731b0', carolingian_sacramental_kingship).
narrative_ontology:cs_drift_state('db72a515-d08c-4832-a2c5-0603807731b0', high_medieval_papal_monarchy, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('db72a515-d08c-4832-a2c5-0603807731b0', '').
narrative_ontology:cs_kernel_id(feudal_oath_reciprocity__ecclesiastical_mediation_reading, feudal_oath_reciprocity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__ecclesiastical_mediation_reading, ecclesiastical_hierarchy).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__ecclesiastical_mediation_reading, parish_priests).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__ecclesiastical_mediation_reading, monastic_orders).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, secular_lords).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, vassal_knights).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, serfs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__ecclesiastical_mediation_reading, secular_lords).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__ecclesiastical_mediation_reading, vassal_knights).
narrative_ontology:constraint_vindicates(feudal_oath_reciprocity__ecclesiastical_mediation_reading, christian_charity_limits_secular_power).
narrative_ontology:constraint_vindicates(feudal_oath_reciprocity__ecclesiastical_mediation_reading, sacramental_oath_binds_conscience_beyond_law).
narrative_ontology:constraint_vindicates(feudal_oath_reciprocity__ecclesiastical_mediation_reading, church_as_mediator_of_temporal_obligations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bishops and popes interpret sacramental oath obligations through canon law and penitential practice. They claim authority to judge whether secular lords fulfill their Christian charity duties toward vassals and peasants. Their interpretive power derives from controlling the sacramental economy — communion, confession, last rites — that legitimizes the oath. They collect no direct revenue from the constraint but gain institutional authority and moral leverage over secular rulers.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, ecclesiastical_hierarchy, agenda_setter,
    institutional, civilizational, analytical, continental).

% Local priests administer the oath-swearing ceremonies, hear confessions of lords who violate charity obligations, and enforce penitential discipline. Their professional identity and social standing are fused with the sacramental system — exit means abandoning their vocation. They benefit from the constraint's enforcement through enhanced pastoral authority and community status, but bear the burden of mediating between lords and peasants in concrete disputes.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, parish_priests, beneficiary,
    organized, generational, identity_locked, local).

% Monasteries serve as centers of charity, hospitality, and manuscript preservation that embody the Christian charity ideal. They receive land grants and tithes from lords seeking spiritual merit, and their prayers are believed to secure the lords' salvation. Their corporate identity depends on the theological framework that makes charity obligatory — they cannot exit without dissolving their order's purpose. They gain material resources and spiritual authority from the constraint's operation.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, monastic_orders, beneficiary,
    organized, generational, identity_locked, regional).

% Lords swear feudal oaths on relics and receive homage from vassals, but the ecclesiastical reading binds them to Christian charity limits on extraction — they cannot demand more than vassals can render without sin. They benefit from the oath's coordination function (vassal loyalty, military service) but pay through constrained extraction rights and penitential exposure. Their exit is constrained: rejecting the sacramental framework risks excommunication and loss of legitimacy, but they can negotiate the boundaries through patronage of compliant clergy.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, secular_lords, payer,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__ecclesiastical_mediation_reading, secular_lords, beneficiary).

% Knights owe military service and counsel to lords, but the ecclesiastical reading gives them theological grounds to resist excessive demands — their oath to God limits their oath to their lord. They benefit from protection against arbitrary extraction but pay through continued service obligations and the cognitive burden of divided loyalty. Their exit is constrained by land tenure and military culture; they can appeal to ecclesiastical courts but risk lordly retaliation.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, vassal_knights, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__ecclesiastical_mediation_reading, vassal_knights, beneficiary).

% Peasants are bound to the land and owe labor, produce, and fees to lords. The ecclesiastical reading offers them theoretical protection — lords must not extract beyond subsistence — but they have no standing to enforce it. Parish priests may intervene charitably, but serfs cannot access canon courts. They pay the full cost of extraction with only intermittent, discretionary relief. Exit is effectively impossible: flight is punished, and the sacramental framework that might protect them also binds them to their station as God's will.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, serfs, payer,
    powerless, immediate, trapped, local).

% Scholars and practitioners of canon law systematize the theological limits on secular power. They write treatises on just price, usury, and the obligations of charity that constrain lordly extraction. Their professional standing depends on the ecclesiastical framework's intellectual coherence. They observe the constraint's operation from within the interpretive community, documenting where lords comply or evade, but do not directly bear its costs or collect its gains.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, canon_lawyers, observer,
    organized, generational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The feudal oath coordinates military service, land tenure, and dispute resolution across a fragmented political landscape by sacralizing the reciprocal bond — the vassal's fidelity and the lord's protection become matters of eternal salvation, not merely temporal contract.
% TRANSFER_FUNCTION: The arrangement moves interpretive authority over temporal obligations from secular lords to ecclesiastical courts and confessors; it moves material resources (tithes, land grants, charitable distributions) from lords to church institutions; it moves spiritual assurance (absolution, salvation) from clergy to laity conditional on compliance with charity norms.
% ABSENT_VOICES: Serfs and unfree peasants have no voice in the canonical debates that define charity's limits — their interests are represented only when clergy choose to advocate. Merchant towns and Jewish communities operate under parallel legal regimes and are excluded from the feudal-sacramental framework entirely. Women of all classes are largely absent from oath-swearing and its ecclesiastical mediation.
% DISAPPEARANCE_RATIONALE: If the ecclesiastical mediation of feudal oaths vanished overnight, lords would lose the primary theological check on their extraction rights; vassals would lose their strongest ground for resisting excessive demands; the church would lose its central institutional leverage over secular society. The feudal system would not collapse — the lord_extraction_reading would become dominant — but the pattern of obligations would shift decisively toward maximal extraction.
% FOUNDING_PROBLEM: The Carolingian and post-Carolingian collapse of central authority left local lords with effective sovereignty but no legitimate framework for their power over vassals and peasants. The church provided a universal moral vocabulary — Christian charity, sacramental oaths, the communion of saints — that could bind lords to reciprocal obligations without requiring a state they did not have.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary monastic chroniclers (Orderic Vitalis, Suger of St. Denis) attest the church's deliberate construction of this framework. Secular law codes (Capitularies, Assizes) increasingly incorporate canonical language. Modern historians debate: Duby and Bloch emphasize the church's role in taming violence; Reynolds and Brown argue the 'feudal mutation' was primarily a secular adaptation the church later sacralized. No single account commands consensus outside its own tradition.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__ecclesiastical_mediation_reading, world_rearranges).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__ecclesiastical_mediation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__ecclesiastical_mediation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feudal_oath_reciprocity__ecclesiastical_mediation_reading_tests).
:- end_tests(feudal_oath_reciprocity__ecclesiastical_mediation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate: the constraint extracts interpretive authority and material resources for the church while constraining lordly extraction, but does not eliminate secular power. Suppression (0.38) is moderate: the constraint relies on internalized sacramental discipline (confession, penance, fear of damnation) more than physical coercion, but excommunication and interdict are real enforcement tools. Theater ratio (0.28) reflects genuine coordination (the oath solves a real collective-action problem in a stateless landscape) alongside performative piety — lords patronize monasteries and endow chapels partly for spiritual merit, partly for legitimacy theater. Accessibility collapse (0.52) is moderate: the sacramental framework makes alternatives (pure power, Roman law revival) cognitively difficult but not impossible — the Investiture Controversy and rise of royal law show exit paths. Resistance (0.45) is moderate: lords resist specific applications (refusing to restore unjust gains, appointing compliant bishops) but rarely reject the framework outright.
 *
 * PERSPECTIVAL GAP:
 *   The ecclesiastical_hierarchy and monastic_orders experience this as a rope (genuine coordination of a fragmented society through shared sacramental meaning). Secular_lords and vassal_knights experience it as a tangled_rope (they gain coordination benefits but pay through constrained extraction and divided loyalty). Serfs experience it as a snare (theoretical protection without enforcement access). The engine computes this per-seat divergence from the structural data — the authored claimed_type (tangled_rope) reflects the constraint's aggregate character, not any single seat's experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (ecclesiastical_hierarchy, parish_priests, monastic_orders) collect interpretive authority, material resources, and spiritual standing from the constraint's operation — their directionality d is low (near 0.15-0.25). Victims (secular_lords, vassal_knights, serfs) bear constrained extraction rights, divided loyalty, and ineffective protection — their d is high (0.65-0.85 for serfs, 0.55-0.70 for lords/knights). The dual-role agents (lords and knights as both payer and beneficiary) sit near symmetric (d ~ 0.5) — they gain the oath's coordination function but pay its theological price. Exit options modulate this: serfs are trapped (d → 1.0), clergy are identity_locked (d → 0.0 for beneficiaries, d → 0.3 for priests as mediators), lords are constrained (d moderated by patronage exit).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (taming lordly power in a stateless landscape) is contested — the church claims it remains live (violence persists), secular historians argue it was solved by state formation the church resisted. This prevents mislabeling: if the founding problem is dead, the constraint is a piton (theatrical maintenance of obsolete function); if live, it remains a tangled_rope (genuine coordination with asymmetric extraction). The ecclesiastical reading's claim that charity limits are eternal (deontological axiom) makes mandatrophy resolution internal to the reading — the church cannot declare its own mandate resolved without abandoning its authority ground. This is documented in the omega on deontological foreclosure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the ecclesiastical_mediation_reading a distinct constraint with its own stable ε, or a perspectival slice of a single feudal_oath_reciprocity constraint?',
    'Apply ε-invariance test: if measuring the constraint via lordly extraction rates vs. ecclesiastical court cases vs. vassal service compliance yields different ε values, they are different constraints. Current evidence suggests distinct ε: lord_extraction_reading ε ~0.65 (high extraction), vassal_coordination_reading ε ~0.25 (low extraction, high coordination), this reading ε ~0.42 (moderate, mixed).',
    'If single constraint, the three readings are observer perspectives on one structure — the framework''s per-seat computation handles this. If three constraints, they form a constraint family linked by network.affects_constraints with distinct classifications. The kernel context declares them separate constraints per DP-001.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel''s readings instantiate one constraint or a constraint family.').

omega_variable(
    deontological_foreclosure_risk,
    'Does the reading''s foundational axiom (sacramental_oath_binds_conscience_beyond_law) foreclose on empirical evidence that the constraint''s coordination function has been superseded by state law?',
    'Track the axiom''s status across the interval: if the church formally acknowledges state law has absorbed the coordination function but maintains the axiom as binding, the axiom is holdable but empirically overridden. If the church revises the axiom (e.g., Vatican II on religious liberty), it becomes overridden. The engine computes foreclosure from grounding_type + drift_state.',
    'If the axiom is empirically_contingent and overridden, the constraint''s claimed_type may drift toward piton (theatrical maintenance). If deontological, it remains holdable regardless of drift — the constraint''s legitimacy is intrinsic, not functional. This reading declares the axiom as deontological, foreclosing mandatrophy resolution from within.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deontological_foreclosure_risk, conceptual, 'Whether the reading''s deontological axiom prevents internal mandatrophy recognition.').

omega_variable(
    serf_protection_effectiveness,
    'How effectively did the ecclesiastical charity constraint actually protect serfs from extraction, versus serving as ideological cover for the existing order?',
    'Compare manorial records of lordly exactions before/after canonical reforms (e.g., Fourth Lateran 1215), and correlate with ecclesiastical court activity on behalf of peasants. If exactions decline and court cases rise, protection is real. If exactions continue unchanged while charity rhetoric increases, it is cover.',
    'Real protection → constraint has genuine coordination function for the powerless (rope element strengthened). Cover → constraint is snare for serfs (extraction with ideological cover). The current moderate ε and mixed seat divergence suggest partial, uneven protection — stronger in regions with active bishops, weaker where lords control appointments.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(serf_protection_effectiveness, empirical, 'Whether the constraint''s charity function materially benefited its most vulnerable subjects.').

omega_variable(
    sacramental_vs_charter_authority,
    'Does the ecclesiastical reading''s authority derive from sacramental ontology (oath binds conscience) or from the church''s institutional position as charter-keeper and court-operator?',
    'Analyze conflicts where sacramental rhetoric and institutional interest diverge — e.g., when a lord violates charity but the local bishop depends on his patronage. If the bishop enforces anyway, sacramental ontology drives the constraint. If the bishop accommodates, institutional interest drives it. The Investiture Controversy and Albigensian Crusade offer test cases.',
    'Sacramental ontology → constraint is rope/tangled_rope with genuine coordination function. Institutional interest → constraint is snare/tangled_rope where church extracts interpretive authority for its own benefit. The reading claims the former; the lord_extraction_reading claims the latter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sacramental_vs_charter_authority, conceptual, 'Whether the constraint''s ecclesiastical authority is ontological or institutional.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 1000, 1300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feudal_oath_ecclesiastical_tr_t1000, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1000, 0.15).
narrative_ontology:measurement(feudal_oath_ecclesiastical_tr_t1050, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1050, 0.18).
narrative_ontology:measurement(feudal_oath_ecclesiastical_tr_t1100, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1100, 0.22).
narrative_ontology:measurement(feudal_oath_ecclesiastical_tr_t1150, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1150, 0.25).
narrative_ontology:measurement(feudal_oath_ecclesiastical_tr_t1200, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1200, 0.27).
narrative_ontology:measurement(feudal_oath_ecclesiastical_tr_t1250, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1250, 0.28).
narrative_ontology:measurement(feudal_oath_ecclesiastical_tr_t1300, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1300, 0.28).

% Extraction over time
narrative_ontology:measurement(feudal_oath_ecclesiastical_be_t1000, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1000, 0.28).
narrative_ontology:measurement(feudal_oath_ecclesiastical_be_t1050, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1050, 0.32).
narrative_ontology:measurement(feudal_oath_ecclesiastical_be_t1100, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1100, 0.35).
narrative_ontology:measurement(feudal_oath_ecclesiastical_be_t1150, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1150, 0.38).
narrative_ontology:measurement(feudal_oath_ecclesiastical_be_t1200, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1200, 0.4).
narrative_ontology:measurement(feudal_oath_ecclesiastical_be_t1250, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1250, 0.41).
narrative_ontology:measurement(feudal_oath_ecclesiastical_be_t1300, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1300, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(feudal_oath_ecclesiastical_su_t1000, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1000, 0.25).
narrative_ontology:measurement(feudal_oath_ecclesiastical_su_t1050, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1050, 0.3).
narrative_ontology:measurement(feudal_oath_ecclesiastical_su_t1100, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1100, 0.33).
narrative_ontology:measurement(feudal_oath_ecclesiastical_su_t1150, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1150, 0.36).
narrative_ontology:measurement(feudal_oath_ecclesiastical_su_t1200, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1200, 0.37).
narrative_ontology:measurement(feudal_oath_ecclesiastical_su_t1250, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1250, 0.38).
narrative_ontology:measurement(feudal_oath_ecclesiastical_su_t1300, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1300, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__ecclesiastical_mediation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.08).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__ecclesiastical_mediation_reading, feudal_oath_reciprocity__lord_extraction_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__ecclesiastical_mediation_reading, feudal_oath_reciprocity__vassal_coordination_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__ecclesiastical_mediation_reading, investiture_controversy_settlement).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__ecclesiastical_mediation_reading, canon_law_codification_1234).

% DUAL FORMULATION NOTE:
% This reading decomposes the feudal_oath_reciprocity kernel alongside lord_extraction_reading (ε ~0.65, snare) and vassal_coordination_reading (ε ~0.25, rope). The ε values differ structurally: this reading centers ecclesiastical interpretive authority as a moderating extraction force; lord_extraction_reading centers maximal secular extraction; vassal_coordination_reading centers fixed charter obligations. All three share the same kernel (the oath as stabilized commitment) but instantiate different constraints with different beneficiary/victim structures and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(feudal_oath_reciprocity__ecclesiastical_mediation_reading, organized, 0.25).
constraint_indexing:directionality_override(feudal_oath_reciprocity__ecclesiastical_mediation_reading, powerful, 0.6).
constraint_indexing:directionality_override(feudal_oath_reciprocity__ecclesiastical_mediation_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
