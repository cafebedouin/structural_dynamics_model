% ============================================================================
% CONSTRAINT STORY: provincial_sovereignty_boundary__compact_federalism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_provincial_sovereignty_boundary__compact_federalism, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: provincial_sovereignty_boundary__compact_federalism
 *   human_readable: Provincial Sovereignty Boundary (Compact Federalism Reading)
 *   domain: political_economy/federalism/resource_governance
 *
 * SUMMARY:
 *   Confederation was framed as a voluntary compact among sovereign provinces
 *   retaining residual authority over local matters. This reading asserts
 *   that federal authority is conditional on provincial consent, that
 *   equalization transfers are negotiable, that provinces retain meaningful
 *   sovereignty over resource policy and environmental regulation, and that
 *   exit from the federation is theoretically available if pursued through
 *   negotiation rather than unilateral declaration. The compact reading sits
 *   in direct structural tension with the constitutional-subordination
 *   reading (provinces are creatures of federal constitution with no exit
 *   right) and the resource-sovereignty reading (s.92A absolute provincial
 *   control over natural resources = territorial sovereignty). The constraint
 *   described here is the standing arrangement under the compact reading's
 *   lights: it extracts moderately through equalization and federal override
 *   while maintaining the fiction of residual provincial authority.
 *
 * KEY AGENTS:
 *   - Federal authority: sets federal-provincial coordination rules; administers equalization; interprets constitutional jurisdiction; collects and redistributes tax revenue.
 *   - Equalization-recipient provinces: receive transfer income; benefit from constraint that limits resource-rich provinces' unilateral escape.
 *   - Resource-rich provinces: bear equalization obligations; face federal resource-policy override; claim inherent sovereignty but are identity-locked into the federation.
 *   - Provincial governments constrained by federal override: negotiate constantly over jurisdiction; prevented from unilateral action in areas claimed as federal.
 *   - Regional separatist movements: excluded from formal power; argue the compact is exhausted and exit is a right, not a privilege.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_sovereignty_boundary__compact_federalism, 0.38).
domain_priors:suppression_score(provincial_sovereignty_boundary__compact_federalism, 0.42).
domain_priors:theater_ratio(provincial_sovereignty_boundary__compact_federalism, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, extractiveness, 0.38).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__compact_federalism, tangled_rope).
narrative_ontology:human_readable(provincial_sovereignty_boundary__compact_federalism, "Provincial Sovereignty Boundary (Compact Federalism Reading)").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__compact_federalism, "political_economy/federalism/resource_governance").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__compact_federalism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__compact_federalism, '7eef60fd-afaa-445d-ace7-73c642217473').
narrative_ontology:cs_kernel_codification('7eef60fd-afaa-445d-ace7-73c642217473', fixed_text).
narrative_ontology:cs_authority_grounding('7eef60fd-afaa-445d-ace7-73c642217473', lineage).
narrative_ontology:cs_interpretation_layer_present('7eef60fd-afaa-445d-ace7-73c642217473').
narrative_ontology:cs_reading_relation('7eef60fd-afaa-445d-ace7-73c642217473', provincial_sovereignty_boundary__constitutional_subordination, coexists_with).
narrative_ontology:cs_reading_relation('7eef60fd-afaa-445d-ace7-73c642217473', provincial_sovereignty_boundary__resource_sovereignty_primacy, influences).
narrative_ontology:cs_axiom('7eef60fd-afaa-445d-ace7-73c642217473', foundational, confederation_as_voluntary_compact_among_sovereigns).
narrative_ontology:cs_axiom_status(confederation_as_voluntary_compact_among_sovereigns, holdable).
narrative_ontology:cs_axiom_grounding('7eef60fd-afaa-445d-ace7-73c642217473', confederation_as_voluntary_compact_among_sovereigns, conventional).
narrative_ontology:cs_axiom('7eef60fd-afaa-445d-ace7-73c642217473', foundational, residual_provincial_sovereignty_doctrine).
narrative_ontology:cs_axiom_status(residual_provincial_sovereignty_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('7eef60fd-afaa-445d-ace7-73c642217473', residual_provincial_sovereignty_doctrine, deontological).
narrative_ontology:cs_reference_frame('7eef60fd-afaa-445d-ace7-73c642217473', confederation_1867_compact_framework).
narrative_ontology:cs_drift_state('7eef60fd-afaa-445d-ace7-73c642217473', contemporary_2026, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7eef60fd-afaa-445d-ace7-73c642217473', '').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__compact_federalism, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__compact_federalism, federal_authority).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__compact_federalism, equalization_recipient_provinces).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, resource_rich_provinces).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, provincial_governments_constrained_by_federal_override).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__compact_federalism, confederation_as_voluntary_compact).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__compact_federalism, residual_provincial_sovereignty_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the terms of federal-provincial coordination through constitutional interpretation and legislative action. Administers equalization transfers, claims jurisdiction over interprovincial trade and climate policy, and interprets the division of powers. Collects tax revenue from all provinces and redistributes it according to equalization formulas it largely controls. The constraint's persistence depends on federal ability to enforce its reading of provincial limits.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, federal_authority, agenda_setter,
    institutional, generational, analytical, national).

% Receive substantial equalization transfers from federal revenue, enabling public service provision that provincial tax bases alone could not fund. They benefit from the federal coordination mechanism and from the constraint that limits resource-rich provinces' ability to escape the equalization pool unilaterally. Their exit options are constrained — departing Confederation means losing transfer income with no guarantee of comparable alternative arrangements.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, equalization_recipient_provinces, beneficiary,
    moderate, generational, constrained, national).

% Bear substantial equalization obligations while federal interpretation of s.92A (provincial resource ownership) constrains their ability to weaponize resource policy for unilateral advantage. They claim inherent sovereignty over their natural resources and argue the compact grants them the right to exit resource-sharing arrangements under duress. Their exit is theoretically negotiable but practically constrained by identity fusion with the federal framework and by the political cost of secession.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, resource_rich_provinces, payer,
    powerful, biographical, identity_locked, national).

% Face federal authority overriding provincial jurisdiction in areas where the compact reading says provinces retain residual authority — climate policy, environmental regulation, interprovincial trade. They must negotiate every expansion of federal reach rather than possessing it as inherent right. The constraint forces constant coordination and renegotiation, preventing unilateral provincial action while also preventing pure federal hierarchy.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, provincial_governments_constrained_by_federal_override, payer,
    organized, biographical, constrained, national).

% Would argue that the compact is exhausted, that resource sovereignty is absolute, and that negotiated exit under duress is an open door to independence. They are excluded from the constraint's operation — federal and provincial governments agree on constitutional boundaries even as they contest interpretation. Separatist movements would overturn both the compact reading and the equalization mechanism if admitted to formal power.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, regional_separatist_movements, excluded,
    organized, biographical, trapped, regional).

% Monitor federal-provincial coordination on trade policy and interprovincial tariff-like barriers. They benefit from federal capacity to enforce interprovincial trade openness (reducing barriers) but also depend on provincial stability and investment security. Their interest is in predictable federal authority, not in whether that authority rests on compact or constitution.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, international_trading_partners, observer,
    institutional, generational, analytical, global).

% Hold treaty and inherent rights that both federal and provincial governments claim jurisdiction over. The provincial sovereignty constraint does not directly govern them, but its outcome — whether provinces are sovereign or subordinate — reshapes indigenous governance space. They are largely excluded from the compact negotiation, though some recent interpretations fold indigenous consent into the federal-provincial dialogue.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, first_nations_and_indigenous_governments, observer,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__compact_federalism, first_nations_and_indigenous_governments, excluded).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(provincial_sovereignty_boundary__compact_federalism, federal_authority).
narrative_ontology:fixing_cost_class(provincial_sovereignty_boundary__compact_federalism, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates resource distribution (equalization), interprovincial trade openness, and national policy coherence (climate, environmental standards) across a geographically and economically diverse federation. The compact framing solves the problem of how diverse units retain autonomy while achieving collective action on matters affecting all.
% TRANSFER_FUNCTION: Moves tax revenue from resource-rich and economically dominant provinces to equalization-recipient provinces; moves regulatory authority from provinces to federal government in areas of interprovincial spillover (environmental policy, trade); moves policy-setting power upward through federal override of provincial decisions federal actors deem nationally important.
% ABSENT_VOICES: Indigenous governments and First Nations are structurally excluded from the provincial sovereignty compact even though they hold concurrent or prior claims to jurisdiction. Separatist movements and resource-sovereignty absolutists would argue the compact is a fiction masking federal subordination of provinces; they are excluded by their opposition to the federal framework itself.
% DISAPPEARANCE_RATIONALE: If the compact reading and its enforcement machinery vanished, equalization transfers would cease unless renegotiated; provinces would unilaterally control resource policy and environmental regulation; interprovincial trade barriers would rise; the federation would either stabilize into a looser coordination mechanism or fragment into independent units. The current economic and political order depends on provincial acceptance of limits on unilateral action.
% FOUNDING_PROBLEM: Thirteen British North American colonies with divergent resources and economies needed to unite for defense and commercial coordination without dissolving into a unitary state that erased regional identity and autonomy. The Confederation compact promised federal authority over trade and defense, residual provincial sovereignty over local matters, and equalization to prevent resource-rich units from dominating or exiting.
% FOUNDING_PROBLEM_CORROBORATION: Federal authority and equalization-recipient provinces attest the founding problem is still live: without coordination, provinces would fragment and national coherence would collapse. Resource-rich provinces and separatist movements attest the founding problem is solved: provinces are economically viable, international actors now deal with them directly, and continued federal control of resource policy and equalization is rent extraction, not necessary coordination. Independent constitutional scholars and political economists outside the benefiting parties document the shift: the founding coordination function (defense, trade openness) is provided by other means; current federal-provincial disputes center on value redistribution and policy override, not collective action on genuine coordination problems.
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__compact_federalism, world_rearranges).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__compact_federalism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__compact_federalism, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(provincial_sovereignty_boundary__compact_federalism, 'none', 1).
narrative_ontology:epsilon_provenance(provincial_sovereignty_boundary__compact_federalism, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(provincial_sovereignty_boundary__compact_federalism_tests).
:- end_tests(provincial_sovereignty_boundary__compact_federalism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at interval end) because the constraint does coordinate genuine problems (interprovincial trade, defense) while also transferring value upward (equalization obligation, policy override). The constraint is NOT pure extraction — the coordination function is real and provinces do retain meaningful authority. Suppression is likewise moderate (0.42): the constraint is enforced (federal override machinery, constitutional courts preventing unilateral secession), but provinces retain organized power and can collectively renegotiate terms. Theater rises slightly (0.12 to 0.28) because the compact's language of 'residual sovereignty' and 'negotiated exit' becomes increasingly performative as federal authority tightens and exit remains theoretically available but practically impossible. The measurement series spans 40 time units (roughly 150 years of Canadian federalism) and tracks accumulating federal assertion of authority despite compact language. All three metrics share a single time grid so each is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The federal agenda-setter and equalization-recipient provinces perceive this constraint as successful coordination they depend on; from their seat, provincial exit is rightfully conditional because unilateral departure would destabilize equalization and trade. Resource-rich provinces perceive the same constraint as enforced extraction disguised by compact language; from their seat, exit rights should be unconditional because they were promised residual sovereignty they now cannot exercise. The engine computes this divergence from power, exit_options, and beneficiary/victim declarations — the authored claim (tangled_rope) reflects federal-seat perception; the metrics reflect the constraint's actual operation as experienced by payer seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal authority and equalization recipients are net beneficiaries (d near 0.0–0.3): they extract value from the constraint, set its terms, and control whether provinces can exit. Resource-rich provinces and provincially-constrained governments are net targets (d near 0.6–1.0): they bear equalization costs, lose unilateral authority, and face federal override. Identity-lock on resource-rich provinces is the key differentiator: they could theoretically exit (the constraint does not physically prevent it), but provincial identity is fused with federalism through institutional entrenchment, political culture, and economic integration — exit is theoretically negotiable but practically trapped. Without the identity-lock designation, directionality would compute as arbitrage (unconstrained exit), which would incorrectly downgrade extractiveness.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids the mandatrophy trap because the founding coordination problem (collective defense, interprovincial trade openness) remains structurally real — the constraint did not atrophy its function, the function matured and became bureaucratized. The risk is misclassification as pure rope (genuine coordination with no extraction); the metrics prevent that. The complementary risk is misclassification as snare (pure extraction with coordination cover); the beneficiary structure prevents that — provinces genuinely depend on equalization and interprovincial trade, not just suffer federal override. Tangled rope is the only type that captures both: the constraint coordinates and extracts simultaneously, and both functions are necessary to explain why it persists despite resource-rich provincial resistance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exit_right_status_ambiguity,
    'Is the provincial right to exit the federation a genuine option (negotiable under duress) or a legal fiction that federal authority prevents from materializing?',
    'A province formally initiates exit negotiation while remaining within the compact framework; federal response clarifies whether exit is treated as a legitimate act requiring negotiation or as ultra vires and subject to constitutional prohibition.',
    'If exit is treatmable as a negotiation within the compact, the constraint is genuinely tangled_rope with asymmetric but real provincial agency. If federal authority refuses to negotiate and asserts unilateral prohibition, the constraint tilts toward snare (pure extraction with subordination cover).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_right_status_ambiguity, empirical, 'Whether negotiated exit is a real option or legal theater.').

omega_variable(
    founding_coordination_vs_contemporary_extraction,
    'How much of the constraint''s current function is the founding coordination problem (defense, trade openness) versus contemporary value redistribution and federal jurisdiction expansion?',
    'Decompose federal budget and constitutional case law to isolate coordination-serving spending (trade administration, defense cost-sharing) from redistribution-serving spending (equalization beyond efficiency cost, environmental override without spillover justification).',
    'A high ratio of founding-problem cost to contemporary-extraction cost supports the rope framing; a low ratio supports reclassification toward snare. This determines mandatrophy status: if the founding problem is dead, the constraint is at risk of being maintained by theater rather than necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_coordination_vs_contemporary_extraction, empirical, 'Whether the constraint''s persistence is explained by ongoing coordination or by entrenched value transfer.').

omega_variable(
    reading_foreclosure_under_resource_absolutism,
    'Does the resource-sovereignty reading logically foreclose the compact reading, or can both coexist within a single federal framework?',
    'If s.92A is interpreted as granting provinces absolute unilateral resource control, can provinces simultaneously accept federal override on interprovincial trade and equalization? If yes, coexist_with is the relation; if no (a province claiming resource absolutism has implicitly rejected the compact), forecloses is the relation.',
    'If forecloses: the kernel has moved toward logically incompatible readings, and one must be selected or a compromise reading authored. If coexists_with: multiple readings remain live and can be held by different provincial governments or in different policy areas, which explains the contemporary Canadian federation''s internal contradiction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_under_resource_absolutism, conceptual, 'Whether resource sovereignty and federal coordination are structurally compatible within one compact.').

omega_variable(
    identity_lock_persistence_post_independence,
    'Would resource-rich provinces maintain their current identity and institutional structure if they negotiated independence from the compact, or would they reorganize under a different reading of sovereignty?',
    'Examine separatist policy platforms in resource-rich provinces to determine whether they propose maintaining compact-style coordination with other independent units, or whether independence entails rejection of federal-style constraints entirely.',
    'If provinces would maintain compact-style coordination post-independence, identity-lock is overstated and directionality should compute more toward arbitrage (easier exit). If they would reorganize under resource-sovereignty or confederal models, identity-lock is justified and exit remains trapped despite being nominally negotiable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_persistence_post_independence, empirical, 'The degree to which provincial identity is fused with federalism versus fused with independence-movement sovereignty claims.').

omega_variable(
    committer_sibling_reading_gap,
    'Is the compact reading a live position within federal authority, or is federal authority structurally committed to the constitutional-subordination reading?',
    'Examine federal policy statements and constitutional litigation positions: does federal authority treat the compact as genuine (provincial consent required for constitutional change) or as a legal convenience it can override (claiming unilateral amending power or treating provincial consent as formality)?',
    'If federal authority operationally adopts constitutional-subordination (provinces are subordinate creatures), the compact reading is a cover story and the constraint tilts toward snare. If federal authority genuinely respects provincial consent as binding (compact reading), the tangled_rope classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_sibling_reading_gap, empirical, 'Whether the federal agenda-setter genuinely believes the compact reading or uses it strategically while operationally assuming subordination.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__compact_federalism, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prov_tr_t0, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(prov_tr_t0, observed).
narrative_ontology:measurement(prov_tr_t8, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 8, 0.16).
narrative_ontology:measurement_basis(prov_tr_t8, observed).
narrative_ontology:measurement(prov_tr_t16, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 16, 0.21).
narrative_ontology:measurement_basis(prov_tr_t16, observed).
narrative_ontology:measurement(prov_tr_t24, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 24, 0.25).
narrative_ontology:measurement_basis(prov_tr_t24, observed).
narrative_ontology:measurement(prov_tr_t32, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 32, 0.27).
narrative_ontology:measurement_basis(prov_tr_t32, observed).
narrative_ontology:measurement(prov_tr_t40, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(prov_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(prov_be_t0, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(prov_be_t0, observed).
narrative_ontology:measurement(prov_be_t8, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 8, 0.26).
narrative_ontology:measurement_basis(prov_be_t8, observed).
narrative_ontology:measurement(prov_be_t16, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 16, 0.31).
narrative_ontology:measurement_basis(prov_be_t16, observed).
narrative_ontology:measurement(prov_be_t24, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 24, 0.35).
narrative_ontology:measurement_basis(prov_be_t24, observed).
narrative_ontology:measurement(prov_be_t32, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 32, 0.37).
narrative_ontology:measurement_basis(prov_be_t32, observed).
narrative_ontology:measurement(prov_be_t40, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 40, 0.38).
narrative_ontology:measurement_basis(prov_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(prov_su_t0, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(prov_su_t0, observed).
narrative_ontology:measurement(prov_su_t8, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 8, 0.32).
narrative_ontology:measurement_basis(prov_su_t8, observed).
narrative_ontology:measurement(prov_su_t16, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 16, 0.37).
narrative_ontology:measurement_basis(prov_su_t16, observed).
narrative_ontology:measurement(prov_su_t24, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 24, 0.4).
narrative_ontology:measurement_basis(prov_su_t24, observed).
narrative_ontology:measurement(prov_su_t32, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 32, 0.41).
narrative_ontology:measurement_basis(prov_su_t32, observed).
narrative_ontology:measurement(prov_su_t40, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 40, 0.42).
narrative_ontology:measurement_basis(prov_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_sovereignty_boundary__compact_federalism, resource_allocation).
narrative_ontology:boltzmann_floor_override(provincial_sovereignty_boundary__compact_federalism, 0.18).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__compact_federalism, provincial_sovereignty_boundary__constitutional_subordination).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__compact_federalism, provincial_sovereignty_boundary__resource_sovereignty_primacy).

% DUAL FORMULATION NOTE:
% The provincial sovereignty boundary kernel is instantiated by three distinct constraint stories, each representing a different reading of the foundational Confederation compact. Compact Federalism (this story) treats the boundary as negotiable and residually provincial; Constitutional Subordination treats it as federal by default with provinces subordinate; Resource Sovereignty Primacy treats it as absolutely provincial for natural resources. The three readings produce different ε values and different stakeholder structures because they define the constraint's referent differently. Compact Federalism is the canonical Canadian legal reading as of 2026; the other two are live positions within Canadian politics and jurisprudence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(provincial_sovereignty_boundary__compact_federalism, powerful, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
