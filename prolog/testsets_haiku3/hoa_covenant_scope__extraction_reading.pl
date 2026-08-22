% ============================================================================
% CONSTRAINT STORY: hoa_covenant_scope__extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hoa_covenant_scope__extraction_reading, []).

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
 *   constraint_id: hoa_covenant_scope__extraction_reading
 *   human_readable: HOA Covenant Fine Proliferation and Selective Enforcement
 *   domain: property_law/collective_governance
 *
 * SUMMARY:
 *   Homeowners Associations employ covenant enforcement mechanisms ostensibly
 *   to preserve property values and community aesthetics. This extraction
 *   reading instantiates the covenant as a revenue-generation and board
 *   power-consolidation tool operated through fine proliferation (expanding
 *   breach categories and penalty levels), selective enforcement (targeting
 *   politically weak residents while overlooking violations by board allies),
 *   and expedited lien procedures that convert HOA debts into foreclosure
 *   threats. The reading coexists with two sibling readings:
 *   coordination_reading (covenant solves genuine externalities in
 *   shared-property governance) and behavioral_control_reading (covenant
 *   enforces aesthetic uniformity as a property-value strategy). The
 *   extraction reading does not foreclose either sibling — all three remain
 *   live positions held by different parties in contested HOA contexts.
 *   Rather, extraction influences both: even coordinated externality
 *   management requires enforcement machinery that can be weaponized for
 *   extraction, and aesthetic control can be implemented through
 *   revenue-generating selective enforcement rather than uniform rules.
 *
 * KEY AGENTS:
 *   - HOA board members: powerful political seats with discretionary authority over fine schedules, enforcement targets, and management contracts
 *   - Property management firms: institutional beneficiaries extracting percentage-of-fines compensation
 *   - Legal counsel: attorneys billing hourly for lien proceedings and extracting attorney fee recovery from homeowners
 *   - Financially vulnerable homeowners: powerless targets trapped by negative equity and selective enforcement
 *   - Renters via pass-through: politically voiceless victims bearing costs through rent increases
 *   - Politically connected homeowners: selective non-enforcement beneficiaries obscured by aesthetic framing
 *   - Former enforcement advocates: identity-locked excluded voices opposing selective extraction
 *   - Legal scholars and reformers: analytical observers documenting predatory patterns
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__extraction_reading, 0.68).
domain_priors:suppression_score(hoa_covenant_scope__extraction_reading, 0.79).
domain_priors:theater_ratio(hoa_covenant_scope__extraction_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__extraction_reading, tangled_rope).
narrative_ontology:human_readable(hoa_covenant_scope__extraction_reading, "HOA Covenant Fine Proliferation and Selective Enforcement").
narrative_ontology:topic_domain(hoa_covenant_scope__extraction_reading, "property_law/collective_governance").

domain_priors:requires_active_enforcement(hoa_covenant_scope__extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__extraction_reading, 'e94fe36c-be1f-4eaf-8dd9-d7adb49b2b14').
narrative_ontology:cs_kernel_codification('e94fe36c-be1f-4eaf-8dd9-d7adb49b2b14', formalized).
narrative_ontology:cs_authority_grounding('e94fe36c-be1f-4eaf-8dd9-d7adb49b2b14', extraction).
narrative_ontology:cs_interpretation_layer_present('e94fe36c-be1f-4eaf-8dd9-d7adb49b2b14').
narrative_ontology:cs_reading_relation('e94fe36c-be1f-4eaf-8dd9-d7adb49b2b14', hoa_covenant_scope__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('e94fe36c-be1f-4eaf-8dd9-d7adb49b2b14', hoa_covenant_scope__behavioral_control_reading, coexists_with).
narrative_ontology:cs_axiom('e94fe36c-be1f-4eaf-8dd9-d7adb49b2b14', foundational, covenant_authority_captures_revenue).
narrative_ontology:cs_axiom_status(covenant_authority_captures_revenue, holdable).
narrative_ontology:cs_axiom_grounding('e94fe36c-be1f-4eaf-8dd9-d7adb49b2b14', covenant_authority_captures_revenue, empirically_contingent).
narrative_ontology:cs_axiom('e94fe36c-be1f-4eaf-8dd9-d7adb49b2b14', secondary, selective_enforcement_concentrates_extraction).
narrative_ontology:cs_axiom_status(selective_enforcement_concentrates_extraction, holdable).
narrative_ontology:cs_axiom_grounding('e94fe36c-be1f-4eaf-8dd9-d7adb49b2b14', selective_enforcement_concentrates_extraction, empirically_contingent).
narrative_ontology:cs_reference_frame('e94fe36c-be1f-4eaf-8dd9-d7adb49b2b14', covenant_as_revenue_mechanism).
narrative_ontology:cs_drift_state('e94fe36c-be1f-4eaf-8dd9-d7adb49b2b14', contemporary_post_litigation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e94fe36c-be1f-4eaf-8dd9-d7adb49b2b14', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__extraction_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, hoa_board_members).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, property_management_firms).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, legal_counsel_providers).
narrative_ontology:constraint_victim(hoa_covenant_scope__extraction_reading, financially_vulnerable_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__extraction_reading, renters_via_pass_through).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, politically_connected_homeowners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the enforcement agenda, vote on covenant rules and fine schedules, and control the board-appointed management company. Board members face escalating pressure to generate revenue for special assessments without raising dues; selective enforcement allows them to target high-fine violations against politically weak homeowners while ignoring similar violations from allies or high-status residents. They extract power through the authority to interpret covenant ambiguities in enforcement decisions.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, hoa_board_members, agenda_setter,
    powerful, biographical, arbitrage, local).

% Contract directly with the HOA board to manage enforcement. They collect fees as a percentage of fines collected or as flat retainers that increase with enforcement activity. Their business model incentivizes aggressive fine schedules, expedited lien procedures, and high-volume violations detection. They maintain market share by satisfying board members' revenue expectations, creating a financial alignment with extraction rather than coordinate maintenance.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, property_management_firms, beneficiary,
    institutional, generational, arbitrage, regional).

% Represent the HOA in enforcement actions and lien proceedings. They bill hourly for litigation, lien preparation, and settlement negotiation. Attorney fees are passed to homeowners as collection costs, creating a direct financial incentive to litigate rather than settle. They lobby for expedited lien procedures and fee-shifting rules that let the HOA recover legal costs from losing homeowners, amplifying extraction from already-targeted residents.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, legal_counsel_providers, beneficiary,
    institutional, biographical, arbitrage, regional).

% Subject to fine notices for covenant violations often presented with discretionary language and uneven enforcement history. They cannot afford legal defense, cannot challenge lien procedures without counsel, and cannot exit without selling at a loss when a lien is on the title. Selective targeting means the same violation incurs fines for them but not for politically connected neighbors. Trapped by negative equity, family stability, or limited housing alternatives; forced to pay fines or lose the house to foreclosure.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, financially_vulnerable_homeowners, payer,
    powerless, biographical, trapped, local).

% Have no voting rights in the HOA but bear covenant fine costs through rent increases imposed by their landlords to cover escalating HOA assessments and special fees. They cannot challenge enforcement, cannot vote for board members, and have no contractual recourse. Their only exit is finding housing elsewhere; in tight markets, that option collapses.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, renters_via_pass_through, payer,
    powerless, immediate, constrained, local).

% Benefit from selective non-enforcement: their covenant violations are overlooked or prosecuted under more favorable standards than those applied to powerless residents. Board connections allow them to challenge fines or negotiate down penalties. They also benefit indirectly as property values in the community remain higher due to aesthetic enforcement, even as extraction from the vulnerable sustains the HOA's revenue.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, politically_connected_homeowners, beneficiary,
    powerful, biographical, arbitrage, local).
narrative_ontology:stakeholder_secondary_role(hoa_covenant_scope__extraction_reading, politically_connected_homeowners, observer).

% Homeowners who initially supported strict covenant enforcement as a quality-protection measure but now see selective targeting and fine proliferation have transformed the system into predatory extraction. They are locked out of board governance through board majority control and cannot raise objections without being perceived as wanting to 'lower community standards.' Their voice is structurally absent from enforcement decisions even though they live under the same regime.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, former_enforcement_advocates, excluded,
    moderate, biographical, identity_locked, local).

% Document the extractive mechanics of HOA enforcement regimes and propose statutory limits on fine escalation, lien procedures, and attorney fee-shifting. They observe the pattern across communities but lack enforcement authority in any single jurisdiction. Their analysis supports the extraction reading but cannot change board incentives directly.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, legal_scholars_and_reformers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hoa_covenant_scope__extraction_reading, hoa_board_members).
narrative_ontology:fixing_cost_class(hoa_covenant_scope__extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enforce aesthetic uniformity and structural maintenance of common property to preserve community values and prevent free-riding on maintenance costs — a legitimate coordination problem in communities with shared amenities and exterior visibility norms.
% TRANSFER_FUNCTION: Moves fines and legal fees from financially vulnerable homeowners to the HOA board (through management retainers and reserve accounts the board controls), property management firms (through percentage-of-fines contracts), and legal counsel (through hourly billing and fee-recovery provisions). The transfer is accelerated through selective enforcement targeting politically weak residents while omitting similarly situated politically connected neighbors.
% ABSENT_VOICES: Renters, future homebuyers priced out by inflated HOA fees, homeowners who lost homes to HOA foreclosure, residents of neighboring communities excluded from board governance despite being subject to covenant rules enacted by neighboring HOA boards — all lack standing to object to enforcement decisions. The board's unilateral authority over interpretation and selective enforcement is never tested by contested readings.
% DISAPPEARANCE_RATIONALE: If covenant enforcement as extraction disappeared, HOA revenue models would collapse; boards would be forced to either (a) cap fine schedules and justify special assessments to residents, (b) abandon selective enforcement and face political accountability for uniform rules, or (c) accept lower reserve funding and less discretionary spending. Management firms and legal practices would shrink. Property values in the community might stabilize lower but would not crash — neighborhoods maintain aesthetic standards through reputation and voluntary collective action even without coercive covenant structures.
% FOUNDING_PROBLEM: Shared property and aesthetic commons in residential communities require coordination to prevent tragedy-of-the-commons deterioration: unmaintained lawns, visible decay, free-riding on community maintenance costs, and damage to property values for all residents.
% FOUNDING_PROBLEM_CORROBORATION: The HOA board and property management industry attest the founding problem remains live, citing ongoing maintenance challenges. Financial reform advocates and academic legal scholars attest the founding problem is substantially solved (aesthetic norms are maintained through reputation even in low-fine communities) and modern enforcement targets revenue generation rather than coordination; comparative analysis of low-enforcement and high-enforcement HOA communities shows negligible difference in maintenance outcomes but dramatic differences in resident satisfaction and financial distress. Outside-beneficiary sources (reform groups, housing advocates, court records of predatory liens) support the shifted-function reading.
narrative_ontology:disappearance_verdict(hoa_covenant_scope__extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(hoa_covenant_scope__extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hoa_covenant_scope__extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hoa_covenant_scope__extraction_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hoa_covenant_scope__extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hoa_covenant_scope__extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hoa_covenant_scope__extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because the fine structure is decoupled from coordination cost — fines escalate with board control rather than with actual maintenance requirements. Theater is elevated (0.58) because enforcement activity is presented as aesthetic preservation while actual activity targets revenue collection and weak targets. The measurement series shows accelerating extraction (0.48→0.68 over 25 periods) as boards rationalize fine schedules and streamline lien procedures to reduce administrative friction. Suppression is substantial (0.79) because homeowners cannot collectively challenge board decisions without being labeled as 'wanting to lower standards,' and exit requires finding alternative housing in markets where covenant-free properties are scarce or premium-priced. The core tangled-rope signature emerges from the simultaneous real coordination function (aesthetic/maintenance norms genuinely solve collective-action problems) and asymmetric extraction (the mechanism for delivering that function is weaponized against the powerless while protecting the powerful).
 *
 * PERSPECTIVAL GAP:
 *   From the board's seat, selective enforcement is justified cost-control (no point fining the same violation twice; board allies already understand the rules). From the financially vulnerable homeowner's seat, the same pattern is predatory targeting. The property management firm perceives efficiency gains and legitimate revenue; the homeowner perceives coercion designed to force payment. The engine computes these divergences per-seat from the authored structural data — the authored claim (tangled_rope) does not reconcile them, and the metrics do not need to match the claim. A rope constraint claimed and metrically low would be unremarkable. A tangled_rope claimed with high extraction and high suppression is exactly where the framework detects whether the stated coordination function is genuine or decorative.
 *
 * DIRECTIONALITY LOGIC:
 *   Board members and management firms occupy agenda-setter and beneficiary roles with powerful/institutional power, arbitrage-grade exit (they set the rules and can exit a community by changing management contracts), and active enforcement authority. Financially vulnerable homeowners occupy payer roles with powerless power, trapped exit (they cannot leave without selling at a loss), and no enforcement authority. The asymmetry is structural: the same covenant rule is a coordination mechanism for the powerful and an extraction tax for the powerless. Selective enforcement amplifies this asymmetry by converting discretion into an extra extraction layer. Renters have constrained exit rather than trapped exit (they can change housing) but their powerlessness and lack of voting rights means they cannot contest board decisions.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy classification because the founding problem (coordination of shared property maintenance) remains live and the extraction mechanism rides directly on the coordination machinery — fines collect both for genuine maintenance enforcement and for revenue generation through the same lien processes. Unlike a piton (where the original function has atrophied but the constraint persists theatrically), here the coordination function is real, the extraction is tied to it, and abandoning extraction would require abandoning the enforcement mechanism that makes coordination work. The tangled_rope classification holds: the constraint cannot be separated into pure coordination and pure extraction without collapsing the enforcement machinery that does both. That inseparability is what makes it tangled rather than snare — if selective enforcement could be removed while preserving uniform rule application, the constraint would be reclassifiable as snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_selectivity_mechanism,
    'Is selective enforcement a deliberate extraction strategy authored by the board, or an emergent consequence of asymmetric political power where the powerless simply have no mechanism to contest fines?',
    'Examine board meeting minutes, enforcement decision logs, and fine schedules for evidence of explicit targeting (e.g., board minutes discussing strategies to ''target new residents'' or ''focus collections on properties without legal resources'') versus examining whether non-enforcement of identical violations against board-connected residents emerges passively from lack of contestation.',
    'If deliberate strategy, the constraint is more predatory and the board more culpable; if emergent consequence of structural inequality, the constraint is still extractive but the mechanism is opacity and power differential rather than active conspiracy. Either way, extraction is the outcome, but the directionality and suppression analysis would differ.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_selectivity_mechanism, empirical, 'Whether selective enforcement is intentional targeting or emergent from asymmetric political power.').

omega_variable(
    separability_of_coordination_and_extraction,
    'Are the coordination function (shared-property maintenance) and the extraction function (revenue generation) structurally separable, or is extraction an inherent byproduct of the enforcement machinery?',
    'Compare outcomes in HOA communities that have implemented mandatory rate caps on fines, uniform enforcement standards, and resident-review boards for lien decisions. If coordination (aesthetic maintenance, property values) holds stable while extraction drops, the functions are separable; if coordination collapses when extraction is constrained, they are inseparable.',
    'Separability would support reclassification as snare (the coordination story is cover) or mandate structural reform (separate the coordination mechanism from the revenue mechanism). Inseparability supports tangled_rope as final classification and suggests reform requires redesigning the enforcement mechanism entirely, not just capping extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(separability_of_coordination_and_extraction, empirical, 'Whether extraction is a byproduct of coordination enforcement or intrinsic to it.').

omega_variable(
    covenant_kernel_stability_across_readings,
    'Do the three readings (coordination, behavioral_control, extraction) reflect genuinely different authority structures within the same kernel, or do they reflect the same authority structure observed from different stakeholder positions?',
    'Trace the historical evolution of a single HOA from founding (coordination framing) through enforcement escalation (behavioral_control framing) to revenue prioritization (extraction framing). If the authority structure changes at each stage (board votes to shift priorities, management contracts are rewritten with new incentives), the readings track genuine shifts in institutional function. If the authority structure remains constant and only the observed effects change, the readings are perspectival rather than structural.',
    'If readings track structural shifts, the kernel is an authority structure that can be reoriented (and foreclosure relations between readings become plausible). If readings are perspectival only, the kernel is the authority structure itself, readings coexist permanently, and no reading can foreclose another — the choice between readings remains perpetually contestable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(covenant_kernel_stability_across_readings, conceptual, 'Whether readings reflect shifts in board institutional intent or different seats'' observations of the same authority structure.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.79) structural (homeowners cannot exit, cannot contest without legal resources, face lien threats) or internalized (homeowners believe they deserve fines, internalize the covenant authority, suppress their own objections)?',
    'Post-exit trajectories: if homeowners who sell and leave the HOA community report persistent deference to HOA-like authority in new contexts, the suppression is partially internalized. If they immediately assert ownership rights and contest fees they would have paid in the HOA, the suppression is structural and contextual.',
    'If suppression is structural, the constraint''s effective suppression could decrease with exit or rule changes. If internalized, the constraint carries its suppressive effects beyond the formal covenant context — residents internalize HOA norms as proper property governance and reproduce them. This would suggest the extraction reading reaches beyond direct coercion into identity/norm capture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression is structural inequality or internalized deference to board authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__extraction_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa__tr_t0, hoa_covenant_scope__extraction_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement_basis(hoa__tr_t0, observed).
narrative_ontology:measurement(hoa__tr_t5, hoa_covenant_scope__extraction_reading, theater_ratio, 5, 0.38).
narrative_ontology:measurement_basis(hoa__tr_t5, observed).
narrative_ontology:measurement(hoa__tr_t10, hoa_covenant_scope__extraction_reading, theater_ratio, 10, 0.44).
narrative_ontology:measurement_basis(hoa__tr_t10, observed).
narrative_ontology:measurement(hoa__tr_t15, hoa_covenant_scope__extraction_reading, theater_ratio, 15, 0.5).
narrative_ontology:measurement_basis(hoa__tr_t15, observed).
narrative_ontology:measurement(hoa__tr_t20, hoa_covenant_scope__extraction_reading, theater_ratio, 20, 0.55).
narrative_ontology:measurement_basis(hoa__tr_t20, observed).
narrative_ontology:measurement(hoa__tr_t25, hoa_covenant_scope__extraction_reading, theater_ratio, 25, 0.58).
narrative_ontology:measurement_basis(hoa__tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(hoa__be_t0, hoa_covenant_scope__extraction_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(hoa__be_t0, observed).
narrative_ontology:measurement(hoa__be_t5, hoa_covenant_scope__extraction_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement_basis(hoa__be_t5, observed).
narrative_ontology:measurement(hoa__be_t10, hoa_covenant_scope__extraction_reading, base_extractiveness, 10, 0.59).
narrative_ontology:measurement_basis(hoa__be_t10, observed).
narrative_ontology:measurement(hoa__be_t15, hoa_covenant_scope__extraction_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement_basis(hoa__be_t15, observed).
narrative_ontology:measurement(hoa__be_t20, hoa_covenant_scope__extraction_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(hoa__be_t20, observed).
narrative_ontology:measurement(hoa__be_t25, hoa_covenant_scope__extraction_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(hoa__be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(hoa__su_t0, hoa_covenant_scope__extraction_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement_basis(hoa__su_t0, observed).
narrative_ontology:measurement(hoa__su_t5, hoa_covenant_scope__extraction_reading, suppression_requirement, 5, 0.72).
narrative_ontology:measurement_basis(hoa__su_t5, observed).
narrative_ontology:measurement(hoa__su_t10, hoa_covenant_scope__extraction_reading, suppression_requirement, 10, 0.74).
narrative_ontology:measurement_basis(hoa__su_t10, observed).
narrative_ontology:measurement(hoa__su_t15, hoa_covenant_scope__extraction_reading, suppression_requirement, 15, 0.76).
narrative_ontology:measurement_basis(hoa__su_t15, observed).
narrative_ontology:measurement(hoa__su_t20, hoa_covenant_scope__extraction_reading, suppression_requirement, 20, 0.78).
narrative_ontology:measurement_basis(hoa__su_t20, observed).
narrative_ontology:measurement(hoa__su_t25, hoa_covenant_scope__extraction_reading, suppression_requirement, 25, 0.79).
narrative_ontology:measurement_basis(hoa__su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hoa_covenant_scope__extraction_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(hoa_covenant_scope__extraction_reading, 0.18).
narrative_ontology:affects_constraint(hoa_covenant_scope__extraction_reading, hoa_covenant_scope__coordination_reading).
narrative_ontology:affects_constraint(hoa_covenant_scope__extraction_reading, hoa_covenant_scope__behavioral_control_reading).

% DUAL FORMULATION NOTE:
% This extraction reading is one of three structurally distinct readings of the hoa_covenant_scope kernel. The coordination_reading treats the same covenant authority as solving genuine shared-property externalities; the behavioral_control_reading treats it as enforcing aesthetic uniformity as a property-value strategy. All three readings share the same kernel (the formal authority of the HOA board to set rules, interpret breaches, and order liens) but instantiate different constraints with different ε values and beneficiary/victim structures. The extraction_reading ε (0.68) is substantially higher than the coordination_reading ε (which would characterize the constraint as primarily serving collective-action resolution). This ε-divergence is not a measurement artifact — it reflects fundamentally different claims about what the HOA mechanism accomplishes. The three stories together form a constraint family documenting the kernel contest that occupies HOA discourse: whether the covenant is coordination, control, or extraction. Each is authored independently with its own metrics and stakeholder structure. This story (extraction_reading) influences both siblings by establishing that even if genuine coordination occurs, the mechanism is weaponized for extraction and the revenue incentives drive board behavior more than coordination needs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hoa_covenant_scope__extraction_reading, powerful, 0.15).
constraint_indexing:directionality_override(hoa_covenant_scope__extraction_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
