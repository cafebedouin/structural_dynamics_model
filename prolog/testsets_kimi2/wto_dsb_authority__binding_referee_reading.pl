% ============================================================================
% CONSTRAINT STORY: wto_dsb_authority__binding_referee_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_dsb_authority__binding_referee_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: wto_dsb_authority__binding_referee_reading
 *   human_readable: WTO DSB Binding Referee Authority
 *   domain: international_law/trade_governance
 *
 * SUMMARY:
 *   The WTO Dispute Settlement Body panel process, as instantiated by the
 *   binding_referee reading of the wto_dsb_authority kernel, interprets
 *   treaty text to issue binding rulings on member state trade policies.
 *   Member states surrendered policy discretion within covered domains in
 *   exchange for reciprocal market access and enforcement against foreign
 *   protectionism. This reading treats the authority as legitimately grounded
 *   in state consent to the DSU; sibling readings dispute this, framing the
 *   authority as merely advisory or as illegitimate judicial activism. This
 *   constraint is part of a three-reading kernel family.
 *
 * KEY AGENTS:
 *   - DSB panel and Appellate Body mechanism (agenda_setter): institutional power, sets the interpretive boundaries of permissible trade policy.
 *   - Powerful member states (beneficiary): gain market access predictability and retaliation leverage; experience the constraint as coordination.
 *   - Weak member states (payer): lose policy autonomy without effective reciprocal enforcement capacity; experience the constraint as asymmetric extraction.
 *   - Exporters and traders (beneficiary): benefit from predictable rules but do not control the apparatus.
 *   - Import-competing domestic sectors (payer): lose protective policy space when state measures are ruled inconsistent.
 *   - Trade law scholars (observer): analyze asymmetries and textual drift from an analytical seat.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_dsb_authority__binding_referee_reading, 0.58).
domain_priors:suppression_score(wto_dsb_authority__binding_referee_reading, 0.62).
domain_priors:theater_ratio(wto_dsb_authority__binding_referee_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_dsb_authority__binding_referee_reading, tangled_rope).
narrative_ontology:human_readable(wto_dsb_authority__binding_referee_reading, "WTO DSB Binding Referee Authority").
narrative_ontology:topic_domain(wto_dsb_authority__binding_referee_reading, "international_law/trade_governance").

domain_priors:requires_active_enforcement(wto_dsb_authority__binding_referee_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_dsb_authority__binding_referee_reading, '55dbc73b-ace1-4c2b-9eaa-17bd5f46c200').
narrative_ontology:cs_kernel_codification('55dbc73b-ace1-4c2b-9eaa-17bd5f46c200', formalized).
narrative_ontology:cs_authority_grounding('55dbc73b-ace1-4c2b-9eaa-17bd5f46c200', lineage).
narrative_ontology:cs_interpretation_layer_present('55dbc73b-ace1-4c2b-9eaa-17bd5f46c200').
narrative_ontology:cs_reading_relation('55dbc73b-ace1-4c2b-9eaa-17bd5f46c200', wto_dsb_authority__advisory_coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('55dbc73b-ace1-4c2b-9eaa-17bd5f46c200', wto_dsb_authority__judicial_activism_reading, coexists_with).
narrative_ontology:cs_axiom('55dbc73b-ace1-4c2b-9eaa-17bd5f46c200', foundational, treaty_consent_creates_binding_panel_authority).
narrative_ontology:cs_axiom_status(treaty_consent_creates_binding_panel_authority, holdable).
narrative_ontology:cs_axiom_grounding('55dbc73b-ace1-4c2b-9eaa-17bd5f46c200', treaty_consent_creates_binding_panel_authority, conventional).
narrative_ontology:cs_axiom('55dbc73b-ace1-4c2b-9eaa-17bd5f46c200', foundational, panel_mandate_limited_to_state_consent).
narrative_ontology:cs_axiom_status(panel_mandate_limited_to_state_consent, holdable).
narrative_ontology:cs_axiom_grounding('55dbc73b-ace1-4c2b-9eaa-17bd5f46c200', panel_mandate_limited_to_state_consent, conventional).
narrative_ontology:cs_reference_frame('55dbc73b-ace1-4c2b-9eaa-17bd5f46c200', treaty_based_binding_adjudication).
narrative_ontology:cs_drift_state('55dbc73b-ace1-4c2b-9eaa-17bd5f46c200', contemporary_ab_crisis, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('55dbc73b-ace1-4c2b-9eaa-17bd5f46c200', '').
narrative_ontology:cs_kernel_id(wto_dsb_authority__binding_referee_reading, wto_dsb_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, powerful_member_states).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, exporters_and_traders).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, weak_member_states).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, import_competing_sectors).
narrative_ontology:constraint_vindicates(wto_dsb_authority__binding_referee_reading, multilateral_trade_liberalization).
narrative_ontology:constraint_vindicates(wto_dsb_authority__binding_referee_reading, treaty_supremacy_in_trade).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicates trade disputes under the DSU, issues panel and Appellate Body reports, and authorizes retaliation under Article 22.6. Defines the boundaries of permissible national trade policy through treaty interpretation.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, dsb_panel_appellate_mechanism, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from binding rulings that open foreign markets and from retaliation leverage to compel compliance by other states. Shape case law through frequent dispute initiation and systemic influence.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, powerful_member_states, beneficiary,
    institutional, generational, constrained, global).

% Must comply with adverse rulings but rarely secure effective retaliation authorization due to small market size. Experience the constraint as compulsory compliance without reciprocal enforcement capacity.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, weak_member_states, payer,
    moderate, generational, constrained, global).

% Rely on DSB rulings to secure predictable market access against foreign protectionism. Lobby governments to initiate disputes but do not control the adjudication apparatus.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, exporters_and_traders, beneficiary,
    powerful, biographical, mobile, global).

% Domestic industries that lose tariffs, subsidies, or regulatory protections when panels rule state measures WTO-inconsistent. Excluded from direct participation in dispute proceedings.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, import_competing_sectors, payer,
    organized, biographical, constrained, national).

% Analyze whether the DSB's binding authority represents genuine treaty consent or masks asymmetric power projection. Document enforcement asymmetries and textual drift.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, trade_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves international trade disputes through rules-based adjudication, replacing unilateral retaliation and power-based bargaining with a multilateral legal process for determining treaty compliance.
% TRANSFER_FUNCTION: Moves the authority to determine trade-policy consistency from national governments to DSB panels and the Appellate Body, transferring policy discretion upstream from member states to appointed adjudicators.
% ABSENT_VOICES: Domestic import-competing industries, labor groups facing liberalization pressure, and subsistence producers in developing countries are structurally excluded from panel proceedings, as only member states possess standing to bring or defend claims.
% DISAPPEARANCE_RATIONALE: Without binding DSB authority, trade disputes would revert to unilateral measures and power-based bargaining; tariff bindings would lose credible enforcement, and the predictability of market access commitments would erode.
% FOUNDING_PROBLEM: The pre-1995 GATT dispute system allowed losing parties to block panel reports, producing chronic non-compliance, unilateral retaliation, and erosion of negotiated concessions.
% FOUNDING_PROBLEM_CORROBORATION: GATT-era negotiators and trade historians corroborate the pre-1995 enforcement crisis from outside the current benefiting parties; however, developing-country critics and legal realists argue the crisis narrative was constructed by export-oriented economies to justify a coercive enforcement architecture.
narrative_ontology:disappearance_verdict(wto_dsb_authority__binding_referee_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_dsb_authority__binding_referee_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_dsb_authority__binding_referee_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(wto_dsb_authority__binding_referee_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_dsb_authority__binding_referee_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_dsb_authority__binding_referee_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(wto_dsb_authority__binding_referee_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(wto_dsb_authority__binding_referee_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects the substantial sovereignty cost member states incur when policy discretion is transferred to adjudicators. Suppression (0.62) is elevated because the system's persistence depends on the threat of authorized retaliation under DSU Article 22.6. Theater ratio (0.22) is moderate-low: most panel activity is functional dispute resolution, but compliance proceedings and retaliation authorization carry performative elements. Accessibility collapse (0.65) is high because unilateral alternatives are treaty-violative once a state joins the WTO. Resistance (0.48) reflects ongoing non-compliance episodes, the Appellate Body appointment crisis, and sustained reform proposals. The metrics are authored independently of the claimed tangled_rope classification; the engine computes per-seat divergence.
 *
 * PERSPECTIVAL GAP:
 *   Powerful member states and export-oriented traders experience the constraint as genuine coordination that secures market access against foreign protectionism. Weak member states and import-competing sectors experience it as asymmetric extraction: the former cannot effectively use retaliation to compel compliance by powerful non-compliant states, while the latter lose protective policy space without recourse. The engine should compute divergent directionality for these seats despite their equal nominal standing as WTO members.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (powerful_member_states, exporters_and_traders) gain market access predictability and retaliation leverage, placing them toward the beneficiary end of the directionality spectrum. Payers (weak_member_states, import_competing_sectors) lose policy autonomy and domestic protection, placing them toward the target end. The DSB mechanism itself sits as agenda_setter with institutional power and constrained exit from the treaty framework it administers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâa pre-1995 GATT enforcement gapâwas genuine, preventing a pure snare classification. However, the solution generated a structural asymmetry: binding authority plus retaliation authorization extracts disproportionately from weak members who cannot reciprocate enforcement. This prevents classification as pure rope (because victims exist, enforcement is required, and extraction is asymmetric) and as pure snare (because a real coordination problem is solved and powerful states voluntarily maintain the system). The tangled_rope classification captures this hybridity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'Is the WTO DSB authority properly read as a binding referee, an advisory coordinator, or a judicial activist, and which structural features of the DSU text determine the answer?',
    'Comparative legal analysis of DSU Articles 3, 6, 16, 17, 19, and 22 against state practice regarding compliance, retaliation, and appellate review.',
    'Determines whether the constraint is classified as tangled rope (this reading), rope (advisory reading), or snare (activism reading).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Kernel reading contest for WTO DSB authority nature').

omega_variable(
    enforcement_asymmetry,
    'Does the authorization of retaliation under DSU Article 22.6 function as a neutral enforcement tool or as an asymmetric extraction mechanism favoring economically powerful states?',
    'Empirical analysis of retaliation requests, authorization rates, and compliance outcomes disaggregated by member state GDP and trade volume.',
    'If asymmetric, the tangled rope classification strengthens; if neutral, the coordination function dominates and classification shifts toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_asymmetry, empirical, 'Whether retaliation enforcement is structurally neutral or power-skewed').

omega_variable(
    sovereignty_transfer_irreversibility,
    'Is the sovereignty trade embedded in the binding referee reading reversible through ordinary political processes, or has it generated path-dependent institutional lock-in?',
    'Comparative case studies of member state exit threats, DSU reform negotiations, and the political cost of WTO withdrawal.',
    'If irreversible, effective extraction for weak member states is higher than the structural snapshot suggests; if reversible, the cost is discounted by future exit options.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_transfer_irreversibility, empirical, 'Reversibility of sovereignty transfer under the DSU').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_dsb_authority__binding_referee_reading, 0, 28).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto_dsb_bind_tr_t0, wto_dsb_authority__binding_referee_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(wto_dsb_bind_tr_t7, wto_dsb_authority__binding_referee_reading, theater_ratio, 7, 0.13).
narrative_ontology:measurement(wto_dsb_bind_tr_t14, wto_dsb_authority__binding_referee_reading, theater_ratio, 14, 0.18).
narrative_ontology:measurement(wto_dsb_bind_tr_t21, wto_dsb_authority__binding_referee_reading, theater_ratio, 21, 0.25).
narrative_ontology:measurement(wto_dsb_bind_tr_t28, wto_dsb_authority__binding_referee_reading, theater_ratio, 28, 0.22).

% Extraction over time
narrative_ontology:measurement(wto_dsb_bind_be_t0, wto_dsb_authority__binding_referee_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(wto_dsb_bind_be_t7, wto_dsb_authority__binding_referee_reading, base_extractiveness, 7, 0.42).
narrative_ontology:measurement(wto_dsb_bind_be_t14, wto_dsb_authority__binding_referee_reading, base_extractiveness, 14, 0.5).
narrative_ontology:measurement(wto_dsb_bind_be_t21, wto_dsb_authority__binding_referee_reading, base_extractiveness, 21, 0.55).
narrative_ontology:measurement(wto_dsb_bind_be_t28, wto_dsb_authority__binding_referee_reading, base_extractiveness, 28, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(wto_dsb_bind_su_t0, wto_dsb_authority__binding_referee_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(wto_dsb_bind_su_t7, wto_dsb_authority__binding_referee_reading, suppression_requirement, 7, 0.5).
narrative_ontology:measurement(wto_dsb_bind_su_t14, wto_dsb_authority__binding_referee_reading, suppression_requirement, 14, 0.56).
narrative_ontology:measurement(wto_dsb_bind_su_t21, wto_dsb_authority__binding_referee_reading, suppression_requirement, 21, 0.6).
narrative_ontology:measurement(wto_dsb_bind_su_t28, wto_dsb_authority__binding_referee_reading, suppression_requirement, 28, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, wto_dsb_authority__advisory_coordination_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, wto_dsb_authority__judicial_activism_reading).

% DUAL FORMULATION NOTE:
% The natural-language concept 'WTO DSB authority' decomposes into three structurally distinct readings: advisory (low extraction, rope-like), binding referee (moderate extraction, tangled rope), and judicial activism (high extraction, snare-like). This story authors the binding_referee reading; sibling stories instantiate the other readings. All three share the DSU treaty text as kernel but assign different epsilon values, beneficiary/victim structures, and stakeholder asymmetries.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
