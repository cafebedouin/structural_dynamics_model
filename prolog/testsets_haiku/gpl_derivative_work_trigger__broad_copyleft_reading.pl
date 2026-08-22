% ============================================================================
% CONSTRAINT STORY: gpl_derivative_work_trigger__broad_copyleft_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_derivative_work_trigger__broad_copyleft_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: gpl_derivative_work_trigger__broad_copyleft_reading
 *   human_readable: GPL Broad Copyleft Derivative Work Trigger (Linking Creates Obligation)
 *   domain: legal/technological/open_source
 *
 * SUMMARY:
 *   This constraint embodies the broad copyleft reading of the GPL: any
 *   linking (including dynamic linking at runtime) with GPL-licensed code
 *   creates a derivative work, triggering source disclosure obligations for
 *   the entire derivative. The reading treats linking as the
 *   boundary-crossing event that invokes copyleft; the competing narrow
 *   reading treats only modification as the trigger. This story instantiates
 *   the broad reading's structural account: who coordinates what, who bears
 *   costs, why vendors are constrained. The reading is contested in law
 *   (courts disagree), in practice (vendors vary in compliance
 *   interpretation), and in philosophy (what constitutes derivation). The
 *   claim and metrics are authored independently: the constraint is CLAIMED
 *   as rope (genuine coordination of commons access) while the metrics
 *   describe moderately high extraction and active enforcement — the engine
 *   measures that gap from the structural data you provide.
 *
 * KEY AGENTS:
 *   - gpl_downstream_users: gain source access rights to any linked derivative; power asymmetry favors them under the broad reading
 *   - open_source_commons_ecosystem: benefits from copyleft pulling proprietary code into the commons; non-agent entity representing the aggregate of shared resources
 *   - proprietary_software_vendors: bear compliance costs and exit constraint; must avoid certain libraries or release code; powerful but extraction-targeted
 *   - gpl_maintainers: set and enforce the interpretation; agenda_setter authority derives from code ownership and community recognition
 *   - permissive_license_advocates: excluded from GPL governance; contest the reading in law and public discourse; represent the alternative interpretation
 *   - jurisdictional_courts: observer seat with authority to confirm or reject the broad reading; split rulings create uncertainty
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__broad_copyleft_reading, 0.68).
domain_priors:suppression_score(gpl_derivative_work_trigger__broad_copyleft_reading, 0.71).
domain_priors:theater_ratio(gpl_derivative_work_trigger__broad_copyleft_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__broad_copyleft_reading, rope).
narrative_ontology:human_readable(gpl_derivative_work_trigger__broad_copyleft_reading, "GPL Broad Copyleft Derivative Work Trigger (Linking Creates Obligation)").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__broad_copyleft_reading, "legal/technological/open_source").

domain_priors:requires_active_enforcement(gpl_derivative_work_trigger__broad_copyleft_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__broad_copyleft_reading, 'eb227298-c47b-4041-9beb-6718c40ff78d').
narrative_ontology:cs_kernel_codification('eb227298-c47b-4041-9beb-6718c40ff78d', fixed_text).
narrative_ontology:cs_authority_grounding('eb227298-c47b-4041-9beb-6718c40ff78d', lineage).
narrative_ontology:cs_interpretation_layer_present('eb227298-c47b-4041-9beb-6718c40ff78d').
narrative_ontology:cs_reading_relation('eb227298-c47b-4041-9beb-6718c40ff78d', gpl_derivative_work_trigger__narrow_linking_permissive_reading, coexists_with).
narrative_ontology:cs_reading_relation('eb227298-c47b-4041-9beb-6718c40ff78d', gpl_derivative_work_trigger__interface_boundary_reading, influences).
narrative_ontology:cs_axiom('eb227298-c47b-4041-9beb-6718c40ff78d', foundational, linking_creates_derivative_work).
narrative_ontology:cs_axiom_status(linking_creates_derivative_work, holdable).
narrative_ontology:cs_axiom_grounding('eb227298-c47b-4041-9beb-6718c40ff78d', linking_creates_derivative_work, deontological).
narrative_ontology:cs_axiom('eb227298-c47b-4041-9beb-6718c40ff78d', secondary, reciprocal_source_access_required_for_commons_sustainability).
narrative_ontology:cs_axiom_status(reciprocal_source_access_required_for_commons_sustainability, holdable).
narrative_ontology:cs_axiom_grounding('eb227298-c47b-4041-9beb-6718c40ff78d', reciprocal_source_access_required_for_commons_sustainability, instrumental).
narrative_ontology:cs_reference_frame('eb227298-c47b-4041-9beb-6718c40ff78d', commons_preservation_through_copyleft_reciprocity).
narrative_ontology:cs_drift_state('eb227298-c47b-4041-9beb-6718c40ff78d', contemporary_vendor_licensing_fragmentation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('eb227298-c47b-4041-9beb-6718c40ff78d', '').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_downstream_users).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, open_source_commons_ecosystem).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__broad_copyleft_reading, proprietary_software_vendors).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__broad_copyleft_reading, closed_source_integrators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive source code and modification rights whenever they obtain software linked with GPL code. The broad reading guarantees them access to the complete source of the derivative work, enabling inspection, auditing, and modification. They depend on the interpretation holding to maintain their freedoms.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_downstream_users, beneficiary,
    organized, generational, mobile, global).

% Benefits from the copyleft mechanism pulling dependent code into the commons: when proprietary code links with GPL code, the broad reading requires the proprietary code be released, expanding the commons. This creates a positive feedback loop where the commons grows and becomes harder to escape.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, open_source_commons_ecosystem, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_non_agent(gpl_derivative_work_trigger__broad_copyleft_reading, open_source_commons_ecosystem).

% Must either release their proprietary code (losing competitive advantage) or avoid linking with GPL libraries entirely (narrowing their technical choices). They bear the enforcement cost through compliance labor, legal exposure, and foregone functionality. Their exit is constrained: they cannot link freely without triggering the obligation.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, proprietary_software_vendors, payer,
    powerful, biographical, constrained, global).

% Smaller firms wanting to integrate best-of-breed components face the choice of either adopting permissive-licensed alternatives (often lower quality or less maintained) or releasing proprietary code. The broad reading eliminates the middle ground of quiet integration with technical linking.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, closed_source_integrators, payer,
    moderate, biographical, constrained, global).

% Enforce and interpret the broad copyleft reading through licensing decisions, legal action, and community norm-setting. They author and defend the GPL terms and decide which linking scenarios trigger obligations. Their authority derives from ownership of the licensed code and recognition by downstream users and courts.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_maintainers, agenda_setter,
    organized, generational, mobile, global).

% Would argue that dynamic linking does not create derivative works and that code reuse should not carry copyleft obligations. They are excluded from the governance of GPL-licensed code but contest the interpretation in public discourse and court filings. Their alternative framing is the interface_boundary_reading and narrow_linking_permissive_reading.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, permissive_license_advocates, excluded,
    powerful, generational, mobile, global).

% Cannot assert patents against GPL-distributed code (GPL itself includes patent license grants), reducing their leverage over linked software. They would prefer narrower copyleft scope but are excluded from GPL governance.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, software_patent_holders, excluded,
    powerful, biographical, trapped, global).

% Adjudicate disputes over what constitutes a derivative work under copyright law. They have authority to confirm or reject the broad copyleft reading through case law. Different courts in different jurisdictions have issued conflicting rulings, creating uncertainty.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, jurisdictional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_derivative_work_trigger__broad_copyleft_reading, open_source_commons_ecosystem).
narrative_ontology:fixing_cost_class(gpl_derivative_work_trigger__broad_copyleft_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of maintaining a commons: without copyleft enforcement, proprietary actors have incentive to extract value from the commons while keeping improvements private. The broad reading creates a coordination mechanism where access to GPL code is conditioned on sharing improvements (code symmetry). Downstream users coordinate on accessing the full source of any derivative work rather than receiving binaries from proprietary intermediaries.
% TRANSFER_FUNCTION: Transfers source code disclosure obligations from a conditional state (triggered only by distribution of modified GPL code) to an expansive state (triggered by linking, even dynamic linking). Proprietary vendors transfer choice autonomy: they must choose between adopting permissive licenses, avoiding certain libraries, releasing code, or licensing exceptions. The constraint moves source access rights from binary users to anyone holding the derivative product.
% ABSENT_VOICES: Proprietary software vendors and their users are partially excluded: proprietary vendors are constrained by the rule but not parties to GPL governance; end-users of proprietary software do not gain the right to demand source (only downstream users of the derivative product do). Courts in restrictive-interpretation jurisdictions would argue for a narrower reading. Patent holders cannot participate in GPL rulemaking.
% DISAPPEARANCE_RATIONALE: If the broad copyleft derivative-work trigger disappeared overnight, proprietary vendors would immediately integrate best-of-breed GPL libraries without releasing source. The commons would shrink as contributors faced weak incentives to contribute to GPL projects knowing proprietary actors could incorporate improvements without reciprocation. Downstream users would lose automatic access to source for linked proprietary code. The open-source ecosystem would reorganize toward permissive licensing and dual-licensing models.
% FOUNDING_PROBLEM: The commons faces a prisoner's dilemma: individual actors are incentivized to extract value (use) from shared code while keeping improvements (code changes) private, causing the commons to stagnate. The broad copyleft reading solves this by making extraction (linking) conditional on sharing, enforcing a symmetry norm.
% FOUNDING_PROBLEM_CORROBORATION: GPL maintainers and open-source advocates attest the prisoner's dilemma is live and the broad reading is necessary to sustain the commons. Proprietary vendors and permissive-license advocates attest the problem is overstated and the broad reading is an overreach that harms software diversity. Courts have issued split rulings: some accept the broad reading (European cases), others reject or limit it (US case law remains unsettled). Independent analysts document both growth of GPL-licensed commons (supporting the coordination claim) and vendor avoidance of GPL dependencies (supporting the extraction claim).
narrative_ontology:disappearance_verdict(gpl_derivative_work_trigger__broad_copyleft_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_derivative_work_trigger__broad_copyleft_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_derivative_work_trigger__broad_copyleft_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gpl_derivative_work_trigger__broad_copyleft_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_derivative_work_trigger__broad_copyleft_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_derivative_work_trigger__broad_copyleft_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_derivative_work_trigger__broad_copyleft_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_derivative_work_trigger__broad_copyleft_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderately high (0.68) because the constraint narrows vendor choice-space and requires code disclosure or avoidance; the extraction is offset by the genuine coordination benefit to downstream users. Suppression is elevated (0.71) because enforcement depends on actively excluding alternatives: vendors cannot quietly integrate via permissive-licensed forks, courts must enforce the derivative-work boundary, and community norm-setting suppresses the narrower interpretations. Theater is low-moderate (0.28): the coordination function is substantive (commons access), but a growing portion of enforcement activity defends the interpretation boundary itself rather than sustaining commons functionality. The measurement series show slow accumulation of extractiveness from t=0 to t=15, then plateau, reflecting increasing vendor compliance (reducing active enforcement need) but also increasing clarity of the broad interpretation's scope (reducing contestation suppression). Suppression and theater both rise more steeply early (interpretation hardening) then plateau as the constraint becomes stabilized in practice.
 *
 * PERSPECTIVAL GAP:
 *   Downstream users and GPL maintainers compute the constraint as coordination (genuinely beneficial, low extraction from their seat) — they gain source access without bearing compliance costs. Proprietary vendors compute it as enforcement (high extraction, constrained exit) — they must choose between disclosure, avoidance, or licensing. The broad reading distributes these computations: beneficiaries under this reading become payers under the narrow reading. The engine computes per-seat types from the structural data; the authored claim reflects the commons-beneficiary framing, while the metrics reflect the vendor-constraint reality.
 *
 * DIRECTIONALITY LOGIC:
 *   Downstream users and the commons are beneficiaries (d → 0.2): they receive source access without bearing enforcement costs. Proprietary vendors are targets (d → 0.85): they face constrained exit and compliance burden. GPL maintainers are near symmetric (d → 0.5): they gain commons access and reciprocal code sharing, but also bear enforcement labor. Courts and patent holders are analytical (d → 0.5): they are neither benefiting from the constraint nor bearing extraction costs directly — they are adjudicating and displaced, respectively. The broad reading's key structural move is expanding the derivative-work boundary to include linking, which pushes proprietary vendors from the beneficiary side (they could integrate via static linking and keep code closed) toward the payer side (even dynamic linking triggers obligations).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint does not show mandatrophy: the founding problem (prisoner's dilemma in commons contributions) remains live, the founding solution (copyleft enforcement) persists, and the cost of fixing it (relicensing vast GPL-licensed code or migrating to permissive alternatives) exceeds any single vendor's benefit. This is stable Rope, not decayed Piton. The contestation (narrow vs. broad vs. interface-boundary readings) is not mandatrophy — it is endemic disagreement about the boundary definition, not about whether the constraint itself is needed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    derivative_work_boundary_definition,
    'What constitutes a derivative work under copyright law: linking (including dynamic linking), modification only, or something else? Does the linking mechanism (static vs. dynamic, direct vs. transitive) matter?',
    'Appellate court rulings (especially in the EU and US), legislative clarification of copyright derivative-work standards, and international harmonization efforts (e.g., through trade agreements). The Affero GPL (AGPL) extends the scope to network interaction, further testing the boundary.',
    'If courts reject the broad reading and adopt the narrow reading, extractiveness drops significantly (0.35–0.45), suppression falls (vendors can integrate with permissive-licensed forks as substitutes), and the constraint reclassifies toward Rope-only or even Snare-flavored (pure licensing play). If courts affirm the broad reading, it crystallizes and theater ratio drops (less contestation).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(derivative_work_boundary_definition, empirical, 'The boundary definition for what counts as a derivative work under copyright law is contested and will be resolved by appellate courts.').

omega_variable(
    broad_vs_narrow_reading_reconcilability,
    'Can the broad and narrow readings coexist within a single legal framework, or do they logically foreclose each other? If they coexist, how — through interpretation, carveouts, or different contexts?',
    'Legislative clarification of the GPL itself (though the GPL is written to remain unchanged) or interpretive guidance from GPL steward organizations (Free Software Foundation). Case law patterns showing consistent narrow vs. broad application by different courts would establish stable coexistence.',
    'If the readings foreclose each other (a single court must choose one), the constraint bifurcates: we should author this as two separate constraints (broad and narrow) in two files, linked by network.affects_constraints, rather than one story with an omega about the boundary. If they coexist (different jurisdictions apply different rules), the omega documents the jurisdictional divergence and the engine models it as empirical uncertainty about which reading applies downstream.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(broad_vs_narrow_reading_reconcilability, conceptual, 'Whether the broad and narrow copyleft readings logically foreclose each other or can coexist in different jurisdictions and communities.').

omega_variable(
    commons_growth_causation,
    'Does the broad copyleft reading actually cause proprietary code to be released into the commons, or does it merely cause proprietary vendors to avoid GPL code and use permissive alternatives instead? Is the commons growth benefit net positive or offset by vendor avoidance?',
    'Empirical tracking of GPL adoption rates, commons code contribution rates, and vendor licensing choices over time. Studies correlating broad-interpretation enforcement events with subsequent commons contributions or vendor avoidance patterns.',
    'If vendors actually release code due to the constraint (net commons growth), extractiveness and beneficiary framing are validated. If vendors primarily avoid GPL code (licensing around the constraint), the coordination benefit is illusory and extractiveness rises (pure enforcement cost without reciprocal gain) — the constraint reclassifies toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commons_growth_causation, empirical, 'Whether the broad copyleft interpretation causes net commons growth or primarily causes vendor avoidance of GPL code.').

omega_variable(
    reading_interpretation_stability,
    'Within the open-source community that adopts the GPL, how stable is the broad reading''s interpretation? Do GPL maintainers genuinely hold this interpretation, or do they apply it inconsistently or strategically depending on the vendor involved?',
    'Audit of licensing decisions by major GPL projects (Linux kernel, GCC, etc.): do they enforce derivative-work obligations consistently across vendors, or selectively? Survey of GPL maintainers about interpretation confidence and reasoning.',
    'If maintainers apply the reading inconsistently, suppression is actually higher (political theater maintaining an interpretation that is not fully believed) and theater_ratio should rise. If they apply it consistently, the constraint is more stable and theater_ratio reflects genuine coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_interpretation_stability, empirical, 'How consistently and sincerely GPL maintainers apply the broad copyleft reading across different vendors and scenarios.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_derivative_work_trigger__broad_copyleft_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(gpl__tr_t0, observed).
narrative_ontology:measurement(gpl__tr_t5, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement_basis(gpl__tr_t5, observed).
narrative_ontology:measurement(gpl__tr_t10, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement_basis(gpl__tr_t10, observed).
narrative_ontology:measurement(gpl__tr_t15, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement_basis(gpl__tr_t15, observed).
narrative_ontology:measurement(gpl__tr_t20, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement_basis(gpl__tr_t20, observed).
narrative_ontology:measurement(gpl__tr_t25, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(gpl__tr_t25, projected).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 0, 0.51).
narrative_ontology:measurement_basis(gpl__be_t0, observed).
narrative_ontology:measurement(gpl__be_t5, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement_basis(gpl__be_t5, observed).
narrative_ontology:measurement(gpl__be_t10, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 10, 0.63).
narrative_ontology:measurement_basis(gpl__be_t10, observed).
narrative_ontology:measurement(gpl__be_t15, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement_basis(gpl__be_t15, observed).
narrative_ontology:measurement(gpl__be_t20, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(gpl__be_t20, observed).
narrative_ontology:measurement(gpl__be_t25, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(gpl__be_t25, projected).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(gpl__su_t0, observed).
narrative_ontology:measurement(gpl__su_t5, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(gpl__su_t5, observed).
narrative_ontology:measurement(gpl__su_t10, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(gpl__su_t10, observed).
narrative_ontology:measurement(gpl__su_t15, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement_basis(gpl__su_t15, observed).
narrative_ontology:measurement(gpl__su_t20, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(gpl__su_t20, observed).
narrative_ontology:measurement(gpl__su_t25, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(gpl__su_t25, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_derivative_work_trigger__broad_copyleft_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(gpl_derivative_work_trigger__broad_copyleft_reading, 0.18).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_derivative_work_trigger__narrow_linking_permissive_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_derivative_work_trigger__interface_boundary_reading).

% DUAL FORMULATION NOTE:
% The GPL derivative-work trigger decomposes into three structurally distinct constraints, each instantiating a different reading of the GPL kernel. This file (broad_copyleft_reading) asserts that linking creates a derivative work; the sibling files assert narrower boundaries (modification only, or clean API exception). The three readings produce different beneficiary/victim structures and different extractiveness profiles. They are not the same constraint viewed from different angles — they are three different claims about what the GPL requires. Linked via network.affects_constraints because the broad reading creates downstream pressure on the narrow reading (vendor behavior shifts as the broad reading is enforced, making the narrow reading harder to sustain), but they do not logically foreclose each other (courts in different jurisdictions apply different interpretations). The coexistence is the empirical contestation the constraint family exists to measure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpl_derivative_work_trigger__broad_copyleft_reading, powerful, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
