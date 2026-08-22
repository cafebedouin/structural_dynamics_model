% ============================================================================
% CONSTRAINT STORY: border_control_legitimacy__jurisdictional_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_control_legitimacy__jurisdictional_sovereignty, []).

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
 *   constraint_id: border_control_legitimacy__jurisdictional_sovereignty
 *   human_readable: Border Control Legitimacy via Jurisdictional Sovereignty
 *   domain: political_philosophy/international_law
 *
 * SUMMARY:
 *   This constraint instantiates the JURISDICTIONAL SOVEREIGNTY reading of
 *   border control legitimacy. Sovereignty is understood not as absolute
 *   discretion to exclude (the sovereignty-primary reading) nor as
 *   subordinate to universal freedom of movement (the
 *   freedom-of-movement-primary reading), but as jurisdictional authority
 *   bounded by proportionality tests and public consent requirements. The
 *   state apparatus sets and enforces admission criteria, justified as
 *   protecting labor standards and welfare system integrity; legitimacy
 *   requires acknowledging dual victim sets (excluded migrants and residents
 *   displaced by enforcement costs) and submitting enforcement to
 *   proportionality scrutiny. The claim/metric gap is authored intentionally:
 *   this reading claims the constraint achieves equilibrium through
 *   legitimacy balance, while the metrics describe substantially extractive,
 *   actively enforced operation with rising theater ratios — the engine
 *   measures whether the balancing claim matches the operational reality.
 *
 * KEY AGENTS:
 *   - state_institutional_apparatus: Agenda-setter, sets admission rules and calibrates enforcement to maintain public consent. Holds institutional power but is constrained by proportionality tests and resistance from both excluded migrants and alternative sovereignty readings.
 *   - excluded_migrants: Powerless payers, bear the cost of non-entry (foregone wages, family separation, vulnerability). Trapped with no formal voice in the consent process that legitimates their exclusion.
 *   - displaced_labor_dependents: Nominal beneficiaries who may gain from restricted labor supply competition, but also payers through enforcement costs and reduced essential-service labor. Constrained exit within national borders; participate in consent via voting but lack direct power over criteria.
 *   - labor_market_employers: Powerful beneficiaries, gain from controlled labor supply that relieves shortage in high-skill sectors. Mobile enough to exit to higher-admission jurisdictions if restrictions exceed labor needs.
 *   - human_rights_monitoring_bodies: Analytical observers, assess whether enforcement violates proportionality and necessity; exert reputational pressure but cannot override state decisions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__jurisdictional_sovereignty, 0.68).
domain_priors:suppression_score(border_control_legitimacy__jurisdictional_sovereignty, 0.71).
domain_priors:theater_ratio(border_control_legitimacy__jurisdictional_sovereignty, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, extractiveness, 0.68).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__jurisdictional_sovereignty, tangled_rope).
narrative_ontology:human_readable(border_control_legitimacy__jurisdictional_sovereignty, "Border Control Legitimacy via Jurisdictional Sovereignty").
narrative_ontology:topic_domain(border_control_legitimacy__jurisdictional_sovereignty, "political_philosophy/international_law").

domain_priors:requires_active_enforcement(border_control_legitimacy__jurisdictional_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__jurisdictional_sovereignty, 'eb66808e-8468-44c2-ad0e-462981b1a3d7').
narrative_ontology:cs_kernel_codification('eb66808e-8468-44c2-ad0e-462981b1a3d7', fixed_text).
narrative_ontology:cs_authority_grounding('eb66808e-8468-44c2-ad0e-462981b1a3d7', lineage).
narrative_ontology:cs_interpretation_layer_present('eb66808e-8468-44c2-ad0e-462981b1a3d7').
narrative_ontology:cs_reading_relation('eb66808e-8468-44c2-ad0e-462981b1a3d7', border_control_legitimacy__sovereignty_primary, coexists_with).
narrative_ontology:cs_reading_relation('eb66808e-8468-44c2-ad0e-462981b1a3d7', border_control_legitimacy__freedom_of_movement_primary, coexists_with).
narrative_ontology:cs_axiom('eb66808e-8468-44c2-ad0e-462981b1a3d7', foundational, sovereignty_includes_jurisdictional_not_absolute_closure).
narrative_ontology:cs_axiom_status(sovereignty_includes_jurisdictional_not_absolute_closure, holdable).
narrative_ontology:cs_axiom_grounding('eb66808e-8468-44c2-ad0e-462981b1a3d7', sovereignty_includes_jurisdictional_not_absolute_closure, deontological).
narrative_ontology:cs_axiom('eb66808e-8468-44c2-ad0e-462981b1a3d7', foundational, legitimacy_requires_proportionality_and_consent_balance).
narrative_ontology:cs_axiom_status(legitimacy_requires_proportionality_and_consent_balance, holdable).
narrative_ontology:cs_axiom_grounding('eb66808e-8468-44c2-ad0e-462981b1a3d7', legitimacy_requires_proportionality_and_consent_balance, deontological).
narrative_ontology:cs_reference_frame('eb66808e-8468-44c2-ad0e-462981b1a3d7', post_wwii_international_law_framework).
narrative_ontology:cs_drift_state('eb66808e-8468-44c2-ad0e-462981b1a3d7', contemporary_migration_pressure_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('eb66808e-8468-44c2-ad0e-462981b1a3d7', '').
narrative_ontology:cs_kernel_id(border_control_legitimacy__jurisdictional_sovereignty, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, state_institutional_apparatus).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, excluded_migrants).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, displaced_labor_dependents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, displaced_labor_dependents).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, labor_market_employers).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, general_public_consent_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces immigration criteria, calibrates enforcement to labor needs and public consent, administers the legitimacy test that justifies the constraint. Collects the authority to regulate labor supply, welfare entitlements, and political participation within territory. Also bears the cost of maintaining enforcement infrastructure and navigating the tension between labor demand and public restriction sentiment.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, state_institutional_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Denied entry or permanent residence despite labor market demand and humanitarian need. Bear the full cost of exclusion: foregone wages (often 5–10x home-country earnings), family separation, vulnerability to trafficking and exploitation in origin contexts. Have no formal representation in the consent process that legitimates their exclusion; voice only through international advocacy bodies that lack enforcement power.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, excluded_migrants, payer,
    powerless, biographical, trapped, global).

% Residents whose wages and employment are structured by restricted labor supply (care work, agriculture, hospitality, construction understaffed by admission restrictions). May benefit from reduced competition for low-skill jobs but bear diffuse costs: enforcement-financed welfare-state shrinkage, reduced essential-service availability, rising service costs (childcare, eldercare). Participate nominally in consent via electoral mechanisms but lack direct influence over admission criteria and face information asymmetry about true labor-market tradeoffs.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, displaced_labor_dependents, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(border_control_legitimacy__jurisdictional_sovereignty, displaced_labor_dependents, beneficiary).

% Benefit from controlled admission that relieves labor shortage in high-skill professional sectors and in essential-service occupations where citizens underSupply labor. Gain from predictable labor costs and can exit to higher-admission jurisdictions if restrictions tighten beyond labor needs. Informal political power through industry associations and lobbying despite not holding formal sovereignty authority.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, labor_market_employers, beneficiary,
    powerful, biographical, mobile, global).

% Nominal holders of the public consent that legitimates enforcement. In practice, consent is aggregated via electoral mechanisms, opinion polling, and representative institutions that compress diverse preferences into aggregate vectors. Benefit when admission is calibrated to reduce labor-market disruption and protect welfare-system integrity; bear distributed costs of enforcement infrastructure and occasional restrictions that spike essential-service prices.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, general_public_consent_holders, beneficiary,
    organized, biographical, constrained, national).

% Nations and regions whose citizens face systematic exclusion by admission criteria (education/wealth screening, colonial-legacy nationality hierarchies, development-stage stratification). Would argue for freedom-of-movement readings or equality-based sovereignty claims but are structurally excluded from the legitimacy-consent conversation because consent is aggregated only within the benefiting territory. Their voice appears only as pressure on proportionality tests via human rights monitoring, not as participants in admission policy.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, alternative_sovereignty_claimants, excluded,
    powerless, generational, trapped, global).

% International and regional human rights bodies (UN treaty bodies, regional human rights courts, NGO networks) document whether enforcement violates proportionality and necessity standards. Issue recommendations and (in some cases) binding judgments that states are formally obligated to comply with. Cannot override state decisions but exert reputational pressure and provide recourse for excluded migrants and displaced residents. Serve as the external accountability layer that sustains the claim that legitimacy constraints are real, not theater.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, human_rights_monitoring_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_control_legitimacy__jurisdictional_sovereignty, state_institutional_apparatus).
narrative_ontology:fixing_cost_class(border_control_legitimacy__jurisdictional_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a jurisdictional regime that allocates labor access, welfare entitlements, and political participation within a bounded territory, coordinating public goods provisioning with labor supply, and enabling democratic consent mechanisms to operate on a bounded polity.
% TRANSFER_FUNCTION: Moves from excluded migrants the opportunity cost of non-entry (foregone wages, family separation, vulnerability), and from residents the cost of enforcement infrastructure and reduced labor supply in essential services; accrues to the state apparatus as jurisdictional control capacity and legitimacy authority, and to employers as a managed labor supply.
% ABSENT_VOICES: Nations and regions whose citizens are systematically excluded would dispute that public consent is legitimately aggregated only within the territory that benefits from exclusion. Migrants themselves are structurally absent from the consent process — their preferences on admission are not directly represented in the legitimacy calculation. Alternative sovereignty readings (freedom-of-movement-primary, sovereignty-primary) would reframe the entire structure but are not seated at the table where this reading dominates.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared and sovereignty reverted to pure jurisdictional authority without border-control legitimacy apparatus, labor markets would reallocate geographically, welfare system financing would require restructuring, and political participation would no longer map cleanly to territorial residence — the state as currently constructed would reorganize into either open-borders or absolute-sovereignty regimes.
% FOUNDING_PROBLEM: Post-WWII international order required a mechanism to reconcile state sovereignty with universal human rights and labor rights: how to allow states to regulate welfare and labor standards while respecting dignity and freedom of movement. The jurisdictional reading attempts to split the difference — sovereignty grants regulatory power but not absolute closure authority; legitimacy depends on balancing protection obligations (to both residents and migrants) with public consent.
% FOUNDING_PROBLEM_CORROBORATION: International human rights bodies and migration scholars attest the founding problem is live and that current enforcement violates proportionality tests (pushing back against pure sovereignty reading). State governments attest that public consent constrains admission (supporting jurisdictional reading). However, neither migrants nor alternative sovereignty claimants attest the problem is solved or well-balanced — external corroboration is divided by reading.
narrative_ontology:disappearance_verdict(border_control_legitimacy__jurisdictional_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(border_control_legitimacy__jurisdictional_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_control_legitimacy__jurisdictional_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(border_control_legitimacy__jurisdictional_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(border_control_legitimacy__jurisdictional_sovereignty, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_control_legitimacy__jurisdictional_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_control_legitimacy__jurisdictional_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_control_legitimacy__jurisdictional_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.68 at interval end because admission criteria concentrate benefit (state apparatus, employers) while diffusing cost (excluded migrants, displaced residents). Suppression is 0.71 because enforcement requires active machinery (border patrol, deportation, visa denial mechanisms) and escalates when political pressure mounts to restrict further. Theater ratio plateaus at 0.42 because the legitimacy discourse (proportionality, public consent, human rights acknowledgment) remains present even as enforcement hardens — the claim that this is balanced and legitimate persists even when metrics show asymmetric extraction, marking the theater/function boundary. Measurement series show extractiveness and suppression rising through year 15–20, then plateauing: the point at which enforcement infrastructure matures and public discourse stabilizes around the jurisdictional reading (neither opening borders fully nor adopting absolute sovereignty). The trajectory reflects the constraint's operation as described: initial tension between labor demand (pushing admission up) and public concern about welfare/cohesion (pushing restriction down), settling into a managed equilibrium where the state administers the balance.
 *
 * PERSPECTIVAL GAP:
 *   The state apparatus experiences this constraint as legitimate coordination: the apparatus negotiates between labor needs and public consent, balancing valid interests, and submits enforcement to proportionality tests. Excluded migrants experience it as pure extraction: they bear costs with no voice in the process and no recourse except humanitarian appeals channeled through external bodies. Displaced labor dependents occupy the middle ground — they participate nominally in consent (voting) but lack power to influence criteria directly, and bear diffuse costs (enforcement-financed welfare shrinkage, reduced essential-service labor availability) while gaining dispersed benefits (reduced low-skill competition). The engine computes these seats' classifications from the structural data: the apparatus emerges as beneficiary/agenda-setter; excluded migrants as powerless target; displaced dependents as moderate-to-constrained occupying an asymmetric middle. This perspectival divergence is the signature of tangled rope: genuine coordination (labor allocation, welfare provisioning) AND asymmetric extraction (migrants excluded from voice, residents bearing enforcement cost).
 *
 * DIRECTIONALITY LOGIC:
 *   State apparatus: d ≈ 0.15 (beneficiary end) — sets the rules, collects legitimacy authority, administers the consent process itself. Excluded migrants: d ≈ 0.92 (target end) — trapped, powerless, bear costs without voice, no exit. Displaced labor dependents: d ≈ 0.55 (near-symmetric) — participate in nominal consent, bear distributed enforcement costs, gain some benefit from labor supply control but are not the primary beneficiaries. Labor market employers: d ≈ 0.25 (beneficiary end) — powerful, mobile, gain from controlled admission, not primary enforcer but align with state's interest in restriction calibration. The derivation chain runs: beneficiaries (state apparatus, employers) → low d; victims (excluded migrants, displaced residents bearing costs) → higher d; exit options (trapped for migrants, constrained for residents) modulate upward toward target; power (powerless for migrants, organized-but-constrained for residents) amplifies the extraction signal for targets. The commentary explains why this is tangled rope and not snare: the coordination function (labor allocation, public-goods provisioning within bounded territory) is genuine; the extraction is not pure predation but asymmetric cost-sharing that the state justifies as necessary to maintain the coordination function itself.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy potential is high. The founding problem (reconcile sovereignty with human rights/labor rights) was live at the constraint's initiation but the jurisdictional reading's claim to 'balanced legitimacy' masks substantial extraction: excluded migrants pay the cost, displaced residents bear enforcement expenses, and the state apparatus collects authority and legitimacy. If public consent erodes (measured by political polarization over immigration, growth of anti-immigration parties, or court invalidation of enforcement practices), the constraint could degrade into pure snare with the coordination story as cover. The theater ratio (0.42) is already elevated, indicating rising performative maintenance of the legitimacy claim — security theater, consent theater, proportionality theater — as the metrics drift toward pure extraction. Mandatrophy would manifest as: founding problem declared 'dead' (labor markets have adjusted, no live coordination problem remains) while enforcement intensifies (pure extraction mechanism persists under inertial institutional momentum). The constraint avoids immediate mandatrophy classification because human rights monitoring bodies still attest proportionality constraints have teeth, courts still invalidate some enforcement actions, and the state still narrativizes the balance rather than abandoning the legitimacy claim. But the trajectory (extraction rising, theater rising, suppression-plateau suggesting enforcement infrastructure has hardened) suggests mandatrophy is a leading indicator worth monitoring.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    public_consent_measurement_ambiguity,
    'What constitutes ''public consent'' to admission restrictions? Is it electoral majorities within the territory (which may exclude affected migrants and origin-nation citizens)? Is it supermajority threshold? Does it require unanimous consent from all affected parties?',
    'Comparative constitutional law analysis and empirical political science research on consent aggregation mechanisms: does the constraint''s legitimacy depend on electoral consent in the admitting territory only, or does proportionality require some weighting of migrants'' preferences?',
    'If consent requires only electoral majority in the admitting territory, the legitimacy test excludes migrants by structural design — the constraint is legitimated by aggregating preferences only from those who benefit from exclusion. If consent must include migrants (via consultation, affected-party participation, or proportionality weighting), the entire enforcement apparatus requires restructuring.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(public_consent_measurement_ambiguity, conceptual, 'What counts as the ''public consent'' that legitimates exclusion?').

omega_variable(
    proportionality_enforcement_divergence,
    'When enforcement practices (deportation, visa denial, detention) diverge from proportionality standards, does the constraint downgrade to pure extraction (snare) or remain tangled rope because the proportionality standard itself persists as a limiting principle?',
    'Empirical tracking of court decisions invalidating enforcement, state compliance with human rights judgments, and temporal correlation between proportionality violations and public consent erosion. If violations spike but enforcement persists (courts are ignored, compliance is nominal), the constraint is degrading toward snare.',
    'If proportionality tests are real and effective (states comply, courts can overturn enforcement), the constraint remains tangled rope with legitimacy constraints. If proportionality is theater (violated routinely, reversed rarely, states ignore judgments), the constraint is effectively pure extraction masked by legitimacy discourse — reclassified as snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_enforcement_divergence, empirical, 'Whether proportionality constraints are effective limits or performative cover for extraction.').

omega_variable(
    labor_dependency_supply_collapse,
    'If enforcement restricts admission below the labor market equilibrium, does the resulting labor shortage depress economic output enough that public consent erodes (voters withdraw support for restriction)? Or does political coordination on welfare-system protection outweigh labor-shortage costs?',
    'Time-series analysis of labor shortages, sector-specific wage growth, welfare-system financing, and electoral support for restriction policies. If restriction persists despite labor shortages and rising costs, public consent narrative is masking extraction.',
    'If labor needs eventually force admission expansion, the constraint self-corrects through the legitimacy mechanism (public demand for labor supply restoration). If restriction persists despite economic costs, the constraint is locked in by state apparatus power (extractive mechanism, not legitimacy-balanced).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_dependency_supply_collapse, empirical, 'Whether public consent oscillates with labor market pressure or remains static despite economic pressure.').

omega_variable(
    alternative_sovereignty_reading_coexistence,
    'Can the jurisdictional-sovereignty reading coexist with the freedom-of-movement-primary reading in the same legal or institutional framework? Or does adoption of one reading logically foreclose the other?',
    'Constitutional interpretation history and comparative law analysis: have jurisdictions held both readings simultaneously (via compromise or textual ambiguity), or do they inevitably choose one over the other?',
    'If readings can coexist (different parties hold them, institutional compromise preserves both), the constraint is a stable equilibrium between contested readings. If one reading forecloses the other, adoption of jurisdictional-sovereignty implies rejection of freedom-of-movement-primary — a structural commitment choice, not a balanced middle ground.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_sovereignty_reading_coexistence, conceptual, 'Whether this reading''s core premise logically forecloses or can coexist with freedom-of-movement primacy.').

omega_variable(
    enforcement_theater_escalation,
    'As extractiveness plateaus (at 0.68) and suppression stabilizes (at 0.71), does theater ratio rising to 0.42 signal the constraint is shifting toward inertial performance (Piton trajectory) rather than balanced legitimacy? Is the increasing performative component evidence of eroding real coordination function?',
    'Qualitative analysis of enforcement discourse: are state justifications for enforcement increasingly abstract and symbolic (security theater, deterrence-by-visibility) rather than concrete and functional (actual labor-shortage mitigation, welfare-system protection)? Are enforcement actions calibrated to proportionality or to political visibility?',
    'If theater is escalating while real coordination function erodes, the constraint is degrading from tangled rope toward piton (institutional inertia, performative maintenance). If theater stabilizes at a moderate level and coordination function persists, the constraint remains stable tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_theater_escalation, empirical, 'Whether rising theater ratio signals degradation toward piton or stable equilibrium in tangled rope.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__jurisdictional_sovereignty, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(bord_tr_t0, observed).
narrative_ontology:measurement(bord_tr_t5, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(bord_tr_t5, observed).
narrative_ontology:measurement(bord_tr_t10, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(bord_tr_t10, observed).
narrative_ontology:measurement(bord_tr_t15, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(bord_tr_t15, observed).
narrative_ontology:measurement(bord_tr_t20, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(bord_tr_t20, observed).
narrative_ontology:measurement(bord_tr_t25, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(bord_tr_t25, observed).
narrative_ontology:measurement(bord_tr_t30, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(bord_tr_t30, observed).
narrative_ontology:measurement(bord_tr_t35, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 35, 0.42).
narrative_ontology:measurement_basis(bord_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(bord_be_t0, observed).
narrative_ontology:measurement(bord_be_t5, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 5, 0.58).
narrative_ontology:measurement_basis(bord_be_t5, observed).
narrative_ontology:measurement(bord_be_t10, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 10, 0.62).
narrative_ontology:measurement_basis(bord_be_t10, observed).
narrative_ontology:measurement(bord_be_t15, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 15, 0.65).
narrative_ontology:measurement_basis(bord_be_t15, observed).
narrative_ontology:measurement(bord_be_t20, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(bord_be_t20, observed).
narrative_ontology:measurement(bord_be_t25, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(bord_be_t25, observed).
narrative_ontology:measurement(bord_be_t30, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(bord_be_t30, observed).
narrative_ontology:measurement(bord_be_t35, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(bord_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 0, 0.54).
narrative_ontology:measurement_basis(bord_su_t0, observed).
narrative_ontology:measurement(bord_su_t5, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 5, 0.6).
narrative_ontology:measurement_basis(bord_su_t5, observed).
narrative_ontology:measurement(bord_su_t10, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 10, 0.65).
narrative_ontology:measurement_basis(bord_su_t10, observed).
narrative_ontology:measurement(bord_su_t15, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(bord_su_t15, observed).
narrative_ontology:measurement(bord_su_t20, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(bord_su_t20, observed).
narrative_ontology:measurement(bord_su_t25, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(bord_su_t25, observed).
narrative_ontology:measurement(bord_su_t30, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(bord_su_t30, observed).
narrative_ontology:measurement(bord_su_t35, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 35, 0.71).
narrative_ontology:measurement_basis(bord_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__jurisdictional_sovereignty, resource_allocation).
narrative_ontology:boltzmann_floor_override(border_control_legitimacy__jurisdictional_sovereignty, 0.18).
narrative_ontology:affects_constraint(border_control_legitimacy__jurisdictional_sovereignty, border_control_legitimacy__sovereignty_primary).
narrative_ontology:affects_constraint(border_control_legitimacy__jurisdictional_sovereignty, border_control_legitimacy__freedom_of_movement_primary).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'border_control_legitimacy' alongside sibling readings 'sovereignty_primary' (absolute closure discretion) and 'freedom_of_movement_primary' (movement as fundamental right). The three readings share the referent (border control regimes) but instantiate different ε values and different victim sets based on which premise each reading prioritizes. This reading (jurisdictional_sovereignty) authorizes ε=0.68 acknowledging dual victims and constrained legitimacy; sovereignty_primary would author ε lower (pure coordination story, no dual victim acknowledgment); freedom_of_movement_primary would author ε higher (fundamental rights violation). The ε divergence is not observer-relative — it reflects the structural reading of what counts as extracted, which depends on which rights baseline the reading endorses (jurisdictional authority vs. absolute discretion vs. movement-as-right). Each reading gets its own constraint story with its own ε and its own victim set. Links are bidirectional via network.affects_constraints: changes to one reading's authority grounding (e.g., courts invalidate absolute-discretion doctrine) create pressure on all three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(border_control_legitimacy__jurisdictional_sovereignty, powerless, 0.92).
constraint_indexing:directionality_override(border_control_legitimacy__jurisdictional_sovereignty, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
