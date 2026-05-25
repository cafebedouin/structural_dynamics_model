% ============================================================================
% CONSTRAINT STORY: palestinian_territorial_fragmentation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_palestinian_territorial_fragmentation, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: palestinian_territorial_fragmentation
 *   human_readable: Palestinian Territorial Fragmentation
 *   domain: political_geography/territorial_control
 *
 * SUMMARY:
 *   Palestinian territorial fragmentation represents a structural constraint
 *   that divides Palestinian-controlled territory into non-contiguous
 *   enclaves (West Bank Areas A, B, C; Gaza Strip; East Jerusalem) with
 *   restricted inter-zone mobility and unified governance. This fragmentation
 *   operates simultaneously as a security control mechanism (Israeli
 *   perspective), a resource/land extraction system (Palestinian victim
 *   perspective), a governance coordination challenge (Palestinian Authority
 *   perspective), and an international legal violation (global norms
 *   perspective). The constraint exhibits characteristics of pure extraction
 *   (snare) from the Palestinian population's structural position, mixed
 *   coordination-extraction (tangled rope) from the Palestinian Authority's
 *   administrative position, coordination benefit (rope) from the Israeli
 *   security and settlement frameworks, and performative violation (piton)
 *   from the international legal system. The theater ratio (0.58) reflects
 *   that extensive verbal commitments to Palestinian territorial sovereignty
 *   exist in UN resolutions, peace agreements, and international law, yet
 *   these norms lack enforcement mechanisms and do not constrain the actual
 *   fragmentation system. The extractiveness (0.68) indicates high asymmetric
 *   benefit to Israeli institutional actors and high asymmetric cost to
 *   Palestinian population and state-viability, with suppression (0.75)
 *   reflecting severe mobility restrictions, administrative barriers, and
 *   permit-dependent access.
 *
 * KEY AGENTS:
 *   - Palestinian Population: Primary victim (powerless/trapped) — subject to territorial fragmentation, mobility restrictions, permit systems, and economic constraints; cannot exit or reform the system
 *   - Palestinian State Viability: Institutional victim (powerless/trapped) — capacity for sovereign statehood undermined by non-contiguous territory; cannot achieve state prerequisites within constraint
 *   - Israeli Settlement Expansion: Primary beneficiary (institutional/arbitrage) — gains land availability, security perimeter, and reduced Palestinian territorial control; benefits from fragmentation as enabling mechanism
 *   - Israeli Security Framework: Secondary beneficiary (institutional/arbitrage) — uses fragmentation for checkpoint networks, graduated security zones, and early-warning infrastructure; experiences fragmentation as coordination mechanism
 *   - Palestinian Authority: Intermediate actor (organized/constrained) — depends on fragmented territorial jurisdictions for governance capacity; coordinates services within constraint; cannot unilaterally exit without abandoning constituencies
 *   - International Legal Norms: Institutional observer (institutional/constrained) — nominally prohibits fragmentation but lacks enforcement capacity; maintains symbolic authority while functionally degraded
 *   - Analytical Observer: External position (analytical/analytical) — risks naturalizing contingent fragmentation as inevitable geographic/political fact rather than maintained institutional system
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(palestinian_territorial_fragmentation, 0.68).
domain_priors:suppression_score(palestinian_territorial_fragmentation, 0.75).
domain_priors:theater_ratio(palestinian_territorial_fragmentation, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(palestinian_territorial_fragmentation, extractiveness, 0.68).
narrative_ontology:constraint_metric(palestinian_territorial_fragmentation, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(palestinian_territorial_fragmentation, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(palestinian_territorial_fragmentation, snare).
narrative_ontology:human_readable(palestinian_territorial_fragmentation, "Palestinian Territorial Fragmentation").
narrative_ontology:topic_domain(palestinian_territorial_fragmentation, "political_geography/territorial_control").

domain_priors:requires_active_enforcement(palestinian_territorial_fragmentation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(palestinian_territorial_fragmentation, israeli_settlement_expansion).
narrative_ontology:constraint_beneficiary(palestinian_territorial_fragmentation, israeli_security_framework).
narrative_ontology:constraint_victim(palestinian_territorial_fragmentation, palestinian_population).
narrative_ontology:constraint_victim(palestinian_territorial_fragmentation, palestinian_state_viability).
narrative_ontology:constraint_victim(palestinian_territorial_fragmentation, territorial_sovereignty).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PALESTINIAN POPULATION (SNARE) — Trapped within fragmented enclaves (West Bank Areas A/B/C, Gaza, Jerusalem) with severe mobility restrictions, checkpoints, and permit systems. No unified territorial control; no exit option from the fragmentation system. Extraction occurs through resource constraints, land dispossession, and institutional barriers to economic development. Maximum suppression via security apparatus and administrative fragmentation.
constraint_indexing:constraint_classification(palestinian_territorial_fragmentation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PALESTINIAN STATE VIABILITY (SNARE) — The institutional capacity for sovereign state formation is structurally undermined by territorial fragmentation. Contiguous territory, unified governance, and resource control are prerequisites for statehood that the constraint prevents. No escape path; bears full cost of the constraint's maintenance.
constraint_indexing:constraint_classification(palestinian_territorial_fragmentation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: PALESTINIAN AUTHORITY (TANGLED ROPE) — Coordinates public services and governance across fragmented territories while simultaneously depending on the fragmentation system for its administrative jurisdictions (Areas A/B). Experiences both coordination function (governing within assigned areas) and extraction (constrained by fragmentation, dependent on permits and Israeli security coordination). Constrained exit — administrative dissolution would abandon constituencies.
constraint_indexing:constraint_classification(palestinian_territorial_fragmentation, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ISRAELI SECURITY FRAMEWORK (ROPE) — Experiences territorial fragmentation as a coordination mechanism for security perimeter control and early-warning infrastructure. Fragmentation enables graduated security zones, checkpoint verification, and intelligence presence without requiring full territorial occupation. Arbitrage option — security goals could theoretically be met through alternative mechanisms. Net beneficiary of the fragmentation system.
constraint_indexing:constraint_classification(palestinian_territorial_fragmentation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ISRAELI SETTLEMENT EXPANSION (ROPE) — Benefits from fragmentation as it reduces Palestinian territorial control and increases land availability for settlements within fragmented Palestinian areas. Experiences fragmentation as enabling coordination of settlement placement within security-controlled zones. Arbitrage option — expansion could be redirected through other mechanisms. Primary beneficiary.
constraint_indexing:constraint_classification(palestinian_territorial_fragmentation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: INTERNATIONAL NORMS (PITON) — UN resolutions and international law nominally prohibit territorial fragmentation as a governance mechanism and demand Palestinian self-determination. These norms persist despite decades of violation and non-enforcement. Theater ratio high — the normative framework is performative (extensively cited, symbolically powerful) but lacks enforcement mechanisms. Piton structure: maintained through institutional inertia and symbolic value despite functional failure.
constraint_indexing:constraint_classification(palestinian_territorial_fragmentation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Risk perspective: territorial fragmentation appears as an immutable geographic/political fact — 'this is simply how the region is organized.' From civilizational/universal scale, the structural mechanics are naturalized as products of conflict dynamics or security necessity rather than contingent institutional choices. However, this is a false summit: fragmentation is actively maintained through administrative systems (permit regimes, settlement policy, checkpoint infrastructure), not a law of nature.
constraint_indexing:constraint_classification(palestinian_territorial_fragmentation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(palestinian_territorial_fragmentation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(palestinian_territorial_fragmentation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(palestinian_territorial_fragmentation, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(palestinian_territorial_fragmentation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(palestinian_territorial_fragmentation, TR),
    TR >= 0.70.

:- end_tests(palestinian_territorial_fragmentation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and increasing. The fragmentation system creates asymmetric benefits (settlement expansion, security perimeter, Israeli state expansion) and asymmetric costs (Palestinian land loss, mobility restrictions, economic constraint). The trajectory from 0.42 to 0.68 over 30 years reflects the cumulative extraction through settlement expansion and administrative densification — each additional layer of fragmentation (checkpoint expansion, Area C restrictions, settlement growth) increases the extractiveness rate. Suppression (0.75): High. The constraint operates through multiple suppression mechanisms: physical barriers (walls, checkpoints), administrative systems (permit regimes, residency restrictions), military control (Area C), and institutional dependence (Palestinian Authority's governance authority in fragmented zones). Agents cannot exit or reform the system without external intervention. Theater ratio (0.58): Moderate-high. International law and peace agreements create extensive performative framework (Oslo Accords, UN resolutions, Two-State Solution rhetoric) that symbolically commits to Palestinian territorial reunification, yet these norms are systematically violated and not enforced. The gap between normative commitment and structural reality creates theater — the international system performs commitment to Palestinian sovereignty while the actual constraint persists. The trajectory from 0.35 to 0.58 reflects increasing theater as the gap between Two-State Solution rhetoric and actual fragmentation deepens.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence across positions. The Palestinian population perceives pure extraction (snare) — the system extracts resources, land, and mobility with no coordination benefit. The Palestinian Authority perceives mixed coordination-extraction (tangled rope) — fragmentation creates administrative jurisdictions they govern but also constrains their capacity. The Israeli security framework perceives coordination (rope) — fragmentation creates security architecture that solves legitimate security-control problems. The Israeli settlement expansion perceives coordination (rope) — fragmentation creates land availability and settlement placement optimization. International norms perceive themselves as prohibitive (mountain-like, universal law) but function as piton — performative but unenforced. The analytical observer risks the false summit (mountain) by naturalizing fragmentation as immutable fact. These perspectives cannot be reconciled within the constraint's frame — no shared understanding exists of what fragmentation 'is' or 'does.' The classification must track this perspectival incompatibility.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position within the fragmentation system. Palestinian population and Palestinian state viability face maximum directionality (d ≈ 0.95, trapped powerless victims) — they are targets of extraction with no exit options and cannot mobilize. Palestinian Authority faces moderate directionality (d ≈ 0.55, organized constrained) — they coordinate within fragmented zones but depend on the fragmentation system for their administrative legitimacy. Israeli security framework and settlement expansion face low directionality (d ≈ 0.10–0.20, institutional arbitrage) — they are beneficiaries with exit options (security could be achieved through other mechanisms, settlements could be located elsewhere) but benefit from fragmentation-enabled coordination. International norms face high directionality but constrained power (d ≈ 0.80, institutional constrained) — they are formally opposed to the constraint but lack enforcement capacity. The perspectival gap between powerless trapped Palestinians (maximum extraction experienced) and institutional arbitrage Israelis (coordination benefit experienced) is structural and unbridgeable within the constraint system.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE CLASSIFICATION RESOLUTION: This constraint avoids mandatrophy confusion by distinguishing between (1) the beneficiary's experience (rope/coordination), (2) the victim's experience (snare/pure extraction), and (3) the naturalized false summit (mountain/law of nature). The snare classification from the victim perspective is robust: high extractiveness, high suppression, minimal coordination function experienced by victims, and structural dependence on the constraint for both Israeli and Palestinian institutional actors. The tangled rope perspective from the Palestinian Authority is analytically distinct — they are simultaneously victims (constrained by fragmentation) and coordinators (governing fragmented zones). The rope perspectives from Israeli actors show that fragmentation provides genuine coordination value (security perimeters, settlement placement) alongside extraction. The false summit (mountain) is correctly identified as false because fragmentation is contingently maintained through policy and institutions, not naturally inevitable. The piton classification of international norms is correct — extensive symbolic commitment without enforcement creates performative rather than functional violation. No mislabeling of coordination as extraction occurs; all types follow legitimately from structural analysis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fragmentation_maintenance_mechanisms,
    'Is territorial fragmentation maintained primarily through active Israeli policy, Palestinian institutional weakness, international inaction, or structural interdependency?',
    'Historical analysis of policy decisions vs. institutional constraints; counterfactual analysis of unified Palestinian territory scenarios under different governance assumptions; identification of reversibility thresholds',
    'If primarily active policy: snare classification is robust. If primarily institutional/structural: may upgrade to tangled_rope (both sides depend on fragmented structure). If primarily international inaction: may upgrade to scaffold (reversible with political commitment). Classification confidence directly dependent on mechanism identification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fragmentation_maintenance_mechanisms, empirical, 'Whether fragmentation is active extraction or structural interdependency').

omega_variable(
    palestinian_mobility_trap_vs_economic,
    'Does Palestinian territorial fragmentation function primarily as a security control mechanism (mobility suppression) or an economic extraction mechanism (resource/land capture)?',
    'Decompose into separate constraint stories for security-mobility and economic-extraction functions; measure extractiveness independently for each; compare ε values',
    'If primarily security: suppression dominates, classification remains snare. If primarily economic: extractiveness may be lower, classification may shift to tangled_rope. Different omega variables apply to each mechanism. This is an ε-invariance candidate — may require story decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(palestinian_mobility_trap_vs_economic, empirical, 'Security control vs. economic extraction as dominant mechanism').

omega_variable(
    settlement_expansion_inevitable_logic,
    'Does Israeli settlement expansion follow inevitably from security/ideological framework, or is it contingently chosen policy that could be reversed within fragmentation constraint?',
    'Policy analysis of settlement decisions; examination of periods of expansion vs. freezes; counterfactual analysis of fragmentation without settlement growth',
    'If inevitable: fragmentation supports settlement as coordination outcome (rope from settler perspective). If contingent: fragmentation enables settlement as a choice (snare from Palestinian perspective). This determines whether beneficiary status is structural or chosen.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settlement_expansion_inevitable_logic, conceptual, 'Whether settlement expansion is necessary outcome of fragmentation').

omega_variable(
    palestinian_state_viability_alternative_models,
    'Would non-contiguous federalist or cantonized Palestinian state structures constitute escape from the snare, or would they perpetuate fragmentation logic under different framing?',
    'Comparative analysis of non-contiguous state models (Cyprus, Bosnia, Lebanon); assessment of whether cantonization reduces or redistributes suppression; evaluation of whether mobility and economic barriers would persist under federal arrangement',
    'If alternatives constitute genuine escape: snare may be bounded (civilizational-scale exit exists even if biographical-scale does not). If alternatives perpetuate fragmentation logic: snare is more robust (exit paths are mirages). This affects long-term classification stability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(palestinian_state_viability_alternative_models, conceptual, 'Whether alternative state models escape fragmentation constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(palestinian_territorial_fragmentation, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pale_tr_t0, palestinian_territorial_fragmentation, theater_ratio, 0, 0.35).
narrative_ontology:measurement(pale_tr_t15, palestinian_territorial_fragmentation, theater_ratio, 15, 0.45).
narrative_ontology:measurement(pale_tr_t30, palestinian_territorial_fragmentation, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(pale_be_t0, palestinian_territorial_fragmentation, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(pale_be_t15, palestinian_territorial_fragmentation, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(pale_be_t30, palestinian_territorial_fragmentation, base_extractiveness, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(palestinian_territorial_fragmentation, enforcement_mechanism).
narrative_ontology:affects_constraint(palestinian_territorial_fragmentation, palestinian_economic_dependence).
narrative_ontology:affects_constraint(palestinian_territorial_fragmentation, israeli_settlement_expansion_logic).
narrative_ontology:affects_constraint(palestinian_territorial_fragmentation, international_norm_enforcement_failure).

% DUAL FORMULATION NOTE:
% Palestinian territorial fragmentation may decompose into separate constraint stories: (1) security-mobility fragmentation (checkpoint systems, Area C control) with ε ≈ 0.55, primarily suppression-based; (2) economic-land fragmentation (settlement expansion, Area C resource control) with ε ≈ 0.72, primarily extraction-based; (3) administrative fragmentation (PA jurisdictional dependence) with ε ≈ 0.45, mixed coordination-extraction. These share structural mechanisms but have different temporal trajectories and beneficiary groups. The unified story presented here treats fragmentation as a single integrated constraint; decomposition would enable more precise ε values per mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(palestinian_territorial_fragmentation, institutional, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
