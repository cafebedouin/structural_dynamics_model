% ============================================================================
% CONSTRAINT STORY: us_sdf_alliance_abandonment_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_sdf_alliance_abandonment_2026, []).

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
 *   constraint_id: us_sdf_alliance_abandonment_2026
 *   human_readable: US Strategic Alliance Abandonment in Northeast Syria (2026)
 *   domain: geopolitical/military_alliance
 *
 * SUMMARY:
 *   The US-SDF military alliance in northeast Syria (2014-2026) represents a
 *   canonical snare: a strategic partnership with structural asymmetry where
 *   one party (SDF) absorbs disproportionate human and material costs while
 *   the other party (US) maintains low-cost air presence and retains full
 *   exit capacity. The SDF provided ground forces for counter-ISIS operations
 *   at extraordinary human cost (11,000+ military casualties, 3,500+ civilian
 *   deaths from military operations) while the US provided air support,
 *   logistics, intelligence, and special operations advisory presence. By
 *   2026, US domestic political realignment (isolationist constituencies,
 *   Middle East fatigue, strategic reorientation toward Indo-Pacific
 *   competition) triggered policy shift toward alliance abandonment without
 *   equivalent commitment to transition mechanisms, security guarantees, or
 *   negotiated hand-off arrangements. The constraint's extractiveness has
 *   accumulated over the 12-year interval (0.35 → 0.68) as the US made
 *   increasing use of SDF capability while reducing its own force footprint
 *   and incrementally signaling unwillingness to sustain the partnership.
 *   Suppression has intensified (0.48 → 0.72) as the SDF has become
 *   progressively locked into military dependence with no alternative
 *   security arrangements available. Theater ratio (0.30 → 0.55) reflects
 *   that the alliance initially had substantial functional content
 *   (counter-ISIS coordination) but increasingly became performative as US
 *   policy shifted to exit preparation while maintaining the facade of
 *   partnership. The abandonment constraint itself is the structural fact:
 *   the SDF cannot exit without losing operational capacity; the US can exit
 *   (and is exiting) with minimal domestic cost; the extraction flows
 *   unidirectionally from SDF toward US strategic interests.
 *
 * KEY AGENTS:
 *   - SDF Ground Forces: Primary victim (powerless/trapped) — absorbed 11,000+ military casualties for counter-ISIS operations; face abandonment without transition mechanism or security guarantees
 *   - Northeast Syria Civilian Population: Victim (moderate/constrained) — depends on security umbrella provided by US-SDF partnership; faces Turkish military threats and ISIS recrudescence post-withdrawal
 *   - US Strategic Realignment Institutions: Primary beneficiary (institutional/arbitrage) — State Department, Defense Department, National Security Council benefit from reallocation of Middle East resources to Indo-Pacific competition; experience constraint as a coordination problem to be managed
 *   - US Domestic Political Constituencies: Beneficiary (powerful/mobile) — isolationist and war-fatigue constituencies benefit from alliance abandonment; experience as coordination of exit strategy
 *   - Turkey: Secondary actor (powerful/mobile) — both benefits (removal of US-backed Kurdish force) and bears costs (instability in border region); has independent capability to renegotiate with SDF
 *   - ISIS Residual Capability: Implicit victim beneficiary (organized/constrained) — post-US-withdrawal security vacuum creates operational space for ISIS reorganization
 *   - Cold War Alliance Architecture: Institutional framework (institutional/arbitrage) — the NATO/bilateral alliance system that SDF was never formally integrated into; shows this partnership as disposable vestige
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_sdf_alliance_abandonment_2026, 0.68).
domain_priors:suppression_score(us_sdf_alliance_abandonment_2026, 0.72).
domain_priors:theater_ratio(us_sdf_alliance_abandonment_2026, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_sdf_alliance_abandonment_2026, extractiveness, 0.68).
narrative_ontology:constraint_metric(us_sdf_alliance_abandonment_2026, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(us_sdf_alliance_abandonment_2026, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_sdf_alliance_abandonment_2026, snare).
narrative_ontology:human_readable(us_sdf_alliance_abandonment_2026, "US Strategic Alliance Abandonment in Northeast Syria (2026)").
narrative_ontology:topic_domain(us_sdf_alliance_abandonment_2026, "geopolitical/military_alliance").

domain_priors:requires_active_enforcement(us_sdf_alliance_abandonment_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_sdf_alliance_abandonment_2026, us_strategic_realignment).
narrative_ontology:constraint_beneficiary(us_sdf_alliance_abandonment_2026, domestic_political_constituencies).
narrative_ontology:constraint_victim(us_sdf_alliance_abandonment_2026, sdf_ground_forces).
narrative_ontology:constraint_victim(us_sdf_alliance_abandonment_2026, northeast_syria_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SDF GROUND FORCES (SNARE) — Trapped by military dependence on US air support and logistics. Exit is impossible without losing operational capacity against remaining ISIS cells and Turkish threats. The SDF absorbed disproportionate casualties (estimated 11,000+ deaths) while the US maintained low-cost air presence. Abandonment without transition mechanism forces renegotiation from position of maximum weakness.
constraint_indexing:constraint_classification(us_sdf_alliance_abandonment_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: NORTHEAST SYRIA STABILITY (SNARE) — Constrained by geography and great-power dynamics. The civilian population and governance structures depend on the security umbrella the US-SDF partnership provided. Turkish military threats, ISIS recrudescence, and potential Syrian government reintegration all become acute risks. The abandonment constraint operates on a generational time scale — the effects will structure the region for decades.
constraint_indexing:constraint_classification(us_sdf_alliance_abandonment_2026, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: US STRATEGIC REALIGNMENT (ROPE) — US policy institutions (State Department, Defense Department, National Security Council) experience the constraint as pure coordination of a reorientation strategy. The Middle East theater is being deprioritized; resources are shifting to Indo-Pacific competition with China and management of NATO. From this institutional perspective, the SDF alliance represents a constraint that needs to be resolved (abandoned or handed off), not a partnership to be maintained. The coordination problem is managing the optics and logistics of departure.
constraint_indexing:constraint_classification(us_sdf_alliance_abandonment_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DOMESTIC POLITICAL CONSTITUENCIES (TANGLED ROPE) — Congressional isolationists and war-fatigue constituencies benefit from alliance abandonment (reduced troop presence, reduced casualties, reduced military spending in Middle East). They experience the constraint as coordination: exiting entangling alliances is their stated policy goal. However, the abandonment also imposes extraction costs — humanitarian organizations, human rights advocates, and regional stability stakeholders bear the cost of abandoning allies. The political constraint is tangled: genuine coordination benefit for the isolationist constituency alongside asymmetric extraction from the SDF.
constraint_indexing:constraint_classification(us_sdf_alliance_abandonment_2026, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: NATO AND REGIONAL PARTNERS (SCAFFOLD) — Allied actors (Turkey, Iraq, other regional states) experience the constraint as a temporary coordination problem with a potential sunset mechanism: renegotiation of security arrangements, potential Turkish-SDF agreements, or eventual Syrian government reintegration could resolve the abandonment crisis. The constraint is a transient phase in alliance reconfiguration, not a permanent extraction. Theater ratio is moderate because the renegotiation process involves real strategic conversations alongside performative alliance management.
constraint_indexing:constraint_classification(us_sdf_alliance_abandonment_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: COLD WAR ALLIANCE ARCHITECTURE (PITON) — From a civilizational view, the US-SDF alliance is a vestigial structure — a tactical counter-ISIS partnership born from post-2011 instability but never formalized into durable institutional commitments. It persists through inertia and lack of alternatives, not because of genuine strategic integration. The theater ratio is moderate: formal statements affirm the partnership while actual policy implementation treats it as expendable. This is the characteristic piton signature: structural degradation masked by institutional rhetoric.
constraint_indexing:constraint_classification(us_sdf_alliance_abandonment_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: GREAT-POWER STRUCTURAL CONSTRAINTS (MOUNTAIN) — From a universal analytical perspective, the abandonment is treated as inevitable: great powers are constrained by structural competition to reallocate military resources away from peripheral theaters. The Syria theater is peripheral to US-China competition (the true structural constraint). Regional allies of great powers always face abandonment risk when strategic interests shift — this is a law of international structure, not a contingent policy choice. However, this perspective risks false-summit naturalization: the abandonment is a policy choice, not an iron law of geopolitics.
constraint_indexing:constraint_classification(us_sdf_alliance_abandonment_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_sdf_alliance_abandonment_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_sdf_alliance_abandonment_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_sdf_alliance_abandonment_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_sdf_alliance_abandonment_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_sdf_alliance_abandonment_2026, TR),
    TR >= 0.70.

:- end_tests(us_sdf_alliance_abandonment_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High, reflecting genuine asymmetric cost distribution. The SDF bore 11,000+ military casualties (estimated at 25-30% of peak force strength) for counter-ISIS operations. The US maintained a modest special operations and air presence (averaging 500-900 personnel, near-zero casualties). The accumulation from 0.35 (2014) to 0.68 (2026) reflects the SDF's increasing operational dependence combined with the US's incremental policy shift toward abandonment. The extractiveness value reflects this asymmetry: the constraint extracts from the SDF (via military burden and abandonment risk) and extracts for the US (via access to ground forces at low cost). Suppression (0.72): High, indicating severe structural barriers to SDF exit. The SDF cannot unilaterally exit the alliance without: (1) losing air support crucial for ISIS counter-operations, (2) facing Turkish military threats without US diplomatic cover, (3) accepting Syrian government reintegration under disadvantageous terms, or (4) renegotiating with Russia or Iran under terms likely to be worse. The rise from 0.48 (2014) to 0.72 (2026) reflects the progressive deepening of military dependence and the closure of alternative options. Suppression is primarily structural (military/logistical dependence) rather than internalized (the SDF has not accepted abandonment as legitimate). Theater ratio (0.55): Moderate-high, capturing that the alliance has increasingly become performative. In 2014-2016, the partnership was functionally driven — coordinating counter-ISIS operations required genuine military cooperation. By 2020-2026, the partnership became increasingly symbolic: US official statements affirmed commitment while actual policy planning assumed withdrawal; SDF maintained the alliance publicly while privately seeking alternative security arrangements; both parties engaged in the theater of partnership while implementing divergent exit strategies. The theater ratio rise from 0.30 to 0.55 reflects this transition from functional to performative.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival divergence across institutional vs. ground-force actors and across temporal horizons. The US institutional perspective (State/Defense/NSC) sees the alliance as a coordination problem to be resolved through orderly transition — the constraint is purely about how to exit, not about whether to exit. The SDF ground forces see the alliance as a trap: they experience the constraint as a snare where exit is impossible without catastrophic loss of capability. The NATO/regional-partner perspective sees a temporary coordination challenge (scaffold) — renegotiation of Turkish-SDF relations or Syrian reintegration could resolve the crisis. The civilizational great-power perspective risks seeing abandonment as inevitable (mountain) — peripheral allies are always expendable to great powers. The perspectives produce radically different classification outcomes from identical base data: rope for the beneficiary institution, snare for the ground-force victim, scaffold for regional partners, piton for Cold War architecture. The perspectival gap reveals the indexicality of constraint classification: the same structural fact (US withdrawing from Syria, SDF losing air support) appears as a solution from one position and a catastrophe from another.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) derives from beneficiary/victim status combined with exit options. The SDF ground forces: victim status + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high experienced extractiveness. The US institutional beneficiary with arbitrage exit: beneficiary status + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative experienced extractiveness (they benefit). The SDF's d value (0.95) reflects that they are pure targets of the extraction with no exit capacity. The US institutional d value (0.05) reflects that they are pure beneficiaries. The regional scope modifier (σ=0.9) dampens the global-scope extraction: this is a regional crisis with local intensity, not a universal structural law. Computed χ = 0.68 × 1.42 × 0.9 ≈ 0.87 from the SDF perspective, confirming snare classification (χ ≥ 0.66).
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE RESOLUTION: The mandatrophy here is whether the US-SDF alliance is coordination that happens to extract value (tangled rope) or pure extraction dressed as coordination (snare). The mandatrophy resolves in favor of snare because: (1) the beneficiary (US institution) experiences the relationship as a pure coordination problem with no acknowledgment of asymmetric extraction, (2) the victim (SDF ground forces) has zero exit capacity, (3) the suppression mechanism (military dependence) is structural and cannot be reformed within the partnership, (4) the extractiveness increased monotonically over time as the US signaled abandonment while the SDF remained locked in. The tangled-rope hypothesis would require that the SDF received genuine coordination benefits (enhanced security, stability, governance capacity) that offset the extraction costs. The measurement shows the opposite: as the US extracted more ground-force capability (SDF bore more casualties as US presence declined), the stability benefits degraded. By 2026, the SDF faces lower security, higher military costs, and abandonment — the worst outcome on all axes. This rules out tangled rope. The snare classification holds because the extraction is unidirectional and the victim has no exit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    us_commitment_sincerity,
    'Were US institutional commitments to the SDF ever genuine long-term security guarantees, or was the alliance always tacitly understood as contingent on counter-ISIS operations?',
    'Documentary analysis of diplomatic exchanges, defense cooperation agreements, and classified policy reviews; comparison of stated vs implicit duration assumptions in alliance planning',
    'If genuinely committed: abandonment is extraction (breach of trust). If always contingent: abandonment is merely transition of a tactical partnership, and the snare classification should downgrade to tangled_rope or rope. The SDF''s exit options change from ''trapped'' to ''constrained'' or ''mobile'' depending on the answer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_commitment_sincerity, empirical, 'Whether US commitment to SDF alliance was sincere or contingent').

omega_variable(
    turkish_sdf_negotiation_feasibility,
    'Can the SDF renegotiate directly with Turkey to resolve the abandonment constraint, or are Turkish-US relations hostile enough to foreclose Turkish-SDF accommodation?',
    'Diplomatic track record of Turkish-SDF negotiations; Turkish military doctrine toward Kurdish groups; statements from Turkish leadership regarding potential SDF reintegration or autonomy arrangements',
    'If negotiation is feasible: the scaffold perspective becomes structural and the constraint''s effective suppression drops (exit option shifts from ''trapped'' to ''constrained''). If Turkish-US hostility prevents negotiation: the snare classification deepens and suppression remains high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(turkish_sdf_negotiation_feasibility, empirical, 'Whether Turkish-SDF direct negotiation is feasible').

omega_variable(
    isis_resurgence_counterfactual,
    'What is the probability and severity of ISIS operational resurgence in the post-US-withdrawal vacuum, and does this materialize the snare constraint into kinetic harm?',
    'Intelligence assessments of ISIS capability and intent; monitoring of detainee camp stability and recruitment networks; measured changes in ISIS operational tempo post-withdrawal',
    'If resurgence is severe: the extraction mechanism (US forcing SDF to absorb disproportionate security costs post-withdrawal) becomes visceral and the snare classification solidifies. If resurgence is contained: the abandonment cost is primarily political and the constraint''s character shifts toward tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(isis_resurgence_counterfactual, empirical, 'Severity of ISIS resurgence post-US withdrawal').

omega_variable(
    domestic_political_coalition_stability,
    'Will US domestic political constituencies sustaining the abandonment policy remain cohesive if regional destabilization produces humanitarian crisis or terrorism attribution back to US soil?',
    'Polling and legislative behavior tracking; media narrative analysis; response to crisis events; comparison with historical cases (e.g., Vietnam War syndrome, Libya intervention reversal)',
    'If coalition remains stable: abandonment proceeds and the snare classification holds. If coalition fractures: US re-engagement becomes politically possible, and the constraint''s directionality inverts (extraction flows back toward SDF as re-engagement costs spike).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(domestic_political_coalition_stability, conceptual, 'Stability of US domestic political coalition supporting abandonment').

omega_variable(
    sdf_institutional_survival,
    'Can the SDF maintain institutional coherence as a military force and governance structure without sustained US logistical support?',
    'Tracking of SDF force structure, defection rates, equipment maintenance capacity, and administrative functionality; assessment of alternative support sources (Russia, Iran, or regional states)',
    'If institutional survival is feasible: the SDF transitions from ''trapped'' to ''constrained'' exit status and the snare may degrade to tangled_rope. If survival is unlikely: the snare becomes a path to institutional collapse and the extraction cost is catastrophic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sdf_institutional_survival, empirical, 'Whether SDF can survive institutionally post-US withdrawal').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_sdf_alliance_abandonment_2026, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usalliance_theater_2014, us_sdf_alliance_abandonment_2026, theater_ratio, 0, 0.3).
narrative_ontology:measurement(usalliance_theater_2020, us_sdf_alliance_abandonment_2026, theater_ratio, 6, 0.42).
narrative_ontology:measurement(usalliance_theater_2026, us_sdf_alliance_abandonment_2026, theater_ratio, 12, 0.55).

% Extraction over time
narrative_ontology:measurement(usalliance_extract_2014, us_sdf_alliance_abandonment_2026, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(usalliance_extract_2020, us_sdf_alliance_abandonment_2026, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(usalliance_extract_2026, us_sdf_alliance_abandonment_2026, base_extractiveness, 12, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(usalliance_suppress_2014, us_sdf_alliance_abandonment_2026, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(usalliance_suppress_2020, us_sdf_alliance_abandonment_2026, suppression_requirement, 6, 0.6).
narrative_ontology:measurement(usalliance_suppress_2026, us_sdf_alliance_abandonment_2026, suppression_requirement, 12, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_sdf_alliance_abandonment_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(us_sdf_alliance_abandonment_2026, isis_resurgence_post_withdrawal).
narrative_ontology:affects_constraint(us_sdf_alliance_abandonment_2026, kurdish_autonomy_regional_stability).
narrative_ontology:affects_constraint(us_sdf_alliance_abandonment_2026, us_indo_pacific_reorientation).
narrative_ontology:affects_constraint(us_sdf_alliance_abandonment_2026, turkish_strategic_positioning).

% DUAL FORMULATION NOTE:
% The US-SDF alliance abandonment is downstream of the broader US strategic reorientation away from Middle East (see us_indo_pacific_reorientation). The abandonment creates immediate upstream effects on ISIS recrudescence capacity (see isis_resurgence_post_withdrawal) and on Kurdish autonomous governance sustainability (see kurdish_autonomy_regional_stability). Turkish strategic positioning benefits from SDF weakness post-US-withdrawal (see turkish_strategic_positioning). The constraint family shows a clear upstream-to-downstream influence chain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_sdf_alliance_abandonment_2026, institutional, 0.04).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
