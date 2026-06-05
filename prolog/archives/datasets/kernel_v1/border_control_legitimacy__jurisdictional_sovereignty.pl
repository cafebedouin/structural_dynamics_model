% ============================================================================
% CONSTRAINT STORY: border_control_legitimacy__jurisdictional_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: border_control_legitimacy__jurisdictional_sovereignty
 *   human_readable: Border Control Legitimacy under Jurisdictional Sovereignty
 *   domain: political_philosophy/international_law/migration
 *
 * SUMMARY:
 *   Border control legitimacy under jurisdictional sovereignty presents a
 *   structural problem: how can a state exercise legitimate authority over
 *   rights and obligations within its territory while remaining bound by
 *   human rights commitments, economic interdependence, and democratic
 *   legitimacy constraints? The constraint does NOT ask whether states have
 *   the right to regulate entry — it does. Rather, it asks how that
 *   regulation is bounded and what makes it legitimate. The dual victim
 *   structure (excluded migrants AND displaced domestic workers) is central
 *   to this reading's distinctiveness. Both groups are affected by the
 *   enforcement apparatus; both have claims to consideration in the
 *   legitimacy calculus. The state cannot serve one group's interests by
 *   simply imposing unlimited costs on the other. This generates a
 *   coordination problem (tangled rope) because the state must balance
 *   protection obligations with labor needs while maintaining public consent
 *   through democratic processes. The constraint's extractiveness (0.58)
 *   reflects genuine asymmetries in enforcement burden — excluded migrants
 *   are absolutely constrained; displaced workers face competition but retain
 *   voice. The suppression measurement (0.65) captures the enforcement
 *   intensity required to maintain labor-market gatekeeping while the theater
 *   ratio (0.68) reflects the performative character of immigration
 *   enforcement — capacity to enforce borders is far more limited than
 *   enforcement signaling suggests.
 *
 * KEY AGENTS:
 *   - Excluded Migrants: Primary structural victim (powerless/trapped) — face absolute border enforcement with no exit option; bear extraction without voice in legitimacy process
 *   - Displaced Domestic Workers: Secondary victim/beneficiary (moderate/constrained) — constrained by labor competition but benefit from wage-floor protection; participate in democratic legitimacy calculus
 *   - Organized Labor Unions: Primary beneficiary (organized/arbitrage) — coordinate labor scarcity and extract rent through credential gatekeeping; genuine coordination function in labor market provision
 *   - State Fiscal Apparatus: Institutional beneficiary (institutional/constrained) — manages public goods provision; genuine coordination need but constrained by proportionality and human rights obligations
 *   - Regional Integration Frameworks: Organized agents (organized/mobile) — building alternative institutional architectures (EU, ASEAN, AU) with supranational labor mobility; represent sunset mechanism for unilateral border closure
 *   - Immigration Enforcement Bureaucracy: Institutional actor (institutional/arbitrage) — maintains performative gatekeeping regime; enforcement capacity limited and selective despite high signaling
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements (absolute state discretion) as inherent in territorial sovereignty
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__jurisdictional_sovereignty, 0.58).
domain_priors:suppression_score(border_control_legitimacy__jurisdictional_sovereignty, 0.65).
domain_priors:theater_ratio(border_control_legitimacy__jurisdictional_sovereignty, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, extractiveness, 0.58).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__jurisdictional_sovereignty, tangled_rope).
narrative_ontology:human_readable(border_control_legitimacy__jurisdictional_sovereignty, "Border Control Legitimacy under Jurisdictional Sovereignty").
narrative_ontology:topic_domain(border_control_legitimacy__jurisdictional_sovereignty, "political_philosophy/international_law/migration").

domain_priors:requires_active_enforcement(border_control_legitimacy__jurisdictional_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__jurisdictional_sovereignty, '750d2a48-914e-4900-8d77-710289e94569').
narrative_ontology:cs_kernel_codification('750d2a48-914e-4900-8d77-710289e94569', formalized).
narrative_ontology:cs_authority_grounding('750d2a48-914e-4900-8d77-710289e94569', lineage).
narrative_ontology:cs_interpretation_layer_present('750d2a48-914e-4900-8d77-710289e94569').
narrative_ontology:cs_reading_relation('750d2a48-914e-4900-8d77-710289e94569', border_control_legitimacy__sovereignty_primary, coexists_with).
narrative_ontology:cs_reading_relation('750d2a48-914e-4900-8d77-710289e94569', border_control_legitimacy__freedom_of_movement_primary, influences).
narrative_ontology:cs_axiom('750d2a48-914e-4900-8d77-710289e94569', foundational, jurisdictional_authority_bounded_by_human_rights).
narrative_ontology:cs_axiom_status(jurisdictional_authority_bounded_by_human_rights, holdable).
narrative_ontology:cs_axiom_grounding('750d2a48-914e-4900-8d77-710289e94569', jurisdictional_authority_bounded_by_human_rights, deontological).
narrative_ontology:cs_axiom('750d2a48-914e-4900-8d77-710289e94569', foundational, dual_victim_acknowledgment_required_for_legitimacy).
narrative_ontology:cs_axiom_status(dual_victim_acknowledgment_required_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('750d2a48-914e-4900-8d77-710289e94569', dual_victim_acknowledgment_required_for_legitimacy, deontological).
narrative_ontology:cs_reference_frame('750d2a48-914e-4900-8d77-710289e94569', bounded_jurisdictional_authority).
narrative_ontology:cs_drift_state('750d2a48-914e-4900-8d77-710289e94569', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('750d2a48-914e-4900-8d77-710289e94569', '').
narrative_ontology:cs_kernel_id(border_control_legitimacy__jurisdictional_sovereignty, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, organized_labor).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, incumbent_citizens).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, state_fiscal_capacity).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, excluded_migrants).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, displaced_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED MIGRANTS (SNARE) — Face absolute border closure backed by state violence; no exit option. The enforcement apparatus treats their exclusion as non-negotiable. Extraction is maximal: they bear the cost of labor market segmentation without voice in the legitimacy calculus. This perspective sees the constraint as pure suppression with minimal coordination function.
constraint_indexing:constraint_classification(border_control_legitimacy__jurisdictional_sovereignty, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DISPLACED DOMESTIC WORKERS (TANGLED ROPE) — Constrained by labor market competition and subject to exclusionary pressure, but also participants in the democratic legitimacy process. They benefit from border restrictions that protect wage floors and employment access, yet bear costs if restrictions drive labor informalization or undermine economic growth. Mixed structural relationship: both victim and beneficiary depending on labor market dynamics.
constraint_indexing:constraint_classification(border_control_legitimacy__jurisdictional_sovereignty, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ORGANIZED LABOR (ROPE) — Primary institutional beneficiary. Unions coordinate labor scarcity and negotiate wage protection, using border closure as a coordination mechanism. However, this perspective acknowledges the constraint as having genuine coordination function: labor scarcity coordination is real. The extraction component (union gatekeeping, wage premium maintenance) is subordinate to the coordination benefit from their structural position.
constraint_indexing:constraint_classification(border_control_legitimacy__jurisdictional_sovereignty, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE FISCAL CAPACITY (TANGLED ROPE) — Genuine coordination problem: providing public goods requires tax base stability. Border control coordinates this by managing public service demand. Legitimate constraint: unmanaged migration can overwhelm public capacity. But enforcement apparatus is constrained by proportionality and necessity — extraction cannot exceed what is required for public goods coordination. Legitimacy crisis when enforcement denies basic human rights (healthcare, non-refoulement obligations) OR when admission undermines public services sufficiently to erode public consent.
constraint_indexing:constraint_classification(border_control_legitimacy__jurisdictional_sovereignty, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REGIONAL INTEGRATION (SCAFFOLD) — EU, ASEAN, African Union frameworks represent temporary coordination solutions with built-in sunset logic. Open internal borders + harmonized social protection constitute a genuine coordination mechanism that bypasses unilateral border closure. This perspective sees the constraint as a temporary stage in a generational transition toward supranational labor mobility frameworks. The scaffold's sunset is institutional: as regional frameworks mature, unilateral border closure legitimacy erodes.
constraint_indexing:constraint_classification(border_control_legitimacy__jurisdictional_sovereignty, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: IMMIGRATION ENFORCEMENT APPARATUS (PITON) — The enforcement machinery (visa regimes, border patrol, asylum adjudication) is substantially performative: it signals commitment to 'border security' while actual enforcement capacity is limited and highly selective. The system persists through institutional inertia despite documented failure rates (visa overstays exceed apprehensions; asylum systems are backlogged; enforcement is disproportionately targeted). Theater ratio measures the gap between declared enforcement intensity and actual capability.
constraint_indexing:constraint_classification(border_control_legitimacy__jurisdictional_sovereignty, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational perspective, this perspective risks naturalizing territorial sovereignty as an immutable boundary condition of the international system. The false-summit risk: treating jurisdictional authority over rights/obligations as logically entailing border closure authority, when the two are structurally distinct. The mountain classification here represents the naturalization that the reading explicitly rejects.
constraint_indexing:constraint_classification(border_control_legitimacy__jurisdictional_sovereignty, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_control_legitimacy__jurisdictional_sovereignty_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(border_control_legitimacy__jurisdictional_sovereignty, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(border_control_legitimacy__jurisdictional_sovereignty, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_control_legitimacy__jurisdictional_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(border_control_legitimacy__jurisdictional_sovereignty, TR),
    TR >= 0.70.

:- end_tests(border_control_legitimacy__jurisdictional_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint embeds a genuine extraction mechanism — organized labor captures wage premiums through labor scarcity; employers avoid labor cost externalities through enforced border closure; the state captures fiscal benefits of controlled migration flows. But the extractiveness is not maximal (which would be 0.80+) because coordination functions are real: the state genuinely needs to manage public services capacity; labor markets benefit from some stability and wage-floor protection; and the framework acknowledges dual victim sets and proportionality constraints, preventing unlimited extraction. The measurement trajectory (0.48 → 0.58) reflects gradual extraction accumulation as enforcement machinery becomes more sophisticated and exclusionary pressure increases. Suppression (0.65): Substantial. The enforcement apparatus requires significant coercive capacity — border walls, deportation machinery, asylum adjudication backlogs, detention systems — to implement exclusion. But suppression is not total (which would be 0.85+) because: (a) some migrants find alternative pathways (smuggling, visa overstays, internal migration); (b) enforcing public consent requires rhetorical justification, not pure force; (c) proportionality constraints limit pure suppression. The measurement trajectory (0.55 → 0.65) reflects enforcement intensification as political pressure for restriction grows. Theater ratio (0.68): Substantial performance. Immigration enforcement signals intensity far exceeding actual capacity. Visa systems are porous (overstays exceed apprehensions). Asylum systems are backlogged (decisions take years). Border interdiction is selective. The apparatus persists through institutional inertia and political signaling, not through demonstrated effectiveness. The measurement trajectory (0.52 → 0.68) reflects growing performative character as enforcement machinery absorbs resources without proportional effectiveness gains.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the tangled_rope classification through irreconcilable perspectival gaps. Excluded migrants perceive snare — absolute entrapment without coordination benefit. Displaced workers perceive tangled_rope — mixed extraction and benefit. Organized labor perceives rope — coordination mechanism with legitimate scarcity value. The state perceives rope — genuine public goods coordination. Regional integration sees scaffold with sunset — temporary institutional stage. The enforcement apparatus sees piton — performative gatekeeping. The analytical observer risks mountain — naturalizing contingent institutional arrangements as immutable. The reading's core claim is that NO single perspective captures the full structure. All are partially true. The excluded migrant's snare is real extraction. The state's coordination problem is real. The displaced worker's ambivalent position is real. The constraint's legitimacy depends on whether the state can honor all three perspectives simultaneously — honoring migrant human rights, protecting citizen labor interests, and maintaining public goods provision. This is the actual structural tension the constraint encodes.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) and effective extractiveness (chi) flow from power level, exit options, and beneficiary/victim status. Excluded migrants (powerless/trapped): canonical d ≈ 1.0 (maximum target status); f(d) ≈ 1.42; chi = 0.58 × 1.42 × 1.0 ≈ 0.82 (snare territory). Displaced workers (moderate/constrained): derived d ≈ 0.65 (both victim of immigration pressure and beneficiary of wage protection); f(d) ≈ 1.00; chi = 0.58 × 1.00 × 1.0 ≈ 0.58 (tangled rope territory). Organized labor (organized/arbitrage): derived d ≈ 0.35 (beneficiary with exit/arbitrage options); f(d) ≈ 0.40; chi = 0.58 × 0.40 × 1.0 ≈ 0.23 (rope territory). State fiscal (institutional/constrained): derived d ≈ 0.50 (symmetric — benefits and costs balance in coordination); f(d) ≈ 0.65; chi = 0.58 × 0.65 × 1.0 ≈ 0.38 (tangled rope territory). The perspectival gap arises because the constraint's base extractiveness (0.58) is moderate — it can classify as snare, rope, or tangled rope depending entirely on the observer's structural position. This is NOT measurement ambiguity; this is the constraint's actual structure: it IS extraction for some agents and coordination for others, simultaneously.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint RESOLVES mandatrophy through the reading frame itself. The kernel contest (sovereignty_primary vs. jurisdictional_sovereignty vs. freedom_of_movement_primary) IS the mandatrophy resolution mechanism. Each reading produces a different classification distribution. The sovereignty_primary reading would classify border closure as rope (pure coordination, no extraction, because the reading acknowledges no binding constraints on state discretion). The freedom_of_movement_primary reading would classify it as snare (pure extraction, no coordination function, because the reading denies any legitimate state exclusion authority). THIS reading (jurisdictional_sovereignty) classifies it as tangled_rope (both coordination and extraction, bounded by proportionality and human rights constraints). The mandatrophy resolves by acknowledging that all three readings are coherent interpretations of the contested kernel — each internally consistent, each with defensible policy implications, and each producing a different empirical structure. The choice between them is not empirically resolvable; it is a choice about which normative commitments should bind the state (absolute state discretion vs. universal human rights vs. balanced coordination obligations). Stating the reading explicitly prevents the false appearance of empirical resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    public_consent_measurement_ambiguity,
    'What constitutes sufficient public consent for border restrictiveness: electoral majorities, opinion polls, or participatory deliberation that includes affected migrants?',
    'Comparative analysis of legitimacy outcomes across different consent-measurement regimes (representative democracy vs. direct democracy vs. regional governance forums); longitudinal tracking of which regimes show higher institutional stability and lower enforcement escalation',
    'If electoral consent alone suffices: constraint remains highly suppressive (snare from migrant perspective). If participatory inclusion required: constraint cannot be legitimate without migrant voice, forcing fundamental restructuring of enforcement architecture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_consent_measurement_ambiguity, conceptual, 'Definition and measurement of public consent in legitimacy calculus').

omega_variable(
    labor_market_empirical_status,
    'Does border closure actually protect domestic wage floors and employment, or does it enable employer monopsony power and labor informalization?',
    'Cross-national econometric analysis of wage and employment outcomes in high-restriction vs. high-openness labor markets, controlling for sector and skill level; identification of conditions under which restriction protects vs. harms domestic workers',
    'If protection effect is real: constraint legitimacy for displaced worker perspective is genuine (tangled rope). If protection is illusory: constraint appears as pure organized-labor rent extraction (snare from displaced worker perspective), eliminating coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(labor_market_empirical_status, empirical, 'Whether border closure protects domestic labor market welfare').

omega_variable(
    proportionality_threshold_for_enforcement,
    'What level of enforcement intensity (deportation rates, family separation, detention conditions) constitutes violation of proportionality principle vs. legitimate implementation of border control?',
    'International human rights law analysis (non-refoulement, right to family life, due process); comparative review of enforcement practices against proportionality standards; identification of enforcement escalation triggers and de-escalation mechanisms',
    'If threshold is clear and enforced: legitimacy is constrained by human rights floor, tangled rope classification holds. If threshold is ambiguous or unenforced: enforcement becomes snare (extraction without proportionality constraint), delegitimizing the entire framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_threshold_for_enforcement, conceptual, 'Proportionality standard for enforcement apparatus').

omega_variable(
    reading_boundary_ambiguity,
    'Does this reading (jurisdictional sovereignty without automatic border closure) remain distinct from the freedom_of_movement reading, or does acknowledgment of dual victim sets and proportionality constraints collapse the distinction?',
    'Comparative analysis of the three sibling readings'' policy implications: sovereighty_primary permits absolute exclusion; jurisdictional_sovereignty permits graduated restriction bounded by proportionality; freedom_of_movement_primary denies border closure legitimacy entirely. Empirical test: can states that adopt this reading''s framework implement enforceable restrictions while respecting proportionality, or does proportionality in practice foreclose meaningful exclusion?',
    'If readings remain structurally distinct: each has independent policy coherence. If practice collapses the distinction: this reading becomes theoretically intermediate but empirically untenable, forcing policy adoption of either sovereighty_primary or freedom_of_movement_primary extremes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_ambiguity, conceptual, 'Whether this reading remains distinct from sibling readings in practice').

omega_variable(
    supranational_framework_inevitability,
    'Is the scaffold perspective''s prediction of generational transition to supranational labor mobility frameworks empirically grounded, or does it represent aspirational reading of regional integration progress?',
    'Tracking of regional integration scope and enforcement: EU internal mobility is high but Brexit shows volatility; ASEAN has not achieved labor mobility parity; African Union frameworks remain weak. Comparison of actual vs. predicted supranational enforcement over 20-year historical window.',
    'If transition is inevitable: scaffold sunset is real institutional mechanism, and this reading has genuine time-limited scope. If transition is contingent/reversible: constraint''s temporal dynamics are far more uncertain, and legitimacy crisis may be permanent rather than staged.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(supranational_framework_inevitability, empirical, 'Inevitability of supranational labor mobility transition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__jurisdictional_sovereignty, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bcl_jsd_tr_t0, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 0, 0.52).
narrative_ontology:measurement(bcl_jsd_tr_t10, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 10, 0.62).
narrative_ontology:measurement(bcl_jsd_tr_t20, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(bcl_jsd_be_t0, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(bcl_jsd_be_t10, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(bcl_jsd_be_t20, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(bcl_jsd_su_t0, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(bcl_jsd_su_t10, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 10, 0.61).
narrative_ontology:measurement(bcl_jsd_su_t20, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__jurisdictional_sovereignty, resource_allocation).
narrative_ontology:affects_constraint(border_control_legitimacy__jurisdictional_sovereignty, labor_market_gatekeeping).
narrative_ontology:affects_constraint(border_control_legitimacy__jurisdictional_sovereignty, fiscal_welfare_state_sustainability).
narrative_ontology:affects_constraint(border_control_legitimacy__jurisdictional_sovereignty, transnational_human_rights_enforcement).

% DUAL FORMULATION NOTE:
% This constraint is ONE reading of the border_control_legitimacy kernel. Sibling constraints (sovereignty_primary and freedom_of_movement_primary) represent alternative readings of the same contested institutional commitment. Each reading produces a different classification structure because each defines the boundaries of legitimate state authority differently. The sibling constraints have different ε values reflecting different empirical assumptions about what coordination functions exist and whether enforcement apparatus serves them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
