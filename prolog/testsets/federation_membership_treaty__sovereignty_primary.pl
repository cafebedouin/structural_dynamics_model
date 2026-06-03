% ============================================================================
% CONSTRAINT STORY: federation_membership_treaty__sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_treaty__sovereignty_primary, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: federation_membership_treaty__sovereignty_primary
 *   human_readable: Free Movement Conditional on State Consent (Sovereignty-Primary Reading)
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint story models the sovereignty-primary reading of the
 *   federation_membership_treaty kernel — the political commitment that free
 *   movement of labor is conditional on member state consent, and that states
 *   retain authority to protect national labor markets and welfare systems
 *   from cross-border competition. This reading grounds legitimacy in the
 *   premise that democratic political communities require control over labor
 *   supply and welfare access to maintain social cohesion, prevent wage
 *   suppression, and prevent welfare-system collapse. The constraint exhibits
 *   Tangled Rope structure: it coordinates labor market policy across member
 *   states (genuine coordination benefit) while simultaneously extracting
 *   from mobile workers through restricted access to employment and
 *   settlement (asymmetric extraction). The extractiveness trajectory (0.42 →
 *   0.58 over 30 years) reflects increasing pressure from economic
 *   integration despite formal state-consent sovereignty. The suppression
 *   trajectory (0.55 → 0.65) reflects rising enforcement intensity: more
 *   elaborate credentialing requirements, tighter welfare eligibility,
 *   stronger workplace inspection for undocumented workers. The theater ratio
 *   (0.35 → 0.48) reflects that formal state sovereignty over borders is
 *   partially performative — actual enforcement has shifted toward
 *   supranational institutions while the sovereignty-primary reading
 *   maintains that control remains national.
 *
 * KEY AGENTS:
 *   - National labor market constituencies: Primary beneficiary (institutional/arbitrage) — protected from cross-border wage competition and employment displacement
 *   - Welfare system administrators: Secondary beneficiary (institutional/constrained) — maintain fiscal control over redistribution through mobility gatekeeping
 *   - Economically mobile workers: Primary victim (powerless/trapped) — access restricted through work permits, credentialing, settlement requirements; cannot exit the federation without abandoning career
 *   - Cross-border talent flows: Secondary victim (moderate/constrained) — some mobility permitted (skilled workers, intra-corporate) but rationed through preferential visas and sector exemptions
 *   - Supranational integration coalition: Organized observer (organized/mobile) — sees sovereignty-primary reading as temporary; building pressure toward integration_primary through jurisprudence and advocacy
 *   - Treaty framework jurisprudence: Institutional observer (institutional/arbitrage) — maintains performative commitment to state consent while substantively interpreting mobility rights broadly (Piton perspective)
 *   - Analytical observer: Civilizational analyst (analytical/analytical) — risks naturalizing contingent political choice (state sovereignty over labor) as immutable natural law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__sovereignty_primary, 0.58).
domain_priors:suppression_score(federation_membership_treaty__sovereignty_primary, 0.65).
domain_priors:theater_ratio(federation_membership_treaty__sovereignty_primary, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, extractiveness, 0.58).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__sovereignty_primary, "Free Movement Conditional on State Consent (Sovereignty-Primary Reading)").
narrative_ontology:topic_domain(federation_membership_treaty__sovereignty_primary, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_treaty__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__sovereignty_primary, '7af7f9c9-f72a-47bd-b78c-a37bbf5b20b1').
narrative_ontology:cs_kernel_codification('7af7f9c9-f72a-47bd-b78c-a37bbf5b20b1', formalized).
narrative_ontology:cs_authority_grounding('7af7f9c9-f72a-47bd-b78c-a37bbf5b20b1', extraction).
narrative_ontology:cs_interpretation_layer_present('7af7f9c9-f72a-47bd-b78c-a37bbf5b20b1').
narrative_ontology:cs_reading_relation('7af7f9c9-f72a-47bd-b78c-a37bbf5b20b1', federation_membership_treaty__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('7af7f9c9-f72a-47bd-b78c-a37bbf5b20b1', federation_membership_treaty__subsidiarity_balance, influences).
narrative_ontology:cs_axiom('7af7f9c9-f72a-47bd-b78c-a37bbf5b20b1', foundational, state_labor_market_authority_primary).
narrative_ontology:cs_axiom_status(state_labor_market_authority_primary, holdable).
narrative_ontology:cs_axiom_grounding('7af7f9c9-f72a-47bd-b78c-a37bbf5b20b1', state_labor_market_authority_primary, deontological).
narrative_ontology:cs_axiom('7af7f9c9-f72a-47bd-b78c-a37bbf5b20b1', foundational, welfare_system_fiscal_sustainability_requires_border_gatekeeping).
narrative_ontology:cs_axiom_status(welfare_system_fiscal_sustainability_requires_border_gatekeeping, holdable).
narrative_ontology:cs_axiom_grounding('7af7f9c9-f72a-47bd-b78c-a37bbf5b20b1', welfare_system_fiscal_sustainability_requires_border_gatekeeping, empirically_contingent).
narrative_ontology:cs_reference_frame('7af7f9c9-f72a-47bd-b78c-a37bbf5b20b1', sovereign_state_labor_market_authority).
narrative_ontology:cs_drift_state('7af7f9c9-f72a-47bd-b78c-a37bbf5b20b1', contemporary_supranational_integration, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7af7f9c9-f72a-47bd-b78c-a37bbf5b20b1', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(federation_membership_treaty__sovereignty_primary, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, national_labor_market_constituencies).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, welfare_system_administrators).
narrative_ontology:constraint_victim(federation_membership_treaty__sovereignty_primary, economically_mobile_workers).
narrative_ontology:constraint_victim(federation_membership_treaty__sovereignty_primary, cross_border_talent_flows).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MOBILE WORKER (SNARE) — Structurally trapped by residency requirements, work permits, and credentialing barriers that preserve national control. Cannot exit the federation without abandoning livelihood. Suppression is high: legal frameworks restrict settlement and employment. Effective extraction flows toward national labor market protection. No coordination benefit from the worker's perspective — pure extraction of mobility premium.
constraint_indexing:constraint_classification(federation_membership_treaty__sovereignty_primary, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: CROSS-BORDER TALENT FLOWS (TANGLED ROPE) — Experiences genuine coordination benefit (some mobility is permitted through exemptions, special sectors, skilled-worker visas) alongside asymmetric extraction (mobility is rationed and controlled). Neither fully trapped nor fully free. Constrained exit means significant career costs to attempting unauthorized relocation or prolonged visa disputes. Real coordination for some flows (EU intra-corporate transfers, healthcare workers in shortage) paired with real extraction for others (care workers, cleaners).
constraint_indexing:constraint_classification(federation_membership_treaty__sovereignty_primary, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: NATIONAL LABOR MARKET PROTECTION AUTHORITY (ROPE) — Primary beneficiary. Experiences the constraint as pure coordination: regulating labor market access solves the collective action problem of protecting local wages and employment. Treaty framework gives institutional authority arbitrage over mobility access. Net flow of benefits toward this agent. Classification as Rope reflects that the constraint genuinely coordinates labor market policy for multiple member states simultaneously — all states benefit from mutual recognition that border protection is legitimate.
constraint_indexing:constraint_classification(federation_membership_treaty__sovereignty_primary, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: WELFARE SYSTEM ADMINISTRATORS (TANGLED ROPE) — Mixed experience. Coordination benefit: state consent to free movement allows social risk pooling and labor reallocation without welfare-system collapse. Extraction: mobile workers accessing welfare create fiscal pressure and justification for harder restrictions. Constrained exit because welfare administrators cannot unilaterally enforce borders without federal cooperation. Real coordination function (maintaining welfare sustainability) paired with real extraction (rationing access to benefits).
constraint_indexing:constraint_classification(federation_membership_treaty__sovereignty_primary, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: SUPRANATIONAL INTEGRATION COALITION (SCAFFOLD) — Sees the sovereignty-primary reading as temporary, destined for erosion. Integration coalition (European Commission, pro-mobility advocacy groups, multinational employers) views national consent as a sunset clause: as economic integration deepens, member states will find unilateral labor restrictions increasingly costly and coordination benefits of open movement increasingly apparent. Mobile exit option reflects that the coalition can shift governance frames and build pressure toward integration_primary reading.
constraint_indexing:constraint_classification(federation_membership_treaty__sovereignty_primary, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: TREATY FRAMEWORK JURISPRUDENCE (PITON) — The formal treaty text codifies state consent, but actual jurisprudence has drifted substantially toward integration_primary reading. Courts interpret free movement broadly despite state-consent language. The sovereignty-primary reading persists largely through institutional inertia in legislative text, while judicial practice has substantially eroded it. Theater ratio is moderate-to-low (0.48) because some genuine enforcement occurs via state-level welfare gatekeeping, but much of the sovereignty preservation is performative — states retain formal authority they cannot effectively exercise.
constraint_indexing:constraint_classification(federation_membership_treaty__sovereignty_primary, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / IMMUTABLE SOVEREIGNTY VIEW (MOUNTAIN) — From civilizational scale, state sovereignty over labor markets appears as an irreducible natural law of federalism: the survival requirement for distinct political communities depends on maintaining some control over resource distribution and cultural reproduction. This perspective sees free movement restrictions as inevitable and unchangeable. However, the structural data contradicts this — the extractiveness (0.58) and beneficiary structure (national labor constituencies, not the population as a whole) reveal that the 'natural law' framing naturalizes a particular interest group's extraction.
constraint_indexing:constraint_classification(federation_membership_treaty__sovereignty_primary, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_treaty__sovereignty_primary_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(federation_membership_treaty__sovereignty_primary, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(federation_membership_treaty__sovereignty_primary, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_treaty__sovereignty_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(federation_membership_treaty__sovereignty_primary, TR),
    TR >= 0.70.

:- end_tests(federation_membership_treaty__sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The sovereignty-primary reading creates real extraction mechanisms: work permits, credentialing barriers, welfare eligibility requirements, and settlement restrictions impose significant costs on mobile workers. However, extractiveness is not maximal (not ≥0.66) because some mobility is genuinely permitted — skilled worker exemptions, intra-corporate transfers, healthcare worker programs, and free movement for EU citizens (formal status contradicting sovereignty-primary claim, but empirically relevant). The rising trajectory reflects that enforcement infrastructure has been strengthened (digital work permits, inter-state data sharing, employer verification) even as the formal claim is that states retain consent-based authority. Suppression (0.65): High and rising. Legal barriers (work permits, credentialing reciprocity requirements, welfare eligibility), economic barriers (differential wage expectations creating self-selection away from entry), and social barriers (discrimination, language requirements, professional gatekeeping) all suppress mobility alternatives. The rising trajectory reflects that legal framework elaboration has made suppression more systematic — bureaucratic infrastructure has replaced pure capacity constraints. Theater ratio (0.48): Moderate. The sovereignty-primary reading maintains formal commitment to state consent authority, but actual enforcement is increasingly delegated to supranational institutions (European Court, regulatory agencies, data networks). The theater reflects that states talk about protecting labor markets while supranational actors substantially determine mobility policy. The rising theater trajectory indicates that formal state authority claims are becoming increasingly divorced from enforcement reality.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a dramatic perspectival gap across observational positions. The beneficiary (national labor market authority) sees pure coordination (Rope) — regulating labor supply solves a collective action problem. The organized coalition (supranational integrationists) sees a temporary constraint with a sunset (Scaffold) — integration_primary reading is the destination. The administrative apparatus (welfare systems, border agencies) sees mixed coordination-extraction (Tangled Rope) — real coordination problems exist alongside real extraction. The victim (mobile workers) sees pure extraction (Snare) — trapped by restrictions with no benefit. Jurisprudence sees degraded authority (Piton) — formally sovereign but substantively eroded by integration. The civilizational analytical observer risks seeing natural law (Mountain) — state control over labor supply as inevitable. The gap reveals that the 'natural law' framing is a false summit: it naturalizes the sovereignty-primary reading's particular beneficiary structure, not a universal truth about federalism.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by each agent's structural relationship to the constraint. National labor market constituencies benefit (arbitrage exit options, institutional power) → d ≈ 0.1 → low effective extraction toward them. Mobile workers are victimized (trapped exit, powerless status) → d ≈ 0.95 → high effective extraction from them. Cross-border talent flows occupy middle position: some coordination benefit (permitted mobility for skilled workers) paired with real extraction (mobility rationing) → d ≈ 0.65. Supranational coalition sees the constraint as temporary and is building exit pathways (mobile exit options, organized power) → d ≈ 0.45. The perspectival gap is large because the constraint's structure produces radically different experienced extractiveness depending on position: beneficiaries see rope (coordination), victims see snare (extraction), organized actors see temporary constraint (scaffold), jurisprudence sees degraded principle (piton), analytical observer risks naturalizing (mountain). The chi formula χ = ε × f(d) × σ(S) scales extractiveness by directionality sigmoid and scope modifier: at continental scope σ=1.1, and with mobile workers' d≈0.95, f(d)≈1.42, the effective extraction is χ ≈ 0.58 × 1.42 × 1.1 ≈ 0.91, placing this constraint at the snare boundary for the primary victim.
 *
 * MANDATROPHY ANALYSIS:
 *   This story resolves the mandatrophy for the sovereignty-primary reading by showing that Tangled Rope classification is appropriate: genuine coordination function exists (states collectively solve labor-market stability problem) AND genuine asymmetric extraction exists (mobile workers bear costs). The mandatrophy would arise if we tried to classify this as pure Rope (only coordination, no extraction) or pure Snare (only extraction, no coordination). The classification is Tangled Rope precisely because both components are real. The perspectival gap — ranging from Rope (beneficiary view) to Snare (victim view) to Piton (jurisprudence view) to Mountain (analytical risk) — documents that the same structural mechanism produces radically different experienced constraints depending on position. The false summit risk (mountain perspective) is real: the claim that 'state control over labor supply is natural law' would be attractive to beneficiaries and risks anchoring analysis. But the structural data (explicit beneficiary set, measurable suppression mechanisms, institutional enforcement) reveals this as a contingent political arrangement, not a natural limit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    welfare_fiscal_burden_causation,
    'Do mobile workers actually impose net fiscal costs on welfare systems, or does this claim rationalize extraction that would occur regardless?',
    'Longitudinal fiscal accounting: comparing welfare expenditure for mobile vs native workers; controlling for selection effects (do higher-income workers migrate, reducing net costs?); accounting for tax contributions',
    'If mobile workers impose real net costs: suppression is justified coordination cost, constraint reclassifies toward Rope. If costs are selection artefacts or offset by tax contributions: suppression is pure extraction, constraint reclassifies toward Snare at institutional level.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(welfare_fiscal_burden_causation, empirical, 'Whether mobile workers create net fiscal burden on welfare systems').

omega_variable(
    labor_market_wage_displacement_mechanism,
    'Do restrictions on cross-border labor flows prevent wage suppression, or do they preserve rents for incumbent workers while extracting from mobile workers?',
    'Comparative wage analysis: sectors with high mobility restrictions vs low restrictions; decomposition of wage effects into skill-biased demand vs labor supply; counterfactual wage paths under alternative mobility regimes',
    'If restrictions prevent genuine wage competition: constraint is coordination (Rope for national labor market actors). If they preserve rents: constraint is extraction (Snare for mobile workers, Tangled Rope for system as a whole).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_market_wage_displacement_mechanism, empirical, 'Wage displacement vs rent-preservation mechanism').

omega_variable(
    reading_foreclosure_integration_primary,
    'Does the sovereignty-primary reading logically foreclose the integration-primary reading within a single federation framework, or do they coexist as competing legitimacy claims?',
    'Constitutional/treaty interpretation analysis: Can both readings be held simultaneously by different member states within one federation? If courts/legislatures can embrace integration_primary while member states claim sovereignty_primary authority, readings coexist. If one must eliminate the other for coherence, foreclosure relation applies.',
    'If forecloses: only one reading can survive; the kernel contest is resolvable in principle. If coexists_with: both readings structure EU politics indefinitely as unresolvable tension. Affects the terminal attractor of federation_membership_treaty kernel evolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_integration_primary, conceptual, 'Whether sovereignty_primary forecloses integration_primary reading').

omega_variable(
    state_consent_formal_vs_functional,
    'Is state consent a meaningful legal/political category, or has supranational institutional power eroded it to performative status?',
    'Case analysis: instances where member states actually refused consent and enforcement held vs instances where formal consent was given but compliance was minimal. Measurement of enforcement variance across states.',
    'If consent is functional: sovereignty-primary reading accurately captures constraint structure. If consent is performative: constraint is more accurately classified as Piton (formally coded as sovereignty but functionally integration-primary). Theater_ratio should be higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_consent_formal_vs_functional, empirical, 'Functional authority of state consent to restrict mobility').

omega_variable(
    subsidiarity_balance_reading_relation,
    'Is the subsidiarity_balance reading a distinct alternative, or does it represent a midpoint between sovereignty_primary and integration_primary?',
    'Doctrinal analysis: Does subsidiarity_balance have independent axioms that are not reducible to scaled versions of sovereignty vs integration claims? Or is it inherently unstable, collapsing toward one pole under enforcement pressure?',
    'If distinct: subsidiarity_balance is a legitimate sibling reading with influences relations to both sovereignty_primary and integration_primary. If unstable: it functions as a ladder that integrationists climb to reach their goal, meaning sovereignty_primary doesn''t truly influence subsidiarity_balance — they don''t coexist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsidiarity_balance_reading_relation, conceptual, 'Whether subsidiarity_balance is structurally independent or unstable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__sovereignty_primary, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fms_tr_t0, federation_membership_treaty__sovereignty_primary, theater_ratio, 0, 0.35).
narrative_ontology:measurement(fms_tr_t15, federation_membership_treaty__sovereignty_primary, theater_ratio, 15, 0.42).
narrative_ontology:measurement(fms_tr_t30, federation_membership_treaty__sovereignty_primary, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(fms_be_t0, federation_membership_treaty__sovereignty_primary, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(fms_be_t15, federation_membership_treaty__sovereignty_primary, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(fms_be_t30, federation_membership_treaty__sovereignty_primary, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fms_su_t0, federation_membership_treaty__sovereignty_primary, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(fms_su_t15, federation_membership_treaty__sovereignty_primary, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(fms_su_t30, federation_membership_treaty__sovereignty_primary, suppression_requirement, 30, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__sovereignty_primary, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_treaty__sovereignty_primary, 0.18).
narrative_ontology:affects_constraint(federation_membership_treaty__sovereignty_primary, federation_membership_treaty__integration_primary).
narrative_ontology:affects_constraint(federation_membership_treaty__sovereignty_primary, federation_membership_treaty__subsidiarity_balance).
narrative_ontology:affects_constraint(federation_membership_treaty__sovereignty_primary, wage_competition_suppression_mechanism).
narrative_ontology:affects_constraint(federation_membership_treaty__sovereignty_primary, welfare_system_fiscal_sustainability).

% DUAL FORMULATION NOTE:
% The federation_membership_treaty kernel decomposes into three structurally distinct constraints corresponding to three readings: sovereignty_primary (this story), integration_primary (free movement constitutive), and subsidiarity_balance (proportionality-bounded). Each reading has its own ε value reflecting different structural emphases: sovereignty_primary emphasizes state authority and national interest protection (ε=0.58, Tangled Rope); integration_primary emphasizes individual mobility rights and supranational enforcement (ε=0.35, estimated Rope); subsidiarity_balance emphasizes proportionality constraints (ε=0.45, estimated Tangled Rope). The three constraints are linked via network.affects_constraints because they are readings of the same kernel. A change in which reading dominates EU jurisprudence would shift which constraint's classification applies to the federation as a whole. This is not observable-dependent classification of a single constraint — it is genuine structural decomposition of a contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_treaty__sovereignty_primary, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
