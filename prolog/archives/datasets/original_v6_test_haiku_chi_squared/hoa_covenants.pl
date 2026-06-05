% ============================================================================
% CONSTRAINT STORY: hoa_covenants
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hoa_covenants, []).

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
 *   constraint_id: hoa_covenants
 *   human_readable: HOA Architectural Review and Restrictive Covenants
 *   domain: economic/social/legal
 *
 * SUMMARY:
 *   Homeowners' association restrictive covenants create a dual-layer
 *   constraint: the legal framework of recorded deed restrictions combined
 *   with the institutional enforcement mechanism of the HOA board. This
 *   constraint exhibits a perspectival spread across all six DR types,
 *   revealing how the same legal structure can function as coordination (from
 *   the board's view), extraction (from the homeowner-innovator's view),
 *   degraded theater (from the long-term institutional view), or a temporary
 *   problem with sunset solutions (from the reform movement's view). The
 *   constraint has shifted over the measurement interval from primarily
 *   coordination-focused (ε=0.30, theater=0.35) toward increasingly
 *   extractive and performative (ε=0.52, theater=0.65), driven by aesthetic
 *   intensification and selective enforcement patterns that diverge from the
 *   original property-value-preservation rationale. The suppression component
 *   (0.68) is structural: homeowners face legal barriers (covenant language),
 *   procedural barriers (amendment supermajorities), financial barriers
 *   (property sale depreciation), and social barriers (neighborhood
 *   reputation). The theater ratio rise indicates that HOA enforcement has
 *   increasingly become about symbolic compliance rather than functional
 *   property maintenance, with selective enforcement creating the appearance
 *   of uniform governance while extracting disproportionately from disfavored
 *   or innovative homeowners.
 *
 * KEY AGENTS:
 *   - Property Owner Seeking Modification: Primary victim (powerless/trapped) — bears full extraction from inability to alter property according to preference; lacks legal or organizational exit
 *   - Compliant Homeowner: Secondary victim (moderate/constrained) — complies with covenants but absorbs opportunity costs; benefits from value preservation but constrained in choices
 *   - HOA Board: Primary beneficiary (institutional/arbitrage) — exercises enforcement discretion; benefits from predictable neighborhood maintenance and implicit political power; can alter interpretation or selectively enforce
 *   - Property Value Preservationists: Secondary beneficiary (moderate/mobile) — benefit from stable neighborhood aesthetics and property values; have some exit via market switching but prefer stability
 *   - Real Estate Development Industry: Complex actor (powerful/mobile) — shapes covenant design during development; extracts through style-preference bias; has exit options (new markets, new styles)
 *   - Property Rights Reform Movement: Organized challenger (organized/mobile) — advocates for covenant modification, sunset clauses, and homeowner opt-outs; has agency through legal reform; builds alternative pathways
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the constraint as necessary property-rights coordination while missing selective enforcement and lock-in mechanisms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenants, 0.52).
domain_priors:suppression_score(hoa_covenants, 0.68).
domain_priors:theater_ratio(hoa_covenants, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenants, extractiveness, 0.52).
narrative_ontology:constraint_metric(hoa_covenants, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(hoa_covenants, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenants, tangled_rope).
narrative_ontology:human_readable(hoa_covenants, "HOA Architectural Review and Restrictive Covenants").
narrative_ontology:topic_domain(hoa_covenants, "economic/social/legal").

domain_priors:requires_active_enforcement(hoa_covenants).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenants, hoa_board_members).
narrative_ontology:constraint_beneficiary(hoa_covenants, property_value_preservationists).
narrative_ontology:constraint_victim(hoa_covenants, homeowner_innovators).
narrative_ontology:constraint_victim(hoa_covenants, property_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROPERTY OWNER SEEKING MODIFICATION (SNARE) — Trapped by covenants recorded in deed; cannot exit without selling property at depressed price or enduring lien enforcement. Bears full suppression from HOA review process and legal penalties. d≈0.92, f(d)≈1.40, σ=0.8 → χ≈0.58.
constraint_indexing:constraint_classification(hoa_covenants, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: COMPLIANT HOMEOWNER (TANGLED ROPE) — Constrained by covenant enforcement and social pressure, but also benefits from property value preservation and neighborhood aesthetic coordination. Experiences both the coordination function (stable neighborhood appearance) and asymmetric extraction (restrictions on their own preferences). d≈0.65, f(d)≈0.95, σ=0.8 → χ≈0.40.
constraint_indexing:constraint_classification(hoa_covenants, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: HOA BOARD (ROPE) — Institutional beneficiary with exit via board rotation and rule interpretation. Experiences constraint as coordination mechanism: covenants solve collective action problem of maintaining neighborhood standards and marketability. d≈0.08, f(d)≈-0.10, σ=0.8 → χ≈-0.05. Net beneficiary.
constraint_indexing:constraint_classification(hoa_covenants, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: RECORDED COVENANT SYSTEM (PITON) — Performative legal structure maintained by institutional inertia. Theater_ratio=0.65 reflects that much covenant enforcement is about symbolic property-value maintenance rather than functional neighborhood coordination. Many HOAs rarely enforce covenants uniformly; enforcement clusters around boundary-testing cases. d≈0.10, f(d)≈-0.08, σ=0.9 → χ≈-0.05.
constraint_indexing:constraint_classification(hoa_covenants, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: PROPERTY RIGHTS REFORM MOVEMENT (SCAFFOLD) — Organized advocates (legal reform groups, homeowner associations challenging broad covenants) see this as a temporary coordination problem with sunset logic. Some jurisdictions are implementing covenant reform, allowing homeowner opt-outs after 20-30 years, or limiting aesthetic enforcement. d≈0.45, f(d)≈0.50, σ=0.9 → χ≈0.24. Low extraction because the movement has agency and documented exit pathways.
constraint_indexing:constraint_classification(hoa_covenants, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: REAL ESTATE DEVELOPMENT INDUSTRY (TANGLED ROPE) — Powerful actors (developers, builders) both shape covenant design and experience constraints when they innovate. Benefits from covenants (predictability, marketability) but also extracts through selective enforcement and covenant design that favors certain architectural styles. d≈0.35, f(d)≈0.35, σ=0.9 → χ≈0.18. Mixed coordination-extraction; industry has substantial exit options (sell properties, move markets).
constraint_indexing:constraint_classification(hoa_covenants, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE / FALSE SUMMIT RISK) — From civilizational view, the constraint combines elements of natural law (property rights require some coordination mechanism) with extractive institutional design (selective enforcement, supermajority amendment rules). The engine's computation reveals this is not a mountain despite naturalizing rhetoric. d≈0.70, f(d)≈1.08, σ=1.0 → χ≈0.56. The snare classification indicates the analytically decisive fact: exit suppression (no way to modify covenants without 80%+ supermajority) is the structural core, making this extraction not coordination.
constraint_indexing:constraint_classification(hoa_covenants, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hoa_covenants_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hoa_covenants, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hoa_covenants, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hoa_covenants, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hoa_covenants, TR),
    TR >= 0.70.

:- end_tests(hoa_covenants_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. HOAs extract through three mechanisms: (1) restriction on property modification prevents value-enhancing customization and creates deadweight loss; (2) selective enforcement creates uncertainty and targeting risk; (3) supermajority amendment rules (typically 80%+) lock in outdated aesthetics, preventing communities from relaxing restrictions even when members desire change. The value is not as high as pure snares (0.70+) because some coordination function persists — covenants do prevent truly detrimental modifications that would harm neighborhood value. The increase from ε=0.30 to ε=0.52 over the interval reflects growing divergence between original value-preservation rationale and contemporary selective enforcement. Suppression (0.68): High. Suppression operates through legal recording (permanent unless supermajority amends), procedural supermajority rules (prevents escape through normal democratic process), financial penalty (property depreciation for non-compliance), and reputational risk (neighborhood ostracism for boundary-testing). Theater ratio (0.65): Moderate-high. Much HOA enforcement is performative: architectural review committees do not conduct structural analysis but assess aesthetic alignment with vague community standards. Enforcement is inconsistent — violations are overlooked for politically-favored residents, emphasized for disfavored ones. The theater has risen over the interval as aesthetic restrictions have proliferated and enforcement has become increasingly about signaling compliance rather than maintaining actual property conditions. Original covenants (ε=0.30, theater=0.35) focused on functional issues (fence height, building setbacks, material durability). Contemporary covenants (ε=0.52, theater=0.65) emphasize style homogeneity (color palette, roof style, landscape aesthetics) with symbolic enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals three distinct constraint experiences: (1) Property owners seeking modification perceive pure extraction (Snare) — they are trapped, suppressed, and receive no coordination benefit because the restrictions prevent their preferred property use. (2) Compliant homeowners perceive hybrid coordination-extraction (Tangled Rope) — they benefit from neighborhood stability and value preservation but are constrained in their own choices and absorb opportunity costs. (3) HOA boards perceive pure coordination (Rope) — they solve a real collective-action problem (preventing incompatible modifications), experience the constraint as enabling their governance function, and have exit via board rotation or rule reinterpretation. The gap widens at longer time horizons: the reform movement sees a temporary coordination problem with a sunset solution (Scaffold), while the institutionalized covenant system sees itself as degraded but inescapable (Piton). The real estate development industry straddles the gap: they benefit from covenant predictability while extracting through style-preference design. The largest perspectival gap emerges from comparing the property owner (trapped/powerless at d≈0.92) with the HOA board (institutional/arbitrage at d≈0.08) — the same legal structure produces opposite valuations depending on structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Property owner seeking modification: Victim + trapped exit → d≈0.92. Structural reality: cannot modify property without legal violation (lien/lawsuit), cannot exit without 20-30% property loss, cannot amend covenant without 80%+ support from neighbors (politically impossible). Near-maximal extraction directionality. Compliant homeowner: Victim + constrained exit → d≈0.65. Mixed experience: constrained in preferred modifications but benefits from value preservation and neighborhood coordination. Can exit through sale but at moderate cost (5-15% depreciation). Moderate extraction. HOA board: Beneficiary + arbitrage exit → d≈0.08. Board members can exit via non-reelection, can reinterpret covenant rules, can selectively enforce. Covenants enable their governance function. Negative extraction (net benefit). Real estate developer: Mixed beneficiary-victim with mobile exit → d≈0.35. Benefits from covenant design (architectural standardization, market differentiation), but constrained when innovation diverges from established style preferences. Mobile exit (can develop in different markets or push envelope in specific projects). Low extraction. Reform advocate: Organized + mobile exit → d≈0.45. Organized opposition to overly restrictive covenants; has agency through legal reform and policy advocacy. Can exit via supporting reform in favorable jurisdictions. Low extraction because agency is real.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that this constraint operates simultaneously in two distinct modes depending on temporal and structural scope: (1) SHORT TERM / LOCAL (biographical horizon, local scope): Tangled Rope or Snare. Covenants serve genuine coordination function (aesthetic consistency, property value maintenance) but generate asymmetric extraction through selective enforcement and amendment lock-in. The 'rope' component is real — unconstrained heterogeneous property modifications would create negative externalities and reduce neighborhood marketability. The 'snare' component is also real — supermajority rules and deed-recorded permanence prevent easy amendment even when the original purpose becomes outdated. (2) LONG TERM / INSTITUTIONAL (generational-civilizational horizon): Piton with Scaffold alternatives. Original coordination function (value preservation) has atrophied as covenants become ritualized and selectively enforced. Theater rises because enforcement diverges from stated purpose. Scaffold alternative emerges as reform movements implement covenant sunset clauses, homeowner opt-outs, and relaxation mechanisms that preserve value-preservation function while eliminating extractive lock-in. The trajectory shows movement from Rope (early) → Tangled Rope (middle, current) → Piton with Scaffold pathway (late). The mandatrophy is not 'which type is correct' but 'how does the constraint's functional character evolve?' The answer: from genuine coordination (rope) toward extractive institutional inertia (piton) unless reformed to sunset/opt-out (scaffold).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    aesthetic_vs_functional_boundary,
    'Where lies the boundary between legitimate neighborhood coordination (functional) and extractive aesthetic control (extraction)?',
    'Comparative analysis of covenant enforcement: pure aesthetic restrictions (color, style) vs structural/safety restrictions (setbacks, materials). Property value impact studies controlling for functional vs aesthetic restrictions.',
    'If aesthetic restrictions alone have minimal value impact: suppression reflects pure extraction (snare). If aesthetic value is measurable: coordination function is real (rope/tangled_rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aesthetic_vs_functional_boundary, empirical, 'Boundary between aesthetic coordination and extractive control').

omega_variable(
    supermajority_amendment_lock_in,
    'Do 80%+ supermajority amendment requirements represent legitimate consensus-based governance or extractive lock-in by entrenched board factions?',
    'Historical data on amendment attempts and outcomes. Comparison of amendment success rates across HOAs with varying thresholds. Analysis of coalitional blocking patterns.',
    'If supermajority amendments succeed at >40%: governance is responsive (scaffold/rope). If <10%: covenants are locked in indefinitely (snare/piton).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(supermajority_amendment_lock_in, empirical, 'Whether supermajority amendment rules lock in covenants or enable governance').

omega_variable(
    selective_enforcement_equity,
    'Does HOA enforcement follow uniform rules or does selective enforcement extract from disfavored neighbors while exempting board allies?',
    'Audit studies comparing enforcement patterns for equivalent violations across socioeconomic, racial, and political demographics of homeowners. Cross-neighborhood comparison of enforcement consistency.',
    'If uniform: suppression is structural but fair (tangled_rope for all). If selective: extraction is masked by legal cover (snare for disfavored groups, rope for insiders).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_enforcement_equity, empirical, 'Whether HOA enforcement is uniform or selective across homeowner groups').

omega_variable(
    property_value_real_vs_stated,
    'Do restrictive covenants actually preserve property values, or do they prevent value-enhancing modifications and extract wealth from innovative homeowners?',
    'Hedonic regression modeling property values in restricted vs unrestricted neighborhoods. Analysis of property appreciation rates before and after covenant relaxation. Case studies of high-restriction vs low-restriction comparable communities.',
    'If covenants preserve 5%+ value: coordination function is real and justified (rope). If value preservation is <2% or negative: covenants extract without benefit (snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(property_value_real_vs_stated, empirical, 'Whether restrictive covenants preserve or reduce property values').

omega_variable(
    exit_option_feasibility,
    'Can trapped homeowners realistically exit (sell property) without severe loss, or does covenant burden depreciate property beyond market recovery?',
    'Comparison of sales prices and time-on-market for equivalent homes in restricted vs unrestricted communities. Exit cost analysis: depreciation discount + transaction costs vs remaining covenant term.',
    'If exit cost <10% of property value: exit is constrained but not trapped (tangled_rope). If >30%: exit is trapped (snare). Affects directionality for all victim perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_option_feasibility, empirical, 'Feasibility of homeowner exit via property sale').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenants, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa_tr_t0, hoa_covenants, theater_ratio, 0, 0.35).
narrative_ontology:measurement(hoa_tr_t15, hoa_covenants, theater_ratio, 15, 0.55).
narrative_ontology:measurement(hoa_tr_t30, hoa_covenants, theater_ratio, 30, 0.65).

% Extraction over time
narrative_ontology:measurement(hoa_be_t0, hoa_covenants, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(hoa_be_t15, hoa_covenants, base_extractiveness, 15, 0.42).
narrative_ontology:measurement(hoa_be_t30, hoa_covenants, base_extractiveness, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hoa_covenants, resource_allocation).
narrative_ontology:affects_constraint(hoa_covenants, property_tax_lock_in).
narrative_ontology:affects_constraint(hoa_covenants, neighborhood_segregation_by_price).

% DUAL FORMULATION NOTE:
% HOA covenants decompose into two structurally distinct constraints: (1) covenant_value_preservation (ε≈0.25, Mountain claim / disputed): the empirical claim that restrictions preserve property values — this is the coordination function justification. (2) covenant_enforcement_lock_in (ε≈0.52, Tangled Rope / Snare): the institutional mechanism that prevents amendment and enables selective enforcement — this is the extraction function. The first is upstream; if value preservation fails empirically, the entire justification for the second collapses and the constraint becomes pure snare. The second is causally downstream: selective enforcement and amendment lock-in only harm homeowners IF they prevent beneficial modifications. Separate them to avoid conflating the coordination rationale with the extraction mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hoa_covenants, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
