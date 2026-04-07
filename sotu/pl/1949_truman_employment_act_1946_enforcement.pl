% ============================================================================
% CONSTRAINT STORY: 1949_truman_employment_act_1946_enforcement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_1949_truman_employment_act_1946_enforcement, []).

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
 *   constraint_id: 1949_truman_employment_act_1946_enforcement
 *   human_readable: Employment Act of 1946: Government Demand-Side Management Commitment
 *   domain: governance/macroeconomic_policy
 *
 * SUMMARY:
 *   The Employment Act of 1946 represents a foundational shift in U.S.
 *   governance: explicit federal commitment to use all government resources
 *   to promote maximum employment, production, and purchasing power. The
 *   constraint operates through demand-side fiscal and monetary coordination,
 *   positioning government as the primary stabilizer of business cycles. The
 *   Act creates genuine coordination benefits (preventing catastrophic
 *   recessions requires state-level demand management) while simultaneously
 *   extracting asymmetric costs from powerless and unorganized workers who
 *   lack enforceable claim on the 'full employment' commitment. The
 *   constraint exhibits six distinct classifications from different
 *   perspectives: it appears as enabling coordination to federal agencies
 *   (rope), as binding protection to organized labor (scaffold with sunset),
 *   as degraded theater to academic economists (piton), as continuing
 *   extraction to unorganized workers (snare), as mixed
 *   coordination-and-regulation to large firms (tangled rope), and as natural
 *   law to the civilizational observer (mountain — false summit candidate).
 *   The theater ratio rises from 0.45 to 0.61 over the measurement interval
 *   as the Council of Economic Advisors and policy machinery become
 *   increasingly performative — reports and recommendations persist while
 *   actual unemployment volatility remains substantial, particularly for
 *   unorganized and regional workers. The extractiveness measurement shows
 *   accumulation from 0.35 to 0.52, reflecting the gradual realization that
 *   demand-side management cannot eliminate structural unemployment and that
 *   the commitment's failure to protect unorganized workers becomes
 *   normalized into policy doctrine.
 *
 * KEY AGENTS:
 *   - Federal Agencies (Congress, Executive, Council of Economic Advisors): Institutional/arbitrage. Primary beneficiary — gains explicit regulatory authority over macroeconomic coordination and legitimacy through full employment commitment.
 *   - Unorganized Workers in Cyclical Industries: Powerless/trapped. Primary victim — subject to cyclical unemployment despite Act's stated commitment; lack enforceable claim on government protection.
 *   - Large Private Enterprises: Powerful/constrained. Secondary victim/beneficiary — constrained by credit controls and anti-monopoly enforcement but benefit from demand stabilization that prevents catastrophic recessions.
 *   - Organized Labor and Progressive Coalition: Organized/mobile. Secondary beneficiary — gained voice in Act's machinery and see full employment as achievable; maintain alternative models (public sector jobs, cooperatives) as exit paths.
 *   - Neoclassical Economics Establishment: Institutional/arbitrage. Tertiary actor — maintains performative commitment to Act while intellectual consensus shifts toward market self-correction and government inefficiency.
 *   - Regional and Sectoral Communities: Powerless to moderate/trapped-to-constrained. Secondary victim — persistent unemployment in specific regions and industries despite national demand-side management reveals constraint's distributional failure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(1949_truman_employment_act_1946_enforcement, 0.52).
domain_priors:suppression_score(1949_truman_employment_act_1946_enforcement, 0.48).
domain_priors:theater_ratio(1949_truman_employment_act_1946_enforcement, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(1949_truman_employment_act_1946_enforcement, extractiveness, 0.52).
narrative_ontology:constraint_metric(1949_truman_employment_act_1946_enforcement, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(1949_truman_employment_act_1946_enforcement, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(1949_truman_employment_act_1946_enforcement, tangled_rope).
narrative_ontology:human_readable(1949_truman_employment_act_1946_enforcement, "Employment Act of 1946: Government Demand-Side Management Commitment").
narrative_ontology:topic_domain(1949_truman_employment_act_1946_enforcement, "governance/macroeconomic_policy").

domain_priors:requires_active_enforcement(1949_truman_employment_act_1946_enforcement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(1949_truman_employment_act_1946_enforcement, federal_agencies).
narrative_ontology:constraint_beneficiary(1949_truman_employment_act_1946_enforcement, regulatory_apparatus).
narrative_ontology:constraint_victim(1949_truman_employment_act_1946_enforcement, private_enterprise).
narrative_ontology:constraint_victim(1949_truman_employment_act_1946_enforcement, unorganized_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNORGANIZED WORKERS (SNARE) — Face involuntary unemployment during downturns despite the Act's stated commitment. Government management of cycles exists but lacks enforcement teeth against cyclical layoffs in textiles, agriculture, construction. Workers cannot exit the constraint; must endure periodic unemployment. High experienced extraction — the promise of full employment remains unfulfilled for workers without union representation.
constraint_indexing:constraint_classification(1949_truman_employment_act_1946_enforcement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LARGE PRIVATE ENTERPRISE (TANGLED ROPE) — Constrained by credit controls and anti-monopoly enforcement (extraction), but benefits from government demand stabilization that prevents catastrophic recessions (coordination). Enterprises must accept regulatory coordination but maintain substantial pricing power and production flexibility. The constraint mixes genuine macroeconomic coordination with asymmetric extraction through regulatory authority — firms cannot fully exit government demand management but have resources to absorb costs.
constraint_indexing:constraint_classification(1949_truman_employment_act_1946_enforcement, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FEDERAL AGENCIES / CONGRESS (ROPE) — Net beneficiaries. The Act grants agencies explicit authority to stabilize cycles through fiscal and monetary coordination. Congress gains legitimacy (full employment commitment) and agencies gain power (regulatory tools). The constraint solves a genuine coordination problem: no private actor can manage demand-side cycles; government can. Pure coordination benefit with low coercion — agencies experience the constraint as enabling, not extractive.
constraint_indexing:constraint_classification(1949_truman_employment_act_1946_enforcement, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ORGANIZED LABOR / PROGRESSIVE COALITION (SCAFFOLD) — See the Employment Act as a temporary institutional bridge toward a stronger social guarantee. Labor unions gained explicit voice in the Act's machinery and see full employment as achievable through coordinated government-labor-industry planning. Their exit path: if government fails to deliver on employment commitment, labor can shift to organizing sector-specific job guarantees or cooperative ownership (mobile exit). The constraint has a sunset logic embedded in its success criterion — either government achieves full employment (constraint becomes unnecessary) or labor pursues alternative models. Theater ratio is elevated because the Act's machinery (Council of Economic Advisors, policy commissions) performs commitment more than it delivers unconditional employment guarantees.
constraint_indexing:constraint_classification(1949_truman_employment_act_1946_enforcement, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: NEOCLASSICAL ECONOMICS (PITON) — By the late 1950s, the Act's Keynesian machinery becomes increasingly performative. Economists maintain the commitment's language and ritual (council reports, policy frameworks) while theoretical priors shift toward belief that markets self-correct and government intervention creates inefficiency. The Act persists through institutional inertia — the Council of Economic Advisors continues producing reports, agencies maintain demand-management tools — but the underlying conviction that government can and should stabilize cycles degrades into theater. Theater ratio rises as the academic consensus pivots toward monetarism and supply-side approaches.
constraint_indexing:constraint_classification(1949_truman_employment_act_1946_enforcement, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL / CIVILIZATIONAL (MOUNTAIN — FALSE SUMMIT CANDIDATE) — From a universal civilizational perspective, business cycles appear as immutable features of market economies — the constraint of economic scarcity and coordination impossibility without government intervention. Market economies inherently generate cycles; government's attempt to manage them appears to be responding to a natural law. However, the structural data contradicts this: identifiable beneficiaries (federal agencies, regulatory apparatus) benefit from the constraint; specific victims (unorganized workers, private firms) bear costs. The engine will flag this as a false summit: the 'immutable economic cycle' framing naturalizes what is actually a contingent institutional arrangement that distributes power toward government agencies.
constraint_indexing:constraint_classification(1949_truman_employment_act_1946_enforcement, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(1949_truman_employment_act_1946_enforcement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(1949_truman_employment_act_1946_enforcement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(1949_truman_employment_act_1946_enforcement, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(1949_truman_employment_act_1946_enforcement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(1949_truman_employment_act_1946_enforcement, TR),
    TR >= 0.70.

:- end_tests(1949_truman_employment_act_1946_enforcement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The Act combines genuine coordination (demand-side stabilization prevents depression) with asymmetric extraction (government gains regulatory authority, unorganized workers bear employment volatility). The initial extractiveness (0.35) reflects the 1946 moment when the coordination benefit was strong and the regulatory apparatus minimal. By 1956, extractiveness rises to 0.52 as government authority expands and unorganized workers face persistent unemployment in specific sectors and regions despite national management — the coordination promise degrades into regulation without delivery. Suppression (0.48): Moderate. Unorganized workers cannot collectively exit the constraint (trapped), but large firms have some arbitrage capacity and organized labor maintains alternative models. The suppression is structural (workers lack political power to enforce the employment commitment) but not totalizing (some sectors and regions achieve strong employment). Theater ratio (0.61): Moderate-high. The Council of Economic Advisors and policy apparatus perform commitment more reliably than they deliver unconditional full employment. Reports continue, policy frameworks persist, but actual unemployment volatility remains and concentrates in politically weak regions and sectors. The theater increases over the measurement interval as the gap between commitment language and implementation widens — the machinery becomes more performative even as extractiveness accumulates.
 *
 * PERSPECTIVAL GAP:
 *   The Employment Act creates maximum perspectival divergence through the asymmetry between coordinating commitment and enforcing mechanism. Federal agencies experience the constraint as pure coordination (rope) — they solve the collective action problem of demand-side cycle management. Unorganized workers experience snare — the commitment to full employment is stated but the enforcement mechanism targets organized constituencies and leaves structural unemployment concentrated in politically weak regions and sectors. Organized labor experiences scaffold — they gained explicit voice in the Act's machinery and maintain alternative models (public sector job creation, cooperatives) as exit paths if government fails. Large enterprises experience tangled rope — they pay regulatory costs (credit controls, antitrust enforcement) but receive demand-stabilization benefits that prevent catastrophic recessions. The neoclassical establishment experiences piton — the institutional machinery persists while underlying intellectual conviction that government can manage cycles fades, leaving reports and frameworks as performative ritual. The civilizational observer risks mountain classification but the structural data reveals false summit — identifiable beneficiaries (federal agencies) and victims (unorganized workers) indicate contingent institutional arrangement, not natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) for each perspective derive from structural position within the extraction and coordination flows. Federal agencies and Congress (beneficiaries, arbitrage exit) experience low d (≈0.15) — the constraint extracts from others toward them. Unorganized workers (victims, trapped exit) experience high d (≈0.95) — the constraint extracts maximum value from them. Large private enterprises (mixed victim-beneficiary, constrained exit) experience moderate d (≈0.55) — they pay regulatory costs but receive demand-stabilization benefits. Organized labor (beneficiary, mobile exit) experience moderate-low d (≈0.30) — they benefit from the Act's machinery and maintain exit options. Neoclassical economists (beneficiaries through intellectual authority, arbitrage exit) experience low d (≈0.10) — they maintain authority while the theoretical commitment to government management fades. The analytical observer (analytical exit) experiences moderate d (≈0.70) — positioned to see the full structure of extraction and coordination but at risk of naturalizing institutional arrangements as immutable necessity.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint demonstrates the full distribution of types across observational positions, with false summit detection as the key diagnostic. The analytical observer at civilizational scope risks mountain classification (business cycles as natural law), but the structural data reveals identifiable beneficiaries (federal agencies gain power) and victims (unorganized workers bear employment volatility). The engine's false summit detector will flag this perspective: the 'immutable economic cycle' framing naturalizes a contingent institutional arrangement. The snare classification (unorganized workers) and rope classification (federal agencies) are not contradictory — they reflect the same constraint from different structural positions. The scaffold classification (organized labor) is accurate if labor genuinely maintains alternative models (public sector employment, cooperatives) and sees the Act as a transitional bridge. The piton classification (neoclassical economists) reflects genuine degradation: the machinery persists (Council reports continue) while underlying conviction in government capacity declines. The tangled rope classification (large firms) captures the mixed extraction-and-coordination mechanism: firms are constrained by regulation but benefit from demand stabilization. The constraint is resolvable into its constituent parts without contradiction: each type is the accurate reading from its respective observational position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_cycle_severity_absent_act,
    'Would business cycles be less severe, equally severe, or more severe in the absence of government demand-side management under the 1946 Act?',
    'Comparative historical analysis of pre-1946 cycles vs. post-1946 cycles; econometric decomposition of cycle amplitude attributable to automatic stabilizers vs. discretionary policy; international comparison with non-Keynesian economies.',
    'If cycles would be more severe without the Act: constraint is genuine coordination (rope classification predominates). If equally or less severe: constraint is primarily extraction masquerading as necessary management (snare/tangled_rope classification becomes dominant). If severity depends on implementation: classification distribution shifts toward scaffold and piton (government policy choices matter, not immutable necessity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_cycle_severity_absent_act, empirical, 'Counterfactual severity of cycles without Act-driven government management').

omega_variable(
    political_economy_of_full_employment_commitment,
    'Is the 1946 Act''s commitment to full employment a binding legal obligation, a non-binding aspiration, or a framework for contestable policy discretion?',
    'Legal analysis of enforcement mechanisms (Congressional mandates, agency authority limits, judicial review standards); historical record of government response to high unemployment (does government mobilize resources or accept unemployment as inevitable?); political consequences of unemployment (does failure to meet commitment trigger policy change or become normalized?).',
    'If binding obligation: victims (unorganized workers) have legal ground for remedy (extractiveness drops, snare becomes rope or tangled_rope). If aspiration: victims lack enforcement mechanism (extractiveness stays high, snare classification confirmed). If contestable discretion: classification depends on which political coalition controls government (piton classification indicates degraded commitment).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(political_economy_of_full_employment_commitment, conceptual, 'Legal and political bindingness of full employment commitment').

omega_variable(
    labor_coalition_capture_vs_capacity,
    'Do organized labor and progressive actors genuinely believe government can deliver full employment through demand-side management, or do they accept the Employment Act as a partial victory while recognizing its insufficient scope?',
    'Historical record of labor union demands and rhetoric (are they satisfied with Act provisions or pushing for stronger guarantees like public job creation?); comparative analysis of countries with explicit job guarantee programs vs. U.S. demand-management approach; analysis of when labor abandons demand-side reliance in favor of sector-specific organizing.',
    'If genuine belief in sufficient scope: scaffold classification is accurate (organized labor sees real exit path through implementation success). If recognized insufficiency: organized labor is partly captured by the constraint''s framework even while organizing alternatives (snare classification of labor rises slightly, theater ratio interpretation changes).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_coalition_capture_vs_capacity, empirical, 'Whether organized labor views Employment Act as sufficient or partial victory').

omega_variable(
    natural_law_framing_as_extraction_cover,
    'Does the naturalizing language about ''economic cycles'' and ''inherent instability'' function as a cover story that justifies government control and privatizes unemployment risk?',
    'Discourse analysis: how often does government rhetoric invoke ''natural cycles'' to explain unemployment rates? Do policy documents contrast government ''response'' to inevitable cycles vs. government ''creation'' of full employment? Comparison with societies that frame employment as achievable right rather than cyclical fluctuation.',
    'If naturalization is effective rhetorical cover: the mountain classification is a false summit, revealing how government frames its own regulatory power as response to natural necessity rather than as active extraction and reallocation. This supports FSM trigger and reclassification to tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_framing_as_extraction_cover, conceptual, 'Whether natural-law framing of cycles serves as extraction cover story').

omega_variable(
    regional_sector_unemployment_persistence,
    'After 25+ years of Act implementation, why do specific regions and sectors persist in high unemployment despite national demand-side management?',
    'Regional unemployment data (are Appalachia, the Great Plains, and industrial heartland disproportionately affected?); sectoral unemployment (agricultural, textile, resource extraction workers face persistent unemployment despite national management); analysis of whether national demand-side management is structurally incapable of addressing structural unemployment.',
    'If regional/sectoral persistence is significant: the Act''s extraction mechanism is revealed — it redistributes unemployment toward politically weak regions and sectors while benefiting concentrated industrial centers (extractiveness rating may rise). If regional/sectoral patterns dissolve: demand-side management genuinely solves the full employment problem (extractiveness may drop, rope classification becomes more plausible).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regional_sector_unemployment_persistence, empirical, 'Persistent regional and sectoral unemployment despite national management').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(1949_truman_employment_act_1946_enforcement, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(empl_act_theater_1946, 1949_truman_employment_act_1946_enforcement, theater_ratio, 0, 0.45).
narrative_ontology:measurement(empl_act_theater_1951, 1949_truman_employment_act_1946_enforcement, theater_ratio, 5, 0.55).
narrative_ontology:measurement(empl_act_theater_1956, 1949_truman_employment_act_1946_enforcement, theater_ratio, 10, 0.61).

% Extraction over time
narrative_ontology:measurement(empl_act_extractiveness_1946, 1949_truman_employment_act_1946_enforcement, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(empl_act_extractiveness_1951, 1949_truman_employment_act_1946_enforcement, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(empl_act_extractiveness_1956, 1949_truman_employment_act_1946_enforcement, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(1949_truman_employment_act_1946_enforcement, resource_allocation).
narrative_ontology:affects_constraint(1949_truman_employment_act_1946_enforcement, federal_reserve_independence_myth).
narrative_ontology:affects_constraint(1949_truman_employment_act_1946_enforcement, monetary_policy_discretion_vs_rules).

% DUAL FORMULATION NOTE:
% The Employment Act represents one pole of the demand-side management constraint family. Downstream constraints concern the technical machinery (Federal Reserve coordination, monetary policy rules) and the intellectual contestation (Keynesian vs. neoclassical economics). All stories in the family share the extractiveness measurement trajectory but diverge in how agents experience the regulatory authority asymmetry.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(1949_truman_employment_act_1946_enforcement, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
