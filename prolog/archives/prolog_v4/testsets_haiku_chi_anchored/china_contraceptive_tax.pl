% ============================================================================
% CONSTRAINT STORY: china_contraceptive_tax
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_china_contraceptive_tax, []).

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
 *   constraint_id: china_contraceptive_tax
 *   human_readable: China's Differential Tax on Contraceptives
 *   domain: economic/political
 *
 * SUMMARY:
 *   China's differential taxation of contraceptives represents a hybrid
 *   constraint combining state demographic objectives with economic
 *   extraction. The 17% VAT on condoms, while exemptions or subsidies apply
 *   to IUDs and sterilization, creates a two-tier contraceptive market where
 *   state preference for permanent or semi-permanent methods is enforced
 *   through fiscal mechanism rather than explicit prohibition. The constraint
 *   exhibits the signature of Tangled Rope: it solves a genuine coordination
 *   problem (aligning individual reproductive choice incentives with state
 *   demographic capacity), but does so through asymmetric cost distribution
 *   that extracts reproductive autonomy from lower-income users who lack
 *   flexibility to switch to subsidized alternatives. The theater ratio
 *   reflects the policy's rhetorical framing as public health optimization
 *   while its actual mechanism is fiscal steering of reproductive choice. As
 *   China's demographic transition advances post-one-child policy reversal,
 *   the constraint's coordination function weakens while its extractive
 *   function becomes more visible—suggesting potential sunset dynamics if
 *   fertility targets relax or reproductive autonomy discourse gains
 *   institutional legitimacy.
 *
 * KEY AGENTS:
 *   - Low-income condom users: Primary victim (powerless/trapped) — bear full cost of 17% VAT with no subsidized alternatives; reproductive autonomy extracted.
 *   - Middle-income condom users: Secondary victim (moderate/constrained) — experience tax as both price burden and state steering; constrained choice.
 *   - IUD and sterilization providers: Primary beneficiary (institutional/arbitrage) — receive state subsidies and capture demand shifted from condoms; benefit from tax-induced scarcity.
 *   - Condom manufacturers: Secondary beneficiary/victim (powerful/arbitrage) — face reduced demand but benefit from protected market against high-volume low-price competitors.
 *   - Chinese state (demographic policy): Intended beneficiary (organized/constrained) — pursues fertility targets through fiscal mechanism; constrained by international reproductive rights discourse.
 *   - Public health coalition: Victim of constraint (organized/constrained) — international and domestic health advocates oppose policy as inequitable and medically unsound.
 *   - Ministry of Finance: Institutional enforcer (institutional/arbitrage) — operates constraint as revenue mechanism and demographic tool; has arbitrage to adjust rates.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(china_contraceptive_tax, 0.52).
domain_priors:suppression_score(china_contraceptive_tax, 0.65).
domain_priors:theater_ratio(china_contraceptive_tax, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(china_contraceptive_tax, extractiveness, 0.52).
narrative_ontology:constraint_metric(china_contraceptive_tax, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(china_contraceptive_tax, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(china_contraceptive_tax, tangled_rope).
narrative_ontology:human_readable(china_contraceptive_tax, "China's Differential Tax on Contraceptives").
narrative_ontology:topic_domain(china_contraceptive_tax, "economic/political").

domain_priors:requires_active_enforcement(china_contraceptive_tax).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(china_contraceptive_tax, state_demographic_goals).
narrative_ontology:constraint_beneficiary(china_contraceptive_tax, iud_manufacturers).
narrative_ontology:constraint_beneficiary(china_contraceptive_tax, sterilization_providers).
narrative_ontology:constraint_victim(china_contraceptive_tax, condom_users).
narrative_ontology:constraint_victim(china_contraceptive_tax, reproductive_autonomy).
narrative_ontology:constraint_victim(china_contraceptive_tax, sexual_health_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME CONDOM USERS (SNARE) — Cannot exit the tax without abandoning condom use entirely or purchasing through informal channels. Bears full cost of 17% VAT with no alternatives subsidized. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.72. Trapped at biographical horizon — reproductive choices made under financial constraint.
constraint_indexing:constraint_classification(china_contraceptive_tax, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MIDDLE-INCOME CONDOM USERS (TANGLED ROPE) — Can afford the tax but experience it as coordinated steering toward state-preferred methods. Constrained by both price and social policy signaling. Some coordination benefit (tax reduction available if they switch to IUD), but extraction of choice autonomy is real. d≈0.70, f(d)≈1.05, σ=1.0 → χ≈0.55.
constraint_indexing:constraint_classification(china_contraceptive_tax, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: IUD AND STERILIZATION PROVIDERS (ROPE) — Beneficiaries of tax exemption and subsidies. Experience the constraint as coordination: state preference for permanent/semi-permanent methods aligns with provider capacity and revenue stability. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.05. Net beneficiary; constraint coordinates provider capacity with state demographic policy.
constraint_indexing:constraint_classification(china_contraceptive_tax, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONDOM MANUFACTURERS (TANGLED ROPE) — Face 17% VAT burden that reduces competitiveness vs subsidized methods. Can arbitrage by lobbying for tax reduction or by shifting production/distribution patterns. But also benefit from the tax as a market-shaping mechanism that may reduce condom demand pressures in favor of methods requiring provider relationships. d≈0.45, f(d)≈0.45, σ=1.0 → χ≈0.24. Mixed: extraction from consumer price sensitivity, coordination benefit from protected market.
constraint_indexing:constraint_classification(china_contraceptive_tax, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: PUBLIC HEALTH AND REPRODUCTIVE RIGHTS COALITION (ROPE) — International and domestic NGOs, WHO, medical societies see the tax differential as pure coordination problem: it aligns incentives with evidence-based sexual health equity. The constraint has low effective extraction because the coalition can articulate alternative framing (condoms as public health good, not luxury consumer good). d≈0.35, f(d)≈0.32, σ=1.2 → χ≈0.20. Constrained by state capacity to enforce tax, but significant leverage through international pressure and medical legitimacy.
constraint_indexing:constraint_classification(china_contraceptive_tax, rope,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: CHINESE MINISTRY OF FINANCE (TANGLED ROPE) — Operates the constraint as both coordination mechanism (aligning tax code with demographic policy) and extraction mechanism (collecting VAT from lower-income users). Has arbitrage: can adjust tax rates, create exemptions for health insurance coverage, or harmonize with neighboring jurisdictions. d≈0.32, f(d)≈0.28, σ=1.0 → χ≈0.15. Active enforcement required; institutional flexibility limits effective extraction.
constraint_indexing:constraint_classification(china_contraceptive_tax, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SCAFFOLD) — From civilizational/global view, the contraceptive tax differential is a temporary policy artifact of the era of fertility concerns (post-one-child policy). As demographic transition completes and reproductive autonomy becomes a documented public health priority, the tax structure becomes obsolete. d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.38. Has_sunset_clause potential: tax harmonization or exemption for condoms could occur within 10-20 years as fertility targets relax.
constraint_indexing:constraint_classification(china_contraceptive_tax, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(china_contraceptive_tax_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(china_contraceptive_tax, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(china_contraceptive_tax, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(china_contraceptive_tax, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(china_contraceptive_tax_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderately high. The constraint extracts reproductive autonomy from lower-income users through price elasticity and lack of subsidized alternatives. However, it is not maximal extraction (0.66+) because: (1) condoms remain legally available; (2) informal/black-market supply exists; (3) switching to subsidized methods is theoretically possible (though with autonomy loss). The 17% VAT translates to real price barrier for low-income users but does not eliminate choice entirely. Rising from 0.35 to 0.52 over the interval reflects increasing state enforcement and declining informal supply. Suppression (0.65): Moderate-high. Real barriers exist to exit: low-income users cannot easily substitute subsidized alternatives due to medical contraindications or autonomy preferences; informal condom channels face enforcement risk; social signaling about state-preferred methods creates reputational cost to condom choice. But suppression is not total (0.80+) because some users accept the cost or find alternatives. Theater ratio (0.58): Moderate. The policy is framed as public health optimization and demographic planning, which comprises ~58% of the stated justification. The actual mechanism is fiscal revenue and behavioral steering, which comprises ~42%. Theater has increased over the interval as international criticism has forced the state to elaborate public health rationales. This is characteristic of Tangled Rope: genuine coordination function (fertility optimization) packaged in partial theater to obscure extraction function.
 *
 * PERSPECTIVAL GAP:
 *   This constraint reveals stark perspectival divergence. Low-income users perceive a Snare (pure extraction, no exit). Middle-income users perceive Tangled Rope (mixed coordination and extraction). Providers perceive Rope (pure coordination with state objectives). Manufacturers perceive Tangled Rope from opposite direction (constrained but protected). International health coalition perceives Rope with contested coordination function. Ministry perceives controlled Tangled Rope with clear sunset potential. The analytical observer sees a temporary scaffold whose sunset will occur when demographic urgency declines. This perspectival range from Snare to Rope to Scaffold indicates a constraint undergoing institutional lifecycle: it began as Rope (coordination), became Tangled Rope (coordination + extraction), and is transitioning toward Scaffold (as demographic transition completes) or Piton (if it persists after becoming functionally obsolete).
 *
 * DIRECTIONALITY LOGIC:
 *   Low-income condom users: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum or near-maximum extraction; no exit options within constraint. Middle-income users: Victim + constrained → d≈0.70, f(d)≈1.05. Significant extraction; can technically switch but at autonomy cost. IUD/sterilization providers: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Negative effective extraction (net beneficiaries). Condom manufacturers: Mixed (victim of demand reduction + beneficiary of protected market) + arbitrage → d≈0.45, f(d)≈0.45. Moderate extraction; high arbitrage capacity. Ministry of Finance: Institutional beneficiary + arbitrage → d≈0.32, f(d)≈0.28. Low effective extraction; institutional flexibility and international pressure provide arbitrage. Public health coalition: Victim + constrained (can advocate but not override policy) → d≈0.35, f(d)≈0.32. Moderate extraction; significant leverage through legitimacy.
 *
 * MANDATROPHY ANALYSIS:
 *   The China contraceptive tax resolves the mandatrophy through institutional differentiation. The state genuinely perceives Rope (coordination of reproductive choices with fertility targets). Low-income users genuinely perceive Snare (coercive extraction). Both are structurally correct within their observational contexts. The mandatrophy is resolved by recognizing that the constraint's classification DEPENDS on power asymmetry: the state has institutional power to define coordination; low-income users lack power to contest it. The policy is Tangled Rope because it requires active enforcement (institutions must maintain the tax, prosecute evasion), benefits an organized actor (state demographic policy), and extracts from powerless actors (low-income condom users). The theater ratio (0.58) reflects the policy's rhetorical strategy: framing extraction as coordination by using public health language. As China's fertility urgency declines post-demographic-transition, the coordination narrative weakens (sunset dynamics), and the constraint either becomes Piton (theater-maintained without function) or collapses entirely. The perspectival divergence is not a failure of the framework but a success: it reveals that the constraint is genuinely experienced differently by agents with different structural positions, and that the state's 'coordination' narrative is only legitimate if state demographic objectives are accepted as public goods—a contestable normative claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demographic_intent_vs_economic_motive,
    'Is the differential tax primarily a demographic engineering tool or a fiscal revenue mechanism disguised as demographic policy?',
    'Analysis of tax design history, legislative justifications, and revenue impact. If designed to maximize revenue, ε would be higher (extraction primary). If designed to minimize tax while shaping behavior, ε would be lower (coordination primary).',
    'If demographic intent: Tangled Rope classification holds, suppression is moderate (tax as carrot/stick, not force). If revenue motive: classification shifts toward Snare, suppression increases (coercive extraction reframed as population management).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_intent_vs_economic_motive, empirical, 'Whether tax is primarily demographic engineering or fiscal revenue').

omega_variable(
    elasticity_of_condom_demand,
    'What is the actual price elasticity of condom demand in China across income cohorts? Does the 17% tax reduce condom use or merely shift to untaxed channels?',
    'Econometric analysis of condom sales pre/post tax implementation, correlation with income levels, and market-share data for alternative contraceptive methods.',
    'If elasticity is low (demand sticky): tax is primarily extractive, not coordinative. Suppression is high (users have no real choice). If elasticity is high: tax successfully steers behavior toward state-preferred methods, supporting coordination narrative. If demand shifts to informal channels: extraction fails, suppression paradoxically increases (enforcement required).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(elasticity_of_condom_demand, empirical, 'Price elasticity of condom demand and substitution patterns').

omega_variable(
    subsidy_transparency_and_accounting,
    'How transparent is the accounting of subsidies for IUDs and sterilization? Are opportunity costs and externalities (complication rates, regret, reproductive autonomy loss) factored into subsidy justification?',
    'Comparative audit of Ministry of Finance subsidy allocations, inclusion of complication costs in health system accounting, and analysis of regret rates among subsidized procedure recipients.',
    'If subsidies are opaque or ignore externalities: constraint is pure extraction (state preferences imposed without accountability). If transparent and externality-inclusive: constraint could be legitimate coordination (comparative public health benefit). Classification could shift to Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsidy_transparency_and_accounting, empirical, 'Transparency and completeness of contraceptive subsidy accounting').

omega_variable(
    enforceability_across_urban_rural_divide,
    'Is the tax enforced uniformly across urban and rural areas? Does enforcement pattern track enforceability infrastructure or demographic targeting?',
    'Tax collection data disaggregated by province and urban/rural region. Analysis of enforcement resource allocation and actual tax compliance rates.',
    'If enforcement is selective/weak in rural areas where condom use is lower: constraint is primarily targeting urban reproductive autonomy (narrower extraction, higher precision). If enforcement is uniform: broader extraction but also reveals demographic targeting more clearly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforceability_across_urban_rural_divide, empirical, 'Geographic variation in tax enforcement and compliance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(china_contraceptive_tax, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cctax_tr_t0, china_contraceptive_tax, theater_ratio, 0, 0.42).
narrative_ontology:measurement(cctax_tr_t5, china_contraceptive_tax, theater_ratio, 5, 0.5).
narrative_ontology:measurement(cctax_tr_t10, china_contraceptive_tax, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(cctax_be_t0, china_contraceptive_tax, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cctax_be_t5, china_contraceptive_tax, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(cctax_be_t10, china_contraceptive_tax, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(china_contraceptive_tax, resource_allocation).
narrative_ontology:affects_constraint(china_contraceptive_tax, china_fertility_policy_enforcement).
narrative_ontology:affects_constraint(china_contraceptive_tax, reproductive_health_equity_china).

% DUAL FORMULATION NOTE:
% The contraceptive tax is downstream of broader fertility policy (affected by one-child policy reversal and demographic transition timeline) but represents a distinct fiscal-constraint on individual reproductive choice. Upstream constraints set the demographic context; this constraint operationalizes preference through tax mechanism. Related constraints address enforcement mechanisms and health equity outcomes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(china_contraceptive_tax, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
