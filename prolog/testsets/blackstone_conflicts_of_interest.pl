% ============================================================================
% CONSTRAINT STORY: blackstone_conflicts_of_interest
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_blackstone_conflicts_of_interest, []).

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
 *   constraint_id: blackstone_conflicts_of_interest
 *   human_readable: Fiduciary Conflict Allocation in Blackstone S-1
 *   domain: corporate_governance/alternative_assets
 *
 * SUMMARY:
 *   Blackstone's 2007 IPO created a novel corporate structure where the
 *   General Partner (GP) retained control over conflict allocation between
 *   the interests of existing investment fund Limited Partners and new public
 *   common unitholders. The S-1 filing explicitly prioritizes fund interests
 *   when conflicts arise. This structural subordination operates as a formal
 *   legal mechanism, not an implicit power imbalance. The constraint exhibits
 *   dual nature: it solves a genuine coordination problem (how does a
 *   dual-mission firm resolve conflicts?) while simultaneously extracting
 *   value from public shareholders through legal prioritization. The theater
 *   ratio has drifted upward over the interval as the fairness opinion and
 *   governance rituals have become more elaborate while the underlying
 *   asymmetry has persisted. Extractiveness has slowly increased as the firm
 *   has grown and made more discretionary calls favoring funds over public
 *   equity in resource allocation decisions (capital deployment, fee
 *   structures, information access).
 *
 * KEY AGENTS:
 *   - Blackstone General Partner: Primary beneficiary (institutional/arbitrage) — controls conflict allocation authority; systematically resolves conflicts in favor of fund interests; captures GP fee and carry asymmetries
 *   - Existing Limited Partners: Secondary beneficiary (institutional/arbitrage) — their fund interests are legally prioritized; experience constraint as coordinating mechanism; have exit via secondaries but choose to stay
 *   - Public Common Unitholders: Primary victim (powerless/trapped) — subordinated by explicit S-1 terms; no voting power over conflict allocation; bear extraction through prioritization asymmetry; cannot exit without accepting market discount
 *   - Secondary Limited Partners (Post-IPO): Tertiary victim (moderate/constrained) — gain some benefits from fund coordination but share trapped position with public unitholders at subordination interface
 *   - Market Pricing Integrity: Abstract victim (analytical/analytical) — the information asymmetry about conflict allocation scope creates mispricing if the market under-values the extraction cost
 *   - Regulatory & Market Oversight Coalition: Organized agent (organized/constrained) — SEC, FINRA, institutional investor coalitions gradually imposing disclosure and governance standards that reduce GP unilateral authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(blackstone_conflicts_of_interest, 0.58).
domain_priors:suppression_score(blackstone_conflicts_of_interest, 0.72).
domain_priors:theater_ratio(blackstone_conflicts_of_interest, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(blackstone_conflicts_of_interest, extractiveness, 0.58).
narrative_ontology:constraint_metric(blackstone_conflicts_of_interest, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(blackstone_conflicts_of_interest, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(blackstone_conflicts_of_interest, tangled_rope).
narrative_ontology:human_readable(blackstone_conflicts_of_interest, "Fiduciary Conflict Allocation in Blackstone S-1").
narrative_ontology:topic_domain(blackstone_conflicts_of_interest, "corporate_governance/alternative_assets").

domain_priors:requires_active_enforcement(blackstone_conflicts_of_interest).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(blackstone_conflicts_of_interest, blackstone_gp_partners).
narrative_ontology:constraint_beneficiary(blackstone_conflicts_of_interest, existing_limited_partners).
narrative_ontology:constraint_beneficiary(blackstone_conflicts_of_interest, blackstone_management_entities).
narrative_ontology:constraint_victim(blackstone_conflicts_of_interest, public_unitholders).
narrative_ontology:constraint_victim(blackstone_conflicts_of_interest, market_pricing_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PUBLIC UNITHOLDER (SNARE) — Trapped in subordinate legal position. The S-1 filing explicitly subordinates public common unitholder interests to the investment funds' interests. Exit means selling at book value or market price with no recourse. No voting power over conflict allocation decisions. Bears full cost of legal prioritization asymmetry.
constraint_indexing:constraint_classification(blackstone_conflicts_of_interest, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SECONDARY LIMITED PARTNER (TANGLED ROPE) — Constrained by commitment period lock-in and illiquidity. Nominally benefits from Blackstone's continued success (coordination function), but subordinate to the GP's conflict allocation authority. Can exit through secondaries market but at discounted terms. Mixed extraction and coordination — receives fund distributions but subject to unilateral GP conflict resolution.
constraint_indexing:constraint_classification(blackstone_conflicts_of_interest, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: BLACKSTONE GENERAL PARTNER (ROPE) — Experiences the constraint as pure coordination mechanism: the S-1 filing explicitly grants the GP unilateral authority to allocate conflicts between fund interests and public shareholder interests in favor of funds. This is coordination with minimal coercive overhead from the GP's perspective — they define the terms and enforce them contractually. Benefits from IPO capital while retaining investment fund primacy.
constraint_indexing:constraint_classification(blackstone_conflicts_of_interest, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: EXISTING LIMITED PARTNERS (ROPE) — Benefit from explicit legal prioritization in the conflict allocation mechanism. The S-1 subordinates new public equity to their interests, ensuring their fund distributions and management fee bases remain protected. Experience the constraint as coordination of allocation rights, not extraction. They have exit rights via secondaries and redemptions; they are choosing to stay because the arrangement favors them.
constraint_indexing:constraint_classification(blackstone_conflicts_of_interest, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY & MARKET OVERSIGHT (SCAFFOLD) — The SEC, FINRA, and institutional investors are gradually imposing conflict-of-interest disclosure and governance standards that reduce the unilateral authority granted in the 2007 S-1. ESG criteria, proxy voting standards, and fiduciary duty litigation have created exit paths from the original constraint structure. Suppression is declining as transparency requirements increase. Sunset mechanism: progressive harmonization of GP-LP and public shareholder governance standards erodes the legal asymmetry. Estimated timeline: 15-20 years for full convergence.
constraint_indexing:constraint_classification(blackstone_conflicts_of_interest, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: LEGAL FAIRNESS OPINION RITUAL (PITON) — The S-1 requirement for a 'fairness opinion' on conflict allocation from independent financial advisors is largely performative. The opinion documents consent to an asymmetry that has already been negotiated and is structurally embedded in the offering terms. The ritual persists through institutional inertia — deemed legally protective but functionally constrained to rubber-stamp the deal terms negotiated by the GP. Theater ratio is high (0.68) because the fairness opinion goes through motions of independent evaluation but operates within a pre-determined framework.
constraint_indexing:constraint_classification(blackstone_conflicts_of_interest, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — The constraint exhibits both genuine coordination (the S-1 mechanism solves the problem of how a firm with dual missions — generating returns for LP funds and serving public shareholders — allocates conflicts) and asymmetric extraction (the allocation mechanism systematically favors insiders over public equity holders). This is the canonical tangled rope: coordination function present (conflict allocation rules exist), asymmetric extraction present (allocation rules privilege legacy interests), active enforcement present (GP exercises unilateral authority under S-1 terms). The civilization-level perspective sees this as a structural feature of the alternative assets model that may erode as market transparency increases.
constraint_indexing:constraint_classification(blackstone_conflicts_of_interest, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(blackstone_conflicts_of_interest_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(blackstone_conflicts_of_interest, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(blackstone_conflicts_of_interest, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(blackstone_conflicts_of_interest, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(blackstone_conflicts_of_interest, TR),
    TR >= 0.70.

:- end_tests(blackstone_conflicts_of_interest_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The S-1 mechanism explicitly subordinates public unitholder interests to fund interests through legal contract and GP discretion. This is not hidden extraction — it is disclosed. However, the true scope and frequency of actual conflicts resolved in fund favor is less transparent than the formal allocation rule. The value reflects that while the mechanism is legal and disclosed, the actual extraction realized is higher than the formal mechanism alone would suggest due to information asymmetries about how the GP exercises discretion. Suppression (0.72): High. Public unitholders have severely limited exit options (selling at market discount or accepting subordination), no voting power over conflict allocation, no transparency into GP decision-making on specific conflicts, and legal structure that makes challenging the allocation basis difficult. Alternative structures (equal voting, independent conflict resolution) are not available. Theater ratio (0.68): Moderately high. The fairness opinion and governance certifications provide legal cover for the asymmetry but are somewhat performative — they document a pre-negotiated structure rather than independently evaluating conflict allocation fairness. The S-1 and annual 10-K disclosures of conflicts are more substantive but still operate within the GP's framing of what constitutes a material conflict.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how the same legal mechanism (unilateral GP conflict allocation authority) is experienced as coordination by those who benefit from it and as extraction by those subordinated by it. The gap is not observational — it reflects real structural differences in how the parties relate to the constraint. The GP has designed and controls the allocation rules; legacy LPs voluntarily remain under these rules (having full information before investing); public unitholders are bound by S-1 terms they did not negotiate and cannot meaningfully contest without exiting. The regulatory coalition's scaffold perspective predicts that governance standards convergence will eventually erode this asymmetry, reducing extraction and collapsing the gap. The piton classification of the fairness opinion ritual reveals that the legal formalism around conflict allocation (the opinions, the disclosures, the governance committee) performs the function of legitimacy without substantially constraining the GP's actual discretion.
 *
 * DIRECTIONALITY LOGIC:
 *   The GP and legacy LPs occupy structural positions where d is low (beneficiaries with arbitrage options), producing negative or low effective extraction — they experience χ as favorable because the constraint directs resources toward them. Public unitholders occupy structural positions where d is high (victims with trapped exit options), producing high effective extraction — they experience χ as severe because they bear costs without control or meaningful exit. The secondary LP position is intermediate: these agents are nominally part of the 'beneficiary' class (they commit to funds) but occupy the same trapped position as public unitholders at the conflict allocation interface, making their d higher than legacy LPs but lower than powerless public holders. The analytical observer position (d = 0.72, arbitrage exit) shows this as Tangled Rope because the observer has sufficient exit and informational access to see both the coordination function and the asymmetric extraction operating simultaneously.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by demonstrating that 'fiduciary conflict allocation' is NOT a single mechanism but a structural relationship that exhibits both genuine coordination and asymmetric extraction simultaneously. The mandate of Blackstone (serve fund LPs) and the new mandate imposed by public equity (serve public unitholders) genuinely conflict in resource allocation decisions. The S-1 mechanism provides a formal resolution procedure: allocate to the GP's original mandate (funds). This is coordination in the narrow sense — a rule exists that all parties know in advance. But the allocation rule itself is asymmetric: it privileges one mandate over the other through legal structure and GP discretion, not through transparent competition between legitimate claims. This is extraction in the narrow sense — one party is systematically advantaged in how conflicts are resolved. Both dimensions are present. The mandatrophy is resolved by classifying this as Tangled Rope, not by denying either dimension. The fairness opinion ritual (Piton) further complicates the picture: it provides performative assurance that the allocation mechanism is fair, but operates within the GP's framing of 'fairness' (proceeding from the assumption that fund interests have legal priority). The public unitholder's Snare perspective reveals that from their structural position, they experience the constraint as pure extraction disguised by fairness language.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conflict_magnitude_threshold,
    'At what level of material conflict between fund interests and public shareholder interests does the S-1 allocation mechanism shift from coordination to pure extraction?',
    'Analysis of historical conflicts where GP chose fund over public unit interests; quantification of economic impact on public unitholders in each case; statistical correlation between conflict frequency and LP fund performance vs public unit returns',
    'If frequent material conflicts exist: classification shifts to Snare from multiple perspectives. If conflicts are rare or low-magnitude: constraint functions as genuine coordination mechanism, classification shifts to Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conflict_magnitude_threshold, empirical, 'Threshold at which conflict allocation becomes extractive').

omega_variable(
    public_unitholder_price_discovery,
    'Did the public market accurately price the subordination of public unitholders to fund interests at IPO and subsequently, or has the market underpriced this extraction asymmetry?',
    'Comparison of Blackstone BDC and preferred return fund performance vs public common unit returns over 10+ year period; analysis of whether subordination discount was priced at IPO; examination of public unitholder documentation and fairness opinion assumptions vs realized outcomes',
    'If market correctly priced subordination: constraint functions transparently despite asymmetry, classification remains Snare but with informed consent. If mispriced: information asymmetry added to legal asymmetry, extraction is hidden, classification shifts to Snare with higher suppression and theater.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(public_unitholder_price_discovery, empirical, 'Whether market priced the public unitholder subordination accurately').

omega_variable(
    gp_conflict_allocation_discretion,
    'What is the actual scope of GP discretion in allocating conflicts? Are there implicit or explicit guardrails that constrain the GP from making obviously extractive choices?',
    'Legal analysis of S-1 language and subsequent amendments; historical examination of conflicts that the GP declined to resolve in fund favor; interviews with Blackstone governance leadership; comparison with conflict allocation mechanisms at competing firms (KKR, Apollo, Carlyle)',
    'If GP has nearly absolute discretion: suppression and extraction are very high. If implicit norms or legal doctrine constrain GP choices: suppression and extraction are lower. Affects whether this is true Snare or bounded Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gp_conflict_allocation_discretion, empirical, 'Actual scope of GP discretion in conflict allocation').

omega_variable(
    regulatory_harmonization_timeline,
    'What is the realistic timeline for SEC/regulatory action to impose uniform fiduciary duty standards that would eliminate the legal basis for the S-1 conflict allocation hierarchy?',
    'Tracking of pending SEC rule-making on alternative asset manager governance; analysis of recent litigation outcomes on fiduciary duty for dual-class structures; survey of institutional investor voting on governance proposals; prediction models based on political cycles and regulatory appetite',
    'If timeline is 5-10 years: scaffold sunset is credible, constraint erodes rapidly. If timeline is 20+ years: scaffold sunset is aspirational, constraint persists through regulatory delay. Affects mandatrophy analysis and whether constraint is truly Scaffold or falsely labeled.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_harmonization_timeline, empirical, 'Timeline for regulatory standards convergence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(blackstone_conflicts_of_interest, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bsoi_tr_t0, blackstone_conflicts_of_interest, theater_ratio, 0, 0.62).
narrative_ontology:measurement(bsoi_tr_t8, blackstone_conflicts_of_interest, theater_ratio, 8, 0.66).
narrative_ontology:measurement(bsoi_tr_t16, blackstone_conflicts_of_interest, theater_ratio, 16, 0.68).

% Extraction over time
narrative_ontology:measurement(bsoi_be_t0, blackstone_conflicts_of_interest, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(bsoi_be_t8, blackstone_conflicts_of_interest, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(bsoi_be_t16, blackstone_conflicts_of_interest, base_extractiveness, 16, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(blackstone_conflicts_of_interest, enforcement_mechanism).
narrative_ontology:affects_constraint(blackstone_conflicts_of_interest, gp_fee_structure_asymmetry).
narrative_ontology:affects_constraint(blackstone_conflicts_of_interest, information_access_allocation).
narrative_ontology:affects_constraint(blackstone_conflicts_of_interest, capital_deployment_discretion).

% DUAL FORMULATION NOTE:
% This constraint is part of the alternative assets governance cluster. It is upstream of fee structure asymmetries and capital deployment mechanisms, as the conflict allocation authority in the S-1 enables the GP to exercise discretion in how those mechanisms operate. The constraint family includes: (1) the formal S-1 conflict allocation mechanism (this story, ε=0.58); (2) the operational enforcement of that mechanism through specific capital allocation decisions (downstream, likely ε=0.62-0.68); (3) the information asymmetry about conflict frequency and scope (downstream, likely ε=0.70+). Decomposition is necessary because the formal mechanism and its operational realization have different ε values reflecting different observables (what the S-1 says vs how the GP actually allocates).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(blackstone_conflicts_of_interest, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
