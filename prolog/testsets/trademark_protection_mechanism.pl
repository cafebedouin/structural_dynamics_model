% ============================================================================
% CONSTRAINT STORY: trademark_protection_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trademark_protection_mechanism, []).

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
 *   constraint_id: trademark_protection_mechanism
 *   human_readable: Trademark Protection Mechanism
 *   domain: intellectual_property/commercial_law
 *
 * SUMMARY:
 *   Trademark protection creates a structural tension between legitimate
 *   anti-confusion coordination (preventing consumer deception about product
 *   origin) and monopolistic extraction (using trademark leverage to block
 *   competitors and extract rents from descriptive language). The constraint
 *   exhibits multiple classification types across different agent positions:
 *   pure coordination for established brand holders with global reach; mixed
 *   coordination-extraction for small competitors who need protection for
 *   their own emerging brands while fearing large-holder enforcement; pure
 *   extraction for generic product sellers and small-name businesses trapped
 *   by global trademark registration; performative institutional machinery
 *   for the registration system itself. The extractiveness value (0.52)
 *   reflects moderate asymmetry: the system provides genuine anti-confusion
 *   benefits (supporting the rope/tangled rope assessments) while enabling
 *   monopolistic enforcement that constrains legitimate competition
 *   (supporting the snare assessments). The theater_ratio increase over the
 *   interval (0.35 → 0.55) indicates that trademark enforcement has become
 *   increasingly performative — litigation over brand dilution, likelihood of
 *   confusion, and naked licensing persists despite uncertain empirical
 *   consumer impact, suggesting the system's leverage function has grown
 *   faster than its coordination function.
 *
 * KEY AGENTS:
 *   - Established Brand Holders: Primary beneficiary (institutional/arbitrage) — accumulate brand value, enforce globally, extract through licensing and litigation leverage
 *   - Small Competitors: Secondary beneficiary and victim (moderate/constrained) — benefit from anti-confusion protection for their own brands while facing enforcement threats from larger holders; cannot afford extensive monitoring and litigation
 *   - Generic Product Sellers: Primary victim (powerless/trapped) — use descriptive terms (e.g., 'banana' shaped phone cases, 'glass' screen protectors) and face cease-and-desist orders despite potential descriptive-use defenses
 *   - Consumers: Secondary beneficiary (powerful/mobile) — benefit from brand signaling and reduced search costs; experience the constraint as coordination, not extraction
 *   - Generic Drug Industry: Organized victim (organized/constrained) — can exploit patent expiration but face trademark extension barriers that prevent market entry even after patents expire
 *   - Trademark Registry System: Institutional actor (institutional/arbitrage) — maintains the registration machinery; sees own enforcement through certification as largely performative
 *   - Analytical Observer: Civilizational position (analytical/analytical) — risks naturalizing trademark protection as an inevitable response to information asymmetry rather than a specific legal construction with alternatives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trademark_protection_mechanism, 0.52).
domain_priors:suppression_score(trademark_protection_mechanism, 0.48).
domain_priors:theater_ratio(trademark_protection_mechanism, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trademark_protection_mechanism, extractiveness, 0.52).
narrative_ontology:constraint_metric(trademark_protection_mechanism, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(trademark_protection_mechanism, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trademark_protection_mechanism, tangled_rope).
narrative_ontology:human_readable(trademark_protection_mechanism, "Trademark Protection Mechanism").
narrative_ontology:topic_domain(trademark_protection_mechanism, "intellectual_property/commercial_law").

domain_priors:requires_active_enforcement(trademark_protection_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trademark_protection_mechanism, established_brand_holders).
narrative_ontology:constraint_beneficiary(trademark_protection_mechanism, consumers_via_brand_signaling).
narrative_ontology:constraint_victim(trademark_protection_mechanism, small_competitors).
narrative_ontology:constraint_victim(trademark_protection_mechanism, generic_product_sellers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GENERIC PRODUCT SELLER (SNARE) — Cannot use descriptive terms resembling established marks without legal risk; trapped by global trademark registration and enforcement mechanisms. Faces cease-and-desist letters, litigation costs, and injunctions. Suppression is nearly total: the trademark system provides no legitimate exit for sellers of genuinely similar products with similar names.
constraint_indexing:constraint_classification(trademark_protection_mechanism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SMALL COMPETITOR (TANGLED ROPE) — Benefits from the anti-counterfeiting coordination function (protects their own emerging brand from confusion) while bearing extraction costs (expensive trademark registration, constant monitoring, licensing barriers, dilution litigation threats). Cannot easily exit the system without losing brand protection, but also cannot compete effectively if larger holders deploy aggressive enforcement.
constraint_indexing:constraint_classification(trademark_protection_mechanism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ESTABLISHED BRAND HOLDER (ROPE) — Experiences the constraint primarily as coordination: trademark protection enables brand value accumulation and consumer reliance. Registration is a one-time cost; enforcement is optional leverage. Net beneficiary with arbitrage exits (can abandon marks, license them, or enforce selectively). The constraint solves the real problem of preventing confusion and counterfeiting.
constraint_indexing:constraint_classification(trademark_protection_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CONSUMER (ROPE/POWERFUL) — Benefits from the anti-confusion coordination: trademarks reduce search costs and enable quality signaling. Can arbitrage between brands; mobile across product categories. No suppression experienced — consumers rationally choose brands based on mark associations. This perspective sees pure coordination with minimal extraction.
constraint_indexing:constraint_classification(trademark_protection_mechanism, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: GENERIC DRUG INDUSTRY (TANGLED ROPE) — Paradoxical position: benefits from patent expiration enabling market entry, but faces trademark renewal and brand extension barriers. Experiences the constraint as mixed coordination (must use non-infringing names to market generics safely) and extraction (trademark holders can litigate descriptive use, blocking generics even after patent expiration). Organized but constrained by trademark leverage.
constraint_indexing:constraint_classification(trademark_protection_mechanism, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: TRADEMARK REGISTRY SYSTEM (PITON) — The institutional machinery (USPTO, WIPO, national registries) maintains a largely performative function: trademark registration creates legal leverage but does not independently verify actual brand confusion or counterfeiting harm. The registration system persists through institutional inertia and legal precedent, though much of its verification work is theater — examiners assess formalities, not empirical market impact. Theater ratio is high because the system's outputs (certificates of registration) create legal power disconnected from actual anti-confusion verification.
constraint_indexing:constraint_classification(trademark_protection_mechanism, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a sufficiently abstract view, consumer confusion is a real problem requiring some coordination mechanism; the trademark system is seen as an inevitable institutional response to a structural feature of markets (information asymmetry about product origin). This perspective risks naturalizing what is actually a contingent legal framework with many possible alternatives (mandatory descriptive labeling, government quality certification, reputation systems without monopolistic protection).
constraint_indexing:constraint_classification(trademark_protection_mechanism, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trademark_protection_mechanism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(trademark_protection_mechanism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(trademark_protection_mechanism, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(trademark_protection_mechanism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(trademark_protection_mechanism, TR),
    TR >= 0.70.

:- end_tests(trademark_protection_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The trademark system provides genuine anti-confusion benefits to consumers and brand holders, but the enforcement mechanisms enable significant rent extraction. Brand holders can deploy litigation threats against competitors even when actual consumer confusion is minimal, and the registry system's certification creates legal leverage disconnected from empirical harm. The 0.52 value reflects that roughly half the system's function is coordination (anti-confusion) and half is extraction (monopolistic enforcement). Suppression (0.48): Moderate. Barriers to exit include global registration costs, enforcement threats, and the inability to use descriptive terms even when genericized colloquially. However, suppression is not total — alternatives exist (rebranding, genericization petitions, international arbitration) and some jurisdictions offer descriptive-use defenses. Theater ratio (0.55): Moderate-high. Trademark litigation over brand dilution and likelihood of confusion persists despite uncertain empirical basis for consumer harm. Examiners assess formal registrability (distinctiveness, non-functionality) but do not verify actual market confusion. The registered certificate creates legal power to sue, independent of whether confusion would occur.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between beneficiaries who see coordination and victims who see extraction. Established brand holders experience the trademark system as solving a real problem (anti-confusion) with minimal personal cost (registration + optional enforcement). Generic sellers experience it as pure extraction: their only exit is abandonment of their own brand identity. The small competitor gap is especially diagnostic: they simultaneously benefit (need anti-confusion protection for their own brand) and suffer (face enforcement threat from larger holders). This split reveals the system's hybrid nature — it genuinely solves coordination problems while enabling asymmetric extraction. The registry system's piton classification reveals that the institutional machinery itself has become largely performative: the real power comes from legal precedent and litigation leverage, not from the registry's verification work.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality (d) is derived from their structural relationship to the extraction flow. Established brand holders are beneficiaries with arbitrage options (can abandon marks, license, or enforce selectively) — low d, negative effective extraction. Generic sellers are victims with no exit — high d, maximum effective extraction. Small competitors are mixed: they benefit from anti-confusion protection for their own brands (moderate d as beneficiaries) but also face enforcement constraints (higher d as targets of larger holders). Consumers benefit and are mobile (low d). Generic drug industry is organized but constrained by trademark leverage (moderate-high d). The registry system benefits from institutional maintenance (low d despite performative function). The directionality values feed into the chi formula: χ = ε × f(d) × σ(S). Beneficiaries with arbitrage experience negative or near-zero χ; trapped victims experience maximum χ. The global scope modifier (σ=1.2) amplifies extractiveness for agents operating internationally.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The constraint is tangled rope because it possesses both genuine coordination function (preventing consumer confusion, enabling brand signaling) and asymmetric extraction (litigation leverage, monopolistic enforcement, genericization prevention). The coordination function is real — trademark protection genuinely reduces search costs for consumers and prevents predatory confusion. The extraction function is also real — brand holders use litigation threats to block competitors even when consumer confusion is minimal, and the enforcement system amplifies power asymmetries. Suppression is high (0.48) because trapped agents have no legitimate exit, but not total because alternatives (rebranding, genericization, legal defenses) exist. Theater ratio (0.55) is moderate-high because trademark litigation persists despite uncertain empirical consumer harm — the system's certification creates legal power independent of actual coordination necessity. Mandatrophy is resolved by recognizing that tangled rope constraints often feel like either rope (to beneficiaries) or snare (to victims) depending on structural position. The classification holds across all perspectives when indexed correctly: rope from the beneficiary's position (coordination with minimal cost), tangled rope from the competitor's position (mixed benefit and cost), snare from the powerless seller's position (extraction with no coordination benefit). No single type is correct; the presheaf over all positions is the answer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    brand_confusion_threshold,
    'What degree of phonetic/visual/conceptual similarity actually causes consumer confusion in measurable purchasing behavior?',
    'Empirical consumer testing data (Lanham Act likelihood of confusion surveys); longitudinal purchase pattern analysis; confusion rates by product category and mark distinctiveness',
    'If threshold is low (5-10% confusion): current trademark scope is appropriate and protective. If threshold is high (>30% confusion): many registrations provide no net anti-confusion benefit, suggesting the system is primarily extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(brand_confusion_threshold, empirical, 'Quantification of actual consumer confusion thresholds across product categories').

omega_variable(
    enforcement_necessity_assumption,
    'Does active enforcement of trademark rights create net consumer welfare benefits, or does it primarily enable brand holders to extract through litigation leverage?',
    'Comparative analysis: jurisdictions with varying enforcement intensity (US aggressive enforcement vs EU balanced exhaustion doctrine vs generic-respecting regions); litigation cost analysis; counterfactual consumer confusion rates in low-enforcement regimes',
    'If enforcement is necessary: tangled rope classification is appropriate — real coordination function justifies suppression costs. If enforcement is largely extractive leverage: reclassify as snare or piton (performative enforcement ritual).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_necessity_assumption, empirical, 'Whether trademark enforcement creates net coordination benefit or primarily enables extraction').

omega_variable(
    genericization_pathway_existence,
    'Can product categories escape trademark protection through genericization, or do trademark holders successfully prevent this conversion even when marks become colloquial?',
    'Historical cases (aspirin, escalator, heroin, xerox); success/failure rates of genericization petitions; correlation between colloquial usage and legal genericization outcomes',
    'If genericization is accessible: suppression is moderate and not total. If trademark holders can prevent genericization despite colloquial use: suppression is higher and the constraint more extractive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(genericization_pathway_existence, empirical, 'Whether genericization provides a real exit pathway for trapped sellers').

omega_variable(
    cross_border_enforcement_coherence,
    'Do trademark protections create coordination benefits globally, or do conflicting national registrations fragment the market and increase compliance burden for legitimate sellers?',
    'Analysis of Madrid Protocol adoption; cost data for multi-jurisdictional compliance; case studies of marks with conflicting registrations in different regions',
    'If globally coherent: rope/tangled rope classification. If fragmentary: reclassify as more snare-like (increased suppression via conflicting rules, increased extraction via compliance costs).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cross_border_enforcement_coherence, empirical, 'Whether cross-border trademark systems coordinate effectively or create fragmented extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trademark_protection_mechanism, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tm_tr_t0, trademark_protection_mechanism, theater_ratio, 0, 0.35).
narrative_ontology:measurement(tm_tr_t5, trademark_protection_mechanism, theater_ratio, 5, 0.48).
narrative_ontology:measurement(tm_tr_t10, trademark_protection_mechanism, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(tm_be_t0, trademark_protection_mechanism, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(tm_be_t5, trademark_protection_mechanism, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(tm_be_t10, trademark_protection_mechanism, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trademark_protection_mechanism, information_standard).
narrative_ontology:affects_constraint(trademark_protection_mechanism, patent_monopoly_extraction).
narrative_ontology:affects_constraint(trademark_protection_mechanism, brand_licensing_leverage).

% DUAL FORMULATION NOTE:
% Trademark protection decomposes into anti-confusion coordination (information standard) and monopolistic enforcement (resource allocation). This story treats them as integrated; alternative decomposition would separate empirical consumer confusion rates (mountain/rope claim) from legal enforcement mechanism (tangled rope/snare claim).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(trademark_protection_mechanism, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
