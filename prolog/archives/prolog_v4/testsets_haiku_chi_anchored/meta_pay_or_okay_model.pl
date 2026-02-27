% ============================================================================
% CONSTRAINT STORY: meta_pay_or_okay_model
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_meta_pay_or_okay_model, []).

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
 *   constraint_id: meta_pay_or_okay_model
 *   human_readable: Meta's "Pay or Okay" Data Monetization Model in the EU
 *   domain: technological/platform_governance
 *
 * SUMMARY:
 *   Meta's 'Pay or Okay' model, implemented in response to EU regulatory
 *   pressure (GDPR, DMA), presents users with a binary choice: consent to
 *   comprehensive behavioral tracking for targeted advertising (remaining on
 *   the free service) or pay a subscription fee (approximately €156/year on
 *   iOS) for ad-free access without tracking. This constraint creates a
 *   complex hybrid of coordination and extraction. From Meta's perspective,
 *   it is a coordination mechanism solving the tension between GDPR
 *   compliance and business model continuity. From the perspective of
 *   privacy-conscious or low-income users, it is a snare with suppressed
 *   alternatives. The constraint's extractiveness (0.58) reflects that Meta
 *   captures significant economic value by monetizing user data through
 *   either the consent path (advertiser revenue) or the payment path
 *   (subscription revenue), while suppressing third pathways (e.g., unpaid,
 *   untracked, reduced-functionality service). The constraint exhibits all
 *   six DR types from different observer positions, making it a diagnostic
 *   case for how platform dominance interacts with regulatory regimes.
 *
 * KEY AGENTS:
 *   - Meta Corporation: Primary beneficiary (institutional/arbitrage) — monetizes user data or user payments; has arbitrage options (comply with regulation OR accept fines)
 *   - EU Data Subjects: Primary victim (powerless/trapped) — no genuinely costless exit; trapped by network effects and ubiquity
 *   - Targeted Advertisers: Secondary beneficiary (institutional/arbitrage) — benefit from behavioral targeting data or willingness-to-pay signal from users on the paid tier
 *   - EU Regulatory Authorities: Institutional actor (powerful/arbitrage) — enforce GDPR/DMA; have enforcement capacity but limited ability to mandate unpaid, fully private service without forcing exit
 *   - Privacy-Conscious Users: Secondary victim (moderate/constrained) — can afford subscription but pay premium for privacy; organized through advocacy groups
 *   - Smaller Platforms & Non-Targeted Competitors: Tertiary victim (organized/constrained) — cannot match Meta's network effects; extractive precedent (consent-or-pay) sets industry norms
 *   - Consent Ritual System: Performative institutional actor (institutional/arbitrage) — GDPR consent persists as theater; dark patterns shape choices
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(meta_pay_or_okay_model, 0.58).
domain_priors:suppression_score(meta_pay_or_okay_model, 0.72).
domain_priors:theater_ratio(meta_pay_or_okay_model, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(meta_pay_or_okay_model, extractiveness, 0.58).
narrative_ontology:constraint_metric(meta_pay_or_okay_model, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(meta_pay_or_okay_model, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(meta_pay_or_okay_model, tangled_rope).
narrative_ontology:human_readable(meta_pay_or_okay_model, "Meta's \"Pay or Okay\" Data Monetization Model in the EU").
narrative_ontology:topic_domain(meta_pay_or_okay_model, "technological/platform_governance").

domain_priors:requires_active_enforcement(meta_pay_or_okay_model).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(meta_pay_or_okay_model, meta_corporation).
narrative_ontology:constraint_beneficiary(meta_pay_or_okay_model, targeted_advertisers).
narrative_ontology:constraint_victim(meta_pay_or_okay_model, eu_data_subjects).
narrative_ontology:constraint_victim(meta_pay_or_okay_model, privacy_commons).
narrative_ontology:constraint_victim(meta_pay_or_okay_model, non_targeted_market_competitors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EU DATA SUBJECT (SNARE) — Caught between two asymmetric costs: paying €156/year for privacy or remaining on the platform with comprehensive behavioral tracking. Neither exit is genuinely voluntary. Trapped by network effects (friends/family/professional contacts), trapped by service ubiquity (authentication, marketplace integration), trapped by switching costs (account history, contacts, groups). d≈0.92, f(d)≈1.38, σ=0.9 → χ≈0.72.
constraint_indexing:constraint_classification(meta_pay_or_okay_model, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: META CORPORATION (ROPE) — Primary beneficiary experiencing the constraint as coordination: the policy mediates between EU regulatory pressure (GDPR, Digital Markets Act) and business model continuity. Meta frames consent-as-option as a coordination mechanism solving the 'how do we remain compliant while maintaining monetization?' problem. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06. Negative effective extraction = net beneficiary.
constraint_indexing:constraint_classification(meta_pay_or_okay_model, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: PRIVACY-CONSCIOUS EU USER (TANGLED ROPE) — Partially organized (privacy advocacy groups, consumer unions) but individually constrained. Experiences both coordination benefit (explicit choice between data trade and fee) and extraction (payment tier designed to be prohibitive for low-income users; no third path such as ad-free with limited targeting). d≈0.68, f(d)≈1.02, σ=0.9 → χ≈0.53.
constraint_indexing:constraint_classification(meta_pay_or_okay_model, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: EU REGULATORY AUTHORITIES (TANGLED ROPE) — Institutional actors with enforcement capacity and arbitrage options (threaten massive fines, require business model restructuring). The 'Pay or Okay' model is itself a coordination response to regulatory pressure, yet it contains extractive elements: the payment tier excludes lower-income users; the consent framing disguises behavioral nudging toward the default (free + tracking). d≈0.35, f(d)≈0.36, σ=0.9 → χ≈0.19. Low effective extraction because regulatory bodies have agency, but the constraint does limit their options (cannot mandate unpaid privacy without forcing service exit).
constraint_indexing:constraint_classification(meta_pay_or_okay_model, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: SMALLER PLATFORMS & DATA-MINIMAL COMPETITORS (SNARE) — Cannot match Meta's network effects or monetization optionality. The 'Pay or Okay' model sets an extractive precedent (users must choose between data and payment across the sector). Network effects trap users on Meta, preventing migration to alternative platforms. d≈0.78, f(d)≈1.12, σ=0.9 → χ≈0.72. High extraction because organized competitors are constrained by Meta's market dominance.
constraint_indexing:constraint_classification(meta_pay_or_okay_model, snare,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: CONSENT FRAMEWORK RITUAL (PITON) — GDPR consent is performed as a gateway, but its function is largely theater. Users are nudged toward accepting behavioral tracking through dark patterns (pre-checked boxes, persistent prompts, friction-laden privacy settings). The 'choice' between pay and track is structurally asymmetric in execution. theater_ratio≈0.62. The consent ritual persists because it satisfies GDPR letter (users can choose) while preserving business model (most users choose free+track). The performative element is high because the framing obscures the true asymmetry: paying €156/year is presented as equally available to a student, retiree, or low-income worker.
constraint_indexing:constraint_classification(meta_pay_or_okay_model, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: DIGITAL ALTERNATIVES & REGULATORY INNOVATION (SCAFFOLD) — Civil society organizations, open-source social networks (Mastodon, Bluesky), and regulatory bodies are building alternative pathways with an implicit sunset: federated social networks, data cooperatives, and potentially stricter regulatory frameworks (Digital Services Act enforcement) that could force Meta to restructure. The scaffold is not yet functional at scale but has clear directionality. d≈0.42, f(d)≈0.42, σ=0.9 → χ≈0.22. Moderate suppression because alternatives exist but are still marginalized; sunset is real but distant (5-15 years).
constraint_indexing:constraint_classification(meta_pay_or_okay_model, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(meta_pay_or_okay_model_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(meta_pay_or_okay_model, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(meta_pay_or_okay_model, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(meta_pay_or_okay_model, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(meta_pay_or_okay_model, TR),
    TR >= 0.70.

:- end_tests(meta_pay_or_okay_model_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Meta extracts value through two channels: (1) advertiser revenue from users who consent to tracking (majority path, approximately 85-90% adoption rate), and (2) subscription revenue from privacy-conscious users. The extraction is not maximal (like a pure payday lender) because users do have a formal choice, and a significant minority (though growing slowly) can afford the subscription. However, the asymmetry is severe: the default (free + tracking) is nudged through dark patterns; the payment path is framed as a premium add-on rather than the escape hatch. The 0.58 reflects high but not absolute extraction. Suppression (0.72): High. Multiple barriers suppress alternatives: (1) network effects (cannot leave without losing contacts/professional presence), (2) switching costs (account history, groups, integrations), (3) lack of viable competitors at comparable scale, (4) regulatory regime that implicitly accepts the binary (pay or consent) rather than mandating a third path, (5) dark patterns that nudge toward consent. Theater ratio (0.38): Moderate-low. This constraint has lower theater than many regulatory-capture scenarios because it implements actual functional change (users who pay do get ad-free, untracked service). However, the framing has theatrical elements: the presentation of 'choice' obscures structural asymmetry; consent language disguises the fact that the default (free + tracking) is achieved through nudging, not genuine preference revelation.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is extreme. Meta sees a coordination success story: the policy enables regulatory compliance while preserving business model, creates a market-segmented option (pay for privacy or accept ads), and generates dual revenue streams. EU data subjects see a snare: exit costs are prohibitive, the 'choice' is asymmetrically framed, and the payment tier excludes low-income users. Privacy-conscious users see tangled rope: they appreciate the explicit choice mechanism but recognize extraction in the pricing (premium for what should be a default right). Regulators see tangled rope from another angle: they have forced Meta to offer a choice mechanism (coordination), but the mechanism itself allows continued extraction (through dark patterns and prohibitive pricing). Competitors see snare: the precedent that users must choose between data and payment locks the industry into a binary that advantages those with massive networks and advertisers (Meta). The open-source/alternative platform movement sees scaffold: the current model has a sunset because regulators or markets will eventually force change. The consent ritual sees piton: the performative elements (checkbox, modal dialogs, privacy settings) persist through institutional inertia despite being widely recognized as theater.
 *
 * DIRECTIONALITY LOGIC:
 *   Meta Corporation: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. EU Data Subjects: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction via network effects. Privacy-Conscious Users: Victim + constrained → d≈0.68, f(d)≈1.02. High extraction but moderated by ability to pay. EU Regulators: Powerful + arbitrage → d≈0.35, f(d)≈0.36. Low effective extraction (regulatory bodies have enforcement capacity). Smaller Platforms: Organized + constrained → d≈0.78, f(d)≈1.12. High extraction via precedent and network effects. Consent Ritual: Institutional + arbitrage → d≈0.08, f(d)≈-0.10, but piton classification comes from theater gate (≥0.70), not from directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is RESOLVED through network decomposition: This constraint story explicitly combines coordination (the response to regulatory pressure) with extraction (the asymmetric pricing and dark patterns). The Tangled Rope classification is not a failure to distinguish; it is the accurate classification. The constraint genuinely has both elements. Meta's perspective sees pure coordination (solving the GDPR compliance problem); the data subject's perspective sees pure extraction (snare). The mandatrophy dissolves when we recognize that the constraint is indexed to observer position. From Meta's institutional perspective with arbitrage options, it is a rope-like coordination. From the powerless user's perspective, it is a snare. The classification is not ambiguous; it is perspectival. The Tangled Rope in the analytical (global, civilizational) perspective is the correct unified classification because it acknowledges both the coordination function (explicit choice mechanism) and the extraction mechanism (dark patterns, prohibitive pricing, network effects). The mandatrophy alert would trigger if all perspectives classified as 'rope' (false coordination) — that would indicate we were hiding extraction. But the existence of multiple perspectives, including the snare, prevents this misclassification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gdpr_consent_interpretation,
    'Does the GDPR permit ''consent or payment'' as a binding choice mechanism, or does it implicitly require unpaid-without-tracking as a mandatory third option?',
    'CJEU (Court of Justice of the European Union) ruling on the Meta case; analysis of GDPR Article 21 (right to object) and Article 6 (consent legality); precedent from Schrems II and Planet49 decisions',
    'If payment + consent are the only options: Meta''s model complies (Tangled Rope classification stands). If unpaid-without-full-tracking is required: Meta''s model breaches GDPR (Snare classification for users deepens; Meta shifts from Rope to Tangled Rope). This is the central legal omega.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gdpr_consent_interpretation, empirical, 'GDPR interpretation of consent-or-payment mechanisms').

omega_variable(
    price_elasticity_intent,
    'Is the €156/year price tier designed to be a genuine alternative or a prohibitive barrier disguised as choice?',
    'Meta''s internal pricing rationale (discoverable through litigation); comparative analysis with other platforms'' subscription pricing; income-distribution impact study showing what percentage of EU population can afford the tier',
    'If price is genuinely inclusive (accessible to ~50% of adult EU population): Tangled Rope classification may hold, with moderate suppression. If price is designed as a barrier (target <10% adoption): classification shifts to Snare for the majority of users; suppression increases to 0.85+.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(price_elasticity_intent, empirical, 'Whether pricing is genuine choice or prohibitive barrier').

omega_variable(
    dark_pattern_magnitude,
    'How much of the free-plus-tracking adoption is driven by dark pattern nudging versus genuine preference?',
    'A/B testing comparison: default-accept vs default-deny consent interfaces; field studies of user comprehension; analysis of consent metrics before/after dark pattern removal',
    'If dark patterns account for >60% of tracking consent: the consent component becomes theater, and the classification shifts to Snare (users are not truly choosing but being nudged). If <30%: users are choosing despite asymmetric framing, and Tangled Rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dark_pattern_magnitude, empirical, 'Magnitude of dark pattern influence on consent').

omega_variable(
    network_effects_exit_viability,
    'Can users realistically exit to alternative platforms, or does the network effect create a de facto trap independent of the pay-or-track binary?',
    'Analysis of switching costs (contacts, groups, professional presence) vs network effects (friend density); case studies of successful migrations to Mastodon, Threads, or other platforms; measurement of critical mass thresholds for alternative adoption',
    'If network effects are absolute (cannot exit regardless of mechanism): exit_options should be ''trapped'' even without the pay-or-okay choice, and extractiveness may be intrinsic to platform dominance rather than the specific model. If exit is genuinely possible: the pay-or-okay choice is the primary extraction mechanism, and it is contingent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(network_effects_exit_viability, empirical, 'Whether network effects create exit traps independent of pay-or-okay').

omega_variable(
    regulatory_endgame,
    'Will the Digital Services Act and ongoing CJEU cases force Meta to restructure, or will the pay-or-okay model become the regulatory equilibrium?',
    'Tracking enforcement actions under DSA; CJEU rulings on structural remedies vs fines; adoption of competing regulatory frameworks (AI Act, proposed Data Act); emergence of viable federated alternatives at >5% EU market share',
    'If forced restructuring: scaffold perspective is confirmed, sundown is real (5-10 years), classification may shift. If regulatory equilibrium: pay-or-okay becomes the stable model, suppression remains high, classification is durable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_endgame, empirical, 'Whether regulation will force structural change or entrench the model').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(meta_pay_or_okay_model, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(metapay_theater_t0, meta_pay_or_okay_model, theater_ratio, 0, 0.25).
narrative_ontology:measurement(metapay_theater_t3, meta_pay_or_okay_model, theater_ratio, 3, 0.38).
narrative_ontology:measurement(metapay_theater_t6, meta_pay_or_okay_model, theater_ratio, 6, 0.38).

% Extraction over time
narrative_ontology:measurement(metapay_extract_t0, meta_pay_or_okay_model, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(metapay_extract_t3, meta_pay_or_okay_model, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(metapay_extract_t6, meta_pay_or_okay_model, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(meta_pay_or_okay_model, resource_allocation).
narrative_ontology:affects_constraint(meta_pay_or_okay_model, gdpr_consent_interpretation).
narrative_ontology:affects_constraint(meta_pay_or_okay_model, platform_network_effects).
narrative_ontology:affects_constraint(meta_pay_or_okay_model, dark_pattern_implementation).

% DUAL FORMULATION NOTE:
% The Meta 'Pay or Okay' model can be decomposed into three related constraints: (1) The consent-or-payment binary as a regulatory response mechanism (this story, ε≈0.58, Tangled Rope), (2) GDPR consent architecture as a performative ritual (ε≈0.42, Piton), (3) Dark pattern implementation as a separate extraction mechanism (ε≈0.65, Snare). This story captures the whole system; the network links indicate that the entire constraint cluster is structurally coupled.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(meta_pay_or_okay_model, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
