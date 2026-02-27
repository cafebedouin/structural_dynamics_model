% ============================================================================
% CONSTRAINT STORY: tiktok_us_divestiture_mandate
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tiktok_us_divestiture_mandate, []).

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
 *   constraint_id: tiktok_us_divestiture_mandate
 *   human_readable: TikTok Trust & Safety Divestiture Mandate
 *   domain: geopolitical/technological/economic
 *
 * SUMMARY:
 *   The US TikTok divestiture mandate represents a high-extractiveness
 *   constraint operating at the intersection of geopolitical competition,
 *   tech platform governance, and corporate sovereignty. The nominal
 *   justification—protecting national security by preventing foreign data
 *   access and influence operations—masks a multifaceted extraction mechanism
 *   targeting ByteDance's core asset, US users' service continuity,
 *   international tech equity norms, and precedent-setting authority for
 *   future asset seizures. The constraint exhibits asymmetric coercion:
 *   ByteDance faces criminal/civil penalties for non-compliance; US acquirers
 *   gain politically-subsidized asset access; US competitors gain market
 *   consolidation benefits; users face service discontinuation; Chinese
 *   financial interests face precedent for future targeting. The theater
 *   ratio (0.58) reflects that security justification dominates public
 *   framing while actual mechanisms (forced sale, acquirer vetting,
 *   regulatory control) operate as economically extractive policies.
 *   ByteDance's powerful institutional position is subordinated through
 *   entrenchment—unable to exit without massive losses—producing effective
 *   extraction χ≈0.94 despite institutional power.
 *
 * KEY AGENTS:
 *   - ByteDance (Powerful/Trapped): Corporate victim forced into distressed asset sale; primary extraction target via regulatory coercion
 *   - TikTok US User Base (Powerless/Trapped): Service captives facing discontinuation or forced ownership transition; secondary extraction target through service loss
 *   - US National Security Apparatus (Institutional/Arbitrage): Beneficiary framing mandate as security coordination; primary extractor leveraging enforcement authority
 *   - Competing US Tech Platforms (Institutional/Arbitrage): Secondary beneficiaries gaining market consolidation, reduced foreign competition, regulatory relief
 *   - US Acquirers (Moderate/Constrained): Forced participants in politically-mandated acquisition with regulatory gatekeeping; mixed coordination/extraction from access to 170M users
 *   - International Tech Equity & Chinese Finance (Powerful/Trapped): Tertiary extraction target; faces precedent-setting authority for security-based asset seizure
 *   - Analytical Observer (Analytical/Analytical): Sees constraint as erosion of rules-based international economic order and generalization of coercive asset seizure mechanisms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tiktok_us_divestiture_mandate, 0.68).
domain_priors:suppression_score(tiktok_us_divestiture_mandate, 0.72).
domain_priors:theater_ratio(tiktok_us_divestiture_mandate, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tiktok_us_divestiture_mandate, extractiveness, 0.68).
narrative_ontology:constraint_metric(tiktok_us_divestiture_mandate, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(tiktok_us_divestiture_mandate, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tiktok_us_divestiture_mandate, snare).
narrative_ontology:human_readable(tiktok_us_divestiture_mandate, "TikTok Trust & Safety Divestiture Mandate").
narrative_ontology:topic_domain(tiktok_us_divestiture_mandate, "geopolitical/technological/economic").

domain_priors:requires_active_enforcement(tiktok_us_divestiture_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tiktok_us_divestiture_mandate, us_national_security_apparatus).
narrative_ontology:constraint_beneficiary(tiktok_us_divestiture_mandate, competing_us_social_media_platforms).
narrative_ontology:constraint_victim(tiktok_us_divestiture_mandate, bytedance_corporate_interests).
narrative_ontology:constraint_victim(tiktok_us_divestiture_mandate, tiktok_us_user_base).
narrative_ontology:constraint_victim(tiktok_us_divestiture_mandate, international_tech_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% ByteDance faces coercive divestiture with no genuine alternative: retain US ops and face criminal/civil penalties, or sell at distressed valuation. d≈0.90, f(d)≈1.38, σ=1.0 → χ≈0.94. Effective extraction χ exceeds base ε due to entrenchment and trapped exit.
constraint_indexing:constraint_classification(tiktok_us_divestiture_mandate, snare,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% 170+ million US TikTok users face service discontinuation or forced ownership transition with no input. Suppression: platform shutdown threat, no alternative social graph transfer mechanism. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.97. Maximum extraction from captive user base.
constraint_indexing:constraint_classification(tiktok_us_divestiture_mandate, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% US government frames mandate as coordination mechanism: aligning tech ownership with national security interests, preventing foreign data access, protecting electoral integrity. Experiences constraint as governance function, not extraction. d≈0.10, f(d)≈0.05, σ=1.0 → χ≈0.03. Net beneficiary with low effective extraction from its own perspective.
constraint_indexing:constraint_classification(tiktok_us_divestiture_mandate, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Competing platforms gain market consolidation benefits, reduced regulatory pressure, and strengthened data moat. Beneficiary + arbitrage → d≈0.15, f(d)≈0.10, σ=1.0 → χ≈0.07. Experiences mandate as competitive coordination: removing foreign competitor via policy rather than market competition.
constraint_indexing:constraint_classification(tiktok_us_divestiture_mandate, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% US acquirers have constrained exit (forced participation in politically-mandated acquisition, regulatory risk if deal fails) but also benefit from access to 170M users + AI training data. Requires active enforcement via regulatory approval gates. Mixed coordination (access to user base) and extraction (political conditionality). d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.51. Moderate effective extraction due to acquisition conditionality and asset control.
constraint_indexing:constraint_classification(tiktok_us_divestiture_mandate, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% China-affiliated tech and financial interests face precedent-setting asset seizure disguised as voluntary divestiture. Creates extraction mechanism for future geopolitical disputes: any foreign tech asset can be forced sale under national security pretext. d≈0.88, f(d)≈1.35, σ=1.2 → χ≈1.13. Effective extraction exceeds base due to global scope (σ=1.2) and precedent-setting enforcement.
constraint_indexing:constraint_classification(tiktok_us_divestiture_mandate, snare,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% From civilizational perspective, divestiture mandate represents erosion of rules-based international economic order. Precedent: US weaponization of national security doctrine creates extraction mechanism applicable to any foreign asset. No appeal mechanism, no WTO recourse, theater of due process covers coercive taking. d≈0.75, f(d)≈1.10, σ=1.2 → χ≈0.89. The constraint appears as systematic extraction of foreign capital disguised as security governance.
constraint_indexing:constraint_classification(tiktok_us_divestiture_mandate, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tiktok_us_divestiture_mandate_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(tiktok_us_divestiture_mandate, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(tiktok_us_divestiture_mandate, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(tiktok_us_divestiture_mandate, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tiktok_us_divestiture_mandate_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68): High-moderate. The divestiture mandate forcibly transfers control of a $15B+ asset (estimated 170M users, data, AI training corpus) from Chinese to US ownership without fair-market acquisition process. However, extractiveness is not maximal (0.68 vs 0.80+) because the mandate allows a form of divestiture (transfer to US entity) rather than complete asset destruction. ByteDance retains theoretical upside from acquisition price, though the price is constrained by regulatory conditions (acquirer vetting, no Chinese investor participation, operational control mandates). If the mandate forced asset liquidation at fire-sale prices or destroyed the asset entirely, ε would approach 0.85. The 0.68 reflects extractiveness through coercive transfer + political conditionality, not pure seizure. Suppression (0.72): High. Multiple mechanisms suppress alternatives: (a) regulatory threat of platform shutdown if divestiture deadline missed; (b) no alternative exit—ByteDance cannot retain US ops under any realistic scenario; (c) acquirer control conditionality—any buyer must be vetted by hostile regulatory body; (d) user base is held hostage—service discontinuation threat coerces acceptance. Theater ratio (0.58): Moderate-high. The mandate is presented as security coordination protecting US electoral integrity, data privacy, and user autonomy. The theater derives from: (a) selective application (other foreign platforms with similar data access remain untargeted); (b) acquirer vetting theater—security reviews are opaque and non-appealable; (c) user protection framing when the actual mechanism is forced ownership transfer that may or may not improve user data handling. The theater has increased over time (from 0.35 at initial proposal to 0.58 at implementation) as security justification has been elaborated while enforcement remains coercive.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a classic extraction-disguised-as-coordination pattern with a five-way perspectival split. ByteDance and users see pure Snare: coercive asset transfer with no exit, no compensation mechanism, no appeal. US national security apparatus sees Rope: legitimate security coordination preventing foreign influence. Competing US tech platforms see Rope: beneficial market consolidation. US acquirers see Tangled Rope: gain access to 170M users (coordination benefit) but face regulatory gatekeeping (extraction cost). International observers see Snare: precedent-setting weaponization of national security doctrine for asset seizure. The analytical observer recognizes this as systematic extraction (Snare) with false-summit Rope framing—the 'security' label naturalizes what is actually coercive transfer of foreign property. The perspectival gap is driven by whether the observer is the extractor (sees Rope) or the extracted-from (sees Snare).
 *
 * DIRECTIONALITY LOGIC:
 *   ByteDance: Powerful institutional actor, but victim of mandate + trapped exit → d≈0.90, f(d)≈1.38. Entrenchment inverts the usual institutional advantage: power becomes liability (large asset worth extracting). US users: Powerless + trapped → d≈0.95, f(d)≈1.42. Maximal extraction—captive service base. US gov: Institutional beneficiary + arbitrage → d≈0.10, f(d)≈0.05. Net extractor, low effective extraction from own perspective (sees mandate as legitimate governance). US tech competitors: Institutional beneficiary + arbitrage → d≈0.15, f(d)≈0.10. Secondary extractors gaining market share. US acquirers: Moderate power + constrained exit (forced participation) + mixed costs/benefits → d≈0.55, f(d)≈0.75. Constrained exit (can't refuse regulatory deal) pushes d higher despite institutional framing. International observers: Powerful but victim of precedent + trapped future exit → d≈0.88, f(d)≈1.35. Global scope σ=1.2 amplifies effective extraction to χ≈1.13.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: The mandate is legitimately classified as a Snare with high effective extraction χ≈0.94 for ByteDance (powerful but trapped) and χ≈0.97 for users (powerless and trapped). The secondary Rope perspectives from US government and tech competitors are PERSPECTIVAL TRUTHS for those actors, not alternate truths—they genuinely experience the mandate as coordination/beneficial policy because they are the net beneficiaries. The mandatrophy resolution hinges on recognizing that a single constraint can be simultaneously Snare from the extracted-from perspective and Rope from the extractor perspective, with no contradiction. The false-summit detection system flags the national security apparatus's Rope classification as potentially false (theater_ratio=0.58 indicates significant performative content), but the classification is not invalid—it is the beneficiary's accurate perspective. The mandate's mandatrophy is RESOLVED because all six perspectives are structurally consistent with ε=0.68, suppression=0.72, theater=0.58. The constraint is not ambiguous about type; it is unambiguous in exhibiting extraction (high ε and suppression) with some performative justification (moderate theater). The Snare classification is correct for ByteDance/users/international observers; the Rope classification is correct for extractors; the Tangled Rope is correct for coerced acquirers. No single type mischaracterizes the constraint—the presheaf of perspectives captures the full extraction structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    national_security_sincerity,
    'Is the divestiture mandate genuinely motivated by data security/foreign influence concerns, or is it economic competition policy disguised as national security?',
    'Comparative analysis: Does US apply equivalent pressure to other foreign platforms (WeChat, Douyin, Telegram) with similar data access risks? Are security thresholds applied uniformly or selectively to US competitors?',
    'If genuine security: constraint is legitimate coordination (Rope from gov perspective, Snare from ByteDance due to entrenchment). If competitive policy: constraint is pure extraction (Snare throughout, mandate is economic predation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(national_security_sincerity, empirical, 'Whether mandate is security-motivated or competitive policy disguised as security').

omega_variable(
    data_exfiltration_risk_magnitude,
    'How credible is the claim that ByteDance would/could systematically exfiltrate US user data to Chinese intelligence? What is the actual exploitation risk vs the asserted risk?',
    'Technical audit: user data access patterns, server location, encryption protocols. Intelligence assessment: documented instances of ByteDance complying with Chinese state data demands vs documented instances of resistance.',
    'If risk is high: security extraction rationale gains credibility; constraint moves toward Rope (coordination around genuine threat). If risk is low/speculative: mandate is security theater masking economic extraction (Snare confirmed).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(data_exfiltration_risk_magnitude, empirical, 'Actual magnitude of ByteDance data exfiltration risk to US national security').

omega_variable(
    forced_sale_valuation_extraction,
    'Does the divestiture mandate force TikTok to sell at below-market valuation, and if so, how much of the mandate''s ''extraction'' comes from unfair pricing vs regulatory enforcement?',
    'Valuation analysis: TikTok''s standalone operating value (revenue, user metrics, ARPU) vs acquisition offer price. Historical comparison: forced divestitures (AT&T breakup, Microsoft antitrust settlements) and whether imposed prices exceeded or fell below fair market value.',
    'If forced at significant haircut: extraction is explicit (Snare confirmed). If fair-market acquisition: extraction is softer (regulatory compliance cost rather than asset seizure).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(forced_sale_valuation_extraction, empirical, 'Whether divestiture mandate forces below-market sale price').

omega_variable(
    precedent_weaponization,
    'Does this mandate establish precedent for weaponizing national security doctrine to force divestiture of any foreign-controlled asset? How binding is this precedent on future administrations?',
    'Institutional analysis: Does authorization language in the mandate restrict future application to TikTok, or does it create general authority for national security-based forced sales? Are other foreign tech assets subsequently targeted under same logic?',
    'If bounded to TikTok: constraint is specific extraction (Snare). If precedent is generalized: constraint becomes extraction mechanism for all future geopolitical disputes (systemic Snare with civilizational scope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precedent_weaponization, conceptual, 'Whether mandate creates generalizable precedent for security-based asset seizure').

omega_variable(
    user_welfare_coordination_vs_extraction,
    'Does the divestiture mandate improve or degrade service quality, data privacy, and algorithm transparency for the 170M US TikTok users? Is the claim of ''protecting users'' valid or theater?',
    'Post-divestiture audit: user data handling under new ownership, algorithm transparency, content moderation consistency, service continuity. Comparison: TikTok''s privacy practices pre-mandate vs US acquirer''s practices on other platforms.',
    'If new ownership improves privacy/autonomy: constraint has real coordination function protecting users (moves toward Rope/Scaffold). If new ownership reproduces data extraction or worsens service: mandate was pure extraction theater targeting users (Snare confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(user_welfare_coordination_vs_extraction, empirical, 'Whether US ownership transition improves or degrades user data welfare').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tiktok_us_divestiture_mandate, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tiktok_tr_t0, tiktok_us_divestiture_mandate, theater_ratio, 0, 0.35).
narrative_ontology:measurement(tiktok_tr_t6, tiktok_us_divestiture_mandate, theater_ratio, 6, 0.48).
narrative_ontology:measurement(tiktok_tr_t12, tiktok_us_divestiture_mandate, theater_ratio, 12, 0.58).

% Extraction over time
narrative_ontology:measurement(tiktok_be_t0, tiktok_us_divestiture_mandate, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(tiktok_be_t6, tiktok_us_divestiture_mandate, base_extractiveness, 6, 0.65).
narrative_ontology:measurement(tiktok_be_t12, tiktok_us_divestiture_mandate, base_extractiveness, 12, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tiktok_us_divestiture_mandate, enforcement_mechanism).
narrative_ontology:affects_constraint(tiktok_us_divestiture_mandate, chinese_tech_asset_seizure_precedent).
narrative_ontology:affects_constraint(tiktok_us_divestiture_mandate, tiktok_algorithm_transparency_mandate).
narrative_ontology:affects_constraint(tiktok_us_divestiture_mandate, us_foreign_tech_screening_authority).

% DUAL FORMULATION NOTE:
% The TikTok divestiture mandate decomposes into two analytically distinct constraints: (1) SECURITY COORDINATION (ε≈0.15, Rope) — legitimate inter-agency coordination to prevent foreign data access through acquirer vetting and operational control requirements; (2) ASSET EXTRACTION (ε≈0.68, Snare) — forced transfer of profitable asset at constrained valuation to US-controlled entity. The combined constraint exhibits both functions but is dominantly extractive (ε=0.68) because the asset transfer mechanism overpowers the security coordination benefit. If the mandate were purely security-focused (no forced divestiture, just data access controls), ε would be 0.15 (Rope). The 0.68 classification reflects that the mandate's primary mechanism is extractive asset transfer, not security protocols.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tiktok_us_divestiture_mandate, institutional, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
