% ============================================================================
% CONSTRAINT STORY: content_moderation_scalability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_content_moderation_scalability, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: content_moderation_scalability
 *   human_readable: Content Moderation Scalability Dilemma
 *   domain: platform_governance/information_infrastructure
 *
 * SUMMARY:
 *   Content moderation at platform scale presents a fundamental coordination
 *   problem disguised as a scalability challenge. Platforms must
 *   simultaneously: (1) prevent harm and illegal content, (2) preserve
 *   legitimate speech and due process, (3) maintain economic viability, and
 *   (4) operate transparently. The constraint exhibits all six DR types from
 *   different structural positions. For powerless content creators and
 *   marginalized communities, moderation is a snare — extraction of speech
 *   rights with no meaningful appeal. For platform operators and advertisers,
 *   moderation is a coordination mechanism — removing extreme content that
 *   would reduce advertiser trust and regulatory compliance. For mainstream
 *   users, it is tangled rope — genuine safety benefit alongside suppression
 *   costs. For regulatory bodies, it is temporary scaffolding — transparency
 *   mandates and appeals processes building toward decentralized
 *   alternatives. For the moderation apparatus itself, it is piton —
 *   performative ritual disconnected from actual fairness. For the analytical
 *   observer, it risks appearing as a scalability law of nature — you cannot
 *   have scale, accuracy, and transparency simultaneously. But this 'law' is
 *   actually the product of deliberate architectural choices: optimizing for
 *   scale and profit margin at the cost of precision and fairness.
 *
 * KEY AGENTS:
 *   - Platform Operators: Primary beneficiary (institutional/arbitrage) — extract economic value and regulatory compliance through low-cost automated moderation; capture network effects
 *   - Advertisers: Secondary beneficiary (powerful/arbitrage) — benefit from brand-safe content environments; moderation enables their participation
 *   - Content Creators: Primary victim (powerless/trapped) — economically dependent on platform distribution; subject to arbitrary algorithmic decisions with opaque appeals
 *   - Marginalized Communities: Secondary victim (powerless/trapped) — disproportionately suppressed by biased moderation; lack organizational capacity to appeal or coordinate
 *   - Mainstream Users: Mixed (moderate/constrained) — benefit from safety but pay costs in false positives, filter bubbles, suppressed legitimate speech
 *   - Regulatory Bodies: Organized intervener (organized/constrained) — building transparency mandates and appeals mechanisms as sunset clauses toward decentralized alternatives
 *   - Moderation Apparatus: Institutional actor (institutional/arbitrage) — maintains performative ritual; benefits from continued opacity and scale pressure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(content_moderation_scalability, 0.58).
domain_priors:suppression_score(content_moderation_scalability, 0.68).
domain_priors:theater_ratio(content_moderation_scalability, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(content_moderation_scalability, extractiveness, 0.58).
narrative_ontology:constraint_metric(content_moderation_scalability, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(content_moderation_scalability, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(content_moderation_scalability, tangled_rope).
narrative_ontology:human_readable(content_moderation_scalability, "Content Moderation Scalability Dilemma").
narrative_ontology:topic_domain(content_moderation_scalability, "platform_governance/information_infrastructure").

domain_priors:requires_active_enforcement(content_moderation_scalability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(content_moderation_scalability, platform_operators).
narrative_ontology:constraint_beneficiary(content_moderation_scalability, advertisers).
narrative_ontology:constraint_beneficiary(content_moderation_scalability, mainstream_users).
narrative_ontology:constraint_victim(content_moderation_scalability, content_creators).
narrative_ontology:constraint_victim(content_moderation_scalability, marginalized_communities).
narrative_ontology:constraint_victim(content_moderation_scalability, platform_reliability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POWERLESS CONTENT CREATOR (SNARE) — Faces algorithmic moderation with no meaningful appeal mechanism. Career depends on platform distribution; cannot exit without economic consequences. Subject to suppression and inconsistent enforcement with no transparency. Maximum extraction — bears all costs of platform safety theater while generating revenue captured by platform.
constraint_indexing:constraint_classification(content_moderation_scalability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MARGINALIZED COMMUNITIES (SNARE) — Moderation algorithms exhibit documented bias against marginalized voices; removal rates and false positives are disproportionate. Cannot organize effective appeals (organizational capacity barrier). Language diversity exacerbates algorithmic failure. Trapped by platform dependence but simultaneously suppressed by moderation systems that treat minority expression as higher risk.
constraint_indexing:constraint_classification(content_moderation_scalability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: PLATFORM OPERATORS & ADVERTISERS (ROPE) — Experience moderation as coordination mechanism: removing extreme content enables advertiser trust and reduces regulatory risk. Scale challenges are real but manageable through distributed enforcement. Primary beneficiaries. Effective extraction flows toward this actor. Arbitrage option exists (can switch to different moderation architectures or alternative platforms without existential risk).
constraint_indexing:constraint_classification(content_moderation_scalability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MAINSTREAM USERS (TANGLED ROPE) — Benefit from moderation (reduced harassment, cleaner feeds). But costs are real: false positives suppress legitimate speech, moderation creates filter bubbles, outsourcing safety burden to private algorithms. Cannot fully exit (network effects) but face high switching costs. Genuine coordination benefit exists alongside asymmetric extraction toward platform operators.
constraint_indexing:constraint_classification(content_moderation_scalability, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY BODIES (SCAFFOLD) — Organized oversight (EU DSA, transparency requirements, appeal mechanisms) represents temporary scaffolding toward decentralized or community-based moderation. Sunset logic: as transparency mandates force algorithm disclosure and appeals processes become mandatory, the opacity that enables current extraction mechanisms decays. Suppression remains high during transition but declining trajectory visible.
constraint_indexing:constraint_classification(content_moderation_scalability, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: MODERATION THEATER (PITON) — The removal/reinstatement cycle, appeals processes, and transparency reports are substantially performative. High theater_ratio reflects that moderation claims (systematic, fair, transparent) are undermined by scale realities (automated decisions, impossible appeals at 500M+ daily uploads). The ritual persists through regulatory pressure and user expectation, but actual verification of moderation quality is minimal. Theater is increasing as regulations force more reporting without improving actual fairness.
constraint_indexing:constraint_classification(content_moderation_scalability, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / SCALABILITY MOUNTAIN (MOUNTAIN) — From analytical distance, the constraint appears as natural law: you cannot simultaneously achieve (1) scale (billions of users), (2) accuracy (high-fidelity moderation decisions), and (3) transparency (explainable reasoning for each decision). At least one must be sacrificed. This perspective risks naturalizing what is actually a contingent choice: platforms have optimized for scale and profit margin at the cost of accuracy and transparency. The mountain framing masks the institutional choices embedded in the trilemma.
constraint_indexing:constraint_classification(content_moderation_scalability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(content_moderation_scalability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(content_moderation_scalability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(content_moderation_scalability, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(content_moderation_scalability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(content_moderation_scalability, TR),
    TR >= 0.70.

:- end_tests(content_moderation_scalability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts from powerless content creators and marginalized communities (who bear suppression costs with no compensation) toward platform operators (who benefit from engagement-maximizing algorithms and regulatory compliance without moderation cost). The extraction is not absolute — platforms do invest in some moderation infrastructure — but the architecture systematically underinvests in fairness for low-revenue user segments while over-optimizing for scale and profitability. The measurement trajectory from 0.38 to 0.58 reflects accumulating extraction: as platforms scaled and competition intensified, moderation-cost-cutting accelerated, shifting more burden onto creators and marginalized voices. Suppression (0.68): High. Multiple suppression mechanisms: (1) algorithmic bias that treats minority expression as higher risk, (2) appeal processes that are too burdensome for individuals to navigate at scale, (3) opacity that prevents understanding why decisions were made, (4) network effects lock content creators into platform dependence despite extraction. Theater ratio (0.64): Moderate-high. The moderation ritual — published removal statistics, transparency reports, appeals processes, content restoration — is substantially performative. Platforms claim systematic fairness while operating opaque algorithms at scales where fairness verification is impossible. Transparency reports show removal statistics but not error rates or bias audit results. Appeals processes exist but overturn rates are low and justifications are generic. Theater has increased over the interval as regulatory pressure forced more reporting without improving actual decision quality.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals the constraint's fundamental asymmetry. Beneficiaries see coordination (Rope) — genuine problem-solving through moderation. Victims see extraction (Snare) — arbitrary suppression with no recourse. Organized regulatory bodies see temporary scaffolding (Scaffold) — sunset built into transparency mandates and appeals requirements that will eventually decentralize moderation authority. The moderation apparatus sees its own degradation (Piton) — once-legitimate verification process now mostly theatrical performance disconnected from actual fairness. The analytical observer risks naturalizing the trilemma (Mountain) — 'you cannot have scale, accuracy, and transparency' — but this risks masking the institutional choice: platforms have traded fairness for profitability. The gap is not about different facts; it is about different structural positions in the extraction flow.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (platform operators, advertisers, mainstream users during normal conditions) experience low effective extraction — the constraint solves genuine coordination problems for them (removing illegal content, preventing harassment, maintaining advertiser trust). Their directionality d values are low (0.1-0.3 range): they benefit from the constraint more than they bear its costs. Victims (content creators, marginalized communities) experience high effective extraction — they bear suppression costs (algorithmic bias, opaque decisions, expensive appeals) with no benefit. Their d values are high (0.85-0.95 range): they are fully targeted by the extraction mechanism. The key insight is that platforms have architected moderation to minimize their own costs while maximizing extraction from those least able to appeal (powerless creators, marginalized voices). The extraction is not accidental byproduct of scale — it is deliberate design choice to achieve profitability thresholds.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by recognizing that moderation is neither pure coordination (Rope) nor pure extraction (Snare), but a hybrid that depends entirely on which agent you are. For beneficiaries, it is genuinely coordinating — preventing harm, enabling advertiser trust, maintaining platform viability. For victims, it is genuinely extractive — suppression without recourse or compensation. The 'mandatrophy' — is this coordination or extraction? — is dissolved by the perspectival classification showing both are true simultaneously. The ethical problem is not that moderation is inherently extractive, but that platforms have optimized the architecture to maximize extraction from those least able to resist (powerless creators, marginalized voices) while minimizing it for those with economic power (advertisers, platform operators). Mandatrophy resolution requires acknowledging this asymmetry is not accidental but designed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_bias_mechanistic_or_political,
    'Is documented algorithmic bias against marginalized content a result of training data bias (mechanistic), or a deliberate underinvestment in fairness for low-revenue user segments (political extraction)?',
    'Comparative analysis of moderation error rates across user segments when controlling for training data composition; audit of investment in minority-language and minority-community moderation systems vs mainstream language investment',
    'If mechanistic: moderation bias is solvable through better training data and fairness constraints. If political: bias is a feature of extraction architecture, not a bug. Affects classification severity and what remediation looks like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_bias_mechanistic_or_political, empirical, 'Whether algorithmic bias is mechanistic or political').

omega_variable(
    appeal_mechanism_sufficiency,
    'Do human review appeals constitute genuine recourse or performative ritual? What fraction of appeals are actually overturned due to reviewer judgment rather than procedural error?',
    'Audit of appeal outcomes: true reversal rate vs procedural reversals; tracking of appeal rejection justifications; comparison to independent fairness reviews of removed content',
    'If appeals are genuine recourse: moderation is Tangled Rope for appellants. If appeals are ritual: moderation is Snare for all but the most persistent. Affects powerless agent classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appeal_mechanism_sufficiency, empirical, 'Whether appeals constitute genuine recourse or theater').

omega_variable(
    decentralized_moderation_viability,
    'Can decentralized community moderation (federated, user-controlled, algorithmic filter bubbles) actually achieve better accuracy and fairness than centralized platform moderation at scale?',
    'Empirical comparison: Mastodon/federated moderation outcomes vs Twitter/centralized moderation on identical content sets; distributed moderation error rates across consensus mechanisms',
    'If viable: scaffold sunset is real — decentralized alternatives will compete and extract moderation authority. If not viable: platforms cannot exit the moderation extraction problem; current architecture is locked in. Affects whether scaffold sunset is structural or aspirational.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(decentralized_moderation_viability, empirical, 'Whether decentralized moderation can match centralized scale').

omega_variable(
    profit_margin_dependency_on_extraction,
    'How much of platform profitability depends on cheap automated moderation vs. profitable content (engagement-maximizing, ad-friendly, low-moderation-cost)? Could human-scale moderation or better fairness systems be implemented at profitability parity?',
    'Financial modeling: cost structure of current moderation vs human-review alternatives; correlation between moderation cost reduction and profit margin expansion; analysis of advertiser willingness to pay premium for verified fair moderation',
    'If moderation-cost reduction is marginal to profit: platforms have freedom to improve fairness without existential risk. If central: moderation extraction is locked into business model. Affects whether remediation is Scaffold (temporary policy fix) or requires fundamental restructuring.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(profit_margin_dependency_on_extraction, empirical, 'Dependency of platform profit margins on moderation cost reduction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(content_moderation_scalability, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(modscale_tr_t0, content_moderation_scalability, theater_ratio, 0, 0.42).
narrative_ontology:measurement(modscale_tr_t3, content_moderation_scalability, theater_ratio, 3, 0.55).
narrative_ontology:measurement(modscale_tr_t6, content_moderation_scalability, theater_ratio, 6, 0.64).

% Extraction over time
narrative_ontology:measurement(modscale_be_t0, content_moderation_scalability, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(modscale_be_t3, content_moderation_scalability, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(modscale_be_t6, content_moderation_scalability, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(content_moderation_scalability, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(content_moderation_scalability, 0.18).
narrative_ontology:affects_constraint(content_moderation_scalability, algorithmic_bias_in_recommendation).
narrative_ontology:affects_constraint(content_moderation_scalability, platform_speech_concentration).
narrative_ontology:affects_constraint(content_moderation_scalability, advertiser_content_control).

% DUAL FORMULATION NOTE:
% Content moderation scalability decomposes into: (1) moderation_decision_accuracy (ε≈0.25, Mountain — inherent difficulty of classification at scale), (2) moderation_fairness_equity (ε≈0.52, Tangled Rope — coordination function for safety, extraction function for marginalized communities), (3) moderation_appeals_theater (ε≈0.42, Piton — performative appeal ritual). This story integrates all three; downstream constraints inherit the extraction mechanisms from moderation architecture.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(content_moderation_scalability, powerful, 0.35).
constraint_indexing:directionality_override(content_moderation_scalability, moderate, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
