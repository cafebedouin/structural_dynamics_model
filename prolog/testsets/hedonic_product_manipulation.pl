% ============================================================================
% CONSTRAINT STORY: hedonic_product_manipulation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hedonic_product_manipulation, []).

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
 *   constraint_id: hedonic_product_manipulation
 *   human_readable: Hedonic Product Manipulation: Engineered Desire and Consumption Extraction
 *   domain: consumer_economics/psychology/product_design
 *
 * SUMMARY:
 *   Hedonic product manipulation describes the systematic engineering of
 *   consumer desire through psychological research, behavioral targeting,
 *   dark patterns, and designed obsolescence. Manufacturers leverage advances
 *   in neuroscience, behavioral economics, and algorithmic personalization to
 *   align consumer behavior with profit maximization in ways that consumers
 *   did not choose and would not endorse upon reflection. The constraint
 *   exhibits a core tension: coordinating supply with demand is a legitimate
 *   economic function (rope perspective), but the mechanisms used —
 *   intermittent reward scheduling, artificial scarcity, social proof
 *   manipulation, emotional targeting, and interface dark patterns — extract
 *   cognitive and financial resources beyond what genuine preference
 *   satisfaction would require. The extractiveness has grown over the
 *   interval (0.35→0.58) as psychological and digital sophistication has
 *   increased. Theater ratio (0.68) reflects that traditional brand
 *   advertising increasingly functions as lifestyle aesthetics divorced from
 *   product information, while algorithmic targeting operates invisibly. The
 *   constraint is simultaneously a coordination mechanism (manufacturers
 *   matching products to revealed desires) and an extraction system
 *   (engineering desire itself to match profitable product categories). This
 *   makes it a paradigmatic tangled rope: genuine coordination function
 *   masked by and entangled with asymmetric extraction.
 *
 * KEY AGENTS:
 *   - Consumer Subjects: Primary victims (powerless/trapped) — face engineered desire loops, behavioral nudges, and psychological targeting with limited awareness or exit options
 *   - Product Manufacturers: Primary beneficiaries (institutional/arbitrage) — capture high margins through behavioral control; exit available (alternative design) but arbitrage reward for staying high
 *   - Marketing Industry: Secondary beneficiary (institutional/arbitrage) — psychological research, targeting platforms, creative services; economically dependent on extraction mechanisms
 *   - Consumer Advocates: Secondary victim (moderate/constrained) — push for transparency and regulation but face resource asymmetry and coordinated industry resistance
 *   - Regulatory Coalition: Organized actor (organized/constrained) — EU, FTC, behavioral economists building transparency mandates and dark pattern bans; constrained by lobbying and cross-border enforcement complexity
 *   - Design Professionals: Structurally ambiguous (institutional/constrained to identity_locked) — may be either externally constrained by economic incentives or identity-fused to manipulation as legitimate practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hedonic_product_manipulation, 0.58).
domain_priors:suppression_score(hedonic_product_manipulation, 0.65).
domain_priors:theater_ratio(hedonic_product_manipulation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hedonic_product_manipulation, extractiveness, 0.58).
narrative_ontology:constraint_metric(hedonic_product_manipulation, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(hedonic_product_manipulation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hedonic_product_manipulation, tangled_rope).
narrative_ontology:human_readable(hedonic_product_manipulation, "Hedonic Product Manipulation: Engineered Desire and Consumption Extraction").
narrative_ontology:topic_domain(hedonic_product_manipulation, "consumer_economics/psychology/product_design").

domain_priors:requires_active_enforcement(hedonic_product_manipulation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hedonic_product_manipulation, product_manufacturers).
narrative_ontology:constraint_beneficiary(hedonic_product_manipulation, marketing_industry).
narrative_ontology:constraint_victim(hedonic_product_manipulation, consumer_autonomy).
narrative_ontology:constraint_victim(hedonic_product_manipulation, cognitive_resources).
narrative_ontology:constraint_victim(hedonic_product_manipulation, household_budgets).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSUMER SUBJECT (SNARE) — Consumers face engineered desire loops (hedonic adaptation, social proof, scarcity framing, intermittent rewards) with no viable exit. Cognitive and financial extraction is high; suppression is structural (neuroscientific understanding of their own vulnerabilities is withheld; alternative consumption narratives are marginalized). The constraint operates through psychological capture rather than material barriers — the consumer is behaviorally trapped even if legally mobile.
constraint_indexing:constraint_classification(hedonic_product_manipulation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CONSUMER ADVOCATE (TANGLED ROPE) — Faces coordinated resistance from industry: legal barriers to disclosure (trade secret protection), resource asymmetry (industry legal budgets), and coordination failures among fragmented consumer groups. Benefits from some coordination gains (awareness campaigns, regulatory pressure) but bears high costs (litigation, institutional capture). Constrained exit due to career dependence and political pressure.
constraint_indexing:constraint_classification(hedonic_product_manipulation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MANUFACTURER (ROPE) — Experiences the constraint as pure coordination: converting psychological research into product features that align consumer behavior with business goals. Genuine coordination function (matching supply to revealed preference) exists alongside asymmetric extraction. High-margin products, behavioral targeting, and designed obsolescence generate profits. Exit is available (pivot to less manipulative design) but arbitrage reward for staying high.
constraint_indexing:constraint_classification(hedonic_product_manipulation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY COALITION (SCAFFOLD) — Organized actors (consumer protection agencies, behavioral economics advocates, EU digital regulation) are building alternative frameworks: algorithmic transparency mandates, dark pattern bans, cooling-off periods, addiction disclosure requirements. These represent a genuine sunset pathway — as regulation matures, the behavioral extraction mechanism loses legal cover. Constrained by industry lobbying but with institutional coordination power.
constraint_indexing:constraint_classification(hedonic_product_manipulation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: ADVERTISING RITUAL (PITON) — Traditional brand advertising (TV, billboard, print) persists despite declining effectiveness and high theater. The mechanism has atrophied — targeted digital advertising produces higher ROI with lower costs — yet traditional formats survive through institutional inertia, creative culture identity, and sunk-cost psychology. Theater ratio exceeds functional contribution. Maintained by aesthetic and professional commitments rather than efficacy.
constraint_indexing:constraint_classification(hedonic_product_manipulation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational view, some hedonic bias is inherent to human psychology: the capacity to be moved by beauty, novelty, and social signals is a fundamental feature of human cognition, not a contingent artifact. Desire-responsive behavior is natural and inevitable. This perspective risks naturalizing what is actually a specific engineering of desire through psychological research, market segmentation, and deliberate feature design. The engine will flag this as a false summit.
constraint_indexing:constraint_classification(hedonic_product_manipulation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hedonic_product_manipulation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hedonic_product_manipulation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hedonic_product_manipulation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hedonic_product_manipulation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hedonic_product_manipulation, TR),
    TR >= 0.70.

:- end_tests(hedonic_product_manipulation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Consumers experience wealth extraction (premium pricing for hedonic bundling), time extraction (engagement loops), and cognitive extraction (attention capture, decision fatigue). But extraction is not maximal because some preference-satisfaction does occur — consumers do derive utility from products, not zero utility. The extractiveness is sustained by psychological sophistication that has grown over the interval (0.35→0.58), reflecting advances in behavioral targeting and interface design. Suppression (0.65): Moderate-high. Structural barriers include: (1) information asymmetry — psychological research underlying manipulation is proprietary and not disclosed; (2) cognitive barriers — biases are themselves opaque to the subjects experiencing them; (3) coordination failure — fragmented consumers cannot organize demand for alternatives; (4) institutional capture — regulatory bodies are lobbied heavily; (5) cultural normalization — manipulation is treated as neutral marketing practice. Theater ratio (0.68): High. Traditional advertising (TV, print, billboards) is largely performative — brand storytelling divorced from product information. Digital advertising is more functionally targeted but operates invisibly, creating a different theater: performance for the algorithm, not the consumer. The theater ratio has grown (0.52→0.68) as advertising has become more aestheticized and algorithmic mechanisms more opaque.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence: manufacturer sees rope (coordination), consumer sees snare (extraction), regulator sees scaffold (temporary with sunset), traditional advertiser sees piton (degraded theater), design professional sees identity_locked or constrained tangled_rope depending on cognitive capture, and analytical observer risks false summit (naturalizing engineered desire). This divergence reflects genuine structural differences in how agents experience the constraint, not merely subjective interpretation. The manufacturer's rope classification is not wrong from their perspective — they are solving a coordination problem. The consumer's snare classification is not wrong from their perspective — they are trapped in engineered desire loops. Both are true simultaneously, revealing the constraint as a genuine tangled rope with perspectival inversion: what looks like coordination to the beneficiary looks like extraction to the victim.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position in the extraction flow. Manufacturers and the marketing industry are beneficiaries with arbitrage options (d ≈ 0.15, low effective extraction); they benefit from the constraint and can exit but receive reward for staying. Consumer subjects are victims with trapped or identity_locked exit (d ≈ 0.95 for trapped, d ≈ 0.89 for identity_locked if they've internalized desire), bearing full or near-full effective extraction. Consumer advocates are constrained-exit victims (d ≈ 0.70), bearing costs but with partial agency through regulatory pressure. The identity_locked classification applies when consumers have internalized the extracted desires — they believe the hedonic bundling reflects their 'true preferences,' making exit psychologically impossible despite structural mobility. This distinguishes suppression into structural (regulatory barriers, information asymmetry) and internalized (consumers can't imagine themselves wanting non-manipulative products) components.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through identifying the coordinate-extraction hybrid structure. The coordination function (matching supply to demand) is genuine and provides real value. The extraction function (engineering demand itself to maximize profit margin rather than consumer utility) is also genuine and extracts resources. The constraint exists because both functions are operationally entangled — you cannot have the coordination without enabling the extraction, and the extraction would be unprofitable without the coordination function as cover. Mandatrophy is resolved by recognizing that the classification depends on what level of analysis you examine: at the micro level (individual product), the constraint looks like rope (I want this product, I buy it, satisfaction occurs). At the macro level (market trajectory), the constraint looks like snare (systematically engineered desires keep extracting wealth despite hedonic adaptation). The tangled_rope classification captures both levels simultaneously. The theater ratio increasing (0.52→0.68) while extractiveness increases suggests that the constraint is losing its coordination legitimacy — theater is substituting for function — and becoming progressively more snare-like. The regulatory coalition's scaffold perspective offers a genuine exit pathway (dark pattern bans, transparency mandates, cooling-off periods) that would reduce extraction while maintaining coordination. Whether this sunset is achievable depends on whether regulatory escape velocity (omega variable) can outpace industry innovation in new manipulation tactics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_vs_preference_satisfaction,
    'Is hedonic manipulation extraction or preference satisfaction? Does meeting revealed desire constitute genuine autonomy-respecting exchange, or does engineered desire invalidate the consent frame?',
    'Longitudinal studies comparing consumer satisfaction trajectories: immediate pleasure (hedonic treadmill effect) vs long-term utility (post-purchase regret, financial strain, debt accumulation); behavioral evidence of preference reversal after emotional cooling-off periods',
    'If manipulation satisfies genuine underlying preferences: constraint reclassifies as rope (coordination of supply and demand). If manipulation creates false preferences that conflict with long-term values: constraint remains snare/tangled_rope (extraction of cognitive and financial resources). Confidence heavily determines whether extraction appears.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_vs_preference_satisfaction, conceptual, 'Whether hedonic manipulation satisfies or contradicts consumer autonomy').

omega_variable(
    psychological_mechanism_attribution,
    'Which specific psychological mechanisms (social proof, scarcity framing, intermittent reward, personalization, dark patterns) are core to the extraction, vs which are epiphenomenal to normal marketing?',
    'Controlled A/B testing of products with and without specific manipulation techniques; measurement of behavioral change attribution; identification of mechanisms that work across products vs domain-specific effectiveness',
    'If core mechanisms are sparse and highly effective: extractiveness can be localized and regulated (targeted dark pattern bans). If extraction is distributed across many overlapping psychological biases: suppression is higher and regulatory intervention less precise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(psychological_mechanism_attribution, empirical, 'Attribution of psychological extraction mechanisms').

omega_variable(
    industry_cognitive_capture,
    'To what extent do design professionals, marketers, and product teams internalize the extraction logic as legitimate business practice, making the constraint self-perpetuating through professional identity fusion?',
    'Career trajectory analysis: do practitioners who question manipulation tactics face career penalties? Do design programs teach manipulation as neutral skill or flag it as ethical concern? Organizational culture studies of product teams.',
    'If high identity fusion: practitioners see manipulation as natural and necessary (identity_locked exit), making supply-side regulatory change difficult. If low fusion: practitioners are constrained by economic incentives but not psychologically captured, leaving room for regulatory nudges and professional norm shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(industry_cognitive_capture, empirical, 'Industry cognitive capture of extraction logic through professional identity').

omega_variable(
    consumer_collective_action_possibility,
    'Can consumers organize around demand for non-manipulative alternatives, or is the collective action problem itself an instance of the suppression mechanism?',
    'Natural experiments: emergence of anti-consumption movements, sharing economies, simplicity trends; measurement of market share captured by ''transparent design'' or ''low-manipulation'' product positioning; analysis of why such movements remain niche despite high stated preference for them',
    'If collective action is feasible: consumers have true exit option (mobile exit), and classification shifts from snare toward constrained. If collective action fails systematically: suppression is higher than base metrics suggest, and snare classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_collective_action_possibility, empirical, 'Whether consumer collective action against manipulation is structurally possible').

omega_variable(
    regulatory_escape_velocity,
    'Do dark pattern bans and transparency mandates reduce extraction, or do they provoke migration to new manipulation tactics that regulators have not yet categorized?',
    'Longitudinal regulatory case studies: measurement of extraction metrics pre- and post-regulation; identification of new manipulation techniques emerging after dark pattern bans; analysis of EU GDPR and digital market rules efficacy',
    'If regulation reduces extraction durably: scaffold perspective confirmed — sunset is real. If manipulation evolves faster than regulation: sunset is aspirational, and constraint remains structurally entrenched (snare/tangled_rope with higher confidence).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_escape_velocity, empirical, 'Whether regulation can durably reduce hedonic manipulation or provokes tactical evolution').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hedonic_product_manipulation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hedonic_tr_t0, hedonic_product_manipulation, theater_ratio, 0, 0.52).
narrative_ontology:measurement(hedonic_tr_t5, hedonic_product_manipulation, theater_ratio, 5, 0.62).
narrative_ontology:measurement(hedonic_tr_t10, hedonic_product_manipulation, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(hedonic_be_t0, hedonic_product_manipulation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hedonic_be_t5, hedonic_product_manipulation, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(hedonic_be_t10, hedonic_product_manipulation, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hedonic_product_manipulation, resource_allocation).
narrative_ontology:boltzmann_floor_override(hedonic_product_manipulation, 0.18).
narrative_ontology:affects_constraint(hedonic_product_manipulation, algorithmic_attention_capture).
narrative_ontology:affects_constraint(hedonic_product_manipulation, social_proof_signaling_markets).
narrative_ontology:affects_constraint(hedonic_product_manipulation, planned_obsolescence_cycles).

% DUAL FORMULATION NOTE:
% Hedonic product manipulation is upstream of specific industry implementations (algorithmic attention capture, social proof signaling) but distinct from them. The constraint represents the general mechanism of desire engineering; specific applications have their own extractiveness values reflecting domain-specific implementations. Linked family: generic hedonic manipulation → attention capture in social media → social proof in e-commerce → planned obsolescence in consumer durables.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hedonic_product_manipulation, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
