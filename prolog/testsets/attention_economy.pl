% ============================================================================
% CONSTRAINT STORY: attention_economy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_attention_economy, []).

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
 *   constraint_id: attention_economy
 *   human_readable: Attention Economy Extraction and Behavioral Lock-In
 *   domain: social/technological/economic
 *
 * SUMMARY:
 *   The attention economy represents a structural constraint where user
 *   attention is harvested, measured, aggregated, and sold as a commodity to
 *   advertisers and information brokers. The constraint operates through
 *   platform algorithms designed to maximize engagement via intermittent
 *   reinforcement, asymmetric information (users cannot see or predict
 *   recommendation logic), and network effects that make exit costly. Unlike
 *   coordination mechanisms that distribute benefits relatively equally, the
 *   attention economy concentrates value extraction toward platform operators
 *   and advertisers while imposing costs on users (attention, autonomy,
 *   privacy), content creators (labor exploitation), and broader systems
 *   (epistemic commons degradation, democratic deliberation quality,
 *   attention span erosion). The constraint exhibits snare characteristics at
 *   scale (powerless users with no exit), identity lock-in for children and
 *   adolescents whose identity formation coincides with platform integration,
 *   tangled rope dynamics for advertisers and creators (some coordination
 *   benefit alongside extraction), piton features in regulation (performative
 *   oversight), and false summit risk in treating the arrangement as natural
 *   law. The measurements show extractiveness acceleration from 0.35 (early
 *   adoption, genuine coordination value) to 0.68 (mature extraction, minimal
 *   coordination benefit) over the 10-year interval, with theater ratio
 *   rising in parallel (indicating regulatory theater lagging extraction
 *   growth).
 *
 * KEY AGENTS:
 *   - Users / Individual Subjects: Primary victims (powerless/trapped and identity_locked) — attention and behavioral data harvested; intermittent reinforcement creates dependency; network effects prevent exit
 *   - Content Creators / Laborers: Secondary victims (moderate/constrained) — platform-dependent for audience; unpaid labor extracted; algorithmic unpredictability prevents reliable monetization
 *   - Cognitive Commons / Democratic Deliberation: Abstract victim (powerless/trapped) — epistemic degradation, attention fragmentation, deliberation quality erosion
 *   - Platform Operators: Primary beneficiary (institutional/arbitrage) — capture behavioral surplus, aggregate and resell attention; benefit from lock-in
 *   - Advertisers / Business Users: Mixed actor (moderate/constrained) — benefit from targeting precision (coordination) but trapped by platform dependency and algorithmic opacity (extraction)
 *   - Regulators / Policymakers: Organized actor (moderate/constrained) — maintain performative oversight; technical complexity and lobbying pressure preserve underlying mechanisms
 *   - Children and Adolescents: Vulnerable identity-formation cohort (powerless/identity_locked) — identity constituted through platform participation; exit structurally blocks peer connection
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(attention_economy, 0.68).
domain_priors:suppression_score(attention_economy, 0.72).
domain_priors:theater_ratio(attention_economy, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(attention_economy, extractiveness, 0.68).
narrative_ontology:constraint_metric(attention_economy, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(attention_economy, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(attention_economy, snare).
narrative_ontology:human_readable(attention_economy, "Attention Economy Extraction and Behavioral Lock-In").
narrative_ontology:topic_domain(attention_economy, "social/technological/economic").

domain_priors:requires_active_enforcement(attention_economy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(attention_economy, platform_operators).
narrative_ontology:constraint_beneficiary(attention_economy, advertising_networks).
narrative_ontology:constraint_beneficiary(attention_economy, attention_brokers).
narrative_ontology:constraint_victim(attention_economy, user_autonomy).
narrative_ontology:constraint_victim(attention_economy, cognitive_commons).
narrative_ontology:constraint_victim(attention_economy, democratic_deliberation).
narrative_ontology:constraint_victim(attention_economy, children_and_adolescents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: USER / BEHAVIORAL CAPTIVE (SNARE) — Trapped by intermittent reinforcement (variable reward schedules), attention friction (switching costs), and psychological dependencies designed into recommendation systems. No meaningful exit without total digital disengagement. Maximum extraction: behavioral surplus harvested, attention commodified, autonomy converted to engagement metric. User perceives immutability of the constraint.
constraint_indexing:constraint_classification(attention_economy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CHILD / ADOLESCENT (SNARE WITH IDENTITY LOCK) — Identity formation stage coincides with platform integration. Social identity, peer connection, and self-concept are constituted through platform participation. Exit would require not just switching services but abandoning peer cohort and reconstructing identity — structurally mobile but identity-fused. Suppression operates through identity as well as through addictive design. Perceived immutability is internalized: 'this is how you stay connected; this is who you are.'
constraint_indexing:constraint_classification(attention_economy, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 3: CONTENT CREATOR / LABORER (SNARE) — Structurally constrained: platform dependency for audience reach, algorithmic unpredictability, platform policy changes. Cannot reliably monetize attention even as it is extracted and brokered. High suppression through asymmetric algorithm opacity and caprice. Extraction: creator's unpaid labor and intellectual property flow to platform while platform captures advertiser revenue. Exit costly — audience is captive to the platform, not to the creator.
constraint_indexing:constraint_classification(attention_economy, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: ADVERTISER / BUSINESS OPERATOR (TANGLED ROPE) — Mixed: genuine coordination function (matching products to audiences at scale, targeting efficiency) coexists with extractive control (algorithmic black box, price opacity, platform lock-in, dependency on algorithm changes). Suppression: technical opacity, lack of algorithm auditability, unilateral platform policy changes. Benefits from targeting precision but bears extraction in form of platform rent-taking and algorithmic unpredictability. Constrained exit: cannot reach scale-equivalent audience outside platforms; switching costs are prohibitive.
constraint_indexing:constraint_classification(attention_economy, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: PLATFORM OPERATOR (ROPE) — Experiences the constraint as pure coordination: aggregating attention, matching supply and demand, enabling expression and commerce at scale. Benefits from arbitrage: arbitrage between user attention (free to platform), creator content (free or low-cost to platform), and advertiser demand (high-cost to advertiser). Suppression is low relative to platform — they control the enforcement mechanism. Effectively zero extraction from platform's perspective; they are the extraction beneficiary.
constraint_indexing:constraint_classification(attention_economy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATOR / POLICYMAKER (PITON) — Theater-driven regulation: Digital Services Act, Online Safety Bill, social media oversight agencies all maintain performative accountability rituals (platforms report metrics, regulators audit reports, media announces findings) while platforms' core attention-capture mechanisms remain unchanged. Suppression: regulatory capture through lobbying, technical complexity obfuscation, jurisdictional gaps. Theater ratio high because regulation persists without changing underlying structural extraction. Regulatory mechanisms degrade as platforms adapt faster than rules update.
constraint_indexing:constraint_classification(attention_economy, piton,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — Risks naturalizing as immutable law of information technology what is actually a contingent institutional arrangement. Frame: 'attention is finite; platforms optimize for engagement as technical necessity; user distraction is inevitable outcome of information abundance.' This perspective obscures that the extraction is designed, not inherent. The 'immutability' is really the lock-in depth and organized institutional defense of the arrangement.
constraint_indexing:constraint_classification(attention_economy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(attention_economy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(attention_economy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(attention_economy, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(attention_economy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(attention_economy, TR),
    TR >= 0.70.

:- end_tests(attention_economy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and rising. Initial extractiveness (0.35) reflected genuine coordination value in early platforms — matching users to relevant content, enabling long-tail creators, facilitating commerce. As platforms matured, they shifted from coordination toward pure extraction: algorithmic opaqueness increased, behavioral targeting deepened, addictive design patterns proliferated. The 0.68 final value reflects that coordination value is now a minority function; extraction is dominant. The acceleration curve (0.35→0.52→0.68) shows rent-seeking layered onto the original coordination mechanism. Suppression (0.72): Very high. Multiple reinforcing suppression mechanisms: (1) Network effects (cannot reach peers outside platforms), (2) Addictive design (intermittent reinforcement), (3) Switching costs (data, contacts, reputation invested), (4) Psychological dependency (identity lock-in for juveniles), (5) Technical opacity (users cannot understand or predict recommendations). Suppression is both structural (external barriers) and internalized (users carry the constraint into new platforms). Theater ratio (0.68): High and rising. Regulatory theater: GDPR consent, DSA oversight, Instagram's 'well-being' features, TikTok's 'balanced feed' toggles are performative — they maintain accountability ritual without disrupting core extraction mechanisms. The theater increase (0.42→0.68) reflects growing regulatory attention matched by sophistication in evading real constraint. Mandatrophy analysis: The snare classification resolves the mandatrophy by clearly delineating that the constraint is primarily extractive (χ ≥ 0.66, confirmed at 0.68) with minimal coordination benefit (the rope component is marginal). The false summit (mountain perspective) is a misclassification — the constraint is not a law of nature but a designed and defended institutional arrangement. The piton perspective (regulatory theater) is legitimate: regulators are engaged in performative maintenance because they lack technical capacity and face organized resistance from beneficiaries.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows maximum polarization. Users at powerless/trapped see a snare with no exit. Platform operators at institutional/arbitrage see pure coordination. The gap is not perceptual illusion but structural reality: the constraint genuinely extracts from users and genuinely coordinates supply-demand for platforms. The analytical observer risks naturalizing this as immutable law of information technology, but the structural data reveals it as a designed and defended institutional arrangement. The piton perspective reveals regulatory failure: oversight persists but doesn't constrain extraction, indicating degraded function masked by theater.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values derive from structural relationship. Users as victims with trapped exit: d ≈ 0.95 → f(d) ≈ 1.42, maximum experienced extraction. Children as victims with identity_locked exit: d ≈ 0.89 → f(d) ≈ 1.28, very high experienced extraction (slightly less than trapped because internalized suppression is sometimes less rigid than external barriers, but the psychological binding is stronger). Platform operators as beneficiaries with arbitrage exit: d ≈ 0.05 → f(d) ≈ -0.12, negative effective extraction (they are benefiting, not paying). Advertisers as mixed with constrained exit: d ≈ 0.55 → f(d) ≈ 0.75, moderate experienced extraction reflecting constraint asymmetry. The scope modifier σ(S) for global scope is 1.2, amplifying extraction proportionally — the constraint's reach across jurisdictions and demographics makes verification and exit even harder. The chi formula captures that the base extractiveness of 0.68 is amplified by direction (how asymmetrically it falls on powerless agents) and scope (how hard to escape globally).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint is classified as snare (extractiveness 0.68 ≥ 0.46, suppression 0.72 ≥ 0.60, χ ≈ 0.92 ≥ 0.66). The mandatrophy — the ambiguity of whether the constraint is primarily coordinative or extractive — is resolved by the perspective structure: the beneficiary's perspective (platform, rope) clearly shows they experience coordination; the victim's perspective (users, snare) clearly shows they experience extraction. The constraint is not ambiguous; it is asymmetric. The mandatrophy resolution does not eliminate the false summit risk: the analytical observer must consciously resist the frame 'attention capture is inevitable in information-rich environments' and recognize that the specific design patterns, opacity, and behavioral conditioning are contingent institutional choices, not laws of nature. The regulatory theater (piton perspective) indicates that oversight institutions exist but are performative — capable of maintaining accountability ritual while extraction mechanisms remain intact.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    behavioral_agency_threshold,
    'At what design specificity does a recommendation system cross from coordination mechanism to behavioral control apparatus?',
    'Behavioral analysis of users with algorithm-generated vs user-selected feeds; measurement of choice reversibility and counterfactual engagement patterns; studies of user''s ability to predict next recommended content',
    'If threshold is crossed: snare classification confirmed. If unclear boundary: constraint may be rope with extractive elements (tangled rope). Affects whether suppression is treated as structural or emergent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_agency_threshold, empirical, 'Behavioral agency threshold in algorithmic recommendation').

omega_variable(
    addiction_mechanism_causality,
    'Are platform engagement metrics driven primarily by intermittent reinforcement design (intentional behavioral conditioning) or by network effects and information preference (emergent from rational user behavior)?',
    'Comparison of engagement metrics across platforms with varying reward schedule designs; analysis of user engagement patterns before/after algorithmic transparency interventions; behavioral data from platforms with and without addictive design patterns',
    'If intentional: snare mechanism confirmed. If emergent: extraction is less intentional, classification shifts toward tangled rope. Affects mandatrophy resolution and whether platforms can claim coordination-only function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(addiction_mechanism_causality, empirical, 'Whether engagement is driven by design intent or emergent properties').

omega_variable(
    exit_option_viability,
    'Can users meaningfully exit attention capture through technological means (decentralized platforms, algorithmic transparency tools, digital detox tools) or is exit structurally blocked by network effects and peer dependency?',
    'Longitudinal tracking of users attempting alternative platforms; measurement of adoption rates and sustained usage of privacy-preserving/attention-minimizing alternatives; analysis of friction introduced by network consolidation',
    'If exit viable: exit_options may be upgraded from trapped to constrained or mobile. If blocked: trapped classification confirmed. Directly affects d-value and effective extraction chi.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_option_viability, empirical, 'Structural viability of exit from attention capture').

omega_variable(
    suppression_structural_vs_internalized,
    'Is measured suppression (0.72) structural (external barriers: network effects, switching costs, legal/economic dependency) or internalized (identity fusion, cognitive capture, normalization of behavior monitoring)?',
    'Post-exit measurement: if users departing platforms show persistent attention-seeking and compulsive-checking behaviors despite barrier removal, suppression is partially internalized. Behavioral analysis of users from pre-smartphone cohorts vs digital natives.',
    'If structural: suppression declines after exit. If internalized: users carry suppression with them — constraint''s effective strength is higher than structural measure. Affects whether exit ever truly occurs or whether users reconstruct the same constraint in new platforms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized suppression mechanism').

omega_variable(
    democratic_harm_quantification,
    'How much of the degradation in democratic deliberation, attention span, and epistemic commons is attributable to attention economy constraint versus to other factors (education, media fragmentation, political polarization)?',
    'Causal analysis comparing countries/demographics with differential platform penetration; measurement of deliberation quality/epistemic outcomes in pre-internet vs post-internet cohorts; controlled studies of social discourse quality under varying attention design conditions',
    'If attention economy is primary driver: snare classification confirmed and victim status of ''democratic deliberation'' is justified. If confounded: extraction mechanism is less clear; constraint may be snare + piton (degraded function plus extraction) rather than pure snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(democratic_harm_quantification, empirical, 'Attribution of democratic harm to attention economy mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(attention_economy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(attn_tr_t0, attention_economy, theater_ratio, 0, 0.42).
narrative_ontology:measurement(attn_tr_t5, attention_economy, theater_ratio, 5, 0.55).
narrative_ontology:measurement(attn_tr_t10, attention_economy, theater_ratio, 10, 0.68).
narrative_ontology:measurement(attn_tr_t7, attention_economy, theater_ratio, 7, 0.62).

% Extraction over time
narrative_ontology:measurement(attn_be_t0, attention_economy, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(attn_be_t5, attention_economy, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(attn_be_t10, attention_economy, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(attn_be_t7, attention_economy, base_extractiveness, 7, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(attention_economy, resource_allocation).
narrative_ontology:boltzmann_floor_override(attention_economy, 0.12).
narrative_ontology:affects_constraint(attention_economy, information_asymmetry_digital_markets).
narrative_ontology:affects_constraint(attention_economy, behavioral_addiction_mechanisms).
narrative_ontology:affects_constraint(attention_economy, democratic_deliberation_epistemic_commons).
narrative_ontology:affects_constraint(attention_economy, advertising_network_surveillance).

% DUAL FORMULATION NOTE:
% The attention economy constraint decomposes into structural sub-constraints: (1) algorithmic opacity (users cannot predict recommendations) — distinct extraction mechanism with separate ε; (2) addictive design (intermittent reinforcement) — behavioral lock-in with separate ε; (3) network effects (peer dependency, data switching costs) — coordination + extraction hybrid. Each sub-constraint could be modeled separately per the ε-invariance principle. This story aggregates them as a single high-level constraint because users experience them as unified extraction apparatus. Downstream constraints (behavioral addiction, democratic harm) inherit the suppression and extractiveness from this parent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(attention_economy, moderate, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
