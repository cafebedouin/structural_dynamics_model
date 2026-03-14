% ============================================================================
% CONSTRAINT STORY: attention_market_capture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_attention_market_capture, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: attention_market_capture
 *   human_readable: Attention Market Capture
 *   domain: technology/media/cognitive_economics
 *
 * SUMMARY:
 *   Attention markets represent a structural constraint that emerges from the
 *   commodification of human cognitive focus within digital platform
 *   ecosystems. The constraint creates a systematic extraction mechanism
 *   where platforms aggregate user attention (through algorithmic
 *   optimization for engagement), sell access to that attention (to
 *   advertisers and content creators), and reinvest extracted attention value
 *   into further optimization loops. The constraint demonstrates all six DR
 *   types from different perspectives: pure extraction (Snare) from the
 *   user's position, coordination (Rope) from the advertiser's position,
 *   institutional hybrid (Tangled Rope) from the regulator's position,
 *   degraded theater (Piton) from the behavioral science establishment's
 *   position, and false naturalization (Mountain) from the universal
 *   analytical observer's position. The extractiveness trajectory shows
 *   monotonic increase from 0.35 to 0.68 over the measurement interval,
 *   reflecting accumulating optimization for attention extraction as
 *   platforms mature and competitive pressure intensifies. Theater ratio
 *   similarly increases from 0.32 to 0.58, indicating growing performative
 *   framing of extraction as 'user benefit' and 'connection' while underlying
 *   mechanisms become more sophisticated in suppressing exit awareness.
 *
 * KEY AGENTS:
 *   - Individual Users: Primary victims (powerless/trapped) — bear full cognitive and behavioral extraction costs with minimal exit options
 *   - Content Creators: Secondary victims (moderate/constrained) — face career dependence on algorithmic amplification and data extraction
 *   - Advertising Apparatus: Primary beneficiary (institutional/arbitrage) — receives high-value audience attention and behavioral prediction data
 *   - Attention Aggregator Platforms: Primary beneficiary (institutional/arbitrage) — captures and commodifies attention; controls algorithmic mechanisms
 *   - Regulatory Coalition: Mixed (organized/constrained) — attempts to balance genuine coordination needs with extraction suppression; faces regulatory capture dynamics
 *   - Behavioral Science Establishment: Tertiary beneficiary (institutional/arbitrage) — provides theoretical cover and maintains institutional framework despite declining functional utility
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent extraction mechanisms as inevitable consequences of information scarcity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(attention_market_capture, 0.68).
domain_priors:suppression_score(attention_market_capture, 0.72).
domain_priors:theater_ratio(attention_market_capture, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(attention_market_capture, extractiveness, 0.68).
narrative_ontology:constraint_metric(attention_market_capture, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(attention_market_capture, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(attention_market_capture, snare).
narrative_ontology:human_readable(attention_market_capture, "Attention Market Capture").
narrative_ontology:topic_domain(attention_market_capture, "technology/media/cognitive_economics").

domain_priors:requires_active_enforcement(attention_market_capture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(attention_market_capture, attention_aggregators).
narrative_ontology:constraint_beneficiary(attention_market_capture, algorithmic_platforms).
narrative_ontology:constraint_victim(attention_market_capture, individual_cognitive_autonomy).
narrative_ontology:constraint_victim(attention_market_capture, information_commons).
narrative_ontology:constraint_victim(attention_market_capture, cognitive_labor_force).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE CAPTURED CONSUMER (SNARE) — Users face maximal extraction. Material barriers to exit include social network lock-in (contacts are platform-bound), substitution costs (alternative platforms lack equivalent user bases), and cognitive switching costs (habit formation and algorithmic personalization create switching drag). No exit option exists that preserves social connectivity. The constraint extracts attention, behavioral data, and cognitive cycles with minimal coordination benefit to the user — the platform claims user benefit through 'connection' but the extraction dynamic dominates.
constraint_indexing:constraint_classification(attention_market_capture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE CONTENT CREATOR (SNARE) — Creators face high extraction but with partial exit optionality. Career dependence on platform reach creates constrained mobility — a creator can theoretically move to alternative platforms but loses algorithmic amplification and audience reach. The platform extracts both attention (from viewers) and behavioral data (creator engagement metrics). Creators experience suppression through algorithmic opacity (no visibility into ranking factors) and through norm-setting (platform culture requirements, content moderation rules). Significant career risk to exit; not fully trapped but severely constrained.
constraint_indexing:constraint_classification(attention_market_capture, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ADVERTISING APPARATUS (ROPE) — Advertisers experience the constraint as coordination. Attention markets enable efficient matching between advertisers and user segments. Platforms solve a genuine coordination problem: aggregating billions of attention units and directing them to paying buyers. Advertisers have exit options (alternative platforms, direct marketing) but the incumbents have captured scale economies. Advertising apparatus benefits from the extraction mechanism (high-value audience data, behavioral targeting precision). This agent perceives the constraint as enabling pure coordination, not extraction.
constraint_indexing:constraint_classification(attention_market_capture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THE REGULATORY COALITION (TANGLED ROPE) — Governments and civil society organizations see genuine coordination (digital identity, economic activity coordination) entangled with severe extraction (surveillance, behavioral prediction, attention commodification). Regulatory agents face constrained exit (cannot simply abandon digital coordination) but also genuine enforcement power. The constraint is actively maintained through regulatory capture dynamics: platforms lobby for lenient oversight; regulators lack technical capacity for alternatives. Mixed coordination-extraction hybrid with asymmetric enforcement.
constraint_indexing:constraint_classification(attention_market_capture, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: THE BEHAVIORAL SCIENCE ESTABLISHMENT (PITON) — Academic behavioral economics and psychology provided the theoretical foundation for attention markets (hyperbolic discounting, choice architecture, nudges). The field once had a genuine coordination function (understanding human decision-making). Over 30 years, the constraint has degraded into theater: behavioral science concepts are now primarily deployed as cover stories for extraction mechanisms ('nudging users for their own good'). The field sees its utility as declining (behavioral findings increasingly questioned, replication crises) but the institutional framework persists through inertia and industry funding. High theater ratio; minimal coordination function remaining.
constraint_indexing:constraint_classification(attention_market_capture, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: THE ANALYTICAL OBSERVER / COGNITIVE SCARCITY VIEW (MOUNTAIN) — From a universal/civilizational perspective, human attention is scarce and finite. Attention markets are seen as unavoidable economic structures that emerge whenever attention becomes commodifiable. This perspective naturalizes the constraint as an immutable consequence of information abundance in digital contexts. However, the structural data reveals this as a false summit: the extraction mechanisms (algorithmic amplification of addiction-prone content, behavioral prediction, attention auction dynamics) are contingent institutional choices, not natural laws. Alternative attention coordination systems exist and are possible.
constraint_indexing:constraint_classification(attention_market_capture, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(attention_market_capture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(attention_market_capture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(attention_market_capture, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(attention_market_capture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(attention_market_capture, TR),
    TR >= 0.70.

:- end_tests(attention_market_capture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint systematically captures and commodifies user attention, behavioral data, and cognitive cycles with minimal reciprocal benefit to the user. The extraction value is directly measurable: advertising revenue per user ($5-50/year in developed markets) vastly exceeds utility value returned to users (convenience, connection, entertainment). The measurement trajectory shows accumulation — platforms have continuously optimized extraction mechanisms over the interval, increasing per-user attention capture. Suppression (0.72): High. Suppression operates through multiple channels: (1) structural network lock-in — social contacts are platform-bound, making exit costly; (2) algorithmic opacity — users cannot see ranking mechanisms or understand why content is promoted; (3) normalized dependency — platform use becomes habitual and socially expected; (4) alternative scarcity — competitor platforms lack equivalent network effects and user bases. Exit barriers are severe but not absolute — users can theoretically switch platforms but face significant friction. Theater ratio (0.58): Moderate-high. Platforms deploy substantial performative framing: 'connecting people,' 'giving everyone a voice,' 'tools for self-expression.' The reality underneath is attention optimization and behavioral prediction. The theater has increased over the interval as platforms have adopted social responsibility narratives while intensifying extraction mechanisms. Theater is elevated but not dominant (like Piton at 0.70+) because the coordination function (connecting users) remains partially real even if subordinate to extraction.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exemplifies perspectival divergence driven by directionality differences. The user sees Snare: they are trapped in an extraction mechanism with suppressed exit, receiving minimal coordination benefit, bearing full cost of attention commodification. The advertiser sees Rope: they experience the constraint as solving a coordination problem (matching attention to valuable audiences) without perceiving extraction (they pay for attention value received). The platform sees Rope: they experience themselves as coordinating supply (user attention) and demand (advertiser budgets). The regulator sees Tangled Rope: genuine coordination (digital identity, economic activity) entangled with severe extraction (surveillance, behavioral prediction), with contested enforcement. The behavioral science establishment sees Piton: their field provided theoretical foundations (hyperbolic discounting, choice architecture, nudges) but these concepts are now deployed primarily as extraction cover stories, leaving the institutional framework degraded but persistent. The universal analytical observer risks seeing Mountain: attention is scarce in information abundance contexts, so attention markets are inevitable structures — but this naturalizes contingent design choices (algorithmic optimization for addiction, behavioral targeting, attention auction) rather than economic laws. The perspectival gap reveals that the constraint's type depends entirely on the agent's structural position in the extraction flow: maximum extraction is experienced by those with maximum trapped exit (users) and minimum extraction by those with arbitrage exit (platforms).
 *
 * DIRECTIONALITY LOGIC:
 *   This constraint exhibits strong coupling between power level, exit options, and classification type. Powerless agents with trapped exit perceive Snare (maximum extraction). Institutional agents with arbitrage exit perceive Rope (minimal extraction). The directionality pipeline computes d from (beneficiary/victim status) + (exit options) + (power level), producing high-d for victims/trapped, low-d for beneficiaries/arbitrage. No directionality overrides are needed — the structural data produces appropriate d values for all perspectives. The five-order perspectival spread (Snare → Rope → Tangled Rope → Piton → Mountain) is unusual and indicates strong perspectival nonlinearity — the constraint appears radically different across observation positions.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: This constraint avoids false coordination classification through clear victim declaration (individual cognitive autonomy, information commons, cognitive labor force) and explicit extraction metrics (attention commodification, behavioral data harvest, behavioral prediction). The constraint could superficially appear as Rope (platforms coordinate supply and demand in attention markets) but the victim declaration reveals the coordination is entangled with extraction. The Tangled Rope perspective shows genuine coordination (regulatory coordination of digital activity) coupled with asymmetric extraction (surveillance, behavioral prediction). The Snare perspective (powerless/trapped) reveals that for users, the coordination function is minimal — they receive connection benefits but at vastly disproportionate cost in attention and data. The mandatrophy resolution affirms: (1) coordination is real but secondary (platforms do coordinate attention flow), (2) extraction is primary and asymmetric (platforms extract far more value than they return to users), (3) classification is Snare from powerless perspective (maximum extraction experienced by those with maximum suppression), (4) different perspectives legitimately classify as Rope/Tangled Rope/Piton/Mountain, but the engine's Snare classification at the powerless perspective correctly identifies the primary extraction dynamic. The theater_ratio increase (0.32 → 0.58) confirms that performative framing ('connecting people,' 'user choice') is accumulating over time — a Goodhart pattern where the stated coordination function (connection) is becoming increasingly detached from actual extraction mechanisms (attention optimization, behavioral prediction).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntary_participation_illusion,
    'Is platform participation genuinely voluntary or is it coercively normalized?',
    'Measurement of participation rates in contexts with genuine alternatives; survey data on perceived exit costs; analysis of switching behavior when new platforms emerge with comparable network effects',
    'If genuinely voluntary: exit_options classification shifts toward ''mobile'' or ''arbitrage'' for more perspectives. If coercively normalized: ''trapped'' becomes more justified. Current assessment assumes normalization-based coercion; data would validate or refute.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_participation_illusion, empirical, 'Whether platform participation is genuinely voluntary or coercively normalized').

omega_variable(
    extraction_versus_coordination_boundary,
    'What proportion of platform activity is genuine user-benefiting coordination versus pure extraction?',
    'User welfare studies comparing engagement satisfaction with attention spent; analysis of user-generated surplus vs platform-captured surplus; measurement of counterfactual user welfare in platform absence',
    'If coordination > 60%: reclassify as Tangled Rope from powerless perspective. If coordination < 20%: confirms Snare classification. Current assessment (0.68 extractiveness) implies ~32% coordination function; threshold-crossing data would validate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_versus_coordination_boundary, empirical, 'Boundary between extraction and coordination in platform activity').

omega_variable(
    algorithmic_inevitability_claim,
    'Are current attention-extraction algorithms technically inevitable or are they contingent design choices optimized for revenue rather than user welfare?',
    'Comparative analysis of alternative algorithmic designs (chronological feeds, user-controlled ranking, distributed systems); assessment of design trade-offs between engagement and user welfare; documentation of alternative feasible system architectures',
    'If contingent choices: constrains the mountain perspective''s naturalization claim. If inevitable given scale: increases plausibility of the mountain framing. Current assessment assumes contingency; technical analysis would determine.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_inevitability_claim, conceptual, 'Whether extraction algorithms are technically inevitable or contingent design choices').

omega_variable(
    suppression_mechanism_internalization,
    'Is suppression of exit primarily structural (network lock-in, switching costs) or internalized (identity fusion with platform, normalized dependency)?',
    'Analysis of exit behavior when structural barriers are reduced (platform migration during outages, competitor emergence); measurement of cognitive framing (do users perceive platform use as ''necessary'' vs ''habit vs ''identity''?); post-exit behavior tracking',
    'If primarily structural: exit_options remain ''trapped'' or ''constrained''. If partially internalized: some perspectives should use ''identity_locked'' exit option. Current assessment assumes structural dominance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural or internalized').

omega_variable(
    interoperability_sunset_feasibility,
    'Could regulated interoperability create a genuine sunset clause for this constraint, or are network effects mathematically irreducible?',
    'Economic modeling of interoperable social platforms; analysis of historical interoperability deployments (email, SMTP, early web); assessment of switching costs under forced portability; technical feasibility studies of federated social networks',
    'If feasible: constraint could reclassify as Scaffold with sunset logic. If mathematically irreducible: Snare persists regardless of regulation. Current assessment assumes partial feasibility (Scaffold perspective) but high uncertainty.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(interoperability_sunset_feasibility, empirical, 'Whether interoperability can create a sunset pathway for attention extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(attention_market_capture, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(attmkt_tr_t0, attention_market_capture, theater_ratio, 0, 0.32).
narrative_ontology:measurement(attmkt_tr_t5, attention_market_capture, theater_ratio, 5, 0.45).
narrative_ontology:measurement(attmkt_tr_t10, attention_market_capture, theater_ratio, 10, 0.58).
narrative_ontology:measurement(attmkt_tr_t7, attention_market_capture, theater_ratio, 7, 0.52).

% Extraction over time
narrative_ontology:measurement(attmkt_be_t0, attention_market_capture, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(attmkt_be_t5, attention_market_capture, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(attmkt_be_t10, attention_market_capture, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(attmkt_be_t7, attention_market_capture, base_extractiveness, 7, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(attention_market_capture, resource_allocation).
narrative_ontology:affects_constraint(attention_market_capture, algorithmic_recommendation_optimization).
narrative_ontology:affects_constraint(attention_market_capture, data_asymmetry_surveillance_markets).
narrative_ontology:affects_constraint(attention_market_capture, behavioral_prediction_dependency).
narrative_ontology:affects_constraint(attention_market_capture, network_effect_lock_in).

% DUAL FORMULATION NOTE:
% Attention market capture decomposes into several structurally distinct constraints: (1) algorithmic recommendation optimization (ε ≈ 0.55, optimization for engagement creates extraction mechanics), (2) data asymmetry in surveillance markets (ε ≈ 0.62, behavioral data extraction), (3) behavioral prediction dependency (ε ≈ 0.58, cognitive model extraction), (4) network effect lock-in (ε ≈ 0.65, structural entrapment through social network binding). These are linked in a constraint family through upstream-downstream relationships: network lock-in enables platforms to implement algorithmic optimization without user exit; algorithmic optimization generates behavioral data for prediction; prediction enables targeted attention extraction. The attention_market_capture story models the integrated extraction mechanism; each sibling story models a distinct structural component.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
