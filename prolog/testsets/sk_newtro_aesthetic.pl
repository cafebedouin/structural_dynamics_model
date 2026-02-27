% ============================================================================
% CONSTRAINT STORY: sk_newtro_aesthetic
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sk_newtro_aesthetic, []).

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
 *   constraint_id: sk_newtro_aesthetic
 *   human_readable: South Korean 'Newtro' Aesthetic Commercialization
 *   domain: social/economic/cultural
 *
 * SUMMARY:
 *   South Korea's 'Newtro' aesthetic trend represents a commercialization
 *   constraint operating at the intersection of generational identity,
 *   cultural heritage, and global premium markets. Younger consumers (Gen Z)
 *   reinterpret traditional Korean aesthetics—ceramics, textile patterns,
 *   typography, architectural elements—through contemporary design
 *   sensibilities, creating demand for 'heritage-informed' lifestyle goods.
 *   Premium brands (LG, Hyundai design studios, K-beauty companies, fashion
 *   labels) capitalize on this aesthetic appetite, extracting design codes
 *   from traditional artisan communities and subcultural innovation spaces,
 *   then reselling them at premium margins with minimal benefit to the
 *   knowledge originators. The constraint's structure exhibits all six
 *   classification types from different structural positions: traditional
 *   artisans experience pure extraction (snare); Gen Z subcultures experience
 *   mixed benefit and cooptation (tangled rope); brands experience pure
 *   coordination gain (rope); heritage institutions see a temporary failure
 *   being solved by organized resistance (scaffold); the state bureaucracy
 *   performs heritage protection without economic effect (piton); and the
 *   civilizational observer risks naturalizing the power asymmetry as
 *   inevitable cultural evolution (false mountain). The theater ratio (0.65)
 *   reflects state certification and institutional heritage lists operating
 *   without enforcement or economic benefit flow-through—performative
 *   heritage validation enables brands to market newtro as 'culturally
 *   authentic' while actual artisans remain economically excluded.
 *
 * KEY AGENTS:
 *   - Traditional Artisans and Heritage Craftspeople: Primary victims (powerless/trapped) — economic exclusion from commercialization of their knowledge; cannot organize or escape dependency on craft income
 *   - Gen Z Consumers and Subcultural Communities: Secondary victims and beneficiaries (moderate/constrained) — identity expression and aesthetic access benefit, but rapid cooptation of subcultural innovation by premium brands limits autonomy
 *   - Premium Lifestyle Brands (Samsung Design, AMOREPACIFIC, local fashion labels): Primary beneficiaries (institutional/arbitrage) — capture aesthetic differentiation and premium pricing; arbitrage into other markets if newtro loses momentum
 *   - Content Creators and Influencers: Secondary beneficiaries (powerful/mobile) — monetize newtro content; can shift to other aesthetics; participate in both documentation and commodification
 *   - Heritage Tourism Operators and Experience Platforms: Beneficiaries (powerful/mobile) — monetize heritage narrative; arbitrage into cultural tourism markets
 *   - Heritage Preservation NGOs and Museums: Organized agents (organized/constrained) — see sunset through formalized licensing and community-based tourism frameworks; currently low-capacity enforcement
 *   - Korean Government Heritage Ministry: Bureaucratic actor (institutional/constrained) — maintains heritage lists and certification; constrained by enforcement capacity; theater-dominant due to lack of economic support mechanisms
 *   - Artisan Guilds and Fair-Trade Networks: Organized resistance (organized/mobile) — building alternative revenue channels; mobile exit via platform cooperatives and international fair-trade markets
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing aesthetic circulation asymmetries as inevitable cultural evolution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sk_newtro_aesthetic, 0.52).
domain_priors:suppression_score(sk_newtro_aesthetic, 0.48).
domain_priors:theater_ratio(sk_newtro_aesthetic, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sk_newtro_aesthetic, extractiveness, 0.52).
narrative_ontology:constraint_metric(sk_newtro_aesthetic, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(sk_newtro_aesthetic, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sk_newtro_aesthetic, tangled_rope).
narrative_ontology:human_readable(sk_newtro_aesthetic, "South Korean 'Newtro' Aesthetic Commercialization").
narrative_ontology:topic_domain(sk_newtro_aesthetic, "social/economic/cultural").

domain_priors:requires_active_enforcement(sk_newtro_aesthetic).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sk_newtro_aesthetic, premium_lifestyle_brands).
narrative_ontology:constraint_beneficiary(sk_newtro_aesthetic, content_creators).
narrative_ontology:constraint_beneficiary(sk_newtro_aesthetic, heritage_tourism_operators).
narrative_ontology:constraint_victim(sk_newtro_aesthetic, traditional_artisans).
narrative_ontology:constraint_victim(sk_newtro_aesthetic, subcultural_authenticity).
narrative_ontology:constraint_victim(sk_newtro_aesthetic, intergenerational_knowledge_transfer).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRADITIONAL ARTISANS (SNARE) — Trapped by economic devaluation. Their craft knowledge is extracted (reinterpreted, aestheticized, commodified) by commercial brands without compensation or attribution. Exit is impossible: they depend on craft income but cannot compete with scaled newtro products. Bear full cost of cultural appropriation without benefit of commercialization.
constraint_indexing:constraint_classification(sk_newtro_aesthetic, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: GEN Z SUBCULTURAL PARTICIPANTS (TANGLED ROPE) — Constrained by social capital dynamics and economic access. Benefit from newtro as identity expression and access to aesthetic codes. But also experience extraction: their subcultural innovation is rapidly commodified, co-opted, and homogenized by premium brands. Constrained exit — cannot fully reject the aesthetic without losing community identity.
constraint_indexing:constraint_classification(sk_newtro_aesthetic, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PREMIUM BRANDS AND PLATFORMS (ROPE) — Primary beneficiaries. Arbitrage exit (can shift to other aesthetics/markets). Newtro represents pure coordination benefit: mobilizes youth consumer attention, creates differentiation, enables premium pricing. Experience the constraint as a coordination mechanism solving the problem of aesthetic market segmentation. Net positive.
constraint_indexing:constraint_classification(sk_newtro_aesthetic, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HERITAGE PRESERVATION MOVEMENT (SCAFFOLD) — Organized agents (museums, cultural NGOs, government heritage boards) see newtro as temporary coordination failure with sunset potential. Current state: commercial extraction dominates. But institutionalization of heritage education, licensing frameworks for artisan attribution, and community-based craft tourism represent exit pathways building over 10-15 years. Low theater if these pathways mature. Structural sunset: formalized cultural licensing and fair-trade heritage standards.
constraint_indexing:constraint_classification(sk_newtro_aesthetic, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: GOVERNMENT CULTURAL BUREAUCRACY (PITON) — Theater-dominant. Heritage protection agencies conduct performative certification (traditional crafts lists, cultural property registers) without enforcement mechanisms or economic support. The bureaucratic apparatus persists through institutional inertia: it legitimizes newtro commercial use while appearing to protect heritage. Functional verification: heritage lists are largely decorative; economic support is symbolic. Theater ratio high because the symbolic validation of newtro as 'heritage-connected' substitutes for actual artisan economic security.
constraint_indexing:constraint_classification(sk_newtro_aesthetic, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ORGANIZED ARTISAN RESISTANCE (TANGLED ROPE) — Cooperative artisan networks (craft guilds, heritage associations, social enterprises) see newtro as a coordination problem they can solve through collective action. Benefits: access to premium markets, collective bargaining power, direct consumer relationships via digital platforms. Costs: coordination overhead, member discipline. Exit is mobile through platform alternatives and international fair-trade networks. Mixed extraction and coordination — the constraint's extraction mechanism is their target for reform.
constraint_indexing:constraint_classification(sk_newtro_aesthetic, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — Civilizational view sees cultural aesthetics circulation as structurally inevitable: all cultures consume, remix, and commercialize aesthetic forms from other cultures and historical periods. The newtro constraint appears as a natural law: the cycle of appropriation and reinterpretation is inherent to cultural evolution itself. However, the base properties contradict mountain classification — suppression (0.48) and extractiveness (0.52) indicate this is a contingent institutional arrangement, not an immutable law. False summit: naturalization of unequal power in aesthetic circulation.
constraint_indexing:constraint_classification(sk_newtro_aesthetic, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sk_newtro_aesthetic_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sk_newtro_aesthetic, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sk_newtro_aesthetic, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sk_newtro_aesthetic, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sk_newtro_aesthetic, TR),
    TR >= 0.70.

:- end_tests(sk_newtro_aesthetic_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): High-moderate. The constraint operates through asymmetric attribution and economic exclusion. Traditional artisans' knowledge is the raw material; brands capture 70-85% of retail margin through 'newtro' branding and design labor. Artisans rarely receive royalties, licensing fees, or formal attribution beyond decorative heritage lists. The extraction is not complete (some artisans do participate in premium supply chains, some brands do acknowledge sources) but the systematic bias runs 50%+ of value toward brand intermediaries. Suppression (0.48): Moderate. Multiple barriers limit artisan alternatives: high cost of direct-to-consumer brand development, limited platform visibility without brand curation, cultural capital requirements to participate in newtro (higher education, social capital, access to design networks), language/digital literacy barriers for older artisans. But suppression is not total—digital platforms enable some direct artisan sales, international fair-trade networks exist, some younger artisans successfully build newtro brands. Theater ratio (0.65): High and increasing. State heritage certification, 'traditional crafts' lists, government cultural ministry initiatives, and artisan 'ambassador' programs all perform heritage protection without economic enforcement. Brands freely use heritage branding ('传统技艺', 'handmade heritage', '전통') while artisans receive symbolic recognition but not compensation. The theater has increased over the interval (0.35→0.65) as bureaucratic heritage validation has become more sophisticated while economic inclusion remains flat.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap emerges from differential exit options and power asymmetries. Premium brands see newtro as pure coordination (rope): they identify a market need (Gen Z aesthetic demand) and solve it through aesthetic reinterpretation—genuine coordination benefit. Traditional artisans see snare: their knowledge is extracted, their economic position deteriorates, and exit is impossible without ceasing practice. Gen Z subcultures see tangled rope: they gain identity tools and aesthetic sovereignty, but their innovation is rapidly commodified and homogenized. The organized artisan movement sees tangled rope with resistance potential (mobile exit through digital platforms and fair-trade networks). The heritage bureaucracy (piton) maintains symbolic protection (heritage lists, certification) without economic enforcement—the theater satisfies political demands for cultural preservation while actual artisans remain uncompensated. The analytical observer risks seeing mountain (inevitable aesthetic circulation) when the base properties reveal a contingent institutional arrangement: the asymmetry is amplified by state certification (legitimizing brand newtro-ness without artisan benefit-sharing) and platform algorithms (favoring brand-curated over artisan-direct listings). Removing these institutional amplifiers would shift the constraint toward rope or scaffold.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (premium_lifestyle_brands, content_creators, heritage_tourism_operators) occupy positions with arbitrage or powerful exit options—they derive low directionality (d ≈ 0.10-0.25) because they can shift markets and strategies without cost. Victims (traditional_artisans, subcultural_authenticity, intergenerational_knowledge_transfer) are trapped or constrained without alternatives—they derive high directionality (d ≈ 0.75-0.95) because they depend on the aesthetic economy without controlling its terms. Gen Z subcultures occupy a mixed position: they benefit from newtro identity expression but experience extraction of their innovation velocity. Their moderate power + constrained exit produces mid-range d (0.50-0.65). Organized artisan networks have higher agency (mobile/organized exit) than isolated artisans (trapped), shifting d downward from ~0.90 to ~0.55. The constraint's extractiveness (0.52) and suppression (0.48) produce tangled_rope classification across perspectives, but the directionality differences determine which agent experiences it as snare vs tangled rope vs rope vs scaffold.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by distinguishing genuine coordination (aesthetic innovation enabling market expression) from extraction (asymmetric attribution and economic exclusion). The brands' rope experience is real—they are solving a genuine coordination problem: matching aesthetic demand to supply. But the solution is built atop institutional arrangements that enable extraction (brand intermediation without artisan benefit-sharing, state certification without enforcement, platform algorithms favoring institutional actors). The constraint classifies as tangled_rope because it possesses both: (a) genuine coordination function (aesthetic market clearing for Gen Z), (b) asymmetric extraction (artisan knowledge captured, value concentrated in brands). Mandatrophy resolution requires recognizing that the 'coordination' benefit accrues to institutions, while the extraction falls on knowledge originators. If artisan benefit-sharing and fair attribution were formalized (organized artisan networks scaling via digital platforms + state enforcement of heritage licensing), the constraint would shift toward rope or scaffold. Until then, it remains tangled rope with organized resistance building alternative institutional pathways.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authenticity_threshold,
    'What degree of reinterpretation distinguishes respectful aesthetic dialogue from cultural extraction?',
    'Comparative analysis of artisan attribution, economic flow-through, and community perception in newtro products vs comparable heritage commodification cases (Japanese ''wabi-sabi'' goods, Italian ''vintage'' fashion, US ''Americana'')',
    'If threshold is high (near-perfect attribution): newtro mostly legitimate remix. If threshold is low (any acknowledgment sufficient): extraction dominates, artisans remain economically excluded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authenticity_threshold, conceptual, 'Boundary between aesthetic reinterpretation and cultural extraction').

omega_variable(
    platform_compensation_viability,
    'Can digital platforms (Instagram, TikTok, e-commerce) effectively route revenue from newtro commerce back to traditional artisans at scale?',
    'Empirical tracking of fair-trade heritage products: artisan income vs brand revenue for 50+ newtro items; platform algorithm bias toward artisan-direct vs brand-mediated listings; regulatory outcomes from proposed cultural licensing frameworks',
    'If viability > 70%: scaffold perspective confirmed — organized artisan networks can solve extraction via platform cooperation. If < 50%: structural barriers remain high, snare classification dominates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(platform_compensation_viability, empirical, 'Whether digital platforms can route revenue to artisans at scale').

omega_variable(
    state_enforcement_capacity,
    'Can Korean government heritage boards enforce fair attribution and benefit-sharing in the newtro market without stifling innovation?',
    'Analysis of outcomes from proposed heritage licensing laws, enforcement actions against unauthorized use, compliance burden on SMEs, cross-border enforcement challenges (newtro consumed globally)',
    'If capacity high: regulatory scaffold becomes real, theater ratio declines. If capacity low: piton classification deepens — bureaucracy becomes purely performative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_enforcement_capacity, empirical, 'State capacity to enforce heritage benefit-sharing without suppressing innovation').

omega_variable(
    subcultural_cooptation_speed,
    'What is the temporal lag between subcultural aesthetic emergence and commercial premium-brand adoption?',
    'Time-series analysis: track emergence of aesthetic elements in underground communities (university craft spaces, thrift shop scenes, online forums) to commercial brand collection launches; identify acceleration trends',
    'If lag < 6 months: subcultural velocity is high, cooptation rate suggests near-complete value extraction from Gen Z communities. If lag > 2 years: communities retain some aesthetic sovereignty.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(subcultural_cooptation_speed, empirical, 'Temporal lag between subcultural emergence and commercial adoption').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sk_newtro_aesthetic, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(newtro_tr_t0, sk_newtro_aesthetic, theater_ratio, 0, 0.35).
narrative_ontology:measurement(newtro_tr_t5, sk_newtro_aesthetic, theater_ratio, 5, 0.5).
narrative_ontology:measurement(newtro_tr_t10, sk_newtro_aesthetic, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(newtro_be_t0, sk_newtro_aesthetic, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(newtro_be_t5, sk_newtro_aesthetic, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(newtro_be_t10, sk_newtro_aesthetic, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sk_newtro_aesthetic, information_standard).
narrative_ontology:affects_constraint(sk_newtro_aesthetic, k_cultural_export_dependence).
narrative_ontology:affects_constraint(sk_newtro_aesthetic, generational_wealth_concentration_korea).

% DUAL FORMULATION NOTE:
% Newtro aesthetic commercialization is structurally distinct from broader K-cultural soft power export (K-pop, Korean cinema). Newtro is a domestic aesthetic circulation constraint that intersects with heritage preservation and artisan economics. Upstream constraint: Korean premium brand market structure (enables brand-intermediated aesthetics). Downstream constraints: heritage tourism dependency, intergenerational knowledge transfer disruption, cultural authenticity erosion.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sk_newtro_aesthetic, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
