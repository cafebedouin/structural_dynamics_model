% ============================================================================
% CONSTRAINT STORY: disney_openai_ip_exclusivity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_disney_openai_ip_exclusivity, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: disney_openai_ip_exclusivity
 *   human_readable: Exclusive IP licensing for generative AI training (Disney/OpenAI)
 *   domain: technological/economic
 *
 * SUMMARY:
 *   Disney's exclusive licensing of its top-200 character IP library to
 *   OpenAI for generative video model training (Sora) creates a structural
 *   tension between coordination and extraction in the AI-era media
 *   landscape. The $1B Disney investment in OpenAI functions superficially as
 *   a partnership solving a genuine technical problem (training data scarcity
 *   for character-level video generation), but simultaneously constructs an
 *   asymmetric barrier that suppresses competing AI developers' access to
 *   equivalent training targets and reshapes the economics of character
 *   derivative creation. The constraint exhibits a perspectival spectrum:
 *   OpenAI and Disney each experience coordination (Rope); competitors see
 *   extraction (Snare); the broader creator ecosystem sees mixed coordination
 *   and extraction (Tangled Rope); policy coalitions see a temporary
 *   institutional arrangement with regulatory sunset potential (Scaffold);
 *   the traditional licensing apparatus appears degraded and inertial
 *   (Piton). The extractiveness value (0.58) reflects that while the
 *   exclusivity does solve genuine coordination problems, the suppression
 *   mechanisms (legal exclusivity, capital barriers to independent character
 *   IP access, asymmetric training data availability) create structural rents
 *   that extend beyond fair coordination costs. The theater ratio (0.38) is
 *   low because the extraction mechanism is transparent — the financial
 *   incentives and competitive advantages are explicit in contract law and
 *   publicly visible in Sora's demonstrated capabilities. Unlike constraints
 *   sustained by obfuscation, this one operates through clear legal
 *   exclusivity.
 *
 * KEY AGENTS:
 *   - OpenAI/Sora Development Team: Primary beneficiary (institutional/arbitrage) — gains exclusive access to high-value training data; maintains arbitrage options with other entertainment IP holders; bears coordination costs (licensing fees, integration complexity) proportional to benefits
 *   - Disney Corporation: Primary beneficiary (institutional/arbitrage) — captures $1B investment, future licensing revenue, strategic positioning in AI-era media; retains arbitrage options (can license to other AI companies or build internal models); solves training data coordination problem
 *   - Competing AI Developers (Anthropic, Google DeepMind, Meta, open-source labs): Primary victims (powerless/trapped) — cannot negotiate equivalent access to Disney character IP; face structural disadvantage in character-level video generation; exit options are trapped (cannot walk away from character fidelity requirements without reduced competitiveness)
 *   - Character Derivative Creator Ecosystem (indie studios, fan creators, licensed merchandise makers): Secondary victims (moderate/constrained) — face displacement from automated generation; benefit from Disney platform access but compete against AI-generated derivatives; constrained exit (Disney platform dependency vs. autonomy)
 *   - Public AI Commons & Open-Source Communities: Structural victim (analytical/analytical) — excluded from training data access; witness concentration of generative AI capability in proprietary hands; bear risk of monopolistic outcome in generative video market
 *   - Regulatory & Policy Coalitions (EU AI regulators, creator protection advocates, antitrust authorities): Organized observers (organized/constrained) — perceive sunset potential through fair-access mandates or regulatory restrictions; maintain constrained exit (limited enforcement mechanisms but growing policy windows)
 *   - Traditional Entertainment Industry Licensing Apparatus: Institutional actor (institutional/arbitrage) — applies legacy IP exclusivity model to AI training; experiences constraint as inertial (functional role atrophied, legal apparatus persists)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(disney_openai_ip_exclusivity, 0.58).
domain_priors:suppression_score(disney_openai_ip_exclusivity, 0.72).
domain_priors:theater_ratio(disney_openai_ip_exclusivity, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(disney_openai_ip_exclusivity, extractiveness, 0.58).
narrative_ontology:constraint_metric(disney_openai_ip_exclusivity, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(disney_openai_ip_exclusivity, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(disney_openai_ip_exclusivity, tangled_rope).
narrative_ontology:human_readable(disney_openai_ip_exclusivity, "Exclusive IP licensing for generative AI training (Disney/OpenAI)").
narrative_ontology:topic_domain(disney_openai_ip_exclusivity, "technological/economic").

domain_priors:requires_active_enforcement(disney_openai_ip_exclusivity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(disney_openai_ip_exclusivity, openai_sora_development).
narrative_ontology:constraint_beneficiary(disney_openai_ip_exclusivity, disney_strategic_positioning).
narrative_ontology:constraint_victim(disney_openai_ip_exclusivity, competing_ai_developers).
narrative_ontology:constraint_victim(disney_openai_ip_exclusivity, character_derivative_ecosystem).
narrative_ontology:constraint_victim(disney_openai_ip_exclusivity, public_generative_model_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMPETING AI DEVELOPERS (SNARE) — Smaller AI labs and open-source communities cannot access Disney's top-200 character library for model training. This exclusivity creates a structural asymmetry: OpenAI's Sora can generate authentic Disney characters, while competitors must build models blind to these high-value training targets. Exit options are trapped — competitors cannot negotiate for equivalent access regardless of technical merit or funding. The constraint extracts competitive advantage through suppression of alternatives. Experienced extractiveness is maximal because the character IP library is irreplaceable for video generation tasks.
constraint_indexing:constraint_classification(disney_openai_ip_exclusivity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CHARACTER DERIVATIVE CREATOR ECOSYSTEM (TANGLED ROPE) — Fan creators, indie studios, and licensed merchandise makers benefit from the broader Disney ecosystem (IP recognition, distribution networks, audience access) but now face a new asymmetry: OpenAI/Sora can generate derivative works using the character library directly, without human creators' labor. The ecosystem has constrained exit options — leaving Disney's platforms is costly, but remaining means competing against automated generation. This is mixed: the constraint enables some coordination (Disney franchising, content distribution) but simultaneously enables extraction of derivative value that previously went to human creators. Active enforcement required: Disney must monitor and license use of generated character content.
constraint_indexing:constraint_classification(disney_openai_ip_exclusivity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: OPENAI/SORA DEVELOPMENT (ROPE) — The exclusive access to Disney's character IP is experienced as pure coordination by OpenAI: Disney has solved a critical problem (training data scarcity for character-level video generation) through the partnership. OpenAI maintains arbitrage options — other entertainment IP holders could provide similar access, or synthetic training data could substitute. The constraint's function is coordination: enabling Sora to generate high-fidelity character content. Extraction overhead is minimal from this perspective — OpenAI pays fair value ($1B investment + ongoing licensing), and the benefit (competitive advantage in video generation) is proportional. This perspective sees the constraint as solving a genuine technical coordination problem.
constraint_indexing:constraint_classification(disney_openai_ip_exclusivity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DISNEY STRATEGIC POSITIONING (ROPE) — Disney benefits from the partnership through capital infusion ($1B), first-mover advantage in generative video, and expansion of character IP value into the AI era. Disney maintains arbitrage options — it could license to other AI companies, retain training data internally, or build its own generative model. The constraint's function is coordination: Disney solves OpenAI's training data bottleneck; OpenAI validates Disney's IP for AI-era markets. Both parties' costs are proportional to benefits. This perspective experiences the constraint as a solved coordination problem, not extraction.
constraint_indexing:constraint_classification(disney_openai_ip_exclusivity, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PUBLIC AI COMMONS & CONTENT CREATOR CLASS (TANGLED ROPE) — The exclusivity constraint has a civilizational time horizon because it shapes whether generative AI becomes a coordination mechanism (distributed creative tools) or an extraction mechanism (concentrated wealth capture). From this perspective, the constraint exhibits both functions: it does coordinate Disney's IP with AI infrastructure (genuine efficiency gain), but it simultaneously extracts future rents by blocking open-source alternatives and undermining human creator compensation. Active enforcement is visible in licensing agreements, legal scrutiny of derived works, and restrictions on model weights. The constraint suppresses alternatives: open-source video models cannot access character IP, and creators cannot collectively bargain for automation compensation. Effective extraction depends on whether alternative character IP sources emerge (open-source animation, international franchises, synthetic character generation). If competitors achieve comparable character fidelity through legal training routes or synthetic generation, the exclusivity contract loses extraction power and becomes pure coordination.
constraint_indexing:constraint_classification(disney_openai_ip_exclusivity, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATORY & POLICY RESPONSE COALITION (SCAFFOLD) — From a coalition view (EU AI regulators, creator advocacy groups, open-source governance bodies), the exclusivity constraint is a temporary institutional arrangement with a sunset clause embedded in the 10-15 year horizon. AI training data access regulations, creator compensation mandates, or synthetic character generation breakthroughs could bypass the constraint entirely. The scaffold perspective sees the constraint as high-suppression but temporary: current legal frameworks lack mechanisms to force licensing or mandate access, so suppression is ~0.72. But policy coordination to require 'fair access' provisions, regulatory restrictions on exclusive AI training deals, or technological substitutes (synthetic characters) would collapse the constraint's extraction mechanism. Theater is low (0.38) because the extraction mechanism is transparent — the financial incentive is obvious. The sunset is driven by policy evolution, not inertia. High suppression is tolerated only because the time horizon is finite.
constraint_indexing:constraint_classification(disney_openai_ip_exclusivity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: TRADITIONAL ENTERTAINMENT INDUSTRY LICENSING MODEL (PITON) — The exclusivity mechanism echoes the legacy licensing model: franchising, merchandise rights, and derivative content control. From this institutional perspective, the constraint is a Piton — the traditional IP licensing apparatus (character exclusivity contracts, derivative work restrictions) is being mechanically applied to AI training, but its functional role has atrophied. In the traditional era, exclusivity prevented market saturation by controlling who could produce merchandise; in the AI era, exclusivity prevents training data access but does NOT prevent unauthorized generation (once a model is trained, generation is trivial). The theater ratio is low (0.38) because the extraction is straightforward licensing revenue, not performative. But the constraint's primary function (controlling distribution and scarcity) no longer works for generative models — OpenAI's Sora doesn't need to renew the license to generate characters; it only needs to have been trained. The licensing apparatus persists through institutional inertia (contract law, corporate strategy templates) rather than functional necessity. As synthetic training data and alternative character sources mature, the exclusivity constraint becomes purely theatrical.
constraint_indexing:constraint_classification(disney_openai_ip_exclusivity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(disney_openai_ip_exclusivity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(disney_openai_ip_exclusivity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(disney_openai_ip_exclusivity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(disney_openai_ip_exclusivity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(disney_openai_ip_exclusivity, TR),
    TR >= 0.70.

:- end_tests(disney_openai_ip_exclusivity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Elevated from initial estimates. The constraint combines genuine coordination (Disney solves Sora's training data bottleneck; OpenAI validates Disney IP for AI markets) with structural extraction (competitors and creators are suppressed). The ratio is not extreme (0.70+) because the coordination function is real and proportional to some of the extracted value — this is not pure predation, but hybrid. The trajectory shows steady accumulation from 0.42 to 0.58 over six years, indicating that as Sora's capabilities compound and competitors fall further behind, the pure extraction component grows relative to coordination. Suppression (0.72): High. Legal exclusivity, capital barriers to independent IP creation, asymmetric access, and contractual restrictions create substantial alternatives suppression. Competitors cannot easily switch to equivalent training data; creators cannot collectively negotiate for compensation. However, suppression is not total (0.85+) because synthetic character generation and regulatory intervention remain credible escape routes within the 10-15 year horizon. Theater ratio (0.38): Low and stable. The extraction mechanism is transparent — financial incentives are explicit (Disney licensing revenue, OpenAI competitive advantage), legal frameworks are clear (IP contracts), and competitive advantages are measurable (Sora's character fidelity). No obfuscation is needed; the mechanism works through straightforward property rights and technical capability asymmetry. The constraint does not require theatrical justification because it operates within established IP law.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is stark and structural. OpenAI and Disney each experience Rope (coordination with proportional cost-benefit) because they hold symmetrical power and arbitrage options — either could walk away and find alternatives. Their effective extraction (chi) is low/negative. Competing AI developers experience Snare (pure extraction, no exit options, trapped power position) because the character IP library is irreplaceable and they cannot negotiate equivalent access. Their effective extraction is maximal. The character derivative ecosystem experiences Tangled Rope (mixed coordination and extraction) because they benefit from Disney ecosystem access but face displacement from automated generation. The public AI commons and policy coalitions experience the constraint as temporary (Scaffold) because regulatory intervention or technological substitutes could collapse the exclusivity mechanism within a generational timescale. The traditional licensing apparatus experiences the constraint as Piton (inertial, performative, degraded functional role) because legal exclusivity cannot prevent generation once training is complete. This perspectival spectrum reveals that the constraint's classification depends entirely on structural position — no single 'correct' type exists; the presheaf over observation positions IS the answer.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim status plus exit options. OpenAI and Disney are beneficiaries with arbitrage options (d ≈ 0.15), yielding low effective extraction chi. Competitors are victims with trapped exit (d ≈ 0.95), yielding maximal chi. The derivative creator ecosystem are victims with constrained (not trapped) exit (d ≈ 0.70), yielding moderate-high chi. The analytical observer (public AI commons) occupies a structural position of analytical exit (d ≈ 0.72), perceiving the constraint as hybrid coordination-extraction with perspectival asymmetry. Directionality overrides are not needed; the structural derivation captures the relationships accurately. The beneficiary declaration (OpenAI, Disney) correctly identifies who benefits; the victim declaration (competitors, derivative creators, public commons) correctly identifies who bears costs.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: Mandatrophy is resolved by the perspectival decomposition. The constraint is NOT a snare (pure extraction with coordination theater) because OpenAI-Disney genuinely solve a coordination problem (training data scarcity). But it is also NOT a rope (pure coordination) because competitors are structurally suppressed with no exit options and no proportional benefit. The Tangled Rope classification (claimed type) captures this hybrid: real coordination function (beneficiaries: openai_sora_development, disney_strategic_positioning) + real asymmetric extraction (victims: competing_ai_developers, character_derivative_ecosystem, public_generative_model_commons) + required active enforcement (licensing contracts, legal IP protection, monitoring of derivative works). The mandatrophy is resolved by showing that the six-type spectrum is not contradiction but perspectival reality. The constraint genuinely appears as Rope from institutional beneficiaries, Snare from trapped competitors, Tangled Rope from the analytical horizon, Scaffold from policy coalitions, and Piton from the legacy licensing apparatus. No single reading is false; all are structural truths about different observer positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    synthetic_character_substitutability,
    'Can synthetically generated or procedurally created character training data achieve fidelity equivalent to Disney''s canonical character library for video generation tasks?',
    'Benchmark comparison of generated video quality: Sora (trained on Disney IP) vs. open-source Sora-equivalents trained on synthetic characters; user preference studies; downstream task performance (character consistency, emotional expression, action recognition)',
    'If synthetic substitutes succeed: exclusive Disney IP becomes optional, not mandatory. Constraint reclassifies as Piton (legacy advantage, not structural necessity). If synthetic generation fails: Disney exclusivity remains structurally critical. Constraint remains Tangled Rope/Snare from competitors'' perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(synthetic_character_substitutability, empirical, 'Whether synthetic character generation can replace canonical Disney training data').

omega_variable(
    regulatory_fair_access_mandate,
    'Will regulatory frameworks (EU AI Act, proposed creator protection laws) require compulsory licensing or fair-access provisions for entertainment IP used in generative model training?',
    'Legislative tracking in EU, US, UK, and China; court precedents on AI training copyright; regulatory agency guidance on ''essential'' IP assets for AI development; negotiated compensation agreements for training data access',
    'If mandated: Disney must grant non-exclusive access or face regulatory penalties. Exclusivity contract becomes unenforceable. Constraint reclassifies as Rope (coordination only) across all institutional perspectives. If blocked: legal exclusivity remains enforceable indefinitely. Constraint remains Tangled Rope/Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_fair_access_mandate, empirical, 'Whether regulation will mandate fair access to entertainment IP for AI training').

omega_variable(
    model_weight_escape_liability,
    'Can competing labs legally reverse-engineer or extract character-generation weights from Sora outputs, and what legal liability attaches to unauthorized training data reconstruction?',
    'Litigation over model weight recovery; technical feasibility studies of style-transfer attacks; comparison of Sora output forensics to canonical character attributes; enforcement actions by Disney against suspected model theft',
    'If weights are escapable: exclusivity is a temporary technical advantage, not a structural constraint. Competitors can build equivalent models from outputs. Constraint becomes Scaffold with sunset driven by technical decay. If weights are secure: exclusivity remains enforced through legal IP protection. Constraint remains binding.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(model_weight_escape_liability, empirical, 'Whether model weights can be legally reverse-engineered from Sora outputs').

omega_variable(
    creator_compensation_collective_bargaining,
    'Can freelance character artists, animators, and derivative creators organize collective bargaining to demand compensation or licensing revenue from AI-generated derivative works?',
    'Union organizing (animator guilds, writers'' guilds, visual effects unions); class-action litigation; legislative automation compensation frameworks; industry negotiation of AI residuals',
    'If successful: creator compensation mandates would require OpenAI/Disney to reserve revenue for human creators. Constraint becomes forced coordination (Tangled Rope with enforcement toward creators). If blocked: derivative creators have no claim on generated content. Constraint remains Snare from their perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creator_compensation_collective_bargaining, preference, 'Whether creators can organize to demand compensation for automated derivative works').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(disney_openai_ip_exclusivity, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(disneyoai_tr_t0, disney_openai_ip_exclusivity, theater_ratio, 0, 0.35).
narrative_ontology:measurement(disneyoai_tr_t3, disney_openai_ip_exclusivity, theater_ratio, 3, 0.37).
narrative_ontology:measurement(disneyoai_tr_t6, disney_openai_ip_exclusivity, theater_ratio, 6, 0.38).

% Extraction over time
narrative_ontology:measurement(disneyoai_be_t0, disney_openai_ip_exclusivity, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(disneyoai_be_t3, disney_openai_ip_exclusivity, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(disneyoai_be_t6, disney_openai_ip_exclusivity, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(disney_openai_ip_exclusivity, resource_allocation).
narrative_ontology:boltzmann_floor_override(disney_openai_ip_exclusivity, 0.42).
narrative_ontology:affects_constraint(disney_openai_ip_exclusivity, training_data_access_asymmetry).
narrative_ontology:affects_constraint(disney_openai_ip_exclusivity, ai_model_capability_stratification).
narrative_ontology:affects_constraint(disney_openai_ip_exclusivity, creator_compensation_automation).

% DUAL FORMULATION NOTE:
% Disney-OpenAI exclusivity decomposes into three downstream constraints: (1) training_data_access_asymmetry (ε≈0.45) — how competitors navigate IP scarcity; (2) ai_model_capability_stratification (ε≈0.52) — institutional stratification of generative capability; (3) creator_compensation_automation (ε≈0.62) — whether derivative creators can collectively bargain for automation compensation. Each upstream story affects the downstream constraints' extractiveness through resource and legal coupling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
