% ============================================================================
% CONSTRAINT STORY: quad_alliance_cohesion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quad_alliance_cohesion, []).

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
 *   constraint_id: quad_alliance_cohesion
 *   human_readable: Quad Alliance Cohesion (India-Japan-Australia-US Strategic Partnership)
 *   domain: geopolitical/strategic_alliance
 *
 * SUMMARY:
 *   The Quad Alliance (India, Japan, Australia, United States) represents a
 *   structural coordination mechanism for managing Indo-Pacific strategic
 *   competition against Chinese regional dominance. However, it
 *   simultaneously extracts costs from non-member states and creates
 *   asymmetric constraints on member agency. The constraint exhibits Tangled
 *   Rope classification from most institutional perspectives (genuine
 *   security and economic coordination coexisting with constrained autonomy),
 *   but classifies as pure extraction (Snare) from the perspective of trapped
 *   smaller Pacific states and as degraded template (Piton) from the
 *   perspective of Cold War institutional memory. The theater ratio rising
 *   from 0.42 to 0.70 indicates that coordinated rhetoric ('like-minded
 *   democracies,' 'rules-based order') is increasingly performing ideological
 *   boundaries rather than solving concrete coordination problems. Base
 *   extractiveness rising from 0.38 to 0.58 reflects growing supply-chain
 *   weaponization, alignment coercion, and smaller-state entrapment as the
 *   competition with China intensifies. The constraint is stable (no collapse
 *   predicted in the 6-year window) but destabilizing (theater growth signals
 *   Piton degradation; extraction growth signals potential reclassification
 *   toward Snare if exit costs increase further).
 *
 * KEY AGENTS:
 *   - United States: Primary architect (institutional/arbitrage) — benefits from efficient anti-hegemonic coalition with minimal enforcement burden; highest exit optionality
 *   - India: Regional balancer (powerful/constrained) — gains security reassurance and technology access; loses negotiating freedom with non-Quad partners; aspires to strategic autonomy
 *   - Japan: Technology-security linchpin (organized/mobile) — coordinates US security guarantee with technology investment; constrained by alignment homogenization pressure
 *   - Australia: Indo-Pacific repositioner (powerful/constrained) — gains trade access and security assurance; faces supply-chain vulnerability and China retaliation risk
 *   - Smaller Pacific States: Trapped intermediaries (powerless/trapped) — dependent on both Quad financing and Chinese investment; no exit option; bears full extraction cost
 *   - ASEAN: Identity-locked consensus-builder (organized/identity_locked) — structurally mobile but bound by non-alignment identity frame; identity frame breaking under pressure
 *   - Cold War Alliance Template: Institutional legacy (institutional/arbitrage) — persists through inertia despite mismatch to multipolar context; theater rising as performativity increases
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quad_alliance_cohesion, 0.58).
domain_priors:suppression_score(quad_alliance_cohesion, 0.48).
domain_priors:theater_ratio(quad_alliance_cohesion, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quad_alliance_cohesion, extractiveness, 0.58).
narrative_ontology:constraint_metric(quad_alliance_cohesion, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(quad_alliance_cohesion, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quad_alliance_cohesion, tangled_rope).
narrative_ontology:human_readable(quad_alliance_cohesion, "Quad Alliance Cohesion (India-Japan-Australia-US Strategic Partnership)").
narrative_ontology:topic_domain(quad_alliance_cohesion, "geopolitical/strategic_alliance").

domain_priors:requires_active_enforcement(quad_alliance_cohesion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quad_alliance_cohesion, us_strategic_dominance).
narrative_ontology:constraint_beneficiary(quad_alliance_cohesion, india_strategic_autonomy).
narrative_ontology:constraint_beneficiary(quad_alliance_cohesion, japan_security_guarantee).
narrative_ontology:constraint_beneficiary(quad_alliance_cohesion, australia_trade_access).
narrative_ontology:constraint_victim(quad_alliance_cohesion, asean_consensus).
narrative_ontology:constraint_victim(quad_alliance_cohesion, chinese_regional_stability).
narrative_ontology:constraint_victim(quad_alliance_cohesion, small_pacific_states).
narrative_ontology:constraint_victim(quad_alliance_cohesion, alliance_neutrals).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALLER PACIFIC STATES (SNARE) — Structurally dependent on both Quad financing and Chinese investment; cannot exit either alignment without economic catastrophe. Trapped between great-power extraction mechanisms with no exit option. Bears full cost of geopolitical competition without participation in decision-making.
constraint_indexing:constraint_classification(quad_alliance_cohesion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: INDIA (TANGLED ROPE) — Genuine coordination benefit (security reassurance, technology transfer, market access) coexists with extraction: constrained alignment reduces negotiating freedom with non-Quad partners (Russia, Iran, Central Asia). High exit cost (losing strategic partner in border disputes with Pakistan/China) but meaningful agency to modulate commitment. Extraction runs asymmetrically toward alignment homogenization at cost to Indian autonomy.
constraint_indexing:constraint_classification(quad_alliance_cohesion, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: UNITED STATES (ROPE) — Primary architect and beneficiary of Quad structure. Experiences the alliance as efficient coordination of anti-hegemonic burden-sharing: each partner contributes specialized capacity (India's continental reach, Japan's technology, Australia's Indian Ocean presence) with minimal enforcement cost from US perspective. Net beneficiary with high exit optionality (can pivot to bilateral agreements or regional partnerships).
constraint_indexing:constraint_classification(quad_alliance_cohesion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: JAPAN (TANGLED ROPE) — Coordinates security guarantee (US extended deterrence against China/Russia) with technology investment and market expansion. Also experiences extraction: alignment constrains relationship-building with non-Quad Asian partners (South Korea, Vietnam, ASEAN consensus). Exit cost is high (loss of US security umbrella in contested East China Sea) but organizational capacity to modulate commitment through selective participation exists (e.g., Quad Plus rhetoric without formal expansion).
constraint_indexing:constraint_classification(quad_alliance_cohesion, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: AUSTRALIA (TANGLED ROPE) — Genuine coordination of security and trade access coexists with extraction via supply-chain vulnerability (technology dependency, trade vulnerability to China retaliation). Constrained exit (backing away damages US alliance that underwrites Australian security) but organized capacity to negotiate terms (domestic politics constraining commitment expansion). Theater rising as rhetoric of 'democratic values' alliance masks resource competition for rare earths and critical minerals.
constraint_indexing:constraint_classification(quad_alliance_cohesion, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: ASEAN CONSENSUS (SNARE with identity_lock) — Structurally mobile (ASEAN is organized, has exit options) but identity-locked to non-alignment principle ('ASEAN Way'). Quad expansion (Quad Plus rhetoric) constrains ASEAN consensus-building by creating pressure to choose sides. Exit would require ASEAN to abandon foundational identity of being the consensus-builder/bridge rather than alliance member. Identity lock binding more tightly than material barriers.
constraint_indexing:constraint_classification(quad_alliance_cohesion, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 7: COLD WAR ALLIANCE TEMPLATE (PITON) — The Quad mimics alliance structures from Cold War institutional memory (NATO-like burden-sharing, 'free world' coordination rhetoric) but functions in multipolar context where the template is increasingly performative. Theater ratio rising: rhetoric of 'like-minded democracies' performs ideological boundary while actual cooperation remains transactional and narrow (intelligence-sharing, naval coordination). The institutional template persists through inertia despite mismatch to current strategic environment.
constraint_indexing:constraint_classification(quad_alliance_cohesion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational perspective, the Quad represents genuine coordination (anti-hegemonic burden-sharing, maritime security, technology standards) combined with structural extraction (coercive alignment dynamics, supply-chain weaponization, smaller-state entrapment). The constraint is neither pure coordination nor pure extraction but hybrid — stable only because no single member faces unsustainable extraction and all perceive net security benefit. Stability depends on maintaining asymmetric extraction costs at tolerable levels.
constraint_indexing:constraint_classification(quad_alliance_cohesion, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quad_alliance_cohesion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(quad_alliance_cohesion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(quad_alliance_cohesion, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(quad_alliance_cohesion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(quad_alliance_cohesion, TR),
    TR >= 0.70.

:- end_tests(quad_alliance_cohesion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.58): High-moderate. The Quad combines genuine security coordination (US provides extended deterrence, India provides continental reach, Japan provides technology, Australia provides Indian Ocean presence) with significant extraction mechanisms. Supply-chain vulnerability (technology dependency on US suppliers or Chinese alternatives), alignment coercion (pressure toward policy homogenization), and smaller-state entrapment (forced choice between Quad and Chinese economic engagement) constitute meaningful extraction. The value reflects that extraction is substantial but not maximum — no single member faces unsustainable costs and all perceive net security benefit. Suppression (0.48): Moderate. Significant barriers to exit include security dependency (especially for Japan/India), economic interdependency, and geopolitical leverage via supply-chain control. However, suppression is not total — all members retain some autonomy to modulate commitment through selective participation, technology policy variation, and bilateral relationship management. Theater ratio (0.65): High and rising. Initial rhetoric of 'like-minded democracies' and 'rules-based order' performed genuine security problem-solving in early years (0.42 theater). As the competition with China intensified, the rhetoric became increasingly performative without corresponding deepening of actual institutional coordination. By year 6, significant divergence exists on China policy (India-Russia ties, Japan-China economics, Australia-China tensions) masked by 'united democracies' framing. The rising theater indicates Goodhart drift — the metric (rhetorical alignment) is substituting for the target (actual policy coordination).
 *
 * PERSPECTIVAL GAP:
 *   The Quad constraint produces maximum perspectival divergence: a single structural arrangement classifies as Rope (US), Tangled Rope (India/Japan/Australia), Snare (smaller Pacific states), Snare with identity_lock (ASEAN), and Piton (Cold War template). The gap between US Rope and smaller-state Snare is particularly acute — the same coordination mechanism that solves the problem for the primary architect extracts maximally from agents with no voice in the decision. ASEAN's perspective adds a critical diagnostic: identity-lock produces Snare classification despite organized power level, revealing that organizational capacity alone does not ensure exit capability if the identity frame naturalizes entrapment. The analytical observer's Tangled Rope perspective reveals the rising theater as Goodhart drift — the rhetorical metric (democratic values alignment) is substituting for the actual target (policy coordination), and the substitution is accelerating as divergence increases.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality reflects each agent's structural position in the extraction flow. Beneficiaries (US gaining anti-hegemonic coalition, India gaining security reassurance, Japan gaining extended deterrence, Australia gaining trade access) derive lower d values. Victims (smaller Pacific states trapped between great-power extraction, ASEAN identity constrained by alignment pressure) derive higher d values. The sigmoid f(d) converts these to experienced extractiveness: US with arbitrage exit experiences negative effective extraction (benefits from the arrangement), constrained actors experience positive but moderate extraction, trapped and identity-locked actors experience high extraction. Suppression is structural (not scaled by directionality): barriers to exit include security dependency, supply-chain leverage, and geopolitical coercion. These operate equally across all members regardless of their strategic benefit — even the beneficiaries face suppression if they attempt to exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The Quad constraint resolves the mandatrophy through perspectival decomposition rather than single-type classification. At the institutional level (US + India + Japan + Australia), it is genuinely Tangled Rope — coordination function (anti-hegemonic burden-sharing) and extraction function (alignment coercion, supply-chain control) coexist and are both necessary to the arrangement's stability. At the smaller-state level, it is Snare — extraction without meaningful coordination benefit. At the ASEAN level, it is Snare with identity-lock — the constraint is not extractive primarily through material barriers but through cognitive frames that bind identity to non-alignment while non-alignment becomes structurally untenable. The analytical observer sees all these as aspects of the same constraint, which prevents the false dichotomy of 'is it coordination or extraction?' The answer is 'yes, both, distributed asymmetrically across the agent space.' The mandatrophy is resolved by recognizing that the classification depends on the observer's position, not on discovering 'the' true type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quad_cohesion_threshold,
    'What level of policy divergence (India-Russia trade, Japan-China economic ties, Australia-China tensions, US pivot credibility) constitutes fatal alliance incoherence vs normal partner variation?',
    'Tracking of formal vs informal Quad positions; analysis of defection patterns during crises; measurement of private vs public alignment on China policy, trade barriers, technology standards',
    'If threshold is high: Quad classification remains Tangled Rope despite significant extraction — members tolerate divergence. If threshold is low: minor divergence triggers reclassification to Piton (performative alliance) or fragmentation into separate dyadic arrangements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quad_cohesion_threshold, empirical, 'What divergence level constitutes fatal alliance incoherence').

omega_variable(
    extraction_mechanism_visibility,
    'Are the extraction mechanisms (supply-chain vulnerability, alignment coercion, smaller-state entrapment) recognized as such by affected agents or naturalized as ''strategic necessity''?',
    'Discourse analysis of how member states frame Quad constraints; comparison of private vs public statements; tracking of domestic opposition to Quad participation',
    'If naturalized: extraction persists because actors don''t perceive it; classification remains stable. If visible: actors may begin exit calculations, pushing toward Piton (rhetorical only) or fragmentation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_mechanism_visibility, empirical, 'Whether extraction mechanisms are recognized or naturalized').

omega_variable(
    asean_identity_lock_durability,
    'Can ASEAN''s non-alignment identity survive indefinite pressure to choose Quad vs China vs non-alignment, or will the identity frame eventually break under sustained structural pressure?',
    'Longitudinal tracking of ASEAN consensus-building capacity; analysis of intra-ASEAN divergence on US-China competition; identification of breaking points where consensus model fails',
    'If identity persists: ASEAN remains trapped (snare). If identity breaks: ASEAN bifurcates into aligned and non-aligned subgroups, reducing ASEAN''s structural power and increasing small-state entrapment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asean_identity_lock_durability, empirical, 'Whether ASEAN non-alignment identity survives structural pressure').

omega_variable(
    us_pivot_credibility,
    'Will US commitment to Indo-Pacific remain credible through a full US-China strategic competition cycle, or will US domestic political shifts (isolationism, budget constraints, China engagement phases) hollow out the Quad''s security coordination function?',
    'Tracking of US defense budget allocations to Indo-Pacific; analysis of US alliance statements across administrations; measurement of actual US military presence and operational commitments',
    'If credible: Quad remains Tangled Rope as security guarantee holds. If hollow: Quad degrades to Piton (performance only) and members recalibrate toward bilateral arrangements or non-alignment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_pivot_credibility, empirical, 'Whether US Indo-Pacific pivot commitment remains credible').

omega_variable(
    technology_standards_divergence,
    'Can the Quad establish competing technology standards (semiconductors, telecommunications, critical minerals) against Chinese alternatives, or will market dynamics force members toward Chinese suppliers regardless of alliance commitment?',
    'Tracking of supply-chain diversification outcomes; measurement of technology standards adoption rates; analysis of cost differentials between Quad-aligned and Chinese suppliers',
    'If standards succeed: extraction through supply-chain vulnerability decreases; classification shifts toward Rope. If standards fail: members face hidden extraction via forced Chinese dependency; classification intensifies toward Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(technology_standards_divergence, empirical, 'Whether Quad can establish competing technology standards').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quad_alliance_cohesion, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quad_tr_t0, quad_alliance_cohesion, theater_ratio, 0, 0.42).
narrative_ontology:measurement(quad_tr_t3, quad_alliance_cohesion, theater_ratio, 3, 0.55).
narrative_ontology:measurement(quad_tr_t6, quad_alliance_cohesion, theater_ratio, 6, 0.65).
narrative_ontology:measurement(quad_tr_t9, quad_alliance_cohesion, theater_ratio, 9, 0.7).

% Extraction over time
narrative_ontology:measurement(quad_be_t0, quad_alliance_cohesion, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(quad_be_t3, quad_alliance_cohesion, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(quad_be_t6, quad_alliance_cohesion, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(quad_be_t9, quad_alliance_cohesion, base_extractiveness, 9, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quad_alliance_cohesion, enforcement_mechanism).
narrative_ontology:affects_constraint(quad_alliance_cohesion, asean_consensus_integrity).
narrative_ontology:affects_constraint(quad_alliance_cohesion, chinese_regional_dominance).
narrative_ontology:affects_constraint(quad_alliance_cohesion, indo_pacific_supply_chains).
narrative_ontology:affects_constraint(quad_alliance_cohesion, us_strategic_pivot_credibility).

% DUAL FORMULATION NOTE:
% The Quad alliance cohesion constraint is upstream of several regional stability constraints. The supply-chain extraction mechanism affects semiconductor supply chain security. The alignment coercion mechanism affects ASEAN consensus formation. The smaller-state entrapment mechanism affects Indo-Pacific geopolitical stability. Each downstream constraint has its own ε value reflecting the empirical status of specific regional outcomes; this constraint models the structural mechanism generating extraction pressure across the region.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quad_alliance_cohesion, institutional, 0.1).
constraint_indexing:directionality_override(quad_alliance_cohesion, powerful, 0.62).
constraint_indexing:directionality_override(quad_alliance_cohesion, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
