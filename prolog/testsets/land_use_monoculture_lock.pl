% ============================================================================
% CONSTRAINT STORY: land_use_monoculture_lock
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_land_use_monoculture_lock, []).

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
 *   constraint_id: land_use_monoculture_lock
 *   human_readable: Land Use Monoculture Lock: Agricultural-Industrial Coordination with Ecosystem Extraction
 *   domain: agriculture/environmental/economic
 *
 * SUMMARY:
 *   Land use monoculture lock represents a constraint system where
 *   agricultural industrialization created genuine coordination benefits in
 *   the 20th century but has become an extraction mechanism in the 21st.
 *   Large-scale commodity crop production solved the problem of feeding
 *   growing urban populations and enabled rural mechanization. However, the
 *   institutional structures, subsidy architecture, and knowledge systems
 *   that embedded monoculture have persisted even as the structural
 *   justifications have weakened due to climate volatility, soil depletion,
 *   and market consolidation. The constraint exhibits all six types from
 *   different positions: it is snare-like for smallholder farmers trapped in
 *   debt cycles; tangled rope for rural communities receiving both employment
 *   and ecological cost; rope for agribusiness corporations whose entire
 *   coordination system depends on monoculture's supply chain
 *   standardization; scaffold for regenerative agriculture coalitions
 *   building exit pathways through certification and carbon markets; piton
 *   for agricultural policy institutions that maintain monoculture promotion
 *   through inertia; and falsely mountain-like when naturalized as an
 *   economic law. The extractiveness has increased from 0.35 to 0.58 over the
 *   interval as input costs have risen and as market consolidation has
 *   increased asymmetries between farmers and agribusiness. Theater ratio has
 *   risen from 0.40 to 0.55 as agricultural policy increasingly uses
 *   sustainability language while maintaining monoculture-optimized subsidy
 *   structures.
 *
 * KEY AGENTS:
 *   - Smallholder Farmers: Primary victims (powerless/trapped) — locked into monoculture by debt cycles, seed patents, and loss of diversification knowledge
 *   - Agribusiness Corporations: Primary beneficiaries (institutional/arbitrage) — capture economies of scale, supply chain standardization, and commodity futures advantages
 *   - Input Suppliers (seeds, fertilizers, pesticides): Secondary beneficiaries (institutional/arbitrage) — business model depends on monoculture-driven input consumption
 *   - Rural Communities: Secondary victims (moderate/constrained) — benefit from employment and infrastructure but lose alternative income sources and bear ecological costs
 *   - Regenerative Agriculture Coalition: Organized agents (organized/constrained) — arXiv of agriculture; building alternative certification and market structures for crop diversity
 *   - Agricultural Policy Apparatus: Institutional actor (institutional/arbitrage) — maintains monoculture bias through subsidy design, extension agent training, and crop insurance structures; sees own function as degraded (piton)
 *   - Soil Ecosystems & Pollinator Communities: Powerless victims (powerless/trapped) — bear ecological extraction with no exit; abstractly victim but ecologically concrete
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(land_use_monoculture_lock, 0.58).
domain_priors:suppression_score(land_use_monoculture_lock, 0.68).
domain_priors:theater_ratio(land_use_monoculture_lock, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(land_use_monoculture_lock, extractiveness, 0.58).
narrative_ontology:constraint_metric(land_use_monoculture_lock, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(land_use_monoculture_lock, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(land_use_monoculture_lock, tangled_rope).
narrative_ontology:human_readable(land_use_monoculture_lock, "Land Use Monoculture Lock: Agricultural-Industrial Coordination with Ecosystem Extraction").
narrative_ontology:topic_domain(land_use_monoculture_lock, "agriculture/environmental/economic").

domain_priors:requires_active_enforcement(land_use_monoculture_lock).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(land_use_monoculture_lock, agribusiness_corporations).
narrative_ontology:constraint_beneficiary(land_use_monoculture_lock, commodity_exporters).
narrative_ontology:constraint_beneficiary(land_use_monoculture_lock, input_suppliers).
narrative_ontology:constraint_victim(land_use_monoculture_lock, smallholder_farmers).
narrative_ontology:constraint_victim(land_use_monoculture_lock, ecosystem_services).
narrative_ontology:constraint_victim(land_use_monoculture_lock, rural_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALLHOLDER FARMER (SNARE) — Trapped by debt cycles, seed patent restrictions, and loss of alternative crop knowledge. Cannot access credit for diversification; monoculture seeds require purchased inputs (fertilizer, pesticides) that create dependency. Exit requires capital, land redistribution, and agronomic retraining — all blocked. Bears full extraction cost through input debt and market price volatility.
constraint_indexing:constraint_classification(land_use_monoculture_lock, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: RURAL COMMUNITY (TANGLED ROPE) — Experiences both coordination and extraction. Monoculture provides employment, infrastructure investment, and market access that rural areas lack under diversified systems. Simultaneously, monoculture depletes soil, eliminates non-commodity income sources (foraging, artisanal production), and concentrates land ownership. Exit is constrained by educational pipeline lock-in and lack of alternative economic structures.
constraint_indexing:constraint_classification(land_use_monoculture_lock, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: AGRIBUSINESS CORPORATION (ROPE) — Experiences monoculture as pure coordination mechanism. Single-crop systems enable mechanization, supply chain standardization, and commodity futures trading. Low verification costs, predictable yields, and global market access. Net beneficiary — the constraint solves their coordination problem of scaling production across regions.
constraint_indexing:constraint_classification(land_use_monoculture_lock, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGENERATIVE AGRICULTURE COALITION (SCAFFOLD) — Organized agents (certification bodies, organic movements, permaculture networks) see monoculture as a temporary institutional lock with sunset mechanisms. Soil restoration protocols, agroforestry certification, and carbon credits create economic alternatives to commodity monoculture. As carbon markets mature and consumer premiums for regenerative products increase, the monoculture system loses economic necessity. Theater is moderate (regenerative certification itself has performative elements), but the exit pathway is real and visible.
constraint_indexing:constraint_classification(land_use_monoculture_lock, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: AGRICULTURAL POLICY APPARATUS (PITON) — Government agencies, agricultural extension services, and development banks maintain monoculture promotion through inertia despite degraded primary function. Commodity crop subsidies, crop insurance designed for monoculture, and extension agent training all embed monoculture assumptions. The apparatus persists because it has institutional constituencies (commodity boards, input supplier lobbies) — not because the original function (feeding growing populations) requires monoculture. Theater ratio is high: agricultural extension agents teach monoculture methods that increase vulnerability to climate shocks, contrary to stated resilience goals.
constraint_indexing:constraint_classification(land_use_monoculture_lock, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, monoculture appears to be a necessary law of modern agriculture: economies of scale, mechanization, and population density supposedly require large-scale single-crop systems. This naturalized framing obscures that monoculture is a contingent institutional arrangement optimized for 20th-century commodity markets and fossil fuel inputs. The engine's false summit detector will identify this as naturalization of a policy-contingent system, not a natural law.
constraint_indexing:constraint_classification(land_use_monoculture_lock, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(land_use_monoculture_lock_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(land_use_monoculture_lock, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(land_use_monoculture_lock, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(land_use_monoculture_lock, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(land_use_monoculture_lock, TR),
    TR >= 0.70.

:- end_tests(land_use_monoculture_lock_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. At the constraint's origin (1960s-1980s), monoculture provided genuine coordination benefits justifying the extraction it imposed. As of 2026, the primary justification (yield efficiency per hectare) persists, but secondary costs have accumulated: soil organic matter depletion, pollinator loss, input cost escalation, climate volatility amplification. The rising extractiveness reflects that the coordination benefit is degrading while the extraction mechanism persists. Suppression (0.68): High. Barriers to exit include: debt cycles requiring commodity crop income to service; seed patent restrictions preventing farmer seed-saving; agronomic knowledge loss (farmers no longer know how to grow diverse crops); subsidy structures that price commodity monoculture below true cost, making diverse crops appear uneconomical; market access barriers for non-commodity products; land concentration that eliminates options for small-scale diversification; and institutional capture of extension services. Theater ratio (0.55): Moderate and rising. Agricultural institutions increasingly use 'sustainability' rhetoric (precision agriculture, climate-smart agriculture) while maintaining commodity monoculture optimization. The gap between stated goals (resilience, soil health) and optimized systems (single-crop dependency) constitutes theater.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the agribusiness perspective (Rope: monoculture solves coordination, low extraction) and the smallholder farmer perspective (Snare: debt traps, trapped exit, maximum extraction) is the defining asymmetry. Both perspectives measure the same structural system, but from opposite directionalities. The gap reveals that what is genuine coordination for the beneficiary is extraction for the victim. The rural community sees both (Tangled Rope) — monoculture does provide employment, but also depletes soil and eliminates alternatives. The policy apparatus sees its own degradation (Piton) — extension services that were designed to increase farmer productivity now increase farmer input dependency. The regenerative coalition sees a temporary system with sunset mechanisms (Scaffold). The civilizational analyst risks naturalizing a contingent 20th-century policy choice.
 *
 * DIRECTIONALITY LOGIC:
 *   The pipeline computes directionality from beneficiary/victim declarations and power level × exit options. Smallholder farmers are declared victims with trapped exit → high d → high f(d) → high experienced extraction (snare perspective). Agribusiness is declared beneficiary with arbitrage exit → low d → low/negative f(d) → negative experienced extraction (rope perspective). Rural communities are victims with constrained exit → moderate d → moderate f(d) → moderate extraction (tangled rope perspective). The regenerative coalition is organized with constrained exit, treating the constraint as temporary → moderate extraction but visible sunset. The policy apparatus benefits from the institutional status quo but is captured within it → moderate d for an institutional actor, producing piton classification from theater rather than from chi. The analytical observer risks seeing monoculture as natural law but lacks the structural data to justify this — the false summit detector flags naturalization.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The constraint satisfies all three gates. (1) Beneficiaries exist: agribusiness, input suppliers, and commodity traders benefit from monoculture's supply chain standardization and market predictability. This is genuine coordination — these agents could not operate at current scale without monoculture. (2) Victims exist: smallholder farmers, soil ecosystems, and pollinator communities bear extraction costs. (3) Active enforcement exists: subsidy design, seed patents, extension service training, and land-clearing regulations enforce monoculture adoption. The tangled rope type prevents misclassification as pure extraction (Snare) — monoculture does solve real coordination problems. It also prevents misclassification as pure coordination (Rope) — the distribution of costs and benefits is asymmetric and maintained through enforcement. As climate volatility increases (omega 4), the coordination benefit may degrade faster than the extraction mechanism, potentially shifting the constraint toward Snare. The theater ratio indicates the policy apparatus increasingly masks extraction with sustainability language while maintaining monoculture-optimized structures — a Piton signature emerging within the Tangled Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    soil_depletion_reversibility,
    'What is the time horizon for soil ecosystem recovery after monoculture exit, and does it exceed the investment payback period for diversification?',
    'Long-term soil quality monitoring in transition farms; quantification of organic matter recovery curves and mycorrhizal network restoration timelines',
    'If recovery < 5 years: exit is economically rational despite short-term yield loss. If recovery > 15 years: exit becomes intergenerational sacrifice, raising identity_locked dynamics (farmer identity locked into monoculture via parent expectations and knowledge inheritance).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(soil_depletion_reversibility, empirical, 'Timeline for soil ecosystem recovery after monoculture exit').

omega_variable(
    market_access_alternative,
    'Do diversified agricultural systems have genuine market access and price stability comparable to commodity monoculture, or is ''market access'' rhetoric substituting for actual supply chain barriers?',
    'Comparative analysis of diversified farm profitability vs monoculture across regions; investigation of why organic and diverse-crop premiums persist but do not scale; supply chain audit for non-commodity agricultural products',
    'If genuine access exists: farmers are not trapped but constrained (rational economic calculation). If access is illusory: farmers are trapped, and the snare perspective is strengthened. This determines whether exit_options should be ''constrained'' or ''trapped'' for the smallholder farmer.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(market_access_alternative, empirical, 'Whether diversified agricultural systems have genuine market access').

omega_variable(
    subsidy_architecture_counterfactual,
    'If commodity subsidies were removed and replaced with equal funding for regenerative agriculture, would farmer preference shift away from monoculture, or are there structural constraints beyond price signals?',
    'Policy counterfactual analysis; examination of subsidy withdrawal cases (EU, India); behavioral studies of farmer crop choice when prices are equal',
    'If preference shifts: extraction is mediated through subsidy design (Tangled Rope becomes more visible). If preference persists: identity-lock and knowledge-lock dimensions are structural (identity_locked exit option becomes appropriate; piton classification is strengthened).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsidy_architecture_counterfactual, empirical, 'Farmer preference for monoculture absent subsidies and price distortions').

omega_variable(
    climate_volatility_threshold,
    'At what level of weather unpredictability does monoculture''s yield stability advantage reverse, and is that threshold being crossed in the interval measurement period?',
    'Analysis of yield variance in monoculture vs diversified systems across climate regimes; extrapolation of regional precipitation/temperature volatility trends',
    'If threshold is crossed during interval: monoculture''s primary economic justification evaporates, and the constraint transitions from Tangled Rope (genuine coordination benefit) to Snare (pure extraction). This omega resolves the mandatrophy: does the coordination function degrade, changing the classification?',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(climate_volatility_threshold, empirical, 'Climate threshold at which monoculture yields become less stable than diversified systems').

omega_variable(
    identity_lock_mechanism_in_agriculture,
    'Is farmer attachment to monoculture crops primarily structural (economic necessity, lack of alternatives) or identity-locked (internalized belief that monoculture is ''modern farming'', tied to social status and family reputation)?',
    'Qualitative research on farmer identity narratives; comparison of crop choice when economic constraints are removed (e.g., land gift, debt forgiveness) vs when they persist',
    'If identity-locked: exit requires not just capital but identity shift (becoming a ''traditional'' or ''organic'' farmer carries perceived status loss). This explains why some farmers refuse diversification even when economically advantageous. Classification of farmer perspective shifts from trapped to identity_locked, deepening the extraction narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_in_agriculture, conceptual, 'Whether farmer attachment to monoculture is structural or identity-locked').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(land_use_monoculture_lock, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(monocult_tr_t0, land_use_monoculture_lock, theater_ratio, 0, 0.4).
narrative_ontology:measurement(monocult_tr_t10, land_use_monoculture_lock, theater_ratio, 10, 0.5).
narrative_ontology:measurement(monocult_tr_t20, land_use_monoculture_lock, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(monocult_be_t0, land_use_monoculture_lock, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(monocult_be_t10, land_use_monoculture_lock, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(monocult_be_t20, land_use_monoculture_lock, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(land_use_monoculture_lock, resource_allocation).
narrative_ontology:affects_constraint(land_use_monoculture_lock, agricultural_subsidies).
narrative_ontology:affects_constraint(land_use_monoculture_lock, seed_patent_monopoly).
narrative_ontology:affects_constraint(land_use_monoculture_lock, soil_carbon_market_arbitrage).

% DUAL FORMULATION NOTE:
% Monoculture lock decomposes into three structurally distinct constraints: (1) agricultural_subsidies (ε=0.42, Tangled Rope) — coordinates commodity market access while extracting from small-scale producers through price distortion; (2) seed_patent_monopoly (ε=0.71, Snare) — pure extraction via intellectual property restriction, no coordination benefit; (3) soil_carbon_market_arbitrage (ε=0.35, Rope) — emerging coordination mechanism for carbon credit allocation across land use types. Land use monoculture lock is a meta-constraint linking these three. Its classification and extractiveness depend on which sub-constraint dominates the agent's experience.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(land_use_monoculture_lock, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
