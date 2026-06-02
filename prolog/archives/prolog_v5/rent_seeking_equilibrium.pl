% ============================================================================
% CONSTRAINT STORY: rent_seeking_equilibrium
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rent_seeking_equilibrium, []).

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
 *   constraint_id: rent_seeking_equilibrium
 *   human_readable: The Toll-Bridge Stagnation
 *   domain: economic/political
 *
 * SUMMARY:
 *   Rent-seeking equilibrium describes a state where economic agents
 *   rationally invest more resources in capturing existing wealth through
 *   political, legal, and regulatory influence than in creating new value
 *   through innovation and productive enterprise. This constraint manifests
 *   as licensing boards that prevent entry, tariff systems that protect
 *   incumbents, occupational restrictions that limit labor mobility, and
 *   regulatory compliance burdens that impose fixed costs crushing small
 *   competitors but leaving large incumbents unharmed. The toll-bridge
 *   metaphor captures the core dynamic: a few agents control access to
 *   markets and extract rents by controlling the bridge rather than by
 *   improving it. This constraint exhibits the full range of DR
 *   classifications depending on structural position. Incumbent beneficiaries
 *   see it as coordination (Rope). Productive innovators see it as a snare
 *   (Snare). Reform coalitions see it as temporary and solvable (Scaffold).
 *   The regulatory apparatus sees its own degradation (Piton). Most
 *   fundamentally, it is a hybrid coordination-extraction mechanism that
 *   enables powerful actors to coordinate their market position through
 *   political influence while extracting from those excluded (Tangled Rope).
 *
 * KEY AGENTS:
 *   - Incumbent Rent-Seekers: Institutional beneficiaries (institutional/arbitrage) — established firms, professional associations, regulated industries that benefit from barriers to entry and regulatory capture; invest heavily in lobbying to maintain protected market position
 *   - Productive Innovators: Primary victims (powerless/trapped) — startups, new market entrants, disruptors that face regulatory barriers, licensing requirements, and compliance costs that exceed their available capital; cannot escape without exiting market entirely
 *   - Regulatory Capture Apparatus: Institutional beneficiary (institutional/arbitrage) — regulatory agencies, licensing boards, standards bodies that maintain their budget and relevance through rent-seeking gatekeeping; benefit from incumbent support
 *   - General Consumer Welfare: Diffuse victim (powerless/trapped) — consumers and workers bear the costs through higher prices, reduced innovation, fewer job opportunities, and constrained quality improvements; too dispersed to organize effective opposition
 *   - Mid-Market Competitors: Moderate victim (moderate/constrained) — firms large enough to survive but small enough to feel compliance burden; participate in rent-seeking to stay viable but face asymmetric extraction relative to incumbents
 *   - Anti-Monopoly Reform Coalition: Organized agent (organized/constrained) — civil society, antitrust agencies, political reform movements, and some tech platforms (seeking to disrupt traditional rent-seeking) that see the system as changeable through deregulation and enforcement
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees the constraint as a hybrid coordination-extraction mechanism where the coordination function (incumbents aligning on regulatory protection) is real but primarily serves asymmetric extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rent_seeking_equilibrium, 0.58).
domain_priors:suppression_score(rent_seeking_equilibrium, 0.68).
domain_priors:theater_ratio(rent_seeking_equilibrium, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rent_seeking_equilibrium, extractiveness, 0.58).
narrative_ontology:constraint_metric(rent_seeking_equilibrium, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(rent_seeking_equilibrium, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rent_seeking_equilibrium, tangled_rope).
narrative_ontology:human_readable(rent_seeking_equilibrium, "The Toll-Bridge Stagnation").
narrative_ontology:topic_domain(rent_seeking_equilibrium, "economic/political").

domain_priors:requires_active_enforcement(rent_seeking_equilibrium).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rent_seeking_equilibrium, incumbent_rent_seekers).
narrative_ontology:constraint_beneficiary(rent_seeking_equilibrium, regulatory_capture_apparatus).
narrative_ontology:constraint_victim(rent_seeking_equilibrium, productive_innovators).
narrative_ontology:constraint_victim(rent_seeking_equilibrium, general_consumer_welfare).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRODUCTIVE INNOVATOR (SNARE) — Faces regulatory barriers, licensing requirements, and incumbent gatekeeping that consume resources without creating value. Cannot exit the market without accepting losses. Zero degrees of freedom — extraction is maximal and inescapable.
constraint_indexing:constraint_classification(rent_seeking_equilibrium, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INCUMBENT RENT-SEEKER (ROPE) — Experiences the constraint as coordination. Political influence over regulation coordinates the market to their advantage. Can arbitrage between jurisdictions with different regulatory regimes. Net beneficiary — the extraction flow moves toward them.
constraint_indexing:constraint_classification(rent_seeking_equilibrium, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: MID-MARKET COMPETITOR (TANGLED ROPE) — Constrained by regulatory costs but can survive by participating in rent-seeking themselves. Benefits from occasional market access through lobbying; bears costs through compliance burdens. Mixed coordination and extraction — some agency but real constraints.
constraint_indexing:constraint_classification(rent_seeking_equilibrium, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ANTI-MONOPOLY REFORM COALITION (SCAFFOLD) — Organized civil society, antitrust agencies, and political reform movements see rent-seeking equilibrium as a temporary institutional failure with a sunset. Deregulation, antitrust enforcement, and open-access mandates are alternative pathways. Extraction remains manageable because exit pathways are visible and partially realized.
constraint_indexing:constraint_classification(rent_seeking_equilibrium, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY APPARATUS (PITON) — The regulatory machinery persists through institutional inertia. Licensing boards, tariff systems, and occupational restrictions continue to operate despite widespread recognition that they primarily serve rent-seeking rather than legitimate public protection. Theater ratio is high — enforcement exists but primary function has atrophied, replaced by performative legitimacy claims.
constraint_indexing:constraint_classification(rent_seeking_equilibrium, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a global civilizational view, rent-seeking equilibrium is partially a coordination mechanism (solving the incumbent's collective action problem in maintaining regulatory barriers) and partially pure extraction (destroying productive potential and consumer welfare). The constraint has genuine coordination function but operates primarily as asymmetric extraction.
constraint_indexing:constraint_classification(rent_seeking_equilibrium, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rent_seeking_equilibrium_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rent_seeking_equilibrium, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rent_seeking_equilibrium, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rent_seeking_equilibrium, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rent_seeking_equilibrium, TR),
    TR >= 0.70.

:- end_tests(rent_seeking_equilibrium_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts significant resources from productive agents through regulatory barriers, compliance costs, and denied market access. However, it is not maximal (0.70+) because some exit mechanisms exist: digital services, geographic arbitrage, and jurisdictional competition create partial escape routes. The innovator can sometimes work around the barrier by changing location or service model. Suppression (0.68): High. Barriers are substantial and multifaceted: licensing requirements, compliance infrastructure, political networks required for regulatory navigation, capital requirements for navigating legal barriers. However, suppression is not absolute (0.85+) because organized reform coalitions have successfully attacked rent-seeking structures (antitrust, deregulation), and digital disruption sometimes jumps over barriers entirely. Theater ratio (0.65): Moderate-high. Regulatory apparatus justifies itself through public interest narratives (consumer protection, quality assurance, safety standards) even though primary function has become rent extraction. The theatrical element includes licensing exams that don't correlate with service quality, compliance documentation that serves no safety function, and standards boards where industry incumbents set rules. Theater has risen over the interval as regulatory complexity has increased but consumer outcomes have stagnated.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces a maximal perspectival gap. Incumbent rent-seekers see a purely functional coordination mechanism — they have aligned political interests and regulatory barriers coordinate the market to their mutual benefit, with low transaction costs (Rope). Productive innovators see a pure extraction mechanism — they bear all costs and gain no benefits (Snare). Mid-market competitors see a hybrid system — they must participate in rent-seeking to survive but face permanent cost disadvantage relative to incumbents (Tangled Rope). The reform coalition sees a temporary institutional failure with visible exit pathways through antitrust enforcement and deregulation (Scaffold). The regulatory apparatus sees its own degradation — it knows licensing serves entry prevention rather than quality assurance, but persists through bureaucratic inertia and incumbent pressure (Piton). The analytical observer at civilizational scale sees the constraint as fundamentally a tangled rope: it has genuine coordination function (enables incumbent collective action) but primarily operates as extraction mechanism (destroys productive potential). All six types appear simultaneously because the constraint's structure genuinely produces different rational classifications from different structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality d is determined by each agent's structural relationship to the constraint. Incumbent rent-seekers are beneficiaries with arbitrage exits (can move capital elsewhere, lobby different jurisdictions) — derived d ≈ 0.15, very low effective extraction experienced. Productive innovators are victims with trapped exits (cannot exit without losing entire business investment) — derived d ≈ 0.95, maximum experienced extraction. Mid-market competitors are victims with constrained exits (can survive by participating in rent-seeking but at higher cost than incumbents) — derived d ≈ 0.70, high experienced extraction. Consumers are victims with trapped exits (cannot easily arbitrage between jurisdictions) — derived d ≈ 0.92. The analytical observer measures the system effect across all agents — d ≈ 0.72, producing the tangled rope classification. The directionality pipeline computes chi = ε × f(d) × σ(S) for each perspective: beneficiaries get negative chi (experience as low-cost coordination); victims get high chi (experience as extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing how a tangled rope (hybrid coordination-extraction) can be confused with pure coordination (Rope) from the beneficiary perspective. The incumbent rent-seekers genuinely experience the constraint as Rope — it solves their collective action problem in maintaining regulatory barriers with minimal coercion (they lobby, not invade competitors). But this is mandatrophy: the beneficiary's experience of low-coercion coordination is built on the victim's experience of high-extraction snare. The constraint is tangled rope because it simultaneously (1) provides genuine coordination benefit to incumbents (aligns them on regulatory protection) and (2) extracts asymmetrically from innovators (denies them market access). The piton classification of the regulatory apparatus is the key diagnostic: high theater (performative justification) combined with low functional efficacy (licensing doesn't predict quality) indicates that the regulatory mechanism is maintaining itself through inertia rather than serving legitimate coordination. The scaffold perspective by the reform coalition shows the constraint is not immutable — antitrust enforcement, deregulation, and digital disruption are creating exit pathways. The constraint is solvable but requires recognizing that 'coordination' and 'extraction' are not alternatives but simultaneous functions of the same mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    productive_vs_rent_seeking_threshold,
    'At what ratio of rent-seeking-to-productive investment does an economy enter irreversible decline, and is that threshold culturally contingent or universal?',
    'Longitudinal economic analysis: comparison of R&D investment, startup formation rates, and regulatory compliance costs across economies; correlation with GDP growth and innovation metrics',
    'If threshold is universal and low (10-15%): current OECD economies may already be past the tipping point. If threshold is high (40%+) or culturally variable: significant policy intervention remains effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(productive_vs_rent_seeking_threshold, empirical, 'Threshold ratio of rent-seeking to productive investment that triggers irreversible decline').

omega_variable(
    regulatory_capture_self_awareness,
    'Do regulatory actors recognize themselves as captured, and does that recognition create pressure for reform or entrench defensive theater?',
    'Qualitative analysis of regulatory agency statements, internal documents, and reform proposals; measurement of time between public acknowledgment of capture and actual structural change',
    'If recognition leads to reform: scaffold sunset timeline accelerates. If recognition triggers defensive theater: piton classification becomes dominant and equilibrium becomes harder to exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_self_awareness, conceptual, 'Whether regulatory self-awareness drives reform or entrenchment').

omega_variable(
    exit_substitution_feasibility,
    'Can decentralized alternatives (remote work, digital services, jurisdictional arbitrage) actually bypass rent-seeking barriers, or do incumbents capture those too?',
    'Empirical tracking: emergence and growth of alternative service models; measurement of incumbent countermeasures (lobbying to restrict alternatives); comparison of compliance costs across channels',
    'If alternatives remain viable: productive innovators retain exit options (mobile classification likely). If incumbents capture alternatives: trap becomes total (trapped classification likely).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_substitution_feasibility, empirical, 'Whether decentralized alternatives can sustainably bypass rent-seeking capture').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rent_seeking_equilibrium, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rent_tr_t0, rent_seeking_equilibrium, theater_ratio, 0, 0.42).
narrative_ontology:measurement(rent_tr_t10, rent_seeking_equilibrium, theater_ratio, 10, 0.55).
narrative_ontology:measurement(rent_tr_t20, rent_seeking_equilibrium, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(rent_be_t0, rent_seeking_equilibrium, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(rent_be_t10, rent_seeking_equilibrium, base_extractiveness, 10, 0.47).
narrative_ontology:measurement(rent_be_t20, rent_seeking_equilibrium, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rent_seeking_equilibrium, enforcement_mechanism).
narrative_ontology:affects_constraint(rent_seeking_equilibrium, regulatory_capture).
narrative_ontology:affects_constraint(rent_seeking_equilibrium, occupational_licensing_barriers).
narrative_ontology:affects_constraint(rent_seeking_equilibrium, startup_survival_rates).

% DUAL FORMULATION NOTE:
% Rent-seeking equilibrium is upstream of specific regulatory captures (taxi medallions, medical licensing) but represents a distinct structural constraint operating at the economy-wide level. The specific regulatory captures have their own extractiveness values reflecting domain-specific barriers; the equilibrium constraint reflects the systemic metaproblem that agents rationally invest more in rent-seeking than production.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rent_seeking_equilibrium, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
