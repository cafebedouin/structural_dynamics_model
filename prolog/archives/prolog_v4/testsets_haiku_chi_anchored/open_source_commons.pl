% ============================================================================
% CONSTRAINT STORY: open_source_commons
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_open_source_commons, []).

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
 *   constraint_id: open_source_commons
 *   human_readable: The Mutual Garden
 *   domain: social/technological
 *
 * SUMMARY:
 *   The open-source commons represents a high-trust coordination environment
 *   where software code and knowledge are shared freely, with no formal
 *   extraction mechanism and minimal suppression of alternatives.
 *   Contributors, consumers, and maintainers interact through transparent
 *   governance, meritocratic decision-making, and voluntary participation.
 *   The constraint exhibits genuine rope characteristics: it solves the
 *   collective action problem of knowledge dissemination by aligning
 *   individual incentives (reputation, skill development, solving personal
 *   problems) with collective benefit (public goods available to all). The
 *   extractiveness has increased modestly over the interval (0.12 to 0.18) as
 *   the commons has matured and professional actors have joined, creating
 *   subtle pressures on volunteer maintainers and expectations of constant
 *   availability. Theater ratio remains low (0.35), indicating that the
 *   function is transparent — code visibility, open discussion, and
 *   merit-based decisions dominate over performative activity. The commons
 *   exhibits all five rope characteristics: low base extraction (no coercive
 *   power structure), low suppression (exit is genuinely available), low
 *   theater (function is what you observe), multiple perspectives viewing it
 *   as pure coordination, and beneficiaries clearly aligned with
 *   contributors.
 *
 * KEY AGENTS:
 *   - Volunteer Contributors: Primary coordinating agent (moderate/mobile) — choose to participate because personal incentives align with commons good
 *   - Commercial Companies: Primary beneficiary (institutional/arbitrage) — extract value from the commons while contributing some labor and funding
 *   - Project Maintainers: Gatekeepers (powerful/mobile at project scale) — coordinate contributions and manage scope; face burnout risk from unsustainable volunteer expectations
 *   - User Base: Consumer beneficiary (powerless/mobile) — access free software; can switch projects easily
 *   - Ecosystem Health: Abstract collective good (powerless/trapped) — benefits from commons participation but has no exit or organizing power
 *   - Foundation Infrastructure: Organizational support (organized/constrained) — GitHub, Linux Foundation, CNCF provide scaffolding for commons sustainability
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees pure coordination mechanism solving knowledge dissemination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(open_source_commons, 0.18).
domain_priors:suppression_score(open_source_commons, 0.12).
domain_priors:theater_ratio(open_source_commons, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(open_source_commons, extractiveness, 0.18).
narrative_ontology:constraint_metric(open_source_commons, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(open_source_commons, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(open_source_commons, rope).
narrative_ontology:human_readable(open_source_commons, "The Mutual Garden").
narrative_ontology:topic_domain(open_source_commons, "social/technological").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(open_source_commons, contributing_developers).
narrative_ontology:constraint_beneficiary(open_source_commons, consuming_projects).
narrative_ontology:constraint_beneficiary(open_source_commons, user_base).
narrative_ontology:constraint_beneficiary(open_source_commons, ecosystem_health).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VOLUNTEER CONTRIBUTOR (ROPE) — Participants in the open-source commons experience pure coordination: contributing code solves their own problems, provides reputation, builds skills, and creates value for others. Exit is mobile (can fork, start new project, or contribute elsewhere). d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.14. Low effective extraction because participation is genuinely voluntary.
constraint_indexing:constraint_classification(open_source_commons, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 2: COMMERCIAL BENEFICIARY (ROPE) — Companies integrating open-source code into products experience the commons as a coordination mechanism that solves resource bottlenecks. Exit is arbitrage (could build proprietary alternatives but cheaper to use open source). d≈0.15, f(d)≈-0.01, σ=1.2 → χ≈-0.002. Negative effective extraction = net subsidy from the commons to commercial actors. This is the beneficiary's perspective on a rope.
constraint_indexing:constraint_classification(open_source_commons, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: LARGE PROJECT MAINTAINER (ROPE) — Linux, Python, Kubernetes maintainers coordinate massive distributed contributor networks with minimal centralized coercion. Power to set standards and define scope, but exit is mobile (can leave project, community can fork). d≈0.48, f(d)≈0.60, σ=1.2 → χ≈0.13. Coordination with soft power, not extraction.
constraint_indexing:constraint_classification(open_source_commons, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: SUSTAINABILITY INFRASTRUCTURE (SCAFFOLD) — Organized actors (GitHub, Linux Foundation, CNCF) are building structures (sponsorship programs, security audits, training) that are temporary scaffolding for mature governance. As projects mature, they transition from volunteer-dependent to professionally-supported. d≈0.40, f(d)≈0.40, σ=1.2 → χ≈0.19. Moderate extraction to fund the commons, but with clear sunset as projects reach sustainability.
constraint_indexing:constraint_classification(open_source_commons, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (ROPE) — From a civilizational view, open-source commons is a pure coordination mechanism. It solves the collective action problem of knowledge dissemination and resource pooling without relying on markets or hierarchies. No suppression of alternatives (proprietary development exists freely). Low theater (the function is what you see — code is visible, collaboration is transparent). d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.25. The analytical frame supports the rope classification.
constraint_indexing:constraint_classification(open_source_commons, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(open_source_commons_tests).
:- end_tests(open_source_commons_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low-moderate. The constraint shows minimal extraction compared to market or hierarchical alternatives. Contributors participate voluntarily, often for intrinsic rewards (learning, reputation, solving personal problems). Commercial beneficiaries do extract value (using free software that cost them nothing to develop), but this is legitimate because: (a) the code is available to anyone, including competitors, (b) contributing companies do fund some development, and (c) alternatives (proprietary development, vendor lock-in) are available. The modest increase over the interval (0.12→0.18) reflects growing professionalization and maintenance burden concentration, not coercive extraction. Suppression (0.12): Very low. Exit is genuinely available — contributors can fork projects, start alternatives, or work on different initiatives. No technical barriers prevent alternative coordination mechanisms. Knowledge is transparent. This is the defining characteristic of a rope. Theater ratio (0.35): Low. The commons function is what you observe: visible code, open discussions, documented decisions, transparent governance. Some theater exists (project marketing, foundation PR, corporate reputation-washing), but it is not the primary mechanism. This low ratio supports rope classification over tangled_rope or piton.
 *
 * PERSPECTIVAL GAP:
 *   The open-source commons exhibits minimal perspectival gaps — all five perspectives classify it as Rope or Scaffold (temporary support). Volunteer contributors and commercial beneficiaries both see pure coordination. Large maintainers see soft-power coordination. The sustainability infrastructure sees temporary scaffolding as projects professionalize. The analytical observer sees a pure coordination mechanism solving knowledge dissemination. The absence of major gaps (no snare, no piton, no false mountain) indicates that the commons is genuinely functioning as coordination, not masking extraction or theater. The constraint's robustness comes from this alignment: no major actor experiences it as coercive or performative.
 *
 * DIRECTIONALITY LOGIC:
 *   Volunteer contributors: Participant + mobile → d≈0.50, f(d)≈0.65. Symmetric — they both contribute and benefit. Commercial beneficiaries: Beneficiary + arbitrage → d≈0.15, f(d)≈-0.01. Net subsidy from commons to commercial sector. Project maintainers: Powerful + mobile → d≈0.48, f(d)≈0.60. Power is present but exit is available; coordination, not coercion. Sustainability infrastructure: Organized + constrained → d≈0.40, f(d)≈0.40. Supporting function; moderate extraction to fund commons operations. User base: Consumer + mobile → d≈0.45, f(d)≈0.55. Benefit without being primary contributors. Analytical observer: Analytical → d≈0.72, f(d)≈1.15. This is the observer's structural position, not an evaluative judgment. The derived directionalities are all in the low-extraction range (d<0.75 for all non-analytical perspectives), supporting rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   COORDINATION CONFIRMED: The open-source commons resolves mandatrophy by demonstrating that ε=0.18, suppression=0.12, and χ<0.35 (across perspectives) are genuinely consistent with a pure rope. No hidden extraction mechanism is disguised as coordination (tangled rope would require ε>0.30 and beneficiaries + victims in tension). No performative activity masks degraded function (piton would require theater>0.70). The constraint's five perspectives converge on rope or scaffold because the underlying coordination mechanism is transparent and voluntary participation is genuinely available. The omegas identify potential futures where burnout, corporate capture, or vendor lock-in could degrade the commons toward tangled_rope or snare — but those futures are not yet realized. The current state is authentic rope. The risks are real vulnerabilities, not present extractions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    maintainer_burnout_extraction,
    'Is the expectation of unpaid volunteer maintenance labor a hidden extraction mechanism that coerces key maintainers?',
    'Longitudinal burnout rate tracking among critical maintainers; correlation between funding availability and contributor retention; analysis of forced hand-offs vs voluntary succession planning',
    'If burnout is systemic extraction: commons shifts toward Tangled Rope or Snare (victims = burned-out maintainers bearing disproportionate labor cost). If burnout is selectable risk: remains Rope (contributors choose difficulty level).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(maintainer_burnout_extraction, empirical, 'Whether maintainer burnout represents hidden extraction or voluntary risk selection').

omega_variable(
    corporate_capture_risk,
    'Can large companies steer open-source project direction toward proprietary interests while maintaining the appearance of commons coordination?',
    'Feature-request and PR approval analysis for projects with corporate majority contributors; governance structure analysis (voting vs. meritocratic); correlation between corporate contribution increase and project dependency lock-in',
    'If corporate capture is structural: commons functions as Tangled Rope from contributor perspective (coordination facade masking extraction). If governance prevents capture: remains Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corporate_capture_risk, empirical, 'Whether corporate contributors can capture commons governance').

omega_variable(
    vendor_lock_in_paradox,
    'Does the proliferation of proprietary wrapper layers and cloud-hosted open-source services create de facto lock-in despite the code being open?',
    'Cost analysis of migration (effort to switch cloud vendors vs cost savings); measurement of switching friction introduced by vendor-specific integrations; comparison of true switching costs vs advertised ''openness''',
    'If lock-in is substantial: commons provides freedom-to-use but not freedom-from-vendor (Snare from user perspective). If friction is low: remains Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vendor_lock_in_paradox, empirical, 'Whether proprietary wrappers create hidden vendor lock-in').

omega_variable(
    knowledge_commons_sustainability,
    'Can the commons model sustain itself without either significant volunteer labor or commercial subsidy, or is it structurally dependent on one or the other?',
    'Historical analysis of successful long-term projects (Linux, Python, Apache): funding sources, contributor demographics, burnout rates; comparison with dead or dying projects; modeling of equilibrium contributor base size',
    'If dependent on subsidy: commons is Scaffold with hidden sunset (sustainable only while funding lasts or while some actors accept labor loss). If self-sustaining: remains Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_commons_sustainability, empirical, 'Whether commons can sustain without external subsidy or volunteer overwork').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(open_source_commons, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(osc_tr_t0, open_source_commons, theater_ratio, 0, 0.25).
narrative_ontology:measurement(osc_tr_t5, open_source_commons, theater_ratio, 5, 0.3).
narrative_ontology:measurement(osc_tr_t10, open_source_commons, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(osc_be_t0, open_source_commons, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(osc_be_t5, open_source_commons, base_extractiveness, 5, 0.15).
narrative_ontology:measurement(osc_be_t10, open_source_commons, base_extractiveness, 10, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(open_source_commons, resource_allocation).
narrative_ontology:affects_constraint(open_source_commons, software_supply_chain_security).
narrative_ontology:affects_constraint(open_source_commons, knowledge_commons_sustainability).
narrative_ontology:affects_constraint(open_source_commons, platform_governance_capture).

% DUAL FORMULATION NOTE:
% The Mutual Garden (open-source commons) is upstream of three dependent constraints: supply-chain security depends on commons reliability; platform governance risks depend on commons maturity; knowledge dissemination efficiency depends on commons health. These dependencies form a constraint family where commons degradation (toward tangled_rope or snare) would cascade to downstream constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
