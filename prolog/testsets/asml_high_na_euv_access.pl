% ============================================================================
% CONSTRAINT STORY: asml_high_na_euv_access
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_asml_high_na_euv_access, []).

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
 *   constraint_id: asml_high_na_euv_access
 *   human_readable: Access to High-NA EUV Lithography
 *   domain: geopolitical/technological/economic
 *
 * SUMMARY:
 *   The Dutch company ASML holds a global monopoly on extreme ultraviolet
 *   (EUV) lithography machines, the technology essential for producing the
 *   world's most advanced semiconductor chips. The next generation, High-NA
 *   EUV, represents an even more critical chokepoint. Access to this
 *   technology, which is controlled by both its extreme cost and by
 *   geopolitical export controls (chiefly from the US to China), dictates
 *   which nations and corporations can compete at the technological frontier.
 *   This constraint is not just a piece of hardware; it is a primary
 *   mechanism for enforcing technological and economic hierarchy in the 21st
 *   century.
 *
 * KEY AGENTS:
 *   - ASML: Primary beneficiary (institutional/arbitrage) — The monopolist controlling the technology.
 *   - Leading-Edge Foundries (TSMC, Intel, Samsung): Secondary beneficiaries (institutional/constrained) — They rely on the tech to maintain their market lead, but are also subject to monopoly pricing.
 *   - Allied Nations (USA, Netherlands, etc.): Tertiary beneficiaries (institutional/mobile) — Use export controls on the technology as a tool of foreign policy.
 *   - Nations Under Export Controls (e.g., China): Primary victim (powerful/trapped) — State-level actors locked out of the technological frontier.
 *   - Aspiring Competitor Foundries: Secondary victims (powerless/trapped) — Cannot enter the market due to insurmountable capital and technology barriers.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(asml_high_na_euv_access, 0.75).
domain_priors:suppression_score(asml_high_na_euv_access, 0.9).
domain_priors:theater_ratio(asml_high_na_euv_access, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(asml_high_na_euv_access, extractiveness, 0.75).
narrative_ontology:constraint_metric(asml_high_na_euv_access, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(asml_high_na_euv_access, theater_ratio, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(asml_high_na_euv_access, tangled_rope).
narrative_ontology:human_readable(asml_high_na_euv_access, "Access to High-NA EUV Lithography").
narrative_ontology:topic_domain(asml_high_na_euv_access, "geopolitical/technological/economic").

domain_priors:requires_active_enforcement(asml_high_na_euv_access).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(asml_high_na_euv_access, asml).
narrative_ontology:constraint_beneficiary(asml_high_na_euv_access, leading_edge_foundries).
narrative_ontology:constraint_beneficiary(asml_high_na_euv_access, allied_nations).
narrative_ontology:constraint_victim(asml_high_na_euv_access, aspiring_competitor_foundries).
narrative_ontology:constraint_victim(asml_high_na_euv_access, nations_under_export_controls).
narrative_ontology:constraint_victim(asml_high_na_euv_access, downstream_consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED NATION (SNARE) — A nation-state subject to export controls (e.g., China) views this constraint as a pure geopolitical weapon. Despite being a 'powerful' actor globally, its exit options are 'trapped' as developing this technology indigenously is a multi-generational challenge. The coordination function is irrelevant as they are denied participation. d is derived from victim status + trapped exit, leading to a high χ, hence Snare.
constraint_indexing:constraint_classification(asml_high_na_euv_access, snare,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE MONOPOLIST (ROPE) — ASML, the sole supplier, frames its technology as a pure coordination good that enables the continuation of Moore's Law for the entire industry. The enormous price is justified as a return on massive R&D investment. As the primary beneficiary with arbitrage exit (they set the terms), their derived 'd' is very low, producing negative effective extraction (χ < 0).
constraint_indexing:constraint_classification(asml_high_na_euv_access, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE KEY CUSTOMER (TANGLED ROPE) — A foundry like TSMC or Intel is both a beneficiary and a victim. They gain a massive competitive advantage by having access, but are also subject to monopoly pricing and have no alternative supplier ('constrained' exit). They experience both the coordination benefit and the severe extraction.
constraint_indexing:constraint_classification(asml_high_na_euv_access, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: THE STARTUP (MOUNTAIN) — For a new company trying to enter the fabrication market, the capital and technological barrier posed by High-NA EUV is absolute and insurmountable. It appears as an unchangeable law of the economic environment. The engine will flag this as a false summit, as the constraint's high ε (0.75) and suppression (0.90) are inconsistent with a true Mountain.
constraint_indexing:constraint_classification(asml_high_na_euv_access, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) — The analyst recognizes the dual nature of the constraint. It serves a critical coordination function (advancing semiconductor technology globally) while simultaneously enabling massive, asymmetric extraction due to its monopoly status, enforced by both market power and state-level export controls. This matches the claimed type.
constraint_indexing:constraint_classification(asml_high_na_euv_access, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(asml_high_na_euv_access_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(asml_high_na_euv_access, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(asml_high_na_euv_access, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(asml_high_na_euv_access, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(asml_high_na_euv_access_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.75) is extremely high, reflecting monopoly pricing on a technology with inelastic demand. Suppression (0.90) is near-total; there are no commercial alternatives to ASML's machines for leading-edge nodes, and this market failure is reinforced by active state-level export controls. Theater Ratio (0.10) is low because the machine is a purely functional, non-performative piece of capital equipment. The `requires_active_enforcement` flag is true due to the government-enforced export restrictions.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. ASML views its technology as a pure coordination mechanism (Rope) enabling Moore's Law. For a sanctioned nation like China, it is an unambiguous instrument of economic warfare (Snare). For a key customer like TSMC, it is a necessary evil they both benefit from and are exploited by (Tangled Rope). For a would-be startup, the barrier is so absolute it seems like a law of nature (Mountain). The framework exposes how these perspectives are all rational responses to different structural positions relative to the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (ASML, allied nations) have arbitrage or mobile exit options, giving them very low 'd' values and thus seeing the constraint as a subsidy or pure coordination (negative or low χ). Victims (excluded nations, competitor foundries) are targets with trapped exit options, giving them very high 'd' values and experiencing the constraint as maximally extractive (high χ). Key customers (TSMC) are beneficiaries but with constrained exit, placing them in the middle, correctly identifying the Tangled Rope structure.
 *
 * MANDATROPHY ANALYSIS:
 *   This story resolves the potential mandatrophy of mislabeling a strategic chokepoint. A simplistic analysis might see it as just a Rope ('it enables progress') or just a Snare ('it's a tool of imperialism'). The framework, by requiring the declaration of both beneficiaries and victims, and by modeling the perspectives of actors with different exit options, correctly identifies the dual nature of the constraint. It has a genuine and critical coordination function, but this function is fused to a severe, actively enforced, asymmetric extraction mechanism. It is the canonical example of a Tangled Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    indigenous_development_feasibility,
    'Can a nation under export controls successfully develop a comparable lithography technology indigenously within a strategic timeframe (e.g., 10-15 years)?',
    'Monitoring of patent filings, R&D spending, and demonstrated results from state-backed programs in areas like novel light sources, advanced optics, and precision mechanics.',
    'If indigenous development is successful, the ''suppression'' metric would fall significantly, and the constraint would weaken from a hard Snare to a more competitive Tangled Rope from the perspective of the excluded nation. If it fails, the Snare classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_development_feasibility, empirical, 'Feasibility of a sanctioned nation developing an indigenous alternative to EUV.').

omega_variable(
    paradigm_shift_obsolescence,
    'Will a future computing paradigm (e.g., optical computing, 3D chip stacking, advanced packaging) reduce the centrality of cutting-edge photolithography for performance gains?',
    'Tracking the performance-per-watt and cost-per-transistor curves of alternative technologies relative to monolithic silicon chips produced with EUV.',
    'If an alternative paradigm becomes economically viable, this entire constraint could degrade into a Piton, where nations compete over a technology that is no longer the primary driver of computing power.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(paradigm_shift_obsolescence, empirical, 'Whether a future paradigm shift will make EUV lithography obsolete.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(asml_high_na_euv_access, 2020, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(asml_tr_t2020, asml_high_na_euv_access, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(asml_tr_t2025, asml_high_na_euv_access, theater_ratio, 2025, 0.1).
narrative_ontology:measurement(asml_tr_t2030, asml_high_na_euv_access, theater_ratio, 2030, 0.1).

% Extraction over time
narrative_ontology:measurement(asml_be_t2020, asml_high_na_euv_access, base_extractiveness, 2020, 0.65).
narrative_ontology:measurement(asml_be_t2025, asml_high_na_euv_access, base_extractiveness, 2025, 0.72).
narrative_ontology:measurement(asml_be_t2030, asml_high_na_euv_access, base_extractiveness, 2030, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(asml_high_na_euv_access, global_infrastructure).
narrative_ontology:affects_constraint(asml_high_na_euv_access, semiconductor_supply_chain).
narrative_ontology:affects_constraint(asml_high_na_euv_access, ai_hardware_sovereignty).
narrative_ontology:affects_constraint(asml_high_na_euv_access, us_china_tech_decoupling).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
