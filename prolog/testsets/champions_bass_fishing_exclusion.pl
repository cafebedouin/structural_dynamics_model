% ============================================================================
% CONSTRAINT STORY: champions_bass_fishing_exclusion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_champions_bass_fishing_exclusion, []).

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
 *   constraint_id: champions_bass_fishing_exclusion
 *   human_readable: Champions Bass Fishing Tournament Exclusionary Practices
 *   domain: economic
 *
 * SUMMARY:
 *   This constraint describes the structure of high-stakes professional bass
 *   fishing tournaments, which function as a de facto gateway to a
 *   professional angling career. The system is characterized by extremely
 *   high financial barriers to entry (boats, equipment, travel, fees) and a
 *   set of unwritten social rules that favor established insiders. While it
 *   provides a genuine coordination function by organizing large-scale
 *   competitions, it simultaneously operates an extractive model, funding
 *   large prizes and organizer profits from the entry fees of a vast number
 *   of amateur participants with a statistically negligible chance of
 *   success.
 *
 * KEY AGENTS:
 *   - Tournament Organizers: Primary beneficiary (institutional/arbitrage) - Set the rules and profit from fees and sponsorships.
 *   - Elite Professional Anglers: Primary beneficiary (powerful/mobile) - Benefit from prize money and sponsorships; high entry barriers limit their competition.
 *   - Aspiring Amateur Anglers: Primary victim (powerless/trapped) - Provide the bulk of the revenue via entry fees but face prohibitive odds.
 *   - Underrepresented Demographics: Secondary victim (powerless/trapped) - Face additional social and cultural barriers to entry and inclusion.
 *   - Non-Tournament Anglers: Tertiary victim (moderate/constrained) - Suffer negative externalities like restricted access to public waterways.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(champions_bass_fishing_exclusion, 0.65).
domain_priors:suppression_score(champions_bass_fishing_exclusion, 0.7).
domain_priors:theater_ratio(champions_bass_fishing_exclusion, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(champions_bass_fishing_exclusion, extractiveness, 0.65).
narrative_ontology:constraint_metric(champions_bass_fishing_exclusion, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(champions_bass_fishing_exclusion, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(champions_bass_fishing_exclusion, tangled_rope).
narrative_ontology:human_readable(champions_bass_fishing_exclusion, "Champions Bass Fishing Tournament Exclusionary Practices").
narrative_ontology:topic_domain(champions_bass_fishing_exclusion, "economic").

domain_priors:requires_active_enforcement(champions_bass_fishing_exclusion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(champions_bass_fishing_exclusion, tournament_organizers).
narrative_ontology:constraint_beneficiary(champions_bass_fishing_exclusion, elite_professional_anglers).
narrative_ontology:constraint_beneficiary(champions_bass_fishing_exclusion, sponsors_and_media_partners).
narrative_ontology:constraint_victim(champions_bass_fishing_exclusion, aspiring_amateur_anglers).
narrative_ontology:constraint_victim(champions_bass_fishing_exclusion, underrepresented_demographics).
narrative_ontology:constraint_victim(champions_bass_fishing_exclusion, non_tournament_anglers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ASPIRING AMATEUR (SNARE) — Trapped by the desire to compete professionally. The high costs of entry fees, boats, and gear, combined with low odds of winning, constitute a pure extraction mechanism. The only path to pro status is through this system. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.92.
constraint_indexing:constraint_classification(champions_bass_fishing_exclusion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: TOURNAMENT ORGANIZER (ROPE) — Experiences the system as a pure coordination mechanism. They organize a complex event, create a media product, and allocate prize money, solving a collective action problem for anglers who want to compete. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.08. Negative extraction indicates a net beneficiary.
constraint_indexing:constraint_classification(champions_bass_fishing_exclusion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees both the genuine coordination function (organizing a competition) and the asymmetric extraction (funneling wealth from a large amateur base to a small elite). The active enforcement of rules and high suppression of alternative paths to professional status confirm the hybrid nature. d≈0.73, f(d)≈1.15, σ=1.2 → χ≈0.90.
constraint_indexing:constraint_classification(champions_bass_fishing_exclusion, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: ELITE PRO (ROPE) — As a primary beneficiary, the elite pro experiences the tournament circuit as a coordination system that enables their career. They have mobility between different tournament series and leverage their status for sponsorships. The high barriers to entry protect their elite status. d≈0.15, f(d)≈-0.01, σ=1.0 → χ≈-0.01.
constraint_indexing:constraint_classification(champions_bass_fishing_exclusion, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: NON-TOURNAMENT ANGLER (SNARE) — Experiences the constraint as coercive extraction from a public good. Their access to public waterways is restricted during high-profile events, and they perceive increased fishing pressure without receiving any benefit. Their exit is constrained to fishing at other times or less desirable locations. d≈0.85, f(d)≈1.15, σ=0.8 → χ≈0.60. χ is just below the snare threshold, but the experience is one of coercion.
constraint_indexing:constraint_classification(champions_bass_fishing_exclusion, snare,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(champions_bass_fishing_exclusion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(champions_bass_fishing_exclusion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(champions_bass_fishing_exclusion, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(champions_bass_fishing_exclusion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(champions_bass_fishing_exclusion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.65) is high, reflecting the significant financial outlay required from participants relative to the highly concentrated prize pool. The majority of anglers pay to lose. Suppression (0.70) is high because there are few, if any, alternative paths to achieving professional status and securing major sponsorships in the sport. One must participate in these specific, high-cost events. Theater Ratio (0.30) is moderate; while there is significant media spectacle (weigh-ins, branded apparel), the core activity of competitive fishing is functional.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For organizers and elite pros (beneficiaries), the system is a Rope—a fair and necessary mechanism to coordinate a high-level sporting event. For the aspiring amateur (victim), it is a Snare—a costly trap where their professional ambitions are monetized with little chance of reciprocation. The analytical view recognizes both functions, classifying it as a Tangled Rope where coordination is inextricably linked with asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (organizers, elite pros) have arbitrage or mobile exit options, giving them low directionality (d) values and leading to a Rope classification. Victims (amateurs) are trapped, as this is the only recognized path to professional status, leading to a high d-value and a Snare classification. The system's structure is defined by this differential in power and exit capability, which the directionality derivation correctly captures.
 *
 * MANDATROPHY ANALYSIS:
 *   This case resolves a potential mandatrophy by refusing to label the system as either a pure meritocratic competition (Rope) or a simple scam (Snare). The Tangled Rope classification is essential, as it acknowledges the real coordination value the tournaments provide while simultaneously accounting for the structurally embedded, asymmetric extraction from the participant base. To ignore the coordination would be to misrepresent the system's appeal and function; to ignore the extraction would be to endorse a narrative that benefits only the insiders.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    meritocracy_vs_gatekeeping,
    'Are the high financial and social barriers to entry a necessary filter for elite talent (meritocracy) or an intentional gatekeeping mechanism to protect the status of incumbent pros and organizers (extraction)?',
    'Analysis of career trajectories of new entrants vs. incumbents, controlling for sponsorship levels. Comparison with sports that have lower entry barriers.',
    'If primarily meritocratic, the system is closer to a Rope. If primarily gatekeeping, it confirms the Snare/Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meritocracy_vs_gatekeeping, conceptual, 'Distinguishing between meritocratic filtering and extractive gatekeeping in tournament entry barriers.').

omega_variable(
    unwritten_rules_impact,
    'What is the quantifiable impact of unwritten social rules and network effects on an angler''s success, independent of skill?',
    'Sociological network analysis of top anglers; surveys and interviews with aspiring amateurs and underrepresented groups to quantify perceived social barriers.',
    'A high impact would increase the ''suppression'' score, reinforcing the Snare classification for outsiders. A low impact would suggest the system is more of a pure economic Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(unwritten_rules_impact, empirical, 'Quantifying the impact of social networks and unwritten rules on tournament success.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(champions_bass_fishing_exclusion, 2004, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cham_tr_t2004, champions_bass_fishing_exclusion, theater_ratio, 2004, 0.15).
narrative_ontology:measurement(cham_tr_t2014, champions_bass_fishing_exclusion, theater_ratio, 2014, 0.25).
narrative_ontology:measurement(cham_tr_t2024, champions_bass_fishing_exclusion, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(cham_be_t2004, champions_bass_fishing_exclusion, base_extractiveness, 2004, 0.45).
narrative_ontology:measurement(cham_be_t2014, champions_bass_fishing_exclusion, base_extractiveness, 2014, 0.55).
narrative_ontology:measurement(cham_be_t2024, champions_bass_fishing_exclusion, base_extractiveness, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(champions_bass_fishing_exclusion, resource_allocation).
narrative_ontology:affects_constraint(champions_bass_fishing_exclusion, public_waterway_access_rights).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
