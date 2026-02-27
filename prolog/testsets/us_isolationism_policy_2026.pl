% ============================================================================
% CONSTRAINT STORY: us_isolationism_policy_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_isolationism_policy_2026, []).

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
 *   constraint_id: us_isolationism_policy_2026
 *   human_readable: US Withdrawal from International Organizations under "America First" Doctrine
 *   domain: geopolitical/multilateral_institutions
 *
 * SUMMARY:
 *   The US withdrawal from international organizations under an 'America
 *   First' doctrine creates a structural constraint with radically asymmetric
 *   consequences across different national and institutional positions. The
 *   constraint exhibits a clear tangled rope signature: it combines a genuine
 *   coordination function (serving the preferences of a dominant coalition
 *   within the US polity to reduce institutional constraints on unilateral
 *   action) with asymmetric extraction (imposing coordination costs on
 *   nations that depend on multilateral mechanisms and have no exit option).
 *   The constraint's extractiveness has increased over the measurement
 *   interval (0.38 → 0.58) as the withdrawal has progressed from policy
 *   proposal to implementation, and theater ratio has increased (0.45 → 0.68)
 *   reflecting both the dramatic performative aspects of withdrawal
 *   announcements and the simultaneous attempt by alternative forums to
 *   demonstrate functional coordination capacity. The suppression level
 *   (0.72) is high because alternative coordination mechanisms remain
 *   fragmented and lack the coordination capacity or legitimacy of
 *   US-anchored multilateralism, constraining options for non-preferred
 *   nations and global public goods provisioning.
 *
 * KEY AGENTS:
 *   - US Executive Coalition: Primary beneficiary (institutional/arbitrage) — captures reduced financial burden, increased strategic flexibility, avoidance of multilateral constraints on unilateral action
 *   - Constituency bases supporting withdrawal (nationalist wing, manufacturing sectors, energy interests): Secondary beneficiary (powerful/arbitrage) — benefits from perception of national interest prioritization and reduced competition from international standards
 *   - Non-Preferred Nations and Developing States: Primary victim (powerless/trapped) — lose coordination access, dispute resolution forums, climate and pandemic coordination mechanisms with no exit option
 *   - Allied Democratic Governments (EU, Japan, Australia, Canada): Secondary victim (organized/constrained) — constrained by security dependencies while attempting to maintain coordination without US participation; benefit from some freed unilateral space but lose coordination gains
 *   - Global Public Goods Provisioning Systems (climate coordination, pandemic surveillance, supply chain standards): Victim (powerless/trapped) — institutional capacity for coordinated action degrades; coordination costs increase for all participants
 *   - Alternative Forum Coalition (EU institutions, coalition governments, ASEAN, African Union): Organized agent building sunset mechanism (organized/constrained) — constructing alternative coordination pathways to replace US-anchored multilateralism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_isolationism_policy_2026, 0.58).
domain_priors:suppression_score(us_isolationism_policy_2026, 0.72).
domain_priors:theater_ratio(us_isolationism_policy_2026, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_isolationism_policy_2026, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_isolationism_policy_2026, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(us_isolationism_policy_2026, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_isolationism_policy_2026, tangled_rope).
narrative_ontology:human_readable(us_isolationism_policy_2026, "US Withdrawal from International Organizations under \"America First\" Doctrine").
narrative_ontology:topic_domain(us_isolationism_policy_2026, "geopolitical/multilateral_institutions").

domain_priors:requires_active_enforcement(us_isolationism_policy_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_isolationism_policy_2026, us_executive_political_coalition).
narrative_ontology:constraint_beneficiary(us_isolationism_policy_2026, domestic_constituencies_prioritized_by_withdrawal).
narrative_ontology:constraint_victim(us_isolationism_policy_2026, multilateral_coordination_capacity).
narrative_ontology:constraint_victim(us_isolationism_policy_2026, non_preferred_nations).
narrative_ontology:constraint_victim(us_isolationism_policy_2026, global_public_goods_provisioning).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-PREFERRED NATIONS (SNARE) — States outside the US strategic prioritization (developing nations, regional rivals) have no exit from the constraint of losing US participation in multilateral coordination. They bear costs (reduced coordination capacity, loss of dispute resolution forums, climate coordination collapse) with no ability to organize collective counter-power. Maximum structural extraction with high suppression: these nations cannot form alternative institutions at equivalent capacity and face coordinated US opposition to parallel structures.
constraint_indexing:constraint_classification(us_isolationism_policy_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ALLIED DEMOCRATIC GOVERNMENTS (TANGLED ROPE) — Nations like EU members, Japan, Australia face constrained exit: they benefit from some US security guarantees even as US withdraws from multilateral coordination, and they attempt to maintain coordination without US participation. Mixed experience: they gain temporary freedom from US veto power in some domains but lose coordination gains from US participation. Organized enough to resist full extraction but constrained by security dependencies and trade vulnerabilities. Active enforcement: US policy actively decouples from alliance coordination mechanisms while maintaining bilateral advantage.
constraint_indexing:constraint_classification(us_isolationism_policy_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: US EXECUTIVE COALITION (ROPE) — The coalition supporting withdrawal experiences the constraint as coordination among actors with shared immediate interests: reducing US financial contributions to multilateral institutions, avoiding constraints on US unilateral action, and prioritizing bilateral relationships that favor US leverage. High arbitrage capacity (can shift between UN participation and alternative arrangements at will). Benefits from withdrawal: reduced financial burden, increased strategic flexibility, ability to avoid compliance costs. Perceives constraint as functional coordination mechanism solving collective action problems among like-minded constituents (nationalist wing of electorate, manufacturing sectors, energy interests). This agent experiences effective extraction as negative (benefits exceed costs).
constraint_indexing:constraint_classification(us_isolationism_policy_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: COALITION FOR MULTILATERAL RENEWAL (SCAFFOLD) — Organized actors (EU institutions, coalition governments, NGO networks) are building alternative coordination pathways to replace US-anchored multilateralism: expanded UN Security Council alternatives, informal coalitions of the willing, regional integration mechanisms (ASEAN, African Union expansion). These actors perceive the constraint as a temporary institutional failure with a sunset: as alternative mechanisms mature and demonstrate coordination value, the constraint's extraction power diminishes. Suppression is being actively reduced through institutional innovation. Sunset logic: if alternative forums successfully coordinate climate action, pandemic response, and trade dispute resolution without US participation, the withdrawal constraint loses its monopoly power over coordination. Theater high initially (dramatic US exits) but declining as alternatives demonstrate function.
constraint_indexing:constraint_classification(us_isolationism_policy_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: RESIDUAL MULTILATERAL INSTITUTIONS (PITON) — The institutions themselves (UN secretariat, WHO governance, UNESCO forums) continue formal functions (technical coordination, information sharing, normative frameworks) with minimal real enforcement power but high theater: continued meetings, declarations, technical working groups persist despite US absence, providing performative legitimacy without functional coordination capacity. Theater ratio high (0.68) because much institutional activity becomes performative when the most powerful member withdraws. Institutions persist through inertia and the lack of superior alternatives for smaller nations. Extraction from these institutions' perspective is moderate (they lose US funding, legitimacy, and enforcement capacity but maintain technical functions). This is degraded coordination theater maintained by institutional persistence.
constraint_indexing:constraint_classification(us_isolationism_policy_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: COORDINATION-DEPENDENT SECTORS (TANGLED ROPE) — Scientific communities, health systems, environmental sectors, and supply chain networks experience the constraint as both enabling and extractive. They benefit from US market access and technical capacity but bear costs from US exit from coordination forums (climate commitments collapse, pandemic surveillance networks fragment, supply chain standards diverge). Moderate power: these sectors can partially coordinate without US participation but lose efficiency and face higher transaction costs. Constrained exit: leaving the fields is not feasible; adaptation to US non-participation is forced cost. Active enforcement: US policy actively decouples from technical coordination standards while maintaining bilateral advantage in commercial relationships. This creates forced adaptation costs for these sectors.
constraint_indexing:constraint_classification(us_isolationism_policy_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (FALSE MOUNTAIN VIEW) — From a civilizational perspective, there is a risk of framing US withdrawal as a natural law: 'nations inherently prioritize their own interests' or 'multilateral constraints on state autonomy are inherent friction.' This natural law framing naturalizes what is actually a contingent institutional choice and policy preference. The analytical observer must identify this as a false summit: the constraint is not immutable but rather a deliberate policy framework with specific beneficiaries and victims. The appearance of naturalness comes from theatrical justification (nationalist rhetoric framing withdrawal as inevitable return to national interest prioritization) rather than from structural immutability. Theater ratio reflects this naturalization theater.
constraint_indexing:constraint_classification(us_isolationism_policy_2026, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_isolationism_policy_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_isolationism_policy_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_isolationism_policy_2026, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_isolationism_policy_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_isolationism_policy_2026, TR),
    TR >= 0.70.

:- end_tests(us_isolationism_policy_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The US executive coalition captures clear extraction benefits (reduced financial contributions, increased strategic leverage, avoided compliance costs) estimated at 0.15-0.20 from the US perspective. However, this is not maximal extraction (which would require 0.70+) because the cost of coordinating withdrawal (managing diplomatic fallout, building alternative arrangements) and the loss of coordination benefits for US-dependent sectors (disrupted supply chains, degraded pandemic response capacity) partially offset gains. The extractiveness value reflects that the primary extraction is toward the US executive coalition and away from non-preferred nations, but the mechanism is mixed coordination-extraction rather than pure extraction. Suppression (0.72): High. The constraint operates with high suppression because nations dependent on multilateral coordination have severely limited options: they cannot exit the global coordination problem (climate, pandemic, trade), they cannot form equivalent coordination mechanisms at comparable capacity, and they face structural disadvantage in bilateral negotiations with the US. Alternative forums are emerging (EU, regional coalitions) but lack the historical legitimacy and institutional capacity of US-anchored multilateralism. Suppression would be higher (0.85+) except for the fact that some alternative coordination is technically feasible and that some US constituencies benefit from continued multilateral participation (sectors dependent on international standards). Theater ratio (0.68): Moderate-high. The constraint exhibits significant theater because much of the dramatic withdrawal involves nationalist rhetoric and performative unilateralism while substantial bilateral coordination continues and alternative forums begin demonstrating some functional capacity. The theater reflects both the performative aspects of announcing withdrawals and the simultaneous institutional reality that coordination problems persist and must be solved through some mechanism. Theater increases from 0.45 to 0.68 over the interval as the theatrical justification for withdrawal accumulates institutional form.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a complete perspectival inversion between the US executive coalition and non-preferred nations. The US coalition sees coordination (rope-like benefits without high extraction cost to them), enabling and simplifying their policy goals. Non-preferred nations see snare-like extraction: they bear costs (coordination degradation, reduced dispute resolution capacity, compliance with US unilateral demands) with no exit. Allied nations experience tangled rope: partial coordination benefit (reduced US veto power in some domains) combined with real extraction cost (loss of coordination gains, constrained adaptation to US requirements). Alternative forum builders see scaffold: temporary institutional failure with a sunset mechanism, as their alternative coordination pathways mature. The piton classification for residual institutions reflects the reality that formal multilateral structures continue with reduced functional capacity but high performative content. The analytical observer risks seeing false naturalism (nations inherently prioritize autonomy over coordination) unless they recognize that the constraint is a contingent institutional choice, not a law of nature. The gap between perspectives is maximized: the beneficiary (US coalition) sees coordination; the trapped victim (non-preferred nations) sees extraction; the analytical observer risks misclassifying contingency as inevitability.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from the structural asymmetry in exit options and beneficiary positioning. The US executive coalition has arbitrage exit options (can shift between multilateral and bilateral arrangements at will, maintain strategic flexibility) combined with beneficiary positioning (captures extraction gains), producing low directionality (d ≈ 0.15-0.25). Allied nations have constrained exit (cannot exit security dependencies or strategic relationships) combined with mixed beneficiary/victim positioning (some freedom from US veto, but loss of coordination gains), producing moderate directionality (d ≈ 0.45-0.55). Non-preferred nations have trapped exit options combined with victim positioning (no escape from coordination problems, dependent on multilateral mechanisms), producing high directionality (d ≈ 0.80-0.90). The engine's sigmoid f(d) applies the institutional power premium to the US coalition (negative χ relative to base ε) and the powerless multiplier to trapped nations (positive χ relative to base ε). Suppression remains at 0.72 across all perspectives because it is a structural property of the constraint (how much alternative coordination is available), not an observer-relative quantity.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that apparent coordination (the US coalition's experience of solving collective action problems around autonomous action) and apparent extraction (non-preferred nations' experience of reduced coordination capacity) are structurally consistent readings of the same constraint from different positions. The US coalition genuinely solves an internal coordination problem (aligning executive power with nationalist constituency preferences, reducing multilateral constraints). This IS coordination from their perspective. But the solution to their coordination problem is implemented by delegitimizing and withdrawing from mechanisms that solve other nations' coordination problems. The constraint is tangled rope because it contains both: the coordination function (for the dominant coalition) is real, the asymmetric extraction (from non-preferred nations) is real, and active enforcement is required (US policy actively decouples from multilateral mechanisms). Mandatrophy is resolved by recognizing that the classification depends on whose coordination problem is being solved. If the question is 'does this solve a coordination problem for some actor?' the answer is yes (US coalition). If the question is 'does this extract asymmetrically from victims?' the answer is yes (non-preferred nations). If the question is 'is there active enforcement?' the answer is yes (US policy actively decouples). All three gates for tangled rope are satisfied, but the coordination/extraction ratio depends entirely on the structural position of the observer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    us_return_probability,
    'How likely is US re-engagement with multilateral institutions within the next 10-20 years, and under what conditions?',
    'Political timeline analysis; identification of conditions triggering US return to key forums; tracking of bilateral vs multilateral preference signaling by successive US administrations',
    'If return probability > 0.6 within 20 years: constraint is temporary institutional adjustment (scaffold classification holds). If return probability < 0.3: constraint becomes structural pivot in global coordination regime (sustained snare/tangled rope from most perspectives).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(us_return_probability, preference, 'Likelihood and conditions for US re-engagement with multilateralism').

omega_variable(
    alternative_forum_sufficiency,
    'Can alternative coordination mechanisms (informal coalitions, regional blocs, EU-led forums, UN minus US) achieve functionally equivalent coordination for climate action, pandemic response, and trade dispute resolution?',
    'Comparative analysis of coordination outcomes pre- and post-US withdrawal; tracking of enforcement rates, participation breadth, and effectiveness of alternative forums on key issue areas',
    'If alternatives achieve > 80% functional equivalence: scaffold sunset is real, constraint power declines over 10-15 years. If alternatives achieve < 60% equivalence: US withdrawal represents structural degradation of global coordination (sustained snare for non-preferred nations).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_forum_sufficiency, empirical, 'Whether alternative forums can replace US-anchored coordination').

omega_variable(
    extraction_vs_autonomy_intentionality,
    'Is US withdrawal primarily driven by extractive benefit-seeking (capturing resources, avoiding compliance costs, increasing strategic leverage) or by genuine institutional autonomy preference (reducing constraints on US decision-making)?',
    'Policy document analysis; tracking of financial flows and strategic advantages gained vs autonomy gained; comparative analysis of US behavior in bilateral vs multilateral contexts; stated policy rationales vs revealed preferences through budget allocation and negotiation positions',
    'If extraction-driven > 60%: constraint classification is snare/tangled rope from external perspective, rope from US beneficiary perspective. If autonomy-driven > 60%: constraint is coordination mechanism (rope/scaffold) serving US institutional preferences, with extraction as secondary effect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_autonomy_intentionality, conceptual, 'Whether withdrawal is driven by extraction benefits or institutional autonomy preference').

omega_variable(
    suppression_sustainability,
    'Can suppression of alternative coordination mechanisms be sustained at 0.72 level, or will alternative forums achieve sufficient legitimacy and participation to reduce suppression costs?',
    'Tracking of participation rates in alternative forums over time; analysis of US capacity to prevent or sanction alternative institutions; monitoring of great power coordination outside US-centric frameworks (China-Russia coordination, EU institutional deepening)',
    'If suppression sustained > 15 years: constraint is structural and extractive (snare/tangled rope). If suppression declines to < 0.50 within 10 years: constraint transitions to lower-extraction regime as alternatives gain legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_sustainability, empirical, 'Sustainability of suppression of alternative coordination mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_isolationism_policy_2026, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usiso_tr_t0, us_isolationism_policy_2026, theater_ratio, 0, 0.45).
narrative_ontology:measurement(usiso_tr_t3, us_isolationism_policy_2026, theater_ratio, 3, 0.58).
narrative_ontology:measurement(usiso_tr_t6, us_isolationism_policy_2026, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(usiso_be_t0, us_isolationism_policy_2026, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(usiso_be_t3, us_isolationism_policy_2026, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(usiso_be_t6, us_isolationism_policy_2026, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_isolationism_policy_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(us_isolationism_policy_2026, multilateral_climate_coordination).
narrative_ontology:affects_constraint(us_isolationism_policy_2026, global_pandemic_surveillance_networks).
narrative_ontology:affects_constraint(us_isolationism_policy_2026, international_dispute_resolution_capacity).

% DUAL FORMULATION NOTE:
% US isolationism operates at the structural level of institutional participation and coordination capacity. Specific issue-area constraints (climate coordination, pandemic response, trade dispute resolution) are downstream effects of the institutional withdrawal. This constraint story models the institutional withdrawal mechanism itself; separate stories should model the specific functional degradations (climate coordination collapse, pandemic response fragmentation) that follow from it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_isolationism_policy_2026, institutional, 0.2).
constraint_indexing:directionality_override(us_isolationism_policy_2026, organized, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
