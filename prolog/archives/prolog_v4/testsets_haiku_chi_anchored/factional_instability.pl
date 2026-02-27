% ============================================================================
% CONSTRAINT STORY: factional_instability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_factional_instability, []).

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
 *   constraint_id: factional_instability
 *   human_readable: The Republican Remedy for Factional Violence
 *   domain: political/institutional
 *
 * SUMMARY:
 *   James Madison's Federalist Paper No. 10 presents the large republic as a
 *   structural remedy for factional violence. By expanding the geographic
 *   scale and increasing the number of representatives filtering local
 *   majorities, the mechanism allegedly prevents tyranny of the majority
 *   while preserving republican form. However, this same mechanism suppresses
 *   minority voices and concentrates power in hands of property-owning
 *   commercial classes. The constraint exhibits Tangled Rope structure: it
 *   genuinely solves a coordination problem (preventing interstate warfare
 *   and anarchic factionalism) while simultaneously enabling asymmetric
 *   extraction (subordinate factions and debtors are filtered out; the
 *   property-owning class retains disproportionate influence). The theater
 *   ratio is low initially (0.25 at ratification) because the mechanism is
 *   presented as purely functional — a technical solution to a mathematical
 *   problem. Over 100 years, theater rises as the functional extraction
 *   mechanism becomes increasingly apparent and is obscured by expanding
 *   democratic rhetoric, producing measured theater of 0.45 by the 1870s.
 *
 * KEY AGENTS:
 *   - Subordinate Factions (Debtors, Labor, Agrarian): Primary victims (powerless/trapped) — geographically dispersed local majorities suppressed by representative filtering; no exit option
 *   - Property-Owning Commercial Class: Primary beneficiaries (organized/constrained) — benefit from stability (coordination) and from filtering mechanism that blocks redistribution threats (extraction)
 *   - Federal Constitutional Authority: Institutional beneficiary (institutional/arbitrage) — gains legitimacy from framing as factional peacemaker; delegates enforcement to representative bodies
 *   - Geographically Dispersed Majority (Yeoman Farmers, Middle Class): Secondary victims (moderate/constrained) — benefit from union preventing warfare but lose direct majority power
 *   - Republican Opposition Coalition (Anti-Federalists, Democratic-Republicans): Organized resistance (organized/mobile) — challenge the permanence of the filtering mechanism; advocate for constitutional amendment
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the large-republic design as an immutable law of pluralist societies rather than contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(factional_instability, 0.52).
domain_priors:suppression_score(factional_instability, 0.48).
domain_priors:theater_ratio(factional_instability, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(factional_instability, extractiveness, 0.52).
narrative_ontology:constraint_metric(factional_instability, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(factional_instability, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(factional_instability, tangled_rope).
narrative_ontology:human_readable(factional_instability, "The Republican Remedy for Factional Violence").
narrative_ontology:topic_domain(factional_instability, "political/institutional").

domain_priors:requires_active_enforcement(factional_instability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(factional_instability, property_owning_commercial_class).
narrative_ontology:constraint_beneficiary(factional_instability, federal_constitutional_authority).
narrative_ontology:constraint_victim(factional_instability, geographically_dispersed_majorities).
narrative_ontology:constraint_victim(factional_instability, subordinate_factions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBORDINATE FACTION (SNARE) — Local majorities in any district face suppression through large-scale representative filtering. Geographic dispersal prevents coalition. Exit option is emigration (trapped). d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.72.
constraint_indexing:constraint_classification(factional_instability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PROPERTY-OWNING COMMERCIAL CLASS (TANGLED ROPE) — Organized actors benefit from stability (coordination function: federal union prevents fragmentation into hostile states). Also benefit from filtering mechanism that suppresses debtor and labor factions. d≈0.35, f(d)≈0.35, σ=1.1 → χ≈0.20.
constraint_indexing:constraint_classification(factional_instability, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: FEDERAL CONSTITUTIONAL AUTHORITY (ROPE) — Institution experiences the constraint as pure coordination: the large republic mechanism solves the collective action problem of factional violence. Multiple institutional veto points reduce tyranny of majority. Arbitrage via delegation to representative bodies. d≈0.08, f(d)≈-0.11, σ=1.1 → χ≈-0.04. Negative effective extraction = net beneficiary (stability/legitimacy).
constraint_indexing:constraint_classification(factional_instability, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: GEOGRAPHICALLY DISPERSED MAJORITY (TANGLED ROPE) — Middle-class and yeoman farmers scattered across districts benefit from union (coordination: prevents interstate warfare) but lose direct majority power via representation filtering. Constrained by existing constitutional structure; limited exit. d≈0.58, f(d)≈0.68, σ=1.0 → χ≈0.35.
constraint_indexing:constraint_classification(factional_instability, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REPUBLICAN OPPOSITION COALITION (SCAFFOLD) — Anti-Federalist and later Democratic-Republican organized actors see the large-republic remedy as temporary — an emergency measure against factional violence that will eventually be superseded by more direct democratic mechanisms. Exit via constitutional amendment or state secession. d≈0.42, f(d)≈0.42, σ=1.0 → χ≈0.22.
constraint_indexing:constraint_classification(factional_instability, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/civilizational perspective, some degree of factional conflict is inherent to pluralistic societies. No mechanism can eliminate factions while preserving liberty. The large republic's filtering is presented as an immutable structural solution. However, structural data (ε=0.52, suppression=0.48) contradicts mountain classification — this is a contingent institutional choice, not a law of nature.
constraint_indexing:constraint_classification(factional_instability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(factional_instability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(factional_instability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(factional_instability, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(factional_instability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(factional_instability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The large-republic mechanism extracts significant political power from subordinate factions through representative filtering. Local majorities cannot directly determine federal outcomes; their preferences are mediated through layers of delegation. The extraction is not total (ε=0.72) because the mechanism does provide genuine protection against anarchic factionalism and interstate warfare. Suppression (0.48): Moderate. Significant barriers to subordinate faction mobilization include geographic dispersion, communication costs, and the electoral mechanism itself. However, suppression is not absolute — subordinate factions do organize (petitions, extra-constitutional mobilization, eventual political party formation). Theater ratio (0.38 rising to 0.45): Moderate-low, increasing over time. Initially, the mechanism is presented as purely technical and functional (mathematical inevitability). As communication technology improves (telegraph, printing press) and the power-asymmetry becomes more apparent, democratic and egalitarian rhetoric expands (theater increases) to obscure the extraction. By the 1870s, the theater includes extensive claims about representation being 'of the people' despite institutional design that actively filters popular will.
 *
 * PERSPECTIVAL GAP:
 *   The property-owning commercial class and federal constitutional authority both see genuine coordination benefits: the large republic prevents factional warfare and creates stable commercial conditions. But the subordinate faction sees pure extraction: their preferences are systematically suppressed without reciprocal benefits. The geographically dispersed majority (yeoman, middle-class) occupies the middle ground — they benefit from union but lose direct power. The analytical observer risks the false summit: naturalizing the filtering mechanism as an inevitable consequence of scale (mountain), when the structural data shows it is a contingent institutional choice that enables extraction while performing coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Subordinate factions: Victims + trapped → d≈0.92, f(d)≈1.38. No exit option; extraction is severe. Property-owning commercial class: Beneficiaries + constrained → d≈0.35, f(d)≈0.35. Significant benefits; some constraint from republican form and constitutional limits. Federal constitutional authority: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary; gains legitimacy. Geographically dispersed majority: Victims + constrained → d≈0.58, f(d)≈0.68. Mixed position; benefit from union but lose direct power. Republican opposition: Organized + mobile → d≈0.42, f(d)≈0.42. Can theoretically exit via amendment or secession; see the constraint as temporary. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Risks naturalizing the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   CORE TENSION: The large-republic remedy performs legitimate coordination (preventing anarchic factionalism, enabling commercial union) while simultaneously enabling extraction (suppressing subordinate faction preferences, protecting property from redistribution). The mandatrophy is whether this is a ROPE (pure coordination) or a SNARE (pure extraction). The tangled_rope classification resolves it: the constraint is genuinely hybrid. It is NOT possible to eliminate the coordination function (union is necessary for security and commerce) without simultaneously losing the extraction mechanism. The property-owning class cannot have stability without representative filtering; subordinate factions cannot have direct power without risking warfare. This is not a false compromise between coordinate and extractive framings — it is a structural constraint that genuinely combines both. However, the extractiveness threshold (0.52) does exceed the boundary for pure coordination (ε≤0.45). The ratio of extraction to coordination is therefore non-negligible. The theater ratio remaining low (0.38 rising to 0.45) reflects that the mechanism is genuinely functional, not merely performative — the filtering works as designed. The increasing theater over time reflects not degradation of function but increasing deployment of democratic rhetoric to obscure the extraction mechanism as communication technology and faction organizing capacity improve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    filtering_efficacy_threshold,
    'At what scale of geographic dispersion does the representative filtering mechanism cease to suppress factional mobilization?',
    'Historical analysis of turnout rates, petition success, and faction coordination capacity across different district sizes and communication technologies (postal era vs telegraph vs internet)',
    'If threshold is easily crossed by new communication technology: constraint becomes purely extractive (Snare from more perspectives). If threshold is robust: constraint maintains coordination function (Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(filtering_efficacy_threshold, empirical, 'Scale threshold for representative filtering efficacy').

omega_variable(
    property_protection_asymmetry,
    'Does the large-republic remedy primarily serve factional peace or primarily serve property protection against debt relief / redistribution factions?',
    'Analysis of Federalist intentions (Papers 10, 51, 84); correlation between states with debtor majorities and their voting patterns on constitutional ratification; subsequent legislation on debtor relief, taxes, banking',
    'If factional peace is primary and property protection is secondary effect: constraint is genuine coordination mechanism (Rope). If property protection is primary and factional peace is legitimizing narrative: constraint is extractive (Snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(property_protection_asymmetry, conceptual, 'Primary mechanism of large-republic design').

omega_variable(
    amendment_pathway_reality,
    'Is the constitutional amendment process a genuine sunset pathway for the representative filtering mechanism, or a de facto permanent constraint due to supermajority requirements?',
    'Analysis of amendment failure rates; study of how veto-player coalitions have blocked amendments that would expand direct democracy or weaken representative filtering',
    'If amendment pathway is real: scaffold classification holds. If amendment pathway is permanently blocked: constraint becomes de facto permanent extraction, shifting scaffold toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_pathway_reality, empirical, 'Whether constitutional amendment provides genuine exit path').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(factional_instability, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fact_tr_t0, factional_instability, theater_ratio, 0, 0.25).
narrative_ontology:measurement(fact_tr_t50, factional_instability, theater_ratio, 50, 0.38).
narrative_ontology:measurement(fact_tr_t100, factional_instability, theater_ratio, 100, 0.45).

% Extraction over time
narrative_ontology:measurement(fact_be_t0, factional_instability, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(fact_be_t50, factional_instability, base_extractiveness, 50, 0.46).
narrative_ontology:measurement(fact_be_t100, factional_instability, base_extractiveness, 100, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(factional_instability, enforcement_mechanism).
narrative_ontology:affects_constraint(factional_instability, representative_democracy_filtering).
narrative_ontology:affects_constraint(factional_instability, interstate_federation_stability).

% DUAL FORMULATION NOTE:
% The large-republic remedy decomposes into two structurally distinct constraints: (1) interstate federation stability (a pure coordination mechanism preventing interstate warfare), (2) representative filtering of faction preferences (an extraction mechanism protecting property from redistribution). The factional_instability constraint combines both. Upstream constraints address the specific mechanisms of filtering and federation separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
