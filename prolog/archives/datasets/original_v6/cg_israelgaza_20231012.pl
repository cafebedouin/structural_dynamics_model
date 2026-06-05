% ============================================================================
% CONSTRAINT STORY: cg_israelgaza_20231012
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cg_israelgaza_20231012, []).

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
 *   constraint_id: cg_israelgaza_20231012
 *   human_readable: Israeli Blockade of Gaza Strip
 *   domain: political/humanitarian
 *
 * SUMMARY:
 *   The Israeli blockade of the Gaza Strip, implemented as a security measure
 *   following the 2007 Hamas takeover, represents one of the clearest
 *   structural examples of a snare constraint in contemporary geopolitics.
 *   The blockade restricts the movement of people and goods in and out of
 *   Gaza through comprehensive control of crossing points, airspace, and
 *   territorial waters. For 16+ years, the constraint has operated through
 *   dual mechanics: (1) genuine security coordination function (preventing
 *   weapons smuggling, interdicting attack planning), and (2) extractive
 *   economic strangulation (unemployment, dependency on aid, loss of
 *   productive capacity). The constraint exhibits high suppression (0.85) —
 *   Gaza's geographic isolation and complete Israeli control of exits
 *   eliminate alternatives — and high extractiveness (0.72) — the civilian
 *   population bears catastrophic costs while security benefits accrue to
 *   Israeli institutions. Theater ratio (0.48) remains moderate because the
 *   security justification retains material credibility, though enforcement
 *   has drifted toward collective punishment rationale. The constraint
 *   qualifies as snare because suppression and extractiveness both exceed
 *   thresholds; mandatrophy is resolved by acknowledging that coordination
 *   and extraction components genuinely coexist — the blockade does provide
 *   security (real), and it does extract from the trapped population (real).
 *   Neither component negates the other; the constraint is hybrid. However,
 *   the snare classification reflects that extraction exceeds coordination
 *   from most analytical perspectives.
 *
 * KEY AGENTS:
 *   - Gaza Civilian Population: Primary victims (powerless/trapped) — 2.3 million people subject to indefinite blockade with zero exit options; bear full extraction cost through unemployment, economic collapse, humanitarian dependency
 *   - Israeli Security Apparatus: Primary beneficiary (institutional/arbitrage) — controls extraction mechanism, benefits from security perimeter, has arbitrage options to adjust enforcement
 *   - Palestinian Authority/Gaza Administration: Secondary actor (moderate/constrained) — shares governance coordination function but severely harmed by blockade's revenue loss and sovereignty restriction
 *   - Israeli Political Leadership: Institutional actor (institutional/arbitrage) — maintains blockade through policy choice; benefits from domestic security narrative and regional positioning
 *   - International Humanitarian Community: Organized observer (organized/constrained) — provides aid within blockade constraints; sees both coordination (humanitarian logistics) and extraction (aid dependency creation)
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent geopolitical arrangement as immutable territorial constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cg_israelgaza_20231012, 0.72).
domain_priors:suppression_score(cg_israelgaza_20231012, 0.85).
domain_priors:theater_ratio(cg_israelgaza_20231012, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cg_israelgaza_20231012, extractiveness, 0.72).
narrative_ontology:constraint_metric(cg_israelgaza_20231012, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(cg_israelgaza_20231012, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cg_israelgaza_20231012, snare).
narrative_ontology:human_readable(cg_israelgaza_20231012, "Israeli Blockade of Gaza Strip").
narrative_ontology:topic_domain(cg_israelgaza_20231012, "political/humanitarian").

domain_priors:requires_active_enforcement(cg_israelgaza_20231012).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cg_israelgaza_20231012, israeli_security_apparatus).
narrative_ontology:constraint_beneficiary(cg_israelgaza_20231012, israeli_political_leadership).
narrative_ontology:constraint_victim(cg_israelgaza_20231012, gaza_civilian_population).
narrative_ontology:constraint_victim(cg_israelgaza_20231012, gaza_economic_capacity).
narrative_ontology:constraint_victim(cg_israelgaza_20231012, palestinian_self_determination).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GAZA CIVILIAN POPULATION (SNARE) — Trapped. No exit from Gaza without Israeli permission. Movement severely restricted; employment, education, medical care, and trade all controlled by blockade. Bears full extraction cost: 16+ years of economic contraction, unemployment >40%, humanitarian dependency on external aid, zero arbitrage options. Maximum experienced extraction — powerless agent with trapped exit.
constraint_indexing:constraint_classification(cg_israelgaza_20231012, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: PALESTINIAN AUTHORITY / GAZA ADMINISTRATION (TANGLED ROPE) — Constrained exit. Shares some coordination function (governance, services delivery within blockade bounds) but experiences severe extraction through loss of revenue, sovereignty, and administrative capacity. Benefits from maintaining institutional roles; severely harmed by blockade's economic strangulation. Moderate power with constrained options produces mixed coordination-extraction.
constraint_indexing:constraint_classification(cg_israelgaza_20231012, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ISRAELI SECURITY APPARATUS (ROPE) — Primary beneficiary. Experiences blockade as coordination mechanism: maintains security perimeter, prevents weapons smuggling, deters attacks. Has arbitrage options (can adjust enforcement intensity, negotiate exceptions, modify checkpoint procedures). Net beneficiary from extraction flow — constraint subsidizes security goals.
constraint_indexing:constraint_classification(cg_israelgaza_20231012, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL HUMANITARIAN COMMUNITY (TANGLED ROPE) — Organized actors (UN, ICRC, NGOs) see blockade as mixing genuine coordination (logistics of humanitarian access, disease prevention, refugee management) with severe extraction (dependency creation, aid conditionality, inability to address root causes). Exit is constrained by institutional mandates and Palestinian dependence. Moderate effective extraction for organized actors with exit paths.
constraint_indexing:constraint_classification(cg_israelgaza_20231012, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ISRAELI POLITICAL ESTABLISHMENT (PITON) — Maintains blockade through institutional inertia despite degraded functionality. Original security justification (prevent weapons/attack) has atrophied into de facto collective punishment regime. Theater ratio high: blockade persists because alternatives haven't replaced it politically, not because it achieves stated security goals. Political cost of lifting blockade exceeds perceived security benefit, creating performative maintenance.
constraint_indexing:constraint_classification(cg_israelgaza_20231012, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / GEOPOLITICAL VIEW (FALSE SUMMIT) — From civilizational perspective, some form of territorial control/border enforcement appears as immutable geopolitical law. Gaza's geography (Sinai border, Israel border, coastline) creates inherent coordination challenges. However, this perspective naturalizes what is actually a contingent political choice — blockade severity and humanitarian scope are policy decisions, not geophysical constants. Engine flags this as false summit.
constraint_indexing:constraint_classification(cg_israelgaza_20231012, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cg_israelgaza_20231012_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cg_israelgaza_20231012, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cg_israelgaza_20231012, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cg_israelgaza_20231012, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cg_israelgaza_20231012, TR),
    TR >= 0.70.

:- end_tests(cg_israelgaza_20231012_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72): High and increasing. The blockade transfers economic value to Israeli security apparatus (cost-free perimeter defense, eliminated internal threats from Gaza-based groups, strategic leverage in Israeli-Palestinian negotiations). The trapped population bears 100% of economic cost: Gaza GDP per capita declined ~40% since 2007; unemployment among youth exceeds 50%; manufactured goods industry collapsed; agriculture severely restricted. The 0.72 value reflects that extraction is severe and open-ended — no mechanism for extraction to decline except through external political change. Suppression (0.85): Very high. Gaza's geography (surrounded by Israel and Egypt, no independent exit points) combined with Israeli military control creates near-total suppression of alternatives. Population cannot exit without Israeli permission; economic activity is impossible without Israeli permission; humanitarian goods cannot enter without Israeli approval. Alternative supply routes (Egypt border) are intermittently open and controlled by Egyptian state. Suppression slightly below 1.0 only because limited humanitarian corridors and occasional Egyptian cooperation provide minimal alternatives. Theater ratio (0.48): Moderate, not high. This distinguishes the blockade from purely theatrical constraints. The security justification retains empirical credibility — Hamas weapons smuggling was documented, attack planning did involve Gaza-based groups. The theater is present in how security rationale has drifted from specific threat prevention toward comprehensive collective punishment, and in how humanitarian rhetoric co-exists with enforcement that contradicts stated humanitarian concern. But the theater is not dominant; the constraint functions as intended (security perimeter maintained). This prevents piton classification.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates divergent classification from identical structural data. The security apparatus and political leadership see coordination (Rope) with their arbitrage options and beneficiary status. The trapped population sees extraction (Snare) with their trapped exit and victim status. The international community sees hybrid (Tangled Rope) because they have constrained exit and see both coordination and extraction. The political establishment's institutional inertia view (Piton) reflects that the blockade persists through policy choice, not security necessity. The false summit (Mountain) perspective risks naturalizing a contingent arrangement. The perspectival gap is extreme because the same structural constraint operates as fundamentally different institutions for different agents — which is the defining signature of snare classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) reflect each agent's structural position relative to extraction flow. The trapped Gaza population has d ≈ 0.95 (full target): they are excluded from exit options entirely, declared as victims, experiencing powerlessness within the constraint. The Israeli security apparatus has d ≈ 0.08 (near full beneficiary): they are declared as beneficiaries, retain arbitrage exit options (can negotiate, adjust enforcement, have alternatives), and benefit from extraction flow. The Palestinian Authority has d ≈ 0.65 (moderate target): they are partly beneficiaries (retain administrative roles) but mostly victims (loss of revenue, sovereignty). The international community has d ≈ 0.72 (analytical observer): they see full structure including both coordination and extraction, with constrained ability to exit humanitarian commitment. The Israeli political establishment has d ≈ 0.15 (partial beneficiary): they benefit from domestic security narrative and regional positioning but face international costs and maintain blockade through institutional choice rather than structural necessity. These d values produce the chi scaling that differentiates perspectives: beneficiaries with arbitrage experience low or negative chi (constraint subsidizes them); trapped agents experience maximum chi; organized agents with partial exits experience moderate chi.
 *
 * MANDATROPHY ANALYSIS:
 *   [RESOLVED MANDATROPHY] Reviewed 2026-03-01. Override: false_natural_law.
 *   MANDATROPHY RESOLVED: The blockade genuinely contains both coordination and extraction components. The coordination function is real (security perimeter prevents specific threats). The extraction is real (trapped population bears 100% cost). Mandatrophy is resolved by acknowledging both components exist while classifying the constraint as snare because: (1) extraction exceeds coordination in magnitude and impact for most analytical perspectives; (2) the primary victims (trapped population) experience zero coordination benefit while bearing maximum extraction cost; (3) the extraction flow is open-ended and volitional (not structurally necessary, sustained by political choice); (4) suppression is so high that exit alternatives are essentially zero, making victims unable to negotiate or organize. The snare classification does not deny the security coordination function; it reflects that this function is asymmetrically distributed — beneficial to one institutional actor, catastrophic to another. The blockade is not a pure mountain (not immutable law of geography) and not a pure rope (not symmetric coordination) precisely because the two components serve opposite institutional interests.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    security_necessity_empirical,
    'To what extent is blockade severity empirically necessary for Israeli security, versus what portion represents extractive surplus?',
    'Comparative analysis: security metrics (attack prevention, smuggling interdiction rates) versus blockade stringency over time; comparison with alternative security frameworks in similarly contested territories; counterfactual analysis of reduced-severity regimes.',
    'If security necessity <30%: classification shifts toward pure snare. If >70%: classification shifts toward rope (genuine coordination for security). Current assessment assumes ~40-50% necessity, placing constraint at tangled rope/snare boundary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(security_necessity_empirical, empirical, 'Empirical necessity of blockade stringency for security versus extractive component').

omega_variable(
    exit_pathway_feasibility,
    'Are there structurally viable alternatives (two-state resolution, regional open-border framework, Gaza demilitarization treaty) that could replace the blockade without Israeli security compromise?',
    'Game-theoretic modeling of alternative frameworks; historical analysis of similar territorial disputes resolved without indefinite blockade; negotiation simulation with credible commitment mechanisms.',
    'If alternatives exist and are politically blocked: extraction mechanism is revealed as volitional, not structural (snare classification strengthened). If alternatives are genuinely infeasible: partial mountain/rope classification may apply.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_pathway_feasibility, conceptual, 'Whether structurally viable alternatives exist to current blockade arrangement').

omega_variable(
    humanitarian_threshold_definition,
    'At what level of blockade stringency does a security measure cross into prohibited collective punishment under international humanitarian law?',
    'International legal analysis; comparison with ICJ precedent on occupation law; documentation of threshold violations through humanitarian access data, health metrics, economic indicators.',
    'If current blockade crosses threshold: classification confirmed as snare with legal violation component. If within threshold: partial legitimacy claim for security coordination function may apply.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(humanitarian_threshold_definition, conceptual, 'Whether blockade violates collective punishment prohibitions under IHL').

omega_variable(
    palestinian_alternative_governance,
    'Would Palestinian self-governance structures demonstrate capacity to implement security arrangements that would justify reduced blockade, or does governance collapse indicate necessity of external control?',
    'Institutional analysis of Palestinian Authority capacity; comparison with other disputed territories managing security autonomously; simulation of governance structures under reduced blockade.',
    'If capacity demonstrated: blockade appears as externally imposed extraction rather than protection of failed governance. If capacity doubtful: partial coordination justification for blockade may apply.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(palestinian_alternative_governance, empirical, 'Palestinian capacity for self-governance under reduced blockade conditions').

omega_variable(
    intergenerational_extraction_permanence,
    'Is the blockade structurally designed as temporary security measure (with time-bound enforcement) or as permanent regime (with indefinite extraction by default)?',
    'Analysis of blockade design documents, official policy statements, enforcement timelines, and termination conditions. Comparison of stated justifications against actual policy implementation; examination of whether relaxation mechanisms or sunset clauses exist.',
    'If designed as temporary: may legitimately classify as scaffold rather than snare. If permanent by default (termination requires political negotiation): extraction mechanism is revealed as volitional and open-ended, strengthening snare classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intergenerational_extraction_permanence, empirical, 'Whether blockade is structurally temporary or permanently designed').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cg_israelgaza_20231012, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cgaz_tr_t0, cg_israelgaza_20231012, theater_ratio, 0, 0.35).
narrative_ontology:measurement(cgaz_tr_t8, cg_israelgaza_20231012, theater_ratio, 8, 0.42).
narrative_ontology:measurement(cgaz_tr_t16, cg_israelgaza_20231012, theater_ratio, 16, 0.48).

% Extraction over time
narrative_ontology:measurement(cgaz_be_t0, cg_israelgaza_20231012, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(cgaz_be_t8, cg_israelgaza_20231012, base_extractiveness, 8, 0.62).
narrative_ontology:measurement(cgaz_be_t16, cg_israelgaza_20231012, base_extractiveness, 16, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cg_israelgaza_20231012, enforcement_mechanism).
narrative_ontology:affects_constraint(cg_israelgaza_20231012, palestinian_economic_capacity).
narrative_ontology:affects_constraint(cg_israelgaza_20231012, israeli_palestinian_power_asymmetry).
narrative_ontology:affects_constraint(cg_israelgaza_20231012, humanitarian_access_mechanisms).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cg_israelgaza_20231012, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
