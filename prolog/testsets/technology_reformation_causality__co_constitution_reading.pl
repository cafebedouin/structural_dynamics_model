% ============================================================================
% CONSTRAINT STORY: technology_reformation_causality__co_constitution_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_reformation_causality__co_constitution_reading, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: technology_reformation_causality__co_constitution_reading
 *   human_readable: Technology-Reformation Co-Constitution: Bidirectional Causality Reading
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   The Reformation (c. 1517-1648) and the printing press co-evolved through
 *   feedback loops: printing enabled distribution of vernacular theology,
 *   which created demand for more printing capacity and innovation; reformed
 *   theology simultaneously shaped what printing was used for and how
 *   printing technology developed. This constraint, instantiating the
 *   co-constitution reading, rejects both technological determinism (printing
 *   made reformation inevitable) and pure beneficiary agency (reformers
 *   deployed printing as a strategic tool they fully controlled). Instead, it
 *   models the bidirectional causality: reformers shaped what the press
 *   produced, the press enabled reformation reach neither could have achieved
 *   separately, and the technology-society interaction itself became the
 *   irreducible causal unit. The constraint exhibits tangled_rope
 *   classification because genuine coordination function (solving the problem
 *   of reaching masses with theology) coexists with asymmetric extraction
 *   (scribal labor displacement, ecclesiastical monopoly erosion, printing
 *   capital concentration). The theater ratio rises over time as manuscript
 *   guilds persist institutionally long after functional obsolescence —
 *   canonical piton degradation. The extractiveness rises as the printing
 *   ecosystem matures and its gatekeeping power consolidates.
 *
 * KEY AGENTS:
 *   - Reformation theology leaders (Luther, Calvin, Zwingli, etc.): Organized/mobile beneficiaries — deployed printing strategically; did not unilaterally control outcomes but shaped what printing was used for
 *   - Printing technology developers and capital holders: Institutional/arbitrage beneficiaries — invested in printing because reformation demand created market; simultaneously enabled reformation reach they did not author
 *   - Scribal copyists and manuscript production workers: Powerless/trapped victims — economically displaced by printing scaling with no viable exit to alternative labor
 *   - Ecclesiastical hierarchy and Church authority: Institutional/constrained mixed agent — benefited from printing's coordination function (standardized theology distribution) while losing interpretive monopoly to reformed theology distribution
 *   - Printing press machines and production infrastructure: Technological agent (perspective 5) — embedded in organizational and social structures; did not autonomously determine outcomes but enabled and was shaped by social demand
 *   - Analytical observer: Civilizational perspective recognizing the co-constitutive interaction itself as the irreducible causal unit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_reformation_causality__co_constitution_reading, 0.38).
domain_priors:suppression_score(technology_reformation_causality__co_constitution_reading, 0.42).
domain_priors:theater_ratio(technology_reformation_causality__co_constitution_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_reformation_causality__co_constitution_reading, tangled_rope).
narrative_ontology:human_readable(technology_reformation_causality__co_constitution_reading, "Technology-Reformation Co-Constitution: Bidirectional Causality Reading").
narrative_ontology:topic_domain(technology_reformation_causality__co_constitution_reading, "history_of_technology/religious_history/media_studies").

domain_priors:requires_active_enforcement(technology_reformation_causality__co_constitution_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_reformation_causality__co_constitution_reading, '884804d3-641a-4914-bcfe-cb3aa209c1a8').
narrative_ontology:cs_kernel_codification('884804d3-641a-4914-bcfe-cb3aa209c1a8', distributed).
narrative_ontology:cs_authority_grounding('884804d3-641a-4914-bcfe-cb3aa209c1a8', distributed).
narrative_ontology:cs_reading_relation('884804d3-641a-4914-bcfe-cb3aa209c1a8', technology_reformation_causality__technological_determinism_reading, influences).
narrative_ontology:cs_reading_relation('884804d3-641a-4914-bcfe-cb3aa209c1a8', technology_reformation_causality__beneficiary_agency_reading, influences).
narrative_ontology:cs_axiom('884804d3-641a-4914-bcfe-cb3aa209c1a8', foundational, bidirectional_causality_irreducible).
narrative_ontology:cs_axiom_status(bidirectional_causality_irreducible, holdable).
narrative_ontology:cs_axiom_grounding('884804d3-641a-4914-bcfe-cb3aa209c1a8', bidirectional_causality_irreducible, empirically_contingent).
narrative_ontology:cs_axiom('884804d3-641a-4914-bcfe-cb3aa209c1a8', foundational, coordination_asymmetry_coexistence).
narrative_ontology:cs_axiom_status(coordination_asymmetry_coexistence, holdable).
narrative_ontology:cs_axiom_grounding('884804d3-641a-4914-bcfe-cb3aa209c1a8', coordination_asymmetry_coexistence, empirically_contingent).
narrative_ontology:cs_reference_frame('884804d3-641a-4914-bcfe-cb3aa209c1a8', mutual_constitution_framework).
narrative_ontology:cs_drift_state('884804d3-641a-4914-bcfe-cb3aa209c1a8', contemporary_historical_scholarship, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('884804d3-641a-4914-bcfe-cb3aa209c1a8', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(technology_reformation_causality__co_constitution_reading, technology_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_reformation_causality__co_constitution_reading, vernacular_reform_movements).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__co_constitution_reading, printing_technology_development).
narrative_ontology:constraint_victim(technology_reformation_causality__co_constitution_reading, ecclesiastical_monopoly_on_interpretation).
narrative_ontology:constraint_victim(technology_reformation_causality__co_constitution_reading, manuscript_scribal_labor_ecosystem).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SCRIBAL AND MANUSCRIPT LABOR (SNARE) — Copyists and professional scribes face technological obsolescence with no exit options. Their labor becomes economically unviable as printing scales; they cannot retrain into printing (guild barriers, capital requirements) or return to previous work. Trapped by loss of market demand, bearing full cost of technological displacement. Maximum extraction from this perspective: the technology-reformation co-evolution directly eliminates their economic niche.
constraint_indexing:constraint_classification(technology_reformation_causality__co_constitution_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: ECCLESIASTICAL HIERARCHY (TANGLED ROPE) — The Church benefits from printing's coordination function (mass distribution of authorized texts, standardized doctrine) but simultaneously loses its monopoly on textual interpretation. Constrained by both the utility of the technology (cannot fully suppress printing without losing administrative efficiency) and the social pressure from reformers exploiting printing. Mixed extraction: authority over text is asymmetrically distributed as reformers gain access to the printing apparatus, but the Church retains institutional coordination benefits. Genuine coordination function (standardized liturgy, unified doctrine distribution) coexists with asymmetric loss of interpretive monopoly.
constraint_indexing:constraint_classification(technology_reformation_causality__co_constitution_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REFORMER-PRINTER COALITION (ROPE) — Reformers and printing technology entrepreneurs jointly solve a coordination problem: how to distribute vernacular theology beyond manuscript reach. Both groups benefit from the partnership. Reformers gain distribution; printers gain market (theological texts become valuable commodity). Mobile exit options: reformers can advocate without print (slower, less effective), printers can print secular texts. The constraint is primarily coordination — solving the problem of reaching masses with doctrine — with minimal coercive overhead. Neither party extracts from the other; both extract from the displaced scribal labor and ecclesiastical monopoly.
constraint_indexing:constraint_classification(technology_reformation_causality__co_constitution_reading, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 4: SCRIBAL GUILDS AND MANUSCRIPT INSTITUTIONS (PITON) — The institutional apparatus of manuscript production (scriptoria, guilds, apprenticeship structures) persists long after printing renders it economically redundant. Theater ratio high: guild regulations, quality standards, and formal apprenticeships continue to be enforced and maintained through institutional momentum, even as the actual functional output becomes decorative and niche-market. Manuscript production survives as luxury craft (illuminated manuscripts for wealthy patrons), but the core coordination function (rapid text distribution) has been entirely captured by printing. The institution maintains theatrical authority but has lost real function — canonical piton signature.
constraint_indexing:constraint_classification(technology_reformation_causality__co_constitution_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: PRINTING TECHNOLOGY INFRASTRUCTURE (TANGLED ROPE) — Printing presses themselves are constrained by the social demand they simultaneously generate. The technology enables reformation messaging distribution, but actual press operation requires capital, skilled labor, paper supplies, and ink production — all dependent on social coordination. The technology does not automatically determine outcomes; it requires embedding in organizational structures (printing shops, distribution networks, binding operations). Printing benefits from the religious ferment it helps amplify, but cannot autonomously determine what gets printed or how widely it spreads. Genuine coordination function (logistics of mass production) coexists with asymmetric control: those who own presses and capital make distribution decisions that favor their interests.
constraint_indexing:constraint_classification(technology_reformation_causality__co_constitution_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the Reformation and printing technology constitute each other through feedback loops. Printing enables reformation theology distribution (coordination function); reformation theology demand drives printing innovation and capital investment (social shaping of technology); both together create the epistemic conditions for vernacular literacy and state formation. Neither technology nor social movement is primary cause; both are necessary and interdependent. The constraint is tangled because genuine coordination (mass text distribution, vernacular theology reaching audiences) coexists with asymmetric distribution of interpretive authority. This reading rejects both technological determinism (printing did not make reformation inevitable) and pure agency determinism (reformers could not have achieved reformation reach without printing). The interaction term itself — the co-evolving constraint — is the structurally irreducible unit.
constraint_indexing:constraint_classification(technology_reformation_causality__co_constitution_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_reformation_causality__co_constitution_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(technology_reformation_causality__co_constitution_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(technology_reformation_causality__co_constitution_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(technology_reformation_causality__co_constitution_reading, TR),
    TR >= 0.70.

:- end_tests(technology_reformation_causality__co_constitution_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): The constraint exhibits moderate extraction reflecting the asymmetric distribution of printing-enabled reach and capital concentration, but not maximal extraction because genuine coordination benefits (theology distribution solving a real coordination problem) are substantial. The interaction between reformer social movements and printing technology is genuinely productive — it solves problems neither could solve alone. But the benefits are asymmetrically distributed: printing capital holders capture more value than reformers; both capture value from displaced scribal labor. The value starts low (0.15) when printing technology is nascent and reformer movements are small; rises to 0.38 as the system matures and its gatekeeping power consolidates. Suppression (0.42): Moderate. The ecclesiastical hierarchy actively suppresses reformed theology distribution through censorship, book banning, and inquisitorial enforcement. But suppression is not total — reformed texts circulate through underground networks, smuggling, and in regions with weak ecclesiastical control. Printing technology itself is not suppressed (it has legitimate uses for Church texts), only certain content. Theater ratio (0.58): Moderate-high and rising. As printing becomes established, manuscript production institutions (scribal guilds, scriptoria) persist through institutional momentum long after functional obsolescence — they maintain theatrical authority through apprenticeships and quality standards but produce decorative luxury goods rather than functional text distribution. The theater rises from 0.35 to 0.58 as the contrast between manuscript institutional performance and actual market demand sharpens.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound and irreducible. The scribal worker sees only extraction and displacement (Snare). The reformer sees primarily coordination and strategic success (Rope). The ecclesiastical authority sees loss of monopoly but retention of coordination benefits (Tangled Rope). The manuscript institution sees theatrical persistence with functional obsolescence (Piton). The printing infrastructure sees itself as embedded in and dependent on the social coordination it enables (Tangled Rope). The analytical observer sees the interaction itself as the causal unit (Tangled Rope). No single perspective is wrong — each captures a real structural feature. The constraint's classification as tangled_rope at the analytical level reflects this: genuine coordination (theology distribution) coexists with asymmetric extraction (labor displacement, capital concentration, authority erosion).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective derives its directionality from its structural position in the technology-reformation interaction. Scribal workers have maximum directionality toward extraction (d ≈ 0.95): they are pure victims of technological displacement. The reformer-printer coalition has low directionality (d ≈ 0.20): they are net beneficiaries of the coordination they create. The Church has intermediate directionality (d ≈ 0.55): they lose interpretive monopoly but gain distribution efficiency. The technological agent itself occupies a paradoxical position: it is not an agent in the conventional sense but is shaped by and shapes the social actors around it — this perspective uses the analytical frame to capture the constraint's structural irreducibility. The co-constitution reading's core claim is that no single directionality captures the full causal structure — the constraint arises from the interaction between reformer intentionality and technological capability, neither of which is primary.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing how a genuinely coordinate problem (distributing theology at mass scale) coexists with asymmetric extraction (from scribal labor, from ecclesiastical authority, toward printing capital). The constraint is not purely coordinate (a mountain or rope) because the beneficiaries of printing include capital holders who extract value beyond what coordination requires. It is not purely extractive (a snare) because the coordination function is genuine and enables outcomes beneficial to multiple parties (reformers, masses gaining vernacular access, printers gaining market). The tangled_rope classification captures both aspects: active enforcement (Church censorship) and genuine coordination (mass theology distribution) coexist, neither reducible to the other. The reading rejects the false choice between technological determinism (technology determines) and pure beneficiary agency (people freely choose) — the constraint shows these are not mutually exclusive. Technology shapes what is possible; reformers shape what is chosen; the interaction is the irreducible causal unit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causality_direction_determination,
    'How much does printing technology''s development respond to reformation demand versus how much does printing development drive reformation possibilities?',
    'Historical analysis of printing innovation timelines: were key technical innovations (movable type refinement, paper production scaling, binding methods) driven by religious text demand or by independent technical evolution? Comparison of innovation rates in regions with high vs. low reformation activity.',
    'If printing driven by reformation demand: co-constitution reading strengthened (bidirectional causality confirmed). If printing development independent of religious demand: technological determinism reading gains empirical support. If mixed (region-dependent): co-constitution reading confirmed with regional variation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(causality_direction_determination, empirical, 'Whether printing innovation was demand-driven by reformation or independently developed').

omega_variable(
    reformer_strategic_use_counterfactual,
    'Could reformation theology have achieved mass circulation through non-printing channels if printing had not been available or had developed differently?',
    'Historical comparison with pre-printing mass movements (Hussite movement, Lollardy): what distribution mechanisms were available and how effective? Counterfactual analysis of alternative technologies (manuscript networks, oral networks, visual imagery). Analysis of reformation regions with limited early press access.',
    'If reformation could have spread substantially without printing: beneficiary_agency reading strengthened (reformers deployed available tools strategically). If reformation spread was printing-dependent: technological determinism reading gains support. If printing enabled but did not determine: co-constitution reading confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reformer_strategic_use_counterfactual, empirical, 'Whether reformation theology could have circulated without printing technology').

omega_variable(
    technology_determinism_versus_coevolution,
    'Is the technological determinism reading''s core premise (printing made reformation inevitable) logically foreclosed by the co-constitution reading, or do both remain live positions?',
    'Logical analysis: does the co-constitution axiom (technology and social actors mutually constitute each other, neither primary) rule out the determinism axiom (technology''s material properties enable particular social outcomes), or can both claims coexist in different frameworks? What framework would a committed technological determinist occupy? What would they have to abandon to accept co-constitution?',
    'If foreclosed: the two readings are incompatible and cannot coexist in a single institutional framework. If coexist: they represent competing empirical hypotheses about causality within a shared theological/historical framework. This omega determines the reading_relation type (forecloses vs. coexists_with).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technology_determinism_versus_coevolution, conceptual, 'Logical relationship between determinism and co-constitution axioms').

omega_variable(
    reformer_agency_attribution,
    'Does the co-constitution reading''s emphasis on bidirectional causality diminish or preserve the attribution of strategic agency to reformation leaders?',
    'Textual analysis: do historians adopting the co-constitution frame credit reformers with intentional strategy (choosing to print, selecting what to print, managing distribution) or treat reform as emergent outcome of technology-society interaction? Comparison of agency language in co-constitution vs. beneficiary_agency literature.',
    'If co-constitution diminishes agency attribution: benefits the technological determinism reading (agency becomes epiphenomenal). If co-constitution preserves agency: distinguishes this reading clearly from determinism and affirms the beneficiary_agency reading''s insight while adding a structural claim about mutual constitution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reformer_agency_attribution, conceptual, 'Whether co-constitution attribution preserves or diminishes reformer strategic agency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_reformation_causality__co_constitution_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(techref_theater_t0_baseline, technology_reformation_causality__co_constitution_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(techref_theater_t25_mid, technology_reformation_causality__co_constitution_reading, theater_ratio, 25, 0.5).
narrative_ontology:measurement(techref_theater_t50_end, technology_reformation_causality__co_constitution_reading, theater_ratio, 50, 0.58).

% Extraction over time
narrative_ontology:measurement(techref_extract_t0_baseline, technology_reformation_causality__co_constitution_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(techref_extract_t25_mid, technology_reformation_causality__co_constitution_reading, base_extractiveness, 25, 0.32).
narrative_ontology:measurement(techref_extract_t50_end, technology_reformation_causality__co_constitution_reading, base_extractiveness, 50, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(techref_suppress_t0_baseline, technology_reformation_causality__co_constitution_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(techref_suppress_t25_mid, technology_reformation_causality__co_constitution_reading, suppression_requirement, 25, 0.38).
narrative_ontology:measurement(techref_suppress_t50_end, technology_reformation_causality__co_constitution_reading, suppression_requirement, 50, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_reformation_causality__co_constitution_reading, information_standard).
narrative_ontology:affects_constraint(technology_reformation_causality__co_constitution_reading, technology_reformation_causality__technological_determinism_reading).
narrative_ontology:affects_constraint(technology_reformation_causality__co_constitution_reading, technology_reformation_causality__beneficiary_agency_reading).

% DUAL FORMULATION NOTE:
% The technology_reformation_causality kernel decomposes into three constraint stories with distinct ε values: determinism_reading (ε ≈ 0.22, Mountain — printing is fundamental cause), beneficiary_agency_reading (ε ≈ 0.28, Rope — reformers control application), and this co_constitution_reading (ε ≈ 0.38, Tangled Rope — bidirectional causality with asymmetric outcomes). The readings are not observational variants of a single constraint; they are structurally distinct causal claims with different measurement bases and different classification outcomes. The epsilon difference reflects irreducible interpretive disagreement about causality, not measurement uncertainty.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
