% ============================================================================
% CONSTRAINT STORY: fraser_river_salmon_regulation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fraser_river_salmon_regulation, []).

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
 *   constraint_id: fraser_river_salmon_regulation
 *   human_readable: Fraser River Salmon Regulation
 *   domain: economic/resource_management/indigenous_relations
 *
 * SUMMARY:
 *   The Fraser River salmon regulation system originated in the late 19th
 *   century as a colonial resource management framework. Initial regulations
 *   were justified by conservation concerns about salmon stock depletion from
 *   industrial-scale harvesting. However, the regulatory apparatus has
 *   evolved into a hybrid system that combines genuine coordination functions
 *   (preventing complete commons collapse) with systematic extraction from
 *   indigenous communities. Indigenous peoples have harvested salmon from the
 *   Fraser River for millennia using sustainable practices; their catch was
 *   relatively stable and ecologically integrated. The regulatory system that
 *   emerged after contact incorporated indigenous communities as subordinate
 *   actors subject to restrictions while privileging commercial fishing
 *   operations with larger quotas and preferential access. Over the 50-year
 *   interval examined here, extractiveness has increased from 0.28 (early
 *   conservation focus) to 0.58 (current state) as commercial interests have
 *   consolidated allocations and indigenous restrictions have tightened
 *   despite stable or increasing salmon stocks when conditions allow. Theater
 *   ratio has risen from 0.35 to 0.64 as conservation rhetoric has divorced
 *   from actual stock outcomes and regulatory decision-making has become
 *   increasingly opaque. The constraint exhibits all the hallmarks of
 *   institutional drift from Rope (pure coordination) toward Tangled Rope
 *   (mixed coordination-extraction hybrid) or Snare (pure extraction),
 *   depending on perspective.
 *
 * KEY AGENTS:
 *   - Indigenous Communities (First Nations): Primary victims (powerless/trapped) — dependent on salmon for subsistence, culture, and treaty rights; bear disproportionate regulatory burden with no exit option
 *   - Small-Scale Indigenous Fishers: Secondary victims (moderate/constrained) — face catch restrictions, seasonal limitations, and method constraints; some livelihood diversification possible but salmon remains culturally central
 *   - Commercial Fishing Operators: Primary beneficiaries (institutional/arbitrage) — receive larger catch allocations and export privileges; can exit into alternative fisheries if salmon declines
 *   - Federal Fisheries Management: Secondary beneficiary (institutional/arbitrage) — derives revenue from licensing fees and administrative permits; manages agency resources through regulatory allocations
 *   - Environmental Conservation Groups: Organized actors (organized/constrained) — originally advocates for genuine conservation but now maintain performative conservation narrative that legitimizes current allocations
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — observes structural divergence between stated conservation goals and actual allocation patterns favoring commercial extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fraser_river_salmon_regulation, 0.58).
domain_priors:suppression_score(fraser_river_salmon_regulation, 0.68).
domain_priors:theater_ratio(fraser_river_salmon_regulation, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fraser_river_salmon_regulation, extractiveness, 0.58).
narrative_ontology:constraint_metric(fraser_river_salmon_regulation, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(fraser_river_salmon_regulation, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fraser_river_salmon_regulation, tangled_rope).
narrative_ontology:human_readable(fraser_river_salmon_regulation, "Fraser River Salmon Regulation").
narrative_ontology:topic_domain(fraser_river_salmon_regulation, "economic/resource_management/indigenous_relations").

domain_priors:requires_active_enforcement(fraser_river_salmon_regulation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fraser_river_salmon_regulation, commercial_fishing_operators).
narrative_ontology:constraint_beneficiary(fraser_river_salmon_regulation, federal_government_revenue).
narrative_ontology:constraint_victim(fraser_river_salmon_regulation, indigenous_communities).
narrative_ontology:constraint_victim(fraser_river_salmon_regulation, salmon_ecosystem_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIGENOUS COMMUNITIES (SNARE) — Structurally dependent on salmon for subsistence, cultural continuity, and treaty rights. Regulations restrict catch volumes, timing, and methods while exempting commercial operations. No exit option: salmon is central to identity and food security. Cannot exit or reorganize effectively against federal enforcement. d≈0.92, f(d)≈1.39, σ=0.9 → χ≈0.73.
constraint_indexing:constraint_classification(fraser_river_salmon_regulation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: SMALL-SCALE INDIGENOUS FISHERS (TANGLED ROPE) — Experience mixed coordination and extraction. Regulations coordinate conservation (genuine benefit to long-term salmon availability) but disproportionately constrain indigenous access while commercial operations receive higher allocations. Constrained exit: some livelihood diversification possible but salmon fishing remains culturally and economically central. d≈0.70, f(d)≈1.05, σ=0.9 → χ≈0.56.
constraint_indexing:constraint_classification(fraser_river_salmon_regulation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: COMMERCIAL FISHING OPERATORS (ROPE) — Benefit from regulatory framework that allocates larger catch quotas to commercial licenses while restricting indigenous subsistence harvests. Regulations appear as coordination mechanism: allocation certainty enables business planning and capital investment. High exit optionality: can diversify into other fisheries or aquaculture if salmon quotas decline. d≈0.08, f(d)≈-0.10, σ=0.9 → χ≈-0.05. Net beneficiary.
constraint_indexing:constraint_classification(fraser_river_salmon_regulation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: FEDERAL FISHERIES MANAGEMENT (ROPE) — Derives coordination and revenue benefits from licensing fees, export permits, and administrative fees. Regulations solve legitimate collective action problem (preventing overexploitation) while generating revenue stream. High exit optionality: federal apparatus can shift to other resource domains if salmon stocks decline further. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.07. Net beneficiary.
constraint_indexing:constraint_classification(fraser_river_salmon_regulation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ENVIRONMENTAL CONSERVATION GROUPS (PITON) — Originally advocated for salmon conservation as primary rationale. Now largely performative: regulations are maintained ostensibly for conservation but scientific evidence shows commercial fishing exemptions and habitat destruction continue despite conservation rhetoric. theater_ratio≈0.64 reflects gap between conservation narrative and actual stock recovery outcomes. Conservation mandate is increasingly theatrical cover for resource extraction. d≈0.55, f(d)≈0.75, σ=0.9 → χ≈0.43.
constraint_indexing:constraint_classification(fraser_river_salmon_regulation, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Observes genuine coordination function (preventing commons collapse in salmon fishery) combined with asymmetric extraction (indigenous communities bear disproportionate regulatory burden while commercial interests receive preferential treatment). Treaty rights violations embedded in framework. System is maintained through enforcement (police, court system, licensing restrictions) rather than voluntary adoption. d≈0.60, f(d)≈0.78, σ=1.0 → χ≈0.45.
constraint_indexing:constraint_classification(fraser_river_salmon_regulation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fraser_river_salmon_regulation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fraser_river_salmon_regulation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fraser_river_salmon_regulation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fraser_river_salmon_regulation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(fraser_river_salmon_regulation, TR),
    TR >= 0.70.

:- end_tests(fraser_river_salmon_regulation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The regulatory system extracts significant value from indigenous communities through catch restrictions, licensing barriers, and enforcement mechanisms that disproportionately affect subsistence harvesting. Commercial operators receive preferential quota allocations despite producing greater ecological impact per unit catch (industrial fishing methods cause more bycatch and habitat disruption). The extraction is substantial but not maximal because the system retains a coordination function (preventing total commons collapse) and some indigenous harvesting remains permitted. Suppression (0.68): High. Enforcement mechanisms include federal licensing requirements (creating barriers to entry), seasonal closures (limiting access windows), method restrictions (prohibiting traditional harvesting approaches), and criminal penalties for unauthorized fishing (backed by police and courts). Alternatives to participation in the regulated system are severely limited — subsistence cannot be obtained through markets at affordable scale, and cultural loss is irreversible. Theater ratio (0.64): Moderate-high. Initial conservation rationale is maintained in policy documents and regulatory announcements, but empirical outcomes show stocks remain stressed despite indigenous catch reductions, while commercial operations continue with higher quotas. Scientific advisory processes exist but are often ignored when recommendations conflict with commercial interests. The gap between stated conservation goals and actual regulatory outcomes reflects theatrical maintenance of legitimacy rather than genuine problem-solving.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is stark and reflects the structural conflict between beneficiaries and victims. Commercial operators and federal management see the system as coordination (Rope) — allocation certainty, predictable enforcement, revenue generation. Indigenous communities see it as pure extraction (Snare) — restricted access to their ancestral resource, enforcement against their traditional practices, loss of cultural continuity. The analytical observer sees Tangled Rope — genuine coordination function combined with systematic asymmetric extraction. The conservation narrative tries to paper over this gap by claiming all restrictions are proportional to ecological necessity, but the evidence suggests restrictions are apportioned by political power and historical access rights rather than by ecological impact. This perspectival gap is not resolvable through better information; it is a structural feature of how the regulatory system allocates burdens and benefits.
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous communities: Victims + trapped → d≈0.92, f(d)≈1.39. Maximal extraction direction. Complete dependence on salmon for subsistence and culture; no exit options; constraints are non-negotiable from their position. Commercial operators: Beneficiaries + arbitrage → d≈0.08, f(d)≈-0.10. Maximal beneficiary direction. Can exit constraint through diversification; regulations provide certainty and preferential access. Federal management: Beneficiaries + arbitrage → d≈0.05, f(d)≈-0.12. Maximal beneficiary direction. Revenue stream and administrative power; no institutional incentive to relax regulations. Environmental groups: Mixed + constrained → d≈0.55, f(d)≈0.75. Originally advocates for indigenous rights and genuine conservation; now constrained within systems that silence their conservation concerns when inconvenient to commercial interests. Analytical observer: neutral analytical position → d≈0.60, f(d)≈0.78. Observes structural asymmetry without direct extraction or benefit; sees hybrid coordination-extraction dynamic.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is central to this constraint because the system appears to many policymakers as pure coordination (Rope: conservation mechanism preventing commons tragedy) while structurally functioning as Tangled Rope (hybrid coordination-extraction) or even Snare (pure extraction) from indigenous perspectives. The resolution requires rejecting the mandatrophy by recognizing that the constraint is NOT a pure conservation problem because: (1) conservation rhetoric is selective — it applies to indigenous subsistence fishing but not commercial fishing, despite commercial operations having greater ecological impact; (2) regulatory allocations do not correlate with ecological impact (indigenous subsistence represents ~5-10% of mortality; commercial operations represent ~60-70%, yet restrictions are inverted); (3) enforcement is asymmetric (indigenous violations trigger prosecution; commercial violations are often administrative). The Tangled Rope classification resolves mandatrophy by identifying the genuine coordination function (preventing total stock collapse) while insisting on accurate characterization of who bears costs and who captures benefits. The system maintains its appearance as legitimate coordination through theater (conservation language, scientific advisory bodies, environmental rhetoric) while the actual allocation pattern reflects extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conservation_vs_extraction_attribution,
    'How much of the salmon stock decline is attributable to indigenous subsistence fishing vs. commercial fishing vs. habitat destruction vs. climate change? Does regulation apportion restrictions proportionally to actual impact?',
    'Population genetics analysis, catch monitoring data, dam/habitat impact quantification, thermal regime modeling. Compare fish loss rates by sector against regulatory burden allocation.',
    'If indigenous subsistence contributes <5% of decline but bears 40% of restrictions: regulatory allocation is purely extractive (Snare). If contribution and burden are proportional: regulation is legitimate coordination (Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conservation_vs_extraction_attribution, empirical, 'Attribution of salmon decline to each sector vs. regulatory burden allocation').

omega_variable(
    treaty_rights_enforcement_gap,
    'Do federal fisheries regulations violate explicit treaty rights to fish for food and ceremonial purposes? If so, are enforcement mechanisms selectively applied to indigenous vs. non-indigenous operators?',
    'Treaty text analysis, prosecution data by ethnicity of operator, injunction patterns, license approval rates by applicant type.',
    'If regulations violate treaty rights and enforcement is selective: this is institutional theft (Snare). If regulations apply equally and treaty rights are legally superseded: constraint is legitimate (Tangled Rope or Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(treaty_rights_enforcement_gap, empirical, 'Whether regulations violate treaty rights and if enforcement is selective').

omega_variable(
    ecosystem_recovery_mechanism,
    'Do current restrictions on indigenous fishing actually improve salmon stock recovery? Is there evidence that relaxing commercial quotas would further damage stocks, justifying their higher allocations?',
    'Time series analysis of stock recovery vs. subsistence catch levels; controlled comparison with fisheries where indigenous allocations were increased; ecosystem modeling of commercial vs. subsistence impact.',
    'If stock decline continues despite indigenous restrictions: extraction rationale collapses (constraint is pure Snare). If stocks recover when commercial quotas tighten: conservation rationale is genuine (Tangled Rope or Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecosystem_recovery_mechanism, empirical, 'Whether indigenous catch restrictions are necessary for ecosystem recovery').

omega_variable(
    regulatory_capture_by_commercial_interests,
    'Have commercial fishing interests captured fisheries management agencies? Are regulatory decisions made in response to indigenous advocacy as frequently as commercial industry advocacy?',
    'Qualitative analysis of policy documents, meeting minutes, lobbying expenditure tracking, revolving-door analysis of agency staff. Compare response time and favorability to requests by sector.',
    'If commercial interests dominate: constraint is deliberately extractive Snare maintained by capture. If interests are balanced: constraint is more legitimately coordinated (Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_by_commercial_interests, empirical, 'Degree of regulatory capture by commercial fishing interests').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fraser_river_salmon_regulation, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fsr_tr_t0, fraser_river_salmon_regulation, theater_ratio, 0, 0.35).
narrative_ontology:measurement(fsr_tr_t25, fraser_river_salmon_regulation, theater_ratio, 25, 0.5).
narrative_ontology:measurement(fsr_tr_t50, fraser_river_salmon_regulation, theater_ratio, 50, 0.64).

% Extraction over time
narrative_ontology:measurement(fsr_be_t0, fraser_river_salmon_regulation, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(fsr_be_t25, fraser_river_salmon_regulation, base_extractiveness, 25, 0.45).
narrative_ontology:measurement(fsr_be_t50, fraser_river_salmon_regulation, base_extractiveness, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fraser_river_salmon_regulation, resource_allocation).
narrative_ontology:affects_constraint(fraser_river_salmon_regulation, canadian_indigenous_treaty_enforcement).
narrative_ontology:affects_constraint(fraser_river_salmon_regulation, west_coast_fisheries_commons).

% DUAL FORMULATION NOTE:
% Fraser River salmon regulation is downstream of broader Canadian colonial resource management frameworks (affects_constraints: canadian_indigenous_treaty_enforcement) and interacts with west coast marine commons dynamics. The ε value of 0.58 reflects the specific regulatory allocation bias; the broader treaty framework has higher extractiveness (≈0.75) and the commons has lower extractiveness if indigenous management were restored (≈0.15).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fraser_river_salmon_regulation, organized, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
