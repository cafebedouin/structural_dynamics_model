% ============================================================================
% CONSTRAINT STORY: scam_doubt_manufacturing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_scam_doubt_manufacturing, []).

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
 *   constraint_id: scam_doubt_manufacturing
 *   human_readable: The Playbook for Manufacturing Scientific Doubt (SCAM)
 *   domain: economic/political
 *
 * SUMMARY:
 *   The SCAM playbook (Systematic, Coordinated, Across-industries Messaging
 *   for doubt manufacturing) represents one of the most durable snares in
 *   modern political economy. Industries facing scientific evidence linking
 *   their products to public harm deploy coordinated strategies to
 *   manufacture uncertainty: funding industry-friendly research, attacking
 *   independent scientists, infiltrating regulatory agencies, and creating
 *   sophisticated disinformation narratives. Unlike crude denial, the
 *   playbook is epistemically sophisticated — it doesn't claim the evidence
 *   is false but argues it is contested, preliminary, or overstated. This
 *   strategy is particularly effective because it exploits real properties of
 *   science (uncertainty is inherent, replication takes time) to manufacture
 *   artificial uncertainty about settled questions. The constraint exhibits
 *   multiple institutional perspectives because different actors experience
 *   the extraction through different lenses. The exposed population is in a
 *   pure snare with no exit. Scientific integrity is an abstract collective
 *   good with no advocate. Independent scientists face targeted suppression.
 *   Regulatory agencies experience mixed coordination (industry cooperation)
 *   and extraction (capture). Industry defendants experience it as pure
 *   coordination. Academic institutions show degradation toward piton through
 *   funding dependency. The analytical observer risks naturalizing this as
 *   inherent scientific contestation rather than institutional corruption.
 *
 * KEY AGENTS:
 *   - Exposed Population: Primary victim (powerless/trapped) — individuals harmed by products continue exposure due to information asymmetry, economic necessity, or ubiquity
 *   - Scientific Integrity Commons: Primary victim (powerless/trapped) — abstract collective good that cannot organize or exit; erosion of epistemic trust across domains
 *   - Industry Defendants: Primary beneficiary (institutional/arbitrage) — tobacco, fossil fuels, chemicals, pharmaceuticals that benefit from regulatory delay and litigation obstruction
 *   - Independent Scientists: Secondary victim (moderate/constrained) — face funding cuts, publication suppression, career threats when publishing contrary evidence
 *   - Regulatory Agencies: Tertiary victim/beneficiary (moderate/constrained) — captured by industry expertise and revolving doors but mandated to protect public health
 *   - Academic Institutions: Secondary beneficiary (institutional/arbitrage) — depend on industry funding; incentivized to produce publishable research regardless of conclusions
 *   - Public Relations and Science Consulting Industry: Tertiary beneficiary (powerful/arbitrage) — profitable services selling doubt manufacturing expertise
 *   - Analytical Observer: Risk of false naturalization (analytical/analytical) — perspectives that see scientific uncertainty as inherent rather than manufactured
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(scam_doubt_manufacturing, 0.68).
domain_priors:suppression_score(scam_doubt_manufacturing, 0.72).
domain_priors:theater_ratio(scam_doubt_manufacturing, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(scam_doubt_manufacturing, extractiveness, 0.68).
narrative_ontology:constraint_metric(scam_doubt_manufacturing, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(scam_doubt_manufacturing, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(scam_doubt_manufacturing, snare).
narrative_ontology:human_readable(scam_doubt_manufacturing, "The Playbook for Manufacturing Scientific Doubt (SCAM)").
narrative_ontology:topic_domain(scam_doubt_manufacturing, "economic/political").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(scam_doubt_manufacturing, industry_defendants).
narrative_ontology:constraint_beneficiary(scam_doubt_manufacturing, regulatory_capture_beneficiaries).
narrative_ontology:constraint_victim(scam_doubt_manufacturing, public_health).
narrative_ontology:constraint_victim(scam_doubt_manufacturing, scientific_integrity).
narrative_ontology:constraint_victim(scam_doubt_manufacturing, policymaking_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXPOSED POPULATION (SNARE) — Individuals exposed to harmful products cannot opt out without migration. They are trapped by economic necessity, geography, or product ubiquity (tobacco, asbestos, lead additives, fossil fuels). Information asymmetry ensures they don't know the true risk until harm manifests. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.63.
constraint_indexing:constraint_classification(scam_doubt_manufacturing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SCIENTIFIC INTEGRITY COMMONS (SNARE) — The epistemic commons has no advocate and no exit. Systematic doubt manufacturing corrodes trust in science across domains, creating civilizational-scale coordination failure. Abstract collective cannot defend itself or sue. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.65.
constraint_indexing:constraint_classification(scam_doubt_manufacturing, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: REGULATORY AGENCIES (TANGLED ROPE) — Capture victims and beneficiaries simultaneously. They benefit from industry cooperation and captured expertise (funding, revolving-door careers) but are constrained by public health mandate and political accountability. Active enforcement of doubt manufacturing playbook is required to maintain the extraction. d≈0.58, f(d)≈0.68, σ=1.0 → χ≈0.46.
constraint_indexing:constraint_classification(scam_doubt_manufacturing, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INDUSTRY DEFENDANTS (ROPE) — Experiences the constraint as coordination: manufacturing doubt delays costly regulation, litigation, product reformulation. The playbook solves a collective action problem for competing firms: all benefit from coordinated epistemic disruption without any single firm bearing the full reputational cost. Arbitrage exit via lobbying, public relations, strategic science funding. d≈0.02, f(d)≈-0.19, σ=1.2 → χ≈-0.10. Net beneficiary.
constraint_indexing:constraint_classification(scam_doubt_manufacturing, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ACADEMIC INSTITUTIONS (PITON) — Universities and research institutes increasingly depend on industry funding. The doubt manufacturing playbook creates performative academic autonomy: scientists can be funded to produce 'research' that appears independent but is selected for desired conclusions. Theater persists through institutional inertia and financial dependency. theater_ratio=0.65 reflects partial degradation — some legitimate research continues alongside captured work. d≈0.35, f(d)≈0.32, σ=1.0 → χ≈0.21.
constraint_indexing:constraint_classification(scam_doubt_manufacturing, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: INDEPENDENT SCIENTISTS (SNARE) — Scientists producing evidence of harm face coordinated attack: funding defunding, publication suppression via journal gatekeeping, ad hominem campaigns, legal threats. Constrained exit — they can leave the field or recant, but not walk away from their previous findings. d≈0.78, f(d)≈1.08, σ=1.2 → χ≈0.56.
constraint_indexing:constraint_classification(scam_doubt_manufacturing, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURALIZATION RISK (MOUNTAIN) — From civilizational scope, an observer might see scientific uncertainty as inherent: 'All empirical claims face challenges. Industry-funded research is part of the normal scientific process.' This naturalizes what is actually a coordinated snare. However, base properties (ε=0.68, suppression=0.72) violate mountain thresholds. The false summit detector fires: this is not a law of nature but an institutional arrangement. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.78, contradicting mountain claim.
constraint_indexing:constraint_classification(scam_doubt_manufacturing, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(scam_doubt_manufacturing_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(scam_doubt_manufacturing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(scam_doubt_manufacturing, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(scam_doubt_manufacturing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(scam_doubt_manufacturing, TR),
    TR >= 0.70.

:- end_tests(scam_doubt_manufacturing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts in multiple currencies: delayed regulation (economic benefit to industry), suppressed litigation (legal benefit), degraded public health (cost to exposed population), erosion of scientific trust (epistemic cost to commons). The 0.68 reflects moderate-to-high extraction — the playbook is effective at delay and obstruction but not absolute prevention. Eventually evidence usually prevails (tobacco, asbestos, lead). Suppression (0.72): High. Coordinated strategy explicitly aims to suppress truth-telling: funding gatekeeping (control which research gets funded), publication suppression (control which findings get published), career threats (suppress researchers), disinformation campaigns (suppress public understanding). The suppression is active and systematic. Theater ratio (0.65): Moderate-high. The playbook is sophisticated theater: it frames itself as 'scientific debate' or 'healthy skepticism' when the intent is obstruction. Performative academic research funded to produce predetermined conclusions. The theater has increased over the 50-year interval as the playbook has become professionalized (consulting firms, sophisticated media strategies).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural extraction appears as coordination to the extractor (industry), as mixed effects to captured institutions (regulators), and as pure snare to victims. Industry defendants genuinely experience the doubt manufacturing playbook as solving a collective action problem — all firms benefit from uncertainty delay, none wants to bear the full reputational cost of a contrarian campaign. From their perspective, it's rope. Regulatory agencies experience tangled rope because they benefit from industry cooperation (funding, expertise, revolving-door careers) while being constrained by public health mandate. They actively enforce the extraction (slowing responses to evidence) but are themselves partially captured. Independent scientists see snare with constrained exit — they can recant or leave the field but cannot undo their findings. The exposed population and scientific integrity commons see pure snare with no exit. The analytical observer faces strong temptation to naturalize this as inherent scientific uncertainty, which the false summit detector should flag.
 *
 * DIRECTIONALITY LOGIC:
 *   Exposed population: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction; no exit due to economic necessity or ubiquity. Scientific integrity: Victim + trapped → d≈0.95, f(d)≈1.42. Absolute extraction; abstract collective cannot exit. Industry defendants: Beneficiary + arbitrage → d≈0.02, f(d)≈-0.19. Net beneficiary through coordinated strategy. Regulatory agencies: Victim + constrained (captured) → d≈0.58, f(d)≈0.68. Mixed because they are both targets (capture victims) and enforcers (extraction participants). Independent scientists: Victim + constrained → d≈0.78, f(d)≈1.08. Significant extraction; can exit field but not prior findings. Academic institutions: Beneficiary + arbitrage (funding dependent) → d≈0.35, f(d)≈0.32. Partial beneficiary through industry dependency. Piton classification comes from theater gate and institutional inertia, not high directionality extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE DOMINANCE WITH INSTITUTIONAL COMPLICATIONS: The constraint resolves the mandatrophy by showing that the snare classification dominates from the perspective of those bearing costs (exposed population, scientific commons, independent scientists), while beneficiaries experience it as coordination (rope) or even beneficial structure. The tangled rope experienced by captured regulators represents the institutional failure point — when nominally independent agencies become extraction participants. The piton classification of academic institutions shows how coordination-dependent institutions (dependent on industry funding) transition toward theater when their primary function (independent research) is compromised. The snare is NOT false — it is the primary structural reality. The rope and piton perspectives reveal institutional corruption and capture, not legitimate coordination. The analytical observer's temptation to see mountain (inherent scientific uncertainty) is precisely the false naturalization that the snare exploits. The playbook's success depends on this naturalization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    funding_independence_threshold,
    'At what level of industry funding does academic independence cease to be a meaningful concept? Is there a bright line or a continuous degradation?',
    'Comparative analysis of research conclusions vs funding source across studies in the same domain; meta-analysis of effect sizes for industry-funded vs independent research; tracking of researcher career outcomes after contrarian findings',
    'If bright line exists (e.g., >30% industry funding = captured): clearer detection mechanism. If continuous: piton classification becomes more difficult to distinguish from rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(funding_independence_threshold, empirical, 'Threshold at which industry funding compromises academic independence').

omega_variable(
    coordinated_vs_emergent_doubt,
    'Is doubt manufacturing a coordinated cartel strategy or an emergent equilibrium outcome of rational industry incentives?',
    'Historical documentation of internal communications (discovered in litigation); tracking of PR consulting contracts; comparison of doubt-manufacturing timing across industries; analysis of whether isolated firms reduce doubt manufacturing when cartel coordination breaks down',
    'If coordinated: conspiracy law applies, snare classification is robust. If emergent equilibrium: each firm acts rationally, snare persists even without explicit conspiracy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordinated_vs_emergent_doubt, empirical, 'Whether doubt manufacturing is coordinated strategy or emergent equilibrium').

omega_variable(
    epistemic_commons_recovery_capacity,
    'Can scientific integrity be recovered once doubt manufacturing playbook has been deployed at scale? Is there a tipping point of institutional damage beyond which recovery becomes analytically impossible?',
    'Historical case studies (tobacco, asbestos, climate): timeline for recovery of public trust, regulator independence, researcher autonomy; measurement of persistent skepticism even after evidence becomes overwhelming',
    'If recovery is possible: snare has an exit path via institutional reform (potential scaffold). If not: permanent structural damage, snare transitions toward mountain (irreversible harm).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(epistemic_commons_recovery_capacity, conceptual, 'Whether scientific integrity can recover from large-scale doubt manufacturing').

omega_variable(
    regulatory_capture_reversibility,
    'Is the tangled rope experienced by regulatory agencies reversible, or does captured regulation become a piton through institutional inertia?',
    'Analysis of regulatory agency performance pre- and post-capture; measurement of enforcement action changes; tracking of regulator budget and independence over time; historical timing of capture vs piton degradation',
    'If reversible: tangled rope classification holds. If irreversible: agencies transition to piton, and the snare becomes more entrenched because the formal regulatory exit is blocked.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_reversibility, empirical, 'Whether regulatory capture is reversible or becomes institutional piton').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(scam_doubt_manufacturing, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scam_tr_t0, scam_doubt_manufacturing, theater_ratio, 0, 0.35).
narrative_ontology:measurement(scam_tr_t25, scam_doubt_manufacturing, theater_ratio, 25, 0.5).
narrative_ontology:measurement(scam_tr_t50, scam_doubt_manufacturing, theater_ratio, 50, 0.65).

% Extraction over time
narrative_ontology:measurement(scam_be_t0, scam_doubt_manufacturing, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(scam_be_t25, scam_doubt_manufacturing, base_extractiveness, 25, 0.53).
narrative_ontology:measurement(scam_be_t50, scam_doubt_manufacturing, base_extractiveness, 50, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(scam_doubt_manufacturing, information_standard).
narrative_ontology:affects_constraint(scam_doubt_manufacturing, regulatory_capture).
narrative_ontology:affects_constraint(scam_doubt_manufacturing, scientific_publication_gatekeeping).
narrative_ontology:affects_constraint(scam_doubt_manufacturing, epistemic_commons_erosion).
narrative_ontology:affects_constraint(scam_doubt_manufacturing, litigation_obstruction).

% DUAL FORMULATION NOTE:
% SCAM doubt manufacturing is the meta-constraint that enables or sustains several downstream constraints. Its primary structural function is to manufacture uncertainty about causal links (product harm), which prevents regulatory action and litigation. Related constraints (regulatory_capture, publication_gatekeeping, epistemic_commons_erosion) are specific instantiations or consequences of the SCAM playbook. The network captures this dependency: SCAM is upstream; specific-domain corruption follows downstream.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(scam_doubt_manufacturing, institutional, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
