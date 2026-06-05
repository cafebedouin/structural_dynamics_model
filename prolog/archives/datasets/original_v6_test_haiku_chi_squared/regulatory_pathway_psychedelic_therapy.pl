% ============================================================================
% CONSTRAINT STORY: regulatory_pathway_psychedelic_therapy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_regulatory_pathway_psychedelic_therapy, []).

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
 *   constraint_id: regulatory_pathway_psychedelic_therapy
 *   human_readable: The Regulatory and Clinical Pathway for Novel Psychedelic Therapies
 *   domain: technological/political/healthcare
 *
 * SUMMARY:
 *   The regulatory pathway for bringing novel psychedelic therapies (DMT,
 *   psilocybin, LSD) to market for treating conditions like
 *   treatment-resistant depression represents a complex hybrid constraint
 *   combining genuine coordination functions (safety verification, long-term
 *   outcome tracking) with substantial extraction mechanisms (cost barriers
 *   excluding smaller innovators, timeline delays blocking access to
 *   patients, theatrical compliance that may exceed genuine safety gains).
 *   The FDA clinical trial apparatus—particularly the Phase III
 *   requirement—generates asymmetric costs and benefits. Established
 *   pharmaceutical companies can amortize the $100M+ per-compound R&D cost
 *   across large future patient populations and benefit from patent
 *   protection. Treatment-seeking patients face indefinite suffering if they
 *   cannot access therapies during the 5-10 year approval timeline.
 *   Independent researchers and smaller biotech firms face regulatory
 *   barriers that protect incumbent interests. The regulatory agencies and
 *   clinical research infrastructure benefit from the legitimacy and
 *   standardization created by rigorous pathways, but also bear
 *   organizational costs of maintaining the system. Over the interval 0-20
 *   years, extractiveness has increased (0.35→0.58) as the cost and
 *   complexity of Phase III trials have grown, while theater_ratio has also
 *   increased (0.48→0.65), indicating that the performative component of
 *   safety verification is growing relative to the actual safety gains. This
 *   dual drift—increasing both extraction and theater—is diagnostic of a
 *   constraint degrading from rope toward tangled_rope or snare.
 *
 * KEY AGENTS:
 *   - Treatment-seeking patients with treatment-resistant depression: Primary victim (powerless/trapped) — cannot access potentially life-saving therapies during regulatory timeline; no exit options except underground therapies or continued suffering
 *   - Independent psychedelic researchers and small biotech firms: Secondary victim (moderate/constrained) — face high barriers to entry (licensing, equipment, funding) that protect incumbent pharmaceutical interests; benefit from shared infrastructure but cannot easily exit the FDA pathway
 *   - Established pharmaceutical companies (Compass Pathways, Atai Life Sciences, etc.): Primary beneficiary (institutional/arbitrage) — benefit from high regulatory barriers that exclude smaller competitors; can amortize R&D costs across larger patient populations; have arbitrage options (geographic, adjacent markets)
 *   - FDA and clinical research infrastructure (IRBs, trial networks, contract research organizations): Institutional beneficiary (organized/constrained) — benefit from legitimacy and standardization of pathways; constrained by statutory mandates and political pressure; bear organizational costs of maintaining system
 *   - Clinical trial apparatus and safety theater: Institutional actor (institutional/arbitrage) — persists through institutional inertia despite diminishing marginal safety gains; beneficiaries include career advancement in regulatory science, CRO employment, trial infrastructure funding
 *   - Regulatory reform and alternative pathways movement: Organized actor (organized/mobile) — breakthrough designation, Section 505(b)(2) pathways, adaptive trials are building sunset mechanisms that gradually reduce approval cost and timeline
 *   - Underground/unregulated psychedelic therapy networks: Shadow actor (analytical/analytical) — operate outside regulatory system; provide access to patients blocked by FDA timeline; lack quality control and safety monitoring; demonstrate that therapy can work without regulatory overhead
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(regulatory_pathway_psychedelic_therapy, 0.58).
domain_priors:suppression_score(regulatory_pathway_psychedelic_therapy, 0.72).
domain_priors:theater_ratio(regulatory_pathway_psychedelic_therapy, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(regulatory_pathway_psychedelic_therapy, extractiveness, 0.58).
narrative_ontology:constraint_metric(regulatory_pathway_psychedelic_therapy, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(regulatory_pathway_psychedelic_therapy, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(regulatory_pathway_psychedelic_therapy, tangled_rope).
narrative_ontology:human_readable(regulatory_pathway_psychedelic_therapy, "The Regulatory and Clinical Pathway for Novel Psychedelic Therapies").
narrative_ontology:topic_domain(regulatory_pathway_psychedelic_therapy, "technological/political/healthcare").

domain_priors:requires_active_enforcement(regulatory_pathway_psychedelic_therapy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(regulatory_pathway_psychedelic_therapy, established_pharmaceutical_companies).
narrative_ontology:constraint_beneficiary(regulatory_pathway_psychedelic_therapy, regulatory_agencies).
narrative_ontology:constraint_beneficiary(regulatory_pathway_psychedelic_therapy, clinical_research_infrastructure).
narrative_ontology:constraint_victim(regulatory_pathway_psychedelic_therapy, treatment_seeking_patients).
narrative_ontology:constraint_victim(regulatory_pathway_psychedelic_therapy, independent_researchers).
narrative_ontology:constraint_victim(regulatory_pathway_psychedelic_therapy, psychedelic_therapy_innovation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TREATMENT-SEEKING PATIENTS (SNARE) — Trapped by illness and regulatory barriers. Cannot access potentially life-saving therapies during the 5-10 year FDA trial timeline. Exit options: suffer indefinitely, seek unregulated/underground therapies with no quality control, or migrate to jurisdictions with faster pathways. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.80. Maximum extraction for a victim with no alternatives.
constraint_indexing:constraint_classification(regulatory_pathway_psychedelic_therapy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INDEPENDENT RESEARCHERS (TANGLED ROPE) — Constrained by regulatory licensing requirements, equipment access, and funding bottlenecks, but benefit from the shared infrastructure (trial protocols, safety monitoring, literature validation) created by the regulatory system. Cannot easily exit the FDA pathway without losing institutional legitimacy. d≈0.68, f(d)≈1.05, σ=0.9 → χ≈0.55. Mixed coordination (shared protocols enable science) and extraction (barriers protect incumbent interests).
constraint_indexing:constraint_classification(regulatory_pathway_psychedelic_therapy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ESTABLISHED PHARMACEUTICAL COMPANIES (ROPE) — Benefit from high regulatory barriers that exclude smaller competitors. Can amortize R&D costs across larger patient bases and coordinate with regulators through established relationships. Experience the constraint as coordination: the FDA pathway legitimizes their compounds and creates defensible IP/patent positions. d≈0.12, f(d)≈0.08, σ=1.2 → χ≈0.05. Net beneficiary; low effective extraction because they have arbitrage options (can shift programs geographically or into adjacent markets).
constraint_indexing:constraint_classification(regulatory_pathway_psychedelic_therapy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY AGENCIES AND CLINICAL INFRASTRUCTURE (TANGLED ROPE) — Organized institutions (FDA, IRBs, clinical trial networks) benefit from the legitimacy and coordination function of standardized pathways, but also bear costs of maintaining the system (staff, oversight, liability insurance). Constrained by statutory mandates and political pressure. d≈0.48, f(d)≈0.65, σ=1.0 → χ≈0.38. Moderate extraction because coordination function (safety standards) is real, but extraction component (protecting incumbent interests, slowing innovation) is also measurable.
constraint_indexing:constraint_classification(regulatory_pathway_psychedelic_therapy, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CLINICAL TRIAL APPARATUS (PITON) — Performative compliance with safety protocols has become partly detached from actual safety gains. Phase III trial infrastructure requires $100M+ and 3-5 years, but marginal safety information from later phases is diminishing. The apparatus persists through institutional inertia (reputation, career paths, accreditation standards) even as newer tools (real-world evidence, adaptive trial designs, biomarkers) could deliver equivalent safety certification faster. theater_ratio=0.65 indicates substantial but not dominant performative content. d≈0.25, f(d)≈0.15, σ=1.0 → χ≈0.10.
constraint_indexing:constraint_classification(regulatory_pathway_psychedelic_therapy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: REGULATORY REFORM AND ALTERNATIVES (SCAFFOLD) — Organized actors (FDA breakthrough designation, Section 505(b)(2) pathways, international regulatory reciprocity, adaptive trial networks) are building sunset mechanisms that gradually reduce the absolute cost and timeline of regulatory approval. Breakthrough designation (10 recent psychedelic approvals in IND stage) creates expedited pathways for serious conditions. d≈0.35, f(d)≈0.32, σ=1.1 → χ≈0.21. Low effective extraction because these actors see a concrete exit strategy and are actively implementing it.
constraint_indexing:constraint_classification(regulatory_pathway_psychedelic_therapy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (APPARENT MOUNTAIN) — From a civilizational view, the regulatory constraint appears immutable: any novel psychoactive compound requires rigorous safety testing before mass human exposure. This naturalizes the constraint as inherent to responsible drug development. However, the structural data (ε=0.58, suppression=0.72, theater=0.65, beneficiaries present, victims present) contradict mountain classification. This is a false summit: the specific FDA trial timeline and cost structure are policy choices, not laws of nature. Adaptive trials, real-world evidence, and biomarkers could achieve equivalent safety certification faster.
constraint_indexing:constraint_classification(regulatory_pathway_psychedelic_therapy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regulatory_pathway_psychedelic_therapy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(regulatory_pathway_psychedelic_therapy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(regulatory_pathway_psychedelic_therapy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(regulatory_pathway_psychedelic_therapy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(regulatory_pathway_psychedelic_therapy, TR),
    TR >= 0.70.

:- end_tests(regulatory_pathway_psychedelic_therapy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Substantial. The regulatory pathway imposes $100M+ costs per compound, 5-10 year timelines, and high failure rates (>90% of compounds do not reach approval). These costs are borne primarily by treatment-seeking patients (who wait indefinitely or access underground therapies), independent researchers (who cannot enter the market), and smaller biotech firms (who cannot compete with large pharma). Established pharmaceutical companies experience this as manageable coordination overhead that protects their investments—the constraint extracts from the powerless and weak, not from the powerful. Suppression (0.72): High. Alternative pathways for demonstrating safety exist (adaptive trials, real-world evidence, biomarkers) but are systematically deprioritized. Patients seeking faster access face legal barriers (Schedule I status in US), institutional barriers (lack of IND pathway clarity for smaller firms), and career barriers (researchers risk reputation for pursuing alternatives). Theater ratio (0.65): Moderate-high. Phase III trial design for psychedelics involves standardized outcome measures (QIDS, CGI) that have been refined for decades, but the psychedelic-specific mechanism of action and the role of set/setting/therapeutic context make traditional RCT theater partially misaligned with actual treatment mechanisms. Adaptive trial designs and real-world evidence could capture these non-traditional mechanisms more accurately. The current apparatus requires expensive, lengthy trials partly because the measurement infrastructure is optimized for small-molecule pharmacology, not for complex psychobiological interventions.
 *
 * PERSPECTIVAL GAP:
 *   The regulatory constraint produces maximum perspectival divergence. Treatment-seeking patients see a pure snare (no coordination benefit, maximum extraction). Established pharma see a rope (coordination function + manageable overhead + protection from competition). Independent researchers see tangled rope (genuine safety coordination + extraction barriers). Regulatory agencies see themselves as rope maintainers (coordination + legitimate costs) but are actually intermediate beneficiaries in a tangled rope structure (extraction flows from patients and small innovators upward to large pharma). The reform movement sees a degrading scaffold (sunset is achievable through alternative pathways). The safety theater sees itself as a piton—necessary ritual maintained by institutional inertia. The analytical observer risks a false mountain (naturalizing 'rigorous testing is inherent to drug development') but the structural data reveals this as a policy choice: the specific FDA pathway costs, timeline, and theater level are contingent design features, not laws of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Treatment-seeking patients: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction; patient is fully targeted and has no exit. Established pharma: Beneficiary + arbitrage → d≈0.12, f(d)≈0.08. Net beneficiary; protective effect of regulatory barriers against competition is arbitrage function (they can move investment to jurisdictions with faster pathways or exit psychedelics entirely for other therapeutic areas). Independent researchers: Victim + constrained → d≈0.68, f(d)≈1.05. Significant extraction; cannot easily exit FDA ecosystem without losing institutional credibility, but some arbitrage options exist (international trials, off-label investigator-initiated research, underground research). Regulatory agencies: Intermediate beneficiary + constrained → d≈0.48, f(d)≈0.65. Neither pure beneficiary nor victim; agencies are constrained by mandate and political pressure but benefit from authority legitimation. Reform movement: Organized + mobile → d≈0.35, f(d)≈0.32. Low extraction because organized actors can see and pursue alternative pathways (breakthrough designation, 505(b)(2)). Underground networks: Analytical/shadow → d≈0.88, f(d)≈1.32. High extraction from regulatory suppression perspective, but low f(d) dampening because they operate outside the formal system (scope is local/unorganized).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RISK (extractiveness 0.58, below 0.70 threshold but asymptotically approaching it): The constraint exhibits both genuine coordination functions (safety verification, long-term outcome tracking, prevention of uncontrolled psychedelic proliferation) AND substantial extraction (cost barriers, timeline delays, protection of incumbent interests, theater drift). The mandatrophy is whether this hybrid is an inherently necessary coordination-extraction mix (tangled rope is the natural equilibrium state) or whether the extraction component is parasitic overlay (extraction could be reduced without compromising coordination). The empirical tests are: (1) Can adaptive trials and real-world evidence achieve equivalent safety certification faster? If yes, current pathway is over-extracted. (2) Are Phase III trials providing diminishing marginal safety information? If yes, continued Phase III requirement is theater + extraction. (3) Are underground therapy outcomes comparable to regulated outcomes? If yes, regulatory extraction exceeds genuine coordination value. (4) Is FDA approval biased toward incumbent pharma interests? If yes, the constraint is regulatory capture (snare wearing rope's clothing). The current JSON does not resolve mandatrophy because the empirical answers to these questions are contested and evolving. Breakthrough designation and adaptive trial adoption suggest the extraction component is being actively reduced—the constraint may be shifting from tangled rope toward rope over the next 10 years. This is a LIVE constraint, not a RESOLVED one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    efficacy_plateau_timeline,
    'At what point does additional Phase III data provide negligible incremental safety/efficacy information for psychedelic treatments of depression?',
    'Meta-analysis of Phase II vs Phase III effect size changes for completed psychedelic trials; cost-benefit analysis of marginal information gain per trial subject and dollar spent',
    'If plateau occurs at current Phase II stage: regulatory pathway is purely extractive (snare). If Phase III adds substantial information: regulatory pathway is partially coordination (tangled rope or rope). If Phase III is near-useless but persisted due to institutional inertia: piton classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficacy_plateau_timeline, empirical, 'Efficacy/safety information plateau point in psychedelic trial sequences').

omega_variable(
    alternative_pathway_sufficiency,
    'Do adaptive trial designs, real-world evidence registries, and biomarker-driven endpoints provide equivalent safety certification as traditional Phase III for psychedelics?',
    'Comparative effectiveness analysis of traditional trials vs adaptive/real-world pathways; post-market adverse event rates for compounds approved via each pathway; long-term outcome tracking',
    'If equivalent: scaffold perspective confirmed — alternative pathways are real, cost/timeline can be reduced, sunset is achievable. If traditional trials are necessary: current pathway is minimally extractive (rope becomes dominant). If alternatives are superior: current pathway is pure extraction and theater (snare + piton confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_pathway_sufficiency, empirical, 'Equivalence of alternative regulatory pathways for psychedelic safety certification').

omega_variable(
    intellectual_property_lock_in,
    'To what extent does the FDA pathway protect pharmaceutical company IP and create barriers to generic/open-source psychedelic production?',
    'Patent landscape analysis; comparison of approval timelines for branded vs generic psychedelics; tracking of licensing agreements and royalty structures; analysis of institutional support for off-patent compound development',
    'If IP lock-in is substantial: regulatory pathway serves as de facto monopoly-preservation mechanism. If minimal: coordination function dominates. If high: victims include generic therapy developers and patients in low-resource settings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intellectual_property_lock_in, empirical, 'IP protection and monopoly dynamics within FDA regulatory pathway').

omega_variable(
    underground_therapy_network_efficacy,
    'What are the actual safety and efficacy outcomes of underground/unregulated psychedelic therapy networks compared to regulated trials?',
    'Longitudinal tracking of underground therapy outcomes (where data collection is possible); adverse event rate comparison; efficacy measurement via self-report and biomarkers where accessible; analysis of selection effects',
    'If underground outcomes are comparable: regulatory barrier appears primarily extractive and theatrical. If significantly worse: regulatory pathway demonstrates genuine coordination value. If mixed (some outcomes better, some worse): tangled rope classification confirmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(underground_therapy_network_efficacy, empirical, 'Safety/efficacy comparison between regulated and underground psychedelic therapies').

omega_variable(
    regulatory_agency_capture,
    'Are FDA guidance documents and approval criteria systematically biased toward incumbent pharmaceutical interests?',
    'Content analysis of FDA guidance; statistical analysis of approval rates by company size and prior relationships; tracking of revolving-door employment between FDA and pharma; analysis of scientific basis vs regulatory history for specific criteria',
    'If captured: regulatory pathway is snare + rent-seeking (high-extractiveness reading confirmed). If independent: regulatory pathway is more rope-like (coordination function genuine). If mixed: tangled rope with directionality toward extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_agency_capture, empirical, 'FDA institutional capture by pharmaceutical incumbents').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(regulatory_pathway_psychedelic_therapy, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(psych_reg_tr_t0, regulatory_pathway_psychedelic_therapy, theater_ratio, 0, 0.48).
narrative_ontology:measurement(psych_reg_tr_t10, regulatory_pathway_psychedelic_therapy, theater_ratio, 10, 0.58).
narrative_ontology:measurement(psych_reg_tr_t20, regulatory_pathway_psychedelic_therapy, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(psych_reg_be_t0, regulatory_pathway_psychedelic_therapy, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(psych_reg_be_t10, regulatory_pathway_psychedelic_therapy, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(psych_reg_be_t20, regulatory_pathway_psychedelic_therapy, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(regulatory_pathway_psychedelic_therapy, enforcement_mechanism).
narrative_ontology:affects_constraint(regulatory_pathway_psychedelic_therapy, psychedelic_therapy_access_inequality).
narrative_ontology:affects_constraint(regulatory_pathway_psychedelic_therapy, therapeutic_research_innovation_bottleneck).
narrative_ontology:affects_constraint(regulatory_pathway_psychedelic_therapy, underground_vs_regulated_therapy_duality).

% DUAL FORMULATION NOTE:
% The regulatory pathway constraint decomposes into multiple structurally distinct sub-constraints: (1) the safety verification requirement (ε≈0.20, mostly coordination, some theater), (2) the cost/timeline barrier (ε≈0.65, mostly extraction), and (3) the institutional inertia in the clinical trial apparatus (ε≈0.72, mostly theater/piton). These are linked: the safety verification justifies the cost/timeline barrier, and institutional inertia slows reform of the apparatus. In this story, they are unified under a single ε=0.58 because the policy question is about the entire FDA pathway as an integrated constraint. If more granular analysis is needed, decompose into three separate stories linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(regulatory_pathway_psychedelic_therapy, institutional, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
