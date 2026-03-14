% ============================================================================
% CONSTRAINT STORY: catalysis_funding_concentration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catalysis_funding_concentration, []).

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
 *   constraint_id: catalysis_funding_concentration
 *   human_readable: Catalysis Research Funding Concentration
 *   domain: research_funding/materials_science
 *
 * SUMMARY:
 *   Catalysis research funding has become increasingly concentrated among a
 *   small number of well-established groups at wealthy institutions over the
 *   past two decades. This constraint exhibits a classic tangled_rope
 *   structure: genuine coordination function (merit-based allocation, quality
 *   assurance through peer review) coexists with asymmetric extraction
 *   (early-career researchers and underfunded institutions bear
 *   disproportionate costs). The rising theater_ratio (0.35 → 0.51) reflects
 *   growing performativity in the assessment mechanisms — citation metrics,
 *   impact factors, and grant scoring systems were introduced as coordination
 *   tools but now function partially as theater that legitimates pre-existing
 *   concentration patterns rather than evaluating merit. The constraint
 *   requires active enforcement through funding agency criteria that
 *   privilege established track records, institutional prestige, and citation
 *   counts.
 *
 * KEY AGENTS:
 *   - Early-career researchers without institutional support: Primary victims (powerless/trapped) — face structural barriers to initial funding; career viability depends on luck of initial placement
 *   - Underfunded institutions: Secondary victims (moderate/constrained) — limited startup resources and equipment, but can participate through collaborations and occasional pilot grants
 *   - Established research groups: Primary beneficiaries (institutional/arbitrage) — designed-for-them system with high agency and low extraction cost; can initiate collaborations and navigate funding landscape
 *   - Funding agencies: Institutional enforcer (institutional/constrained) — embed the concentration through program officer decisions and merit criteria; face pressure to maintain the status quo despite recognizing problems
 *   - Citation and impact metrics infrastructure: Piton mechanism (institutional/arbitrage) — legitimate coordination tool that has degraded into performative theater maintaining concentration
 *   - Open science and alternative funding coalition: Organized challenger (organized/mobile) — building decentralized funding pathways and alternative credibility mechanisms with potential sunset trajectory
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catalysis_funding_concentration, 0.58).
domain_priors:suppression_score(catalysis_funding_concentration, 0.62).
domain_priors:theater_ratio(catalysis_funding_concentration, 0.51).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catalysis_funding_concentration, extractiveness, 0.58).
narrative_ontology:constraint_metric(catalysis_funding_concentration, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(catalysis_funding_concentration, theater_ratio, 0.51).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catalysis_funding_concentration, tangled_rope).
narrative_ontology:human_readable(catalysis_funding_concentration, "Catalysis Research Funding Concentration").
narrative_ontology:topic_domain(catalysis_funding_concentration, "research_funding/materials_science").

domain_priors:requires_active_enforcement(catalysis_funding_concentration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catalysis_funding_concentration, established_research_groups).
narrative_ontology:constraint_beneficiary(catalysis_funding_concentration, wealthy_institutions).
narrative_ontology:constraint_victim(catalysis_funding_concentration, early_career_researchers).
narrative_ontology:constraint_victim(catalysis_funding_concentration, underfunded_institutions).
narrative_ontology:constraint_victim(catalysis_funding_concentration, catalysis_field_diversity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EARLY-CAREER RESEARCHER (SNARE) — Faces structural barriers: must publish in high-impact journals to be fundable, but funding agencies prioritize groups with established track records. Cannot exit without abandoning the field. Maximum extraction: career trajectory depends on initial luck of institutional placement.
constraint_indexing:constraint_classification(catalysis_funding_concentration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: UNDERFUNDED INSTITUTION RESEARCHER (TANGLED ROPE) — Constrained by limited startup funds and equipment, but benefits from collaboration networks and occasional pilot grants. Faces real but surmountable barriers. Genuine coordination function exists (multi-institution collaborations) alongside asymmetric extraction (unequal resource access).
constraint_indexing:constraint_classification(catalysis_funding_concentration, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ESTABLISHED GROUP (ROPE) — Experiences funding system as coordination mechanism. Can initiate collaborations, attract postdocs, and navigate funding landscape. Net beneficiary with low extraction cost — the system was designed to work for them.
constraint_indexing:constraint_classification(catalysis_funding_concentration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FUNDING AGENCY (TANGLED ROPE) — Institutionally embedded in a system that creates genuine coordination (peer review, merit assessment) alongside systematic extraction (bias toward established groups, incumbent protection). Requires_active_enforcement: true because program officers actively maintain review criteria that advantage concentrated funding. Constrained exit because changing the system requires systemic reform.
constraint_indexing:constraint_classification(catalysis_funding_concentration, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CITATION METRICS (PITON) — Journal impact factors and citation counts were introduced as coordination tools to assess research quality, but now function as theater maintaining funding concentration. Theater_ratio (0.51) reflects partial degradation: citations correlate with visibility (a real signal) but are gamed through citation cartels, review bias, and publisher manipulation. The assessment persists through institutional inertia despite known gaming.
constraint_indexing:constraint_classification(catalysis_funding_concentration, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: OPEN SCIENCE COALITION (SCAFFOLD) — Organized agents (preprint archives, open peer review platforms, blockchain funding mechanisms, decentralized autonomous organizations) are building alternative funding pathways that bypass traditional concentration mechanisms. Has_sunset_clause implicit: as open-science norms mature and alternative funding sources (crowdfunding, philanthropic networks, institutional repositories) establish credibility, traditional funding concentration loses grip. Sunset estimated at 15-25 years.
constraint_indexing:constraint_classification(catalysis_funding_concentration, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From the civilizational/universal view, funding concentration appears inevitable: limited resources must be allocated somehow, and concentration on proven researchers is an efficient heuristic. Peer review and merit assessment appear as immutable features of how science distributes resources. However, the structural data contradicts this — the concentration is maintained by choice (active enforcement), not by natural law. Engine will compute false summit, revealing naturalization of contingent institutional arrangements.
constraint_indexing:constraint_classification(catalysis_funding_concentration, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catalysis_funding_concentration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(catalysis_funding_concentration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(catalysis_funding_concentration, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(catalysis_funding_concentration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(catalysis_funding_concentration, TR),
    TR >= 0.70.

:- end_tests(catalysis_funding_concentration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The funding system concentrates resources on established groups while early-career researchers face systematic disadvantage. However, the extraction is not maximal (0.66+) because some distribution mechanisms exist (program-specific funding, international collaborations, industrial sponsorship) and because some early-career researchers do successfully navigate the system. The trend from 0.35 to 0.58 over 20 years shows progressive concentration. Suppression (0.62): Moderate-high. Multiple barriers exist: limited startup funding availability for new groups, publication bias (negative results less likely published), career risk of pursuing risky directions (funding agencies favor established trajectories), and institutional barriers (equipment access, computational resources, networking). Suppressiveness reflects structural constraints on entry. Theater ratio (0.51): Moderate. Citation metrics and impact factors were genuine coordination tools but now serve partly to rationalize existing concentration — high-citation research is more visible (real signal) but also more likely to be by established groups (systematic bias). Gaming of metrics through citation cartels and review bias adds performative content. The gradual increase reflects accumulated metric manipulation over the interval.
 *
 * PERSPECTIVAL GAP:
 *   Gap between beneficiary perception (Rope — coordination system) and victim perception (Snare — pure extraction) is maximal. The beneficiary and victim are experiencing structurally opposite phenomena from the same constraint. Funding agency perspective (Tangled Rope) sits between these: they recognize coordination function (merit assessment) but also enforce extraction (incumbent protection through criteria). The gap reveals that what the beneficiary calls 'coordination' the victim calls 'gatekeeping.' The analytical observer's false-summit mountain classification is the largest structural mischaracterization — naturalization of a contingent institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position relative to the funding concentration flow. Established groups (beneficiaries + arbitrage options) experience d ≈ 0.15 → low/negative effective extraction. Early-career researchers (victims + trapped) experience d ≈ 0.95 → maximum extraction. Underfunded institutions (victims + constrained) experience d ≈ 0.75 → high extraction. Funding agencies (institutional + constrained, conflicted role) experience d ≈ 0.55 → symmetric extraction/coordination mix. The piton classification derives from theater_ratio ≥ 0.70 threshold (currently 0.51, not piton-gate) and from the degradation of citation tools into performative mechanisms. The scaffold classification for alternative funding reflects that organized agents have exit options and are building sunset mechanisms.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolves through recognizing that funding concentration serves genuine coordination (assessing research quality, distributing limited resources) AND achieves extractive outcome (concentrating advantage on already-advantaged groups). The tangled_rope classification is diagnostically correct: both the coordination function (peer review, merit assessment) and the extraction mechanism (career risk, startup barriers, citation gaming) are structurally real. Calling this pure coordination (rope) would dismiss the real barriers early-career researchers face. Calling it pure extraction (snare) would dismiss the genuine quality-assurance function of peer review. The tangled_rope classification is the only type that captures both simultaneously. The resolution is structural: the coordination and extraction mechanisms are mechanically entangled. Separating them would require decoupling merit assessment from resource allocation — the coordination function could be preserved while extraction is reduced through alternative funding distribution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    merit_assessment_circularity,
    'Does funding concentrate on established groups because they are more capable, or do they appear more capable because they have had more funding opportunities?',
    'Longitudinal tracking of early-career researcher success rates controlling for initial funding level; comparison of impact factors for equally-capable researchers with different funding histories; randomized funding allocation pilot studies',
    'If causation is purely merit-based: concentration is functional (rope). If funding-driven: concentration is extractive (snare/tangled_rope). If bidirectional: complex feedback loop requiring structural intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(merit_assessment_circularity, empirical, 'Causality between funding concentration and measured merit').

omega_variable(
    alternative_funding_credibility,
    'Can decentralized or non-traditional funding sources (crowdfunding, philanthropic networks, open peer review) establish sufficient credibility to displace traditional peer review as the legitimacy gatekeeper?',
    'Tracking adoption rates of alternative funding models; longitudinal career outcomes for researchers funded via non-traditional sources; institutional acceptance of alternative credentials in hiring and promotion',
    'If alternative sources gain credibility: scaffold sunset becomes real (extractiveness decreases over time). If traditional gatekeeping persists: concentration mechanism is structurally entrenched (snare classification dominant).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_funding_credibility, empirical, 'Whether alternative funding sources can displace traditional peer review legitimacy').

omega_variable(
    coordination_vs_extraction_boundary,
    'What proportion of funding concentration reflects necessary coordination (directing resources to proven capabilities) versus extractive rent-seeking (incumbent protection)?',
    'Analysis of funding decisions for comparable proposals from established vs emerging groups; tracking of success rates for high-risk breakthrough research in concentrated vs distributed funding models; historical case studies of paradigm shifts funded under different concentration regimes',
    'High coordination component: rope/scaffold classification. High extraction component: snare/piton classification. Mixed: tangled_rope is correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, conceptual, 'Boundary between coordination function and extractive concentration').

omega_variable(
    suppression_mechanism_internalization,
    'Is suppression primarily structural (external barriers to funding access) or internalized (self-selection out by early-career researchers who perceive the system as closed)?',
    'Surveys of PhD students and postdocs on perceived barriers; tracking of field exit rates correlated with funding availability; comparison of persistence rates between researchers with early success vs those facing repeated rejection',
    'If primarily structural: constraints on exit are external; if internalized: agents carry the suppression with them even after barriers are removed. Affects post-intervention prediction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs internalized suppression in funding barriers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catalysis_funding_concentration, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cfc_tr_t0, catalysis_funding_concentration, theater_ratio, 0, 0.35).
narrative_ontology:measurement(cfc_tr_t10, catalysis_funding_concentration, theater_ratio, 10, 0.43).
narrative_ontology:measurement(cfc_tr_t20, catalysis_funding_concentration, theater_ratio, 20, 0.51).

% Extraction over time
narrative_ontology:measurement(cfc_be_t0, catalysis_funding_concentration, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cfc_be_t10, catalysis_funding_concentration, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(cfc_be_t20, catalysis_funding_concentration, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catalysis_funding_concentration, resource_allocation).
narrative_ontology:affects_constraint(catalysis_funding_concentration, peer_review_gatekeeping).
narrative_ontology:affects_constraint(catalysis_funding_concentration, publication_bias_negative_results).
narrative_ontology:affects_constraint(catalysis_funding_concentration, early_career_precarity).

% DUAL FORMULATION NOTE:
% Catalysis funding concentration is downstream of broader research funding system dynamics but represents a distinct structural constraint with its own extraction mechanisms. Upstream constraints (peer review gatekeeping, publication bias) feed into this constraint through citation metrics and reputation systems. Downstream constraints (early-career researcher precarity, brain drain from underfunded regions) are amplified by funding concentration.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catalysis_funding_concentration, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
