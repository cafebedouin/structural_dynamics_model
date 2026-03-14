% ============================================================================
% CONSTRAINT STORY: intellectual_community_fragmentation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_intellectual_community_fragmentation, []).

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
 *   constraint_id: intellectual_community_fragmentation
 *   human_readable: Intellectual Community Fragmentation
 *   domain: epistemology/institutional
 *
 * SUMMARY:
 *   Intellectual community fragmentation emerges from the tension between the
 *   coordination benefits of disciplinary specialization and the extractive
 *   mechanisms through which specialized domains maintain control over their
 *   epistemic commons. Specialization initially coordinates knowledge
 *   production through shared standards, methods, and literatures — a genuine
 *   public good. Over time, however, disciplinary boundaries become enforced
 *   through institutional mechanisms (publication venues, hiring criteria,
 *   funding silos) that extract value from would-be integrators while
 *   restricting the emergence of synthetic understanding. The constraint
 *   operates across all domains but manifests most severely at knowledge
 *   frontiers where problems exceed single-disciplinary scope. This story
 *   exhibits all six DR types from different structural positions, making it
 *   a diagnostic exemplar for how institutional coordination transforms into
 *   extraction.
 *
 * KEY AGENTS:
 *   - Early Career Researchers: Primary victims (powerless/trapped) — face career penalties for cross-disciplinary work; must choose specialization to advance
 *   - Integrative Knowledge Production: Structural victim (powerless/trapped) — complex problems requiring synthesis remain unsolved due to fragmentation barriers
 *   - Disciplinary Gatekeepers: Primary beneficiaries (institutional/arbitrage) — control publication venues, citation networks, hiring decisions; benefit from specialization enforcement
 *   - Interdisciplinary Research Initiatives: Organized agents (organized/constrained) — coordinate integration efforts; also face extraction through resource scarcity and prestige asymmetries
 *   - Journal Review System: Institutional actor (institutional/arbitrage) — maintains performative review for cross-disciplinary work; preserves fragmentation through rejection of 'unrigorous' synthesis
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing specialization efficiency as immutable law of knowledge organization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(intellectual_community_fragmentation, 0.58).
domain_priors:suppression_score(intellectual_community_fragmentation, 0.52).
domain_priors:theater_ratio(intellectual_community_fragmentation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(intellectual_community_fragmentation, extractiveness, 0.58).
narrative_ontology:constraint_metric(intellectual_community_fragmentation, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(intellectual_community_fragmentation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(intellectual_community_fragmentation, tangled_rope).
narrative_ontology:human_readable(intellectual_community_fragmentation, "Intellectual Community Fragmentation").
narrative_ontology:topic_domain(intellectual_community_fragmentation, "epistemology/institutional").

domain_priors:requires_active_enforcement(intellectual_community_fragmentation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(intellectual_community_fragmentation, disciplinary_gatekeepers).
narrative_ontology:constraint_beneficiary(intellectual_community_fragmentation, specialized_subdomain_leaders).
narrative_ontology:constraint_victim(intellectual_community_fragmentation, cross_disciplinary_synthesis).
narrative_ontology:constraint_victim(intellectual_community_fragmentation, early_career_researchers).
narrative_ontology:constraint_victim(intellectual_community_fragmentation, integrative_knowledge).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EARLY CAREER RESEARCHER (SNARE) — Trapped within disciplinary silos enforced through funding mechanisms, publication venues, and hiring criteria. Attempts at cross-disciplinary work face rejection from both parent discipline and target discipline. Career advancement requires specialization; generalism is penalized. No exit without abandoning credentials.
constraint_indexing:constraint_classification(intellectual_community_fragmentation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INTEGRATIVE KNOWLEDGE (SNARE) — Systemic fragmentation prevents emergence of synthetic understanding. Complex problems requiring cross-domain integration remain unsolved. The constraint maintains disciplinary extraction at the cost of collective epistemic capability. No mechanism for coordinating knowledge across specializations.
constraint_indexing:constraint_classification(intellectual_community_fragmentation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: DISCIPLINARY GATEKEEPER (ROPE) — Benefits from fragmentation through control of publication venues, citation networks, and hiring decisions within their domain. Fragmentation appears as legitimate specialization and quality control. The constraint enables extraction framed as coordination of standards.
constraint_indexing:constraint_classification(intellectual_community_fragmentation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERDISCIPLINARY INITIATIVES (TANGLED ROPE) — Organized groups (centers, institutes, journals) attempt to bridge silos and enable synthesis. They coordinate cross-domain work AND face extraction through resource scarcity, prestige asymmetries, and institutional resistance from disciplinary gatekeepers. Some agency and some benefit, but significant constraints.
constraint_indexing:constraint_classification(intellectual_community_fragmentation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: JOURNAL REVIEW SYSTEM (PITON) — Peer review for cross-disciplinary work is largely theater: reviewers from each parent discipline find the other half of the work unrigorous or unfamiliar. Rejection often preserves fragmentation rather than enforcing standards. Theater ratio high because the review ritual persists despite low diagnostic utility for integrative work.
constraint_indexing:constraint_classification(intellectual_community_fragmentation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, specialization is inherent to complex knowledge production: no human can master all domains, and expertise requires depth. Fragmentation appears immutable — a structural property of knowledge organization. However, this naturalizes what is actually contingent institutional structure (funding mechanisms, publication venues, hiring criteria). The engine will flag this as a false summit.
constraint_indexing:constraint_classification(intellectual_community_fragmentation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(intellectual_community_fragmentation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(intellectual_community_fragmentation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(intellectual_community_fragmentation, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(intellectual_community_fragmentation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(intellectual_community_fragmentation, TR),
    TR >= 0.70.

:- end_tests(intellectual_community_fragmentation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Fragmentation prevents integrative knowledge production and penalizes early-career synthesis attempts. Career advancement requires specialization; generalism is economically irrational under current incentives. The extractiveness has grown over the interval as specialization barriers have hardened and interdisciplinary work has become more costly. Suppression (0.52): Moderate-high. Barriers include funding mechanisms segregated by discipline, publication venues that privilege domain-internal work, hiring criteria that reward specialization, and the tacit knowledge required to cross domains. But suppression is not total — interdisciplinary initiatives exist, and some researchers successfully integrate. Theater ratio (0.68): High and rising. Journal review for cross-disciplinary work is substantially performative — reviewers from each domain find the other domain's standards unfamiliar, leading to rejection that preserves fragmentation rather than enforcing rigor. The theater has increased over the interval as methodological diversity has expanded and cross-domain review has become less coherent.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows how the same structural phenomenon — disciplinary specialization — classifies as coordination (Rope) from the gatekeeper's perspective, as extraction (Snare) from the early-career researcher's perspective, as a temporary problem to be solved (Scaffold) from the interdisciplinary initiative's perspective, as degraded theater (Piton) from the journal review system's perspective, as mixed coordination-extraction (Tangled Rope) from the organized coalition's perspective, and as an immutable natural law (Mountain, false summit) from the analytical observer's perspective. No single classification is 'correct' — the presheaf over multiple structural positions reveals both the coordination function (genuine benefits of specialization) and the extraction mechanism (gatekeeping barriers that prevent integration).
 *
 * DIRECTIONALITY LOGIC:
 *   Disciplinary gatekeepers experience low effective extraction (low d) through beneficiary status and arbitrage options — they control venues and can exit the constraint by creating new specialized niches. Early-career researchers experience high extraction (high d) through trapped exit and victim status — they must accept specialization requirements or abandon career prospects. Integrative initiatives experience moderate extraction (moderate d) — they have agency and can exit (transition to organized coalition activity), but face resource constraints and institutional resistance. The piton classification derives from the theater gate: journal review for interdisciplinary work is performative, and the review ritual persists through institutional inertia despite low diagnostic utility. The mountain classification at the analytical context reflects the risk that specialization efficiency naturalizes what is actually a contingent institutional arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolves through perspectival decomposition. Disciplinary specialization genuinely coordinates knowledge production (beneficiary perspective sees Rope). But institutional enforcement of specialization extracts value by preventing integration (victim perspective sees Snare). Interdisciplinary initiatives attempt to build integration pathways but face institutional resistance (organized perspective sees Tangled Rope). The analytical observer risks naturalizing contingent institutional structures as immutable properties of knowledge organization (false mountain). The constraint is real and consequential, but the 'immutability' framing that justifies it is a false summit. The extractive mechanisms are institutional and could be restructured through funding reform, hiring criterion changes, and publication venue diversification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    integration_mechanism_sufficiency,
    'Can existing interdisciplinary structures (centers, journals, funding programs) overcome fragmentation, or is the constraint structural to the incentive system itself?',
    'Longitudinal tracking of cross-disciplinary citation flows, hiring patterns for interdisciplinary researchers, and research output coherence over 10-20 years',
    'If mechanisms are sufficient: constraint is Scaffold with sunset as integration succeeds. If structural: constraint is Snare, and integration efforts are theater masking persistent extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(integration_mechanism_sufficiency, empirical, 'Whether interdisciplinary mechanisms can overcome structural fragmentation').

omega_variable(
    specialization_efficiency_tradeoff,
    'What portion of fragmentation''s costs (lost synthesis, delayed integration) is genuine specialization efficiency versus extractive gatekeeping?',
    'Comparison of integration timelines and quality with and without specialization barriers; analysis of problems unsolved due to fragmentation',
    'If efficiency gains are substantial: fragmentation is Rope with legitimate extraction costs. If efficiency gains are marginal: fragmentation is Snare masquerading as efficiency.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(specialization_efficiency_tradeoff, conceptual, 'Degree to which fragmentation reflects genuine specialization efficiency').

omega_variable(
    emergence_thresholds,
    'What fraction of the intellectual community must actively integrate before system-level synthesis becomes possible?',
    'Network analysis of when cross-disciplinary citation density reaches critical mass; empirical observation of breakthroughs coinciding with integration threshold crossing',
    'If threshold is low (<20%): scaffolding efforts show rapid returns and constraint softens. If threshold is high (>50%): coalition must grow substantially; extraction persists longer.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(emergence_thresholds, empirical, 'Critical mass threshold for emergent integration').

omega_variable(
    institutional_lock_in,
    'Is fragmentation maintained primarily by active enforcement (gatekeeping) or by inertia in institutional structures (hiring, funding, career paths)?',
    'Policy experiments removing specific barriers (funding for interdisciplinary work, blinded hiring for methodological diversity); observation of whether removal creates integration or maintains fragmentation',
    'If active enforcement: removing barriers shows rapid integration (constraint softens). If inertia: removal has slow or no effect (constraint is structural).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_lock_in, empirical, 'Active enforcement versus institutional inertia in maintaining fragmentation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(intellectual_community_fragmentation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(icf_tr_t0, intellectual_community_fragmentation, theater_ratio, 0, 0.52).
narrative_ontology:measurement(icf_tr_t10, intellectual_community_fragmentation, theater_ratio, 10, 0.6).
narrative_ontology:measurement(icf_tr_t20, intellectual_community_fragmentation, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(icf_be_t0, intellectual_community_fragmentation, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(icf_be_t10, intellectual_community_fragmentation, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(icf_be_t20, intellectual_community_fragmentation, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(intellectual_community_fragmentation, information_standard).
narrative_ontology:affects_constraint(intellectual_community_fragmentation, disciplinary_gatekeeping).
narrative_ontology:affects_constraint(intellectual_community_fragmentation, career_specialization_lock).
narrative_ontology:affects_constraint(intellectual_community_fragmentation, publication_venue_fragmentation).

% DUAL FORMULATION NOTE:
% Intellectual community fragmentation is the macro-level constraint; it decomposes into domain-specific mechanisms including disciplinary gatekeeping (epistemology/methodology enforcement), career specialization lock (early-career incentive structure), and publication venue fragmentation (journal specialization). Each has its own extractiveness value reflecting domain-specific barriers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(intellectual_community_fragmentation, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
