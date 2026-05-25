% ============================================================================
% CONSTRAINT STORY: helsinki_bus_theory
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_helsinki_bus_theory, []).

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
 *   constraint_id: helsinki_bus_theory
 *   human_readable: The Helsinki Bus Station Theory (Creative Persistence)
 *   domain: social/psychological
 *
 * SUMMARY:
 *   The Helsinki Bus Station Theory (popularized by Anne Lamott and Vera
 *   Nazarian) proposes that creative mastery requires passing through a
 *   mandatory imitation phase. Like waiting at a bus station, the emerging
 *   creator cannot skip the waiting period — they must absorb existing
 *   styles, techniques, and cultural forms before original voice emerges.
 *   This constraint exhibits mixed coordination and extraction
 *   characteristics. From the perspective of established creative
 *   communities, the imitation requirement is a genuine coordination
 *   function: it ensures cultural continuity and transmits embodied craft
 *   knowledge. From the perspective of emerging creators, the same
 *   requirement is experienced as suppressive extraction: years of
 *   psychological and economic sacrifice before autonomy emerges, with
 *   institutional gatekeepers controlling access and validation. The
 *   constraint's extractiveness (0.52) reflects this hybrid nature — there is
 *   real learning value in imitation, but the institutional enforcement
 *   mechanisms (credentialing requirements, mentor gatekeeping, market
 *   barriers against pre-original-voice creators) add extraction beyond what
 *   strict knowledge transmission would require. Theater ratio has increased
 *   from 0.42 to 0.68 over the measurement interval, indicating that the
 *   performative aspects of creative apprenticeship (MFA prestige, residency
 *   network signaling, mentorship rituals) have grown relative to direct
 *   knowledge transmission, especially in domains where digital alternatives
 *   now provide effective skill acquisition outside formal structures.
 *
 * KEY AGENTS:
 *   - Emerging Creators: Primary victims (powerless/trapped) — early-stage artists, writers, musicians, designers facing years of required imitation and suppressed autonomy
 *   - Established Creative Communities: Primary beneficiaries (institutional/arbitrage) — MFA programs, workshops, mentorship networks, galleries, publishers that benefit from the imitation requirement as gatekeeping mechanism
 *   - Mid-Career Creators: Secondary agents (moderate/constrained) — 5-10 year experience creators with partial agency and constrained exit; benefit from imitation foundation but experience continued suppression
 *   - Senior Mentors and Gatekeepers: Secondary beneficiaries (institutional/arbitrage) — established artists with status to credential newcomers; maintain control over recognition and resource allocation
 *   - Digital Platforms and Alternative Pathways: Organized disruptors (organized/constrained) — arXiv equivalents for creative work (self-publishing, crowdfunding, online portfolios) creating competing validation structures
 *   - The Apprenticeship Model: Institutional performer (organized/constrained) — the formalized master-apprentice structure that persists through inertia rather than effectiveness
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(helsinki_bus_theory, 0.52).
domain_priors:suppression_score(helsinki_bus_theory, 0.65).
domain_priors:theater_ratio(helsinki_bus_theory, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(helsinki_bus_theory, extractiveness, 0.52).
narrative_ontology:constraint_metric(helsinki_bus_theory, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(helsinki_bus_theory, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(helsinki_bus_theory, tangled_rope).
narrative_ontology:human_readable(helsinki_bus_theory, "The Helsinki Bus Station Theory (Creative Persistence)").
narrative_ontology:topic_domain(helsinki_bus_theory, "social/psychological").

domain_priors:requires_active_enforcement(helsinki_bus_theory).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(helsinki_bus_theory, established_creative_communities).
narrative_ontology:constraint_beneficiary(helsinki_bus_theory, senior_mentors_and_gatekeepers).
narrative_ontology:constraint_victim(helsinki_bus_theory, early_career_creators).
narrative_ontology:constraint_victim(helsinki_bus_theory, creative_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMERGING CREATOR (SNARE) — The early-stage artist, writer, musician, or designer faces an inescapable requirement: years of copying, studying, and working within established forms before original voice emerges. Exit options are severely constrained — quitting before the imitation phase leads to identity abandonment; continuing through it extracts time, resources, and psychological coherence. The creative powerless bear full cost of the waiting period.
constraint_indexing:constraint_classification(helsinki_bus_theory, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ESTABLISHED CREATIVE COMMUNITIES (ROPE) — Schools, MFAs, workshops, mentorship networks, and peer groups benefit from the imitation requirement: it ensures that new entrants absorb craft, discipline, and cultural continuity before claiming novelty. The established community experiences this as a genuine coordination function — it solves the problem of transmitting embodied knowledge across generations. Net beneficiary position with arbitrage options (can shape how mentorship operates, can select protégés).
constraint_indexing:constraint_classification(helsinki_bus_theory, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: MID-CAREER CREATORS (TANGLED ROPE) — Artists 5-10 years into practice occupy a hybrid position. They benefit from the imitation foundation (craft competence enables originality) but still experience suppression from the continuing expectation of convergence. They have partial agency (can begin to diverge) but constrained exit (market still doesn't recognize them, funding and criticism remain bound to established idioms). Both coordination and extraction present.
constraint_indexing:constraint_classification(helsinki_bus_theory, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ALTERNATIVE CREATIVE PATHWAYS (SCAFFOLD) — Digital platforms, self-publishing, indie labels, crowdfunding, and online portfolios create temporary alternatives to the imitation bottleneck. Emerging creators can now build audiences and test originality before completing the full apprenticeship arc. These pathways bypass some suppression (no gatekeepers blocking preprint/prototype sharing) but remain constrained by market realities. The sunset logic is moderate: as digital alternatives mature, the traditional imitation requirement weakens for some domains (written word, visual art, music production) but persists for others (dance, architecture, craft requiring embodied apprenticeship).
constraint_indexing:constraint_classification(helsinki_bus_theory, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: APPRENTICESHIP MODEL (PITON) — The centuries-old master-apprentice structure persists in creative fields long after its primary transmission function has weakened. MFA programs, artist residencies, and mentorship networks maintain the theatrical performance of guided imitation even when direct knowledge transfer could be automated or peer-learned. The ritual persists through institutional inertia (because it signals quality and legitimacy) rather than because it is the most efficient transmission mechanism. Theater ratio reflects this: the formal mentorship structure performs credentialing and socialization functions that exceed its actual knowledge-transmission role.
constraint_indexing:constraint_classification(helsinki_bus_theory, piton,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational vantage, the requirement that original creative voice emerges through a period of imitation appears to be an inescapable cognitive/cultural law: humans internalize patterns before recombining them; culture builds on inherited forms; authentic originality requires embodied mastery of existing forms. This perspective risks naturalizing what may be contingent — the engine's false summit detector should flag whether the apparent inevitability conceals institutional enforcement mechanisms.
constraint_indexing:constraint_classification(helsinki_bus_theory, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(helsinki_bus_theory_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(helsinki_bus_theory, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(helsinki_bus_theory, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(helsinki_bus_theory, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(helsinki_bus_theory, TR),
    TR >= 0.70.

:- end_tests(helsinki_bus_theory_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The imitation requirement contains genuine learning value (not pure extraction), but institutional enforcement mechanisms add significant cost beyond knowledge transmission. Time cost: 5-10 years of below-market-rate or unpaid work is typical. Psychological cost: suppressed autonomy and identity during the imitation phase. Economic cost: credential requirements (MFA tuition), geographic concentration (moving to creative hubs), gatekeeping access. But the value is also real — imitation does develop craft competence. The 0.52 value reflects both genuine learning and institutional rent-seeking. Suppression (0.65): High. Multiple barriers prevent exit: quitting before originality recognition carries identity and social cost; continuing requires economic sacrifice; gatekeepers control access to validation; market discrimination against pre-original-voice work; geographic and financial barriers to accessing mentorship networks. Suppression is structural, not total — some creators do exit through self-publishing, but the barriers are significant. Theater ratio (0.58): Moderate-high, rising. MFA programs perform credentialing and socialization functions that exceed their direct knowledge-transmission role. Artist residencies create prestige through scarcity rather than unique learning. Mentorship rituals (studio visits, critique sessions) have become semi-performative — the social validation matters as much as the actual feedback. Theater ratio increase over the interval reflects growing institutional elaboration of the apprenticeship model even as digital alternatives provide direct knowledge access.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence because the same structural phenomenon is genuinely serving both coordination and extraction functions simultaneously. Established communities are not lying when they say imitation is necessary for authentic creativity — it appears to be partially true. Emerging creators are not wrong when they experience it as suppression — the institutional enforcement is also real. The gap is not perceptual error but structural reality: the institution has captured a legitimate coordination function and layered extraction mechanisms onto it. Mentorship is both real knowledge transmission and credential gatekeeping. Imitation is both skill-building and market-suppression. The tangled_rope classification resolves the ambiguity by acknowledging both mechanisms.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are determined by structural position and exit options. Emerging creators with trapped exit and victim status derive high d (0.90-0.95), producing high experienced extractiveness. Established communities with institutional power and arbitrage exit derive low d (0.05-0.15), producing low or negative experienced extractiveness. Mid-career creators with constrained exit and mixed victim/beneficiary status derive moderate d (0.55-0.65), producing moderate experienced extractiveness. The beneficiary declaration (established communities and mentors) and victim declaration (emerging creators and creative autonomy) drive the directionality computation. The tangled_rope classification requires both: coordination function (genuine knowledge transmission) and asymmetric extraction (gatekeeping, credential requirements, time cost).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating how a genuine coordination function (knowledge transmission through imitation) can be captured and weaponized into an extraction mechanism (gatekeeping, credential barriers, suppressed market access). The early architecture of the constraint—apprenticeship in craft traditions—was primarily coordination: master teaches apprentice, apprentice internalizes technique, becomes master. But as creative markets professionalized and credentialing systems emerged, the same structural requirement became an extraction mechanism. The MFA credential is both knowledge transmission and gatekeeping. The mentorship network is both teaching and access control. The imitation requirement is both skill-building and suppression of early originality. The resolution is not to pick one interpretation but to measure how the ratio has shifted over time. The rising theater_ratio (0.42→0.68) indicates that the performative aspects have grown relative to direct transmission, suggesting the constraint is increasingly captured extraction. The tangled_rope classification holds across the interval, but with increasing χ, indicating the extraction layer is thickening. Alternative pathways (digital platforms, self-publishing) that provide knowledge transmission without gatekeeping (scaffold perspective) offer the possibility of decoupling the coordination function from the extraction mechanism — this is the real measurement of whether the imitation requirement is inevitable or institutional.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    imitation_duration_threshold,
    'What duration of imitation is structurally necessary for authentic originality, and what duration is institutional enforcement?',
    'Historical analysis of creators who achieved recognition through non-imitation pathways vs traditional apprenticeship; cross-domain comparison (music vs visual art vs literature) of typical duration before originality recognition',
    'If necessary duration is 1-2 years: imitation requirement is largely institutional enforcement (higher extractiveness). If necessary duration is 7-10 years: constraint is closer to natural structural law (lower extractiveness).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imitation_duration_threshold, empirical, 'Threshold distinguishing necessary imitation from institutional enforcement').

omega_variable(
    gatekeeping_vs_transmission,
    'Does institutional gatekeeping (MFAs, mentorship requirements, residency programs) serve knowledge transmission or primarily credential signaling and access control?',
    'Comparative effectiveness: self-taught creators'' skill acquisition rates vs formally trained; analysis of what knowledge actually transfers through formal programs vs what is accessible through alternative means (books, tutorials, peer learning); tracking of creators who gained recognition without formal credentialing',
    'If primarily transmission: constraint is coordination mechanism (Rope). If primarily gatekeeping: constraint is pure extraction mechanism (Snare/Tangled Rope with higher extractiveness).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gatekeeping_vs_transmission, empirical, 'Whether gatekeeping serves knowledge transmission or credential control').

omega_variable(
    digital_pathway_substitution,
    'Can digital platforms, online communities, and crowdfunding fully substitute for the traditional imitation bottleneck, or do they merely create parallel pathways with different bottlenecks?',
    'Longitudinal tracking of creators entering through digital-first pathways; comparison of audience reach and market recognition for digital-native vs traditionally-trained creators; analysis of whether digital pathways reduce total duration of pre-recognition work or merely redistribute it',
    'If true substitution: scaffold sunset is real and accelerating (extractiveness declining over time). If parallel pathways: digital alternatives reduce traditional suppression but create new ones (algorithm curation, audience saturation); overall extractiveness stable or shifting rather than declining.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(digital_pathway_substitution, empirical, 'Whether digital platforms substitute for or parallel the imitation bottleneck').

omega_variable(
    cultural_domain_variance,
    'Does the imitation requirement differ fundamentally across cultural domains (music, visual art, literature, design, architecture, dance), or is the apparent variance merely reflecting different gatekeeping structures?',
    'Cross-domain comparative analysis: typical time to recognition, role of formal training in each domain, prevalence of self-taught successful creators, market barriers vs skill barriers in each field',
    'If fundamental variance: constraint is domain-specific (different omegas, different stories for each domain). If gatekeeping variance: single constraint with domain-indexed perspectives may be more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_domain_variance, conceptual, 'Whether imitation requirement varies fundamentally across creative domains').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(helsinki_bus_theory, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hbs_tr_t0, helsinki_bus_theory, theater_ratio, 0, 0.42).
narrative_ontology:measurement(hbs_tr_t10, helsinki_bus_theory, theater_ratio, 10, 0.58).
narrative_ontology:measurement(hbs_tr_t20, helsinki_bus_theory, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(hbs_be_t0, helsinki_bus_theory, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(hbs_be_t10, helsinki_bus_theory, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(hbs_be_t20, helsinki_bus_theory, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(helsinki_bus_theory, information_standard).
narrative_ontology:affects_constraint(helsinki_bus_theory, creative_market_gatekeeping).
narrative_ontology:affects_constraint(helsinki_bus_theory, artist_economic_precarity).

% DUAL FORMULATION NOTE:
% The Helsinki Bus Station Theory decomposes into at least two distinct structural claims: (1) Imitation is cognitively/culturally necessary for authentic originality (ε≈0.15, Mountain at analytical level), and (2) Institutional gatekeeping uses imitation requirements as market suppression (ε≈0.52, Tangled Rope at creator level). The current story addresses both, but domain-specific analysis (music vs visual art vs literature) may warrant separate stories for how the necessity-vs-gatekeeping ratio varies by creative domain. The upstream constraint (cognitive necessity) influences but does not determine the downstream constraint (institutional gatekeeping implementation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(helsinki_bus_theory, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
