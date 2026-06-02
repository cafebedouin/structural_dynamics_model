% ============================================================================
% CONSTRAINT STORY: institutional_gatekeeping
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_gatekeeping, []).

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
 *   constraint_id: institutional_gatekeeping
 *   human_readable: Institutional Gatekeeping and Access Control
 *   domain: institutional/social/economic
 *
 * SUMMARY:
 *   Institutional gatekeeping is the mechanism by which organizations control
 *   access to membership, resources, opportunities, and legitimacy. This
 *   constraint exhibits a fundamental tension between the genuine
 *   coordination function of gatekeeping (distinguishing capable from
 *   incapable, maintaining institutional integrity, ensuring legitimate
 *   membership) and its extraction function (controlling supply, capturing
 *   economic rents, excluding competitors, enforcing conformity). The same
 *   structural mechanism — standards enforcement, credential requirements,
 *   access control — simultaneously serves coordination and extraction. The
 *   constraint's seven perspectives reveal this ambiguity: from the excluded
 *   aspirant's view it is pure extraction (snare); from the incumbent
 *   gatekeeper's view it is pure coordination (rope); from the disruption
 *   coalition's view it is a temporary problem with alternative solutions
 *   (scaffold); from the legacy system's view it is a performative ritual
 *   sustained by inertia (piton). The theater_ratio (0.58) reflects that
 *   credentialing systems increasingly substitute theatrical compliance
 *   (credential possession, ritual signaling, demographic conformity) for
 *   actual competence verification. The rising trajectory (0.42 → 0.58 over
 *   20 units) indicates that as alternative competence signals emerge
 *   (portfolios, open-source contributions, skills testing), traditional
 *   gatekeeping has responded not by improving competence verification but by
 *   increasing theater — adding more credentials, more specialized
 *   requirements, more gatekeeping overhead — to preserve gatekeeper control.
 *
 * KEY AGENTS:
 *   - Excluded Aspirants: Primary victims (powerless/trapped) — cannot access institutional pathways; face absolute barriers to gatekeepers' permission to compete
 *   - Aspiring Insiders: Secondary victims (moderate/constrained) — face high access costs but some pathways exist; experience mixed coordination and extraction
 *   - Incumbent Gatekeepers: Primary beneficiaries (institutional/arbitrage) — control supply of access; capture economic rents; see gatekeeping as legitimate coordination
 *   - Disruption Coalition: Organized agents (organized/mobile) — building alternative credentialing systems (online platforms, skills-based hiring, open-source reputation, blockchain credentials) with exit dynamics
 *   - Legacy Credentialing System: Institutional actor (institutional/arbitrage) — traditional gatekeeping mechanisms maintained through inertia; high theater ratio indicates performative function
 *   - Secondary Gatekeepers: Institutional actors (moderate/constrained) — mid-level enforcers (hiring managers, department chairs, journal editors) who both enforce and experience extraction
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent gatekeeping as inherent to all institutions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_gatekeeping, 0.52).
domain_priors:suppression_score(institutional_gatekeeping, 0.65).
domain_priors:theater_ratio(institutional_gatekeeping, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_gatekeeping, extractiveness, 0.52).
narrative_ontology:constraint_metric(institutional_gatekeeping, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(institutional_gatekeeping, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_gatekeeping, tangled_rope).
narrative_ontology:human_readable(institutional_gatekeeping, "Institutional Gatekeeping and Access Control").
narrative_ontology:topic_domain(institutional_gatekeeping, "institutional/social/economic").

domain_priors:requires_active_enforcement(institutional_gatekeeping).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_gatekeeping, incumbent_gatekeepers).
narrative_ontology:constraint_beneficiary(institutional_gatekeeping, institutional_insiders).
narrative_ontology:constraint_victim(institutional_gatekeeping, excluded_aspirants).
narrative_ontology:constraint_victim(institutional_gatekeeping, field_diversity).
narrative_ontology:constraint_victim(institutional_gatekeeping, innovation_marginal_perspectives).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED ASPIRANT (SNARE) — Faces absolute barriers to institutional access: requires gatekeepers' permission to compete. No alternative pathways. Education, credentials, networks all controlled by the gatekeeper. Cannot exit without abandoning career aspirations entirely. Maximum experienced extraction.
constraint_indexing:constraint_classification(institutional_gatekeeping, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ASPIRING INSIDER (TANGLED ROPE) — Faces high costs to access (credentialing gauntlet, apprenticeship, cultural conformity requirements) but some pathways exist. Constrained by resource requirements, time investment, and social capital barriers. Also benefits from access to institutional resources, networks, and legitimation once credentials are earned. Mixed coordination (credentialing serves legitimate skill-verification) and extraction (gatekeepers capture disproportionate value from the aspiring insider's investment).
constraint_indexing:constraint_classification(institutional_gatekeeping, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INCUMBENT GATEKEEPER (ROPE) — Experiences the gatekeeper constraint as pure coordination: maintaining standards, managing competence verification, ensuring institutional legitimacy. Can exit to alternative institutions or markets. Net beneficiary but sees constraint as functional (coordination not extraction). Benefits include: control over supply of labor/participants, ability to set compensation/opportunity rates, prestige and authority.
constraint_indexing:constraint_classification(institutional_gatekeeping, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DISRUPTION COALITION (SCAFFOLD) — Organized agents (alternative credentialing systems, online platforms, decentralized networks, open-source communities) are building parallel access pathways with lower gatekeeper overhead. These alternatives have a sunset logic: as they mature and gain legitimacy, the traditional gatekeeper's extraction mechanism loses force. Low experienced extraction because organized agents have agency and see concrete alternatives emerging.
constraint_indexing:constraint_classification(institutional_gatekeeping, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY CREDENTIALING SYSTEM (PITON) — Traditional gatekeeping mechanisms (university degrees, guild membership, professional licensing) persist largely through institutional inertia. The actual skill-verification function has atrophied: credentials serve signaling and network-access roles rather than competence assurance. Theater ratio is high because credentialing theater (exam performance, thesis format, ritualized apprenticeship) has replaced actual skill demonstration as the gatekeeper's primary function. The system maintains itself through self-reinforcement (gatekeepers require credentials because gatekeepers require credentials) rather than genuine need.
constraint_indexing:constraint_classification(institutional_gatekeeping, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: SECONDARY GATEKEEPER (TANGLED ROPE) — Mid-level gatekeepers (department chairs, hiring managers, journal editors) who enforce primary gatekeeper standards but also navigate constraints from above (institutional policies) and below (aspirant pressure). Both enforce extraction and experience it. Can partially exit by loosening standards, but face professional consequences from primary gatekeepers. Mixed role: coordinator of legitimate standards but also extractor of rents from aspirants.
constraint_indexing:constraint_classification(institutional_gatekeeping, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some gatekeeper function is inherent to any institutional structure: organizations must distinguish members from non-members, capable from incapable, insiders from outsiders. Without gatekeeping, no institution can maintain identity or function. This perspective risks naturalizing contingent gatekeeping mechanisms (extractive credentialism, network closure, arbitrary standards) as inherent to organization itself. The engine's false summit detector will reveal this as naturalization of institutional contingency.
constraint_indexing:constraint_classification(institutional_gatekeeping, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_gatekeeping_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_gatekeeping, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_gatekeeping, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_gatekeeping, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_gatekeeping, TR),
    TR >= 0.70.

:- end_tests(institutional_gatekeeping_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Gatekeepers capture economic rents (wage premiums, position scarcity, access control), restrict supply to artificially maintain scarcity, and enforce conformity requirements that exceed legitimate competence concerns. However, extraction is not total (0.70+) because some gatekeeping serves genuine coordination: institutions do need to distinguish members from non-members, some credentialing does reflect real competence gaps, and alternative systems have not yet fully replaced gatekeepers. The rising trajectory (0.35 → 0.52) reflects that gatekeepers are increasing extraction intensity as alternatives emerge — adding more requirements, more specialized credentials, more gatekeeping theater — rather than improving actual competence verification. Suppression (0.65): High. Barriers to bypassing gatekeeping include: educational monopolies, professional licensing requirements, credentialing time costs, network access restrictions, alternative system stigma (self-reinforcing because gatekeepers control employment), and regulatory/legal enforcement of gatekeeper authority. Suppression is substantial but not absolute (0.80+) because alternative pathways are emerging and some mobility exists for exceptional talent. Theater ratio (0.58): Moderate-high. Credentialing has increasingly substituted theatrical compliance (passing exams, completing prescribed curricula, demographic/cultural conformity) for actual competence demonstration. Portfolio-based hiring, skills testing, and open-source track records show that meaningful competence can be verified without theater, yet gatekeepers maintain theatrical requirements (degrees, certifications, standardized tests) as access gates.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between the incumbent gatekeeper's rope (genuine coordination) and the excluded aspirant's snare (pure extraction). Both views describe real structural features: gatekeeping does coordinate legitimate standards AND restrict supply to extract rents. The disagreement is not empirical but perspectival — from the gatekeeper's institutional position, the coordination function is primary and visible; from the aspirant's position of exclusion, the extraction function is primary and visible. The secondary gatekeeper's tangled_rope perspective shows that mid-level enforcers experience both functions simultaneously: they coordinate standards (their stated function) but also enforce gatekeepers' rent extraction (their actual structural role). The piton perspective reveals that the gatekeeper system responds to pressure by increasing theater rather than improving function — a diagnostic sign that the coordination rationale is becoming post-hoc justification rather than genuine need. The scaffold perspective (disruption coalition) documents that alternative credentialing systems are demonstrably functional, which means the mountain perspective's 'inherent to institutions' framing is demonstrably false — gatekeeping is contingent institutional arrangement, not natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries and victims are structurally distinct: incumbent gatekeepers and the institutional gatekeeping system are net beneficiaries (d ≈ 0.08-0.15: benefit from extraction, have arbitrage exits); excluded aspirants are net victims (d ≈ 0.95: bear full cost, trapped); aspiring insiders are mixed (d ≈ 0.55-0.60: face high access costs but eventual benefit status). The disruption coalition (d ≈ 0.40) is partially victimized (gatekeepers suppress alternatives) but has sufficient organization and mobile exit options to reduce experienced extraction. Secondary gatekeepers (d ≈ 0.55) are both enforcers and subjects of extraction — they enforce gatekeeping standards that extract from below but face pressure from above to maintain extraction intensity even when it exceeds legitimate competence verification. The directionality derives from this structural asymmetry: gatekeepers benefit from supply restriction; gatekeepees bear the cost of access restriction and conformity requirements.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by showing that institutional gatekeeping genuinely exhibits both coordination and extraction functions, making the tangled_rope classification the analytical truth. The piton perspective documents that traditional gatekeeping is increasingly performative (theater_ratio 0.58, rising trajectory), meaning the coordination rationale is becoming post-hoc cover for extraction. The scaffold perspective documents that alternative systems solve the coordination problem (competence verification) without the extraction mechanism (supply restriction), which proves that genuine coordination does not require traditional gatekeeping's current intensity. The mountain perspective risks false naturalization — it claims gatekeeping is inherent to institutions, but the alternative system evidence shows that institutions can coordinate without gatekeeping's current extraction overhead. The snare and rope perspectives are both structurally accurate from their positions, but the constraint's true nature is tangled: it solves a real coordination problem (distinguishing capable from incapable) while simultaneously solving an extraction problem (restricting supply, capturing rents). The rising theater_ratio (0.42 → 0.58) suggests that as alternatives emerge, gatekeepers are responding not by improving coordination (demonstrating actual competence value) but by increasing extraction (adding requirements, stacking credentials, increasing gatekeeping theater).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimate_credentialing_threshold,
    'What proportion of gatekeeping overhead is legitimate competence verification versus extractive rent-seeking?',
    'Correlation analysis: compare gatekeeper-imposed barriers with measurable competence gaps; measure whether gatekeepers who claim to verify competence actually use competence signals (portfolio review, capability testing) versus pure signaling barriers (credentials, network membership, demographic conformity)',
    'If legitimate portion > 60%: gatekeeping is primarily coordination (Rope for more perspectives). If legitimate portion < 30%: gatekeeping is primarily extraction (Snare from more perspectives). The extracted value can be estimated from the gap.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimate_credentialing_threshold, empirical, 'Ratio of legitimate competence verification to extractive overhead in gatekeeping').

omega_variable(
    alternative_credentialing_viability,
    'Do alternative credentialing systems (portfolio-based, skills-tested, blockchain-verified, open-source track records) actually produce equivalent or superior competence signals compared to traditional gatekeepers?',
    'Longitudinal outcome tracking: compare success rates, failure rates, and competence assessment accuracy of individuals credentialed through traditional gates versus alternative systems; measure employer/user satisfaction and long-term performance',
    'If alternatives are equivalent or superior: scaffold perspective confirmed — traditional gatekeeping''s extraction mechanism will erode as alternatives gain legitimacy. If alternatives underperform: traditional gatekeeping retains genuine coordination function and cannot be decomposed as pure extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_credentialing_viability, empirical, 'Competence equivalence of alternative credentialing systems').

omega_variable(
    gatekeeper_substitution_economics,
    'What are the actual economic rents extracted by gatekeepers (wage premium, compensation markup, position scarcity value) and what percentage of these rents reflect genuine scarcity versus artificial restriction?',
    'Market analysis: measure gatekeepers'' compensation premium versus non-gated equivalents; model the elasticity of supply response if gatekeeping barriers were partially relaxed; estimate deadweight loss from excluded talent',
    'If rents > 40% of gatekeeper compensation: gatekeeping is substantially extractive. If rents < 10%: gatekeeping may be legitimate supply-scarcity response. Intermediate values require institutional-specific analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeper_substitution_economics, empirical, 'Economic rent extraction through gatekeeping').

omega_variable(
    gatekeeping_mechanism_identity_lock,
    'To what extent do gatekeepers'' professional identities depend on their gatekeeper role? Is gatekeeping essential to the gatekeeper''s self-concept or merely their job function?',
    'Interview/ethnographic analysis: assess whether gatekeepers perceive relaxing standards as professional identity threat versus practical adjustment; measure identity-lock severity (would identity dissolution accompany role loss?)',
    'If identity-locked: gatekeepers will resist alternatives even when alternatives are empirically superior, making sunset dynamics slower than scaffold perspective assumes. If identity-neutral: gatekeepers may accommodate alternatives faster.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gatekeeping_mechanism_identity_lock, conceptual, 'Identity fusion of gatekeepers to their gatekeeper role').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_gatekeeping, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gate_tr_t0, institutional_gatekeeping, theater_ratio, 0, 0.42).
narrative_ontology:measurement(gate_tr_t10, institutional_gatekeeping, theater_ratio, 10, 0.55).
narrative_ontology:measurement(gate_tr_t20, institutional_gatekeeping, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(gate_be_t0, institutional_gatekeeping, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(gate_be_t10, institutional_gatekeeping, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(gate_be_t20, institutional_gatekeeping, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_gatekeeping, resource_allocation).
narrative_ontology:affects_constraint(institutional_gatekeeping, professional_licensing_extraction).
narrative_ontology:affects_constraint(institutional_gatekeeping, educational_credentialism).
narrative_ontology:affects_constraint(institutional_gatekeeping, organizational_hierarchy_preservation).

% DUAL FORMULATION NOTE:
% Institutional gatekeeping decomposes into several structurally distinct constraints: professional licensing (regulatory gatekeeping with legal enforcement), educational credentialism (academic gatekeeping with time/cost barriers), and organizational hierarchy (internal gatekeeping with career progression barriers). Each has its own epsilon value and network of affected constraints. This story addresses the generic institutional gatekeeping mechanism; domain-specific stories address licensing (higher epsilon, higher legal enforcement overhead) and credentialism (higher theater ratio, lower legitimate competence signal).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_gatekeeping, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
