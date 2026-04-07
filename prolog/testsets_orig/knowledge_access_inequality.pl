% ============================================================================
% CONSTRAINT STORY: knowledge_access_inequality
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_knowledge_access_inequality, []).

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
 *   constraint_id: knowledge_access_inequality
 *   human_readable: Knowledge Access Inequality
 *   domain: education/information/economic
 *
 * SUMMARY:
 *   Knowledge access inequality is the structural constraint that gatekeeps
 *   participation in epistemic and educational commons through price,
 *   credential requirement, geographic isolation, and institutional monopoly.
 *   This constraint exhibits tangled coordination and extraction: genuine
 *   knowledge distribution and curation functions exist alongside systematic
 *   extraction from those with least capacity to pay. The constraint operates
 *   across multiple institutional levels — from journal paywalls and textbook
 *   pricing (information monopoly) to credential gatekeeping (labor market
 *   exclusion) to institutional capture of accreditation systems (theatrical
 *   legitimation). The rising theater_ratio (0.35 → 0.48) reflects that
 *   credential systems are increasingly performative: tuition and time
 *   investment signal conformity and network access more than transferable
 *   skill, while actual knowledge acquisition increasingly happens through
 *   free channels (YouTube, Wikipedia, open courseware). The rising
 *   extractiveness (0.42 → 0.58) reflects layered rent-seeking: as digital
 *   reproduction costs approach zero, information gatekeepers have raised
 *   prices and barriers rather than lowered them, capturing policy and
 *   institutional structures to sustain extraction. Simultaneously, the open
 *   knowledge movement (Creative Commons, open-access publishing, Wikipedia,
 *   Khan Academy, OpenStax) is building alternative pathways with genuine
 *   sunset logic: as free resources reach critical mass and employer
 *   acceptance of portfolio-based credentials grows, the closed-access
 *   extraction mechanism loses comparative advantage.
 *
 * KEY AGENTS:
 *   - Knowledge Seekers Without Means: Primary victim (powerless/trapped) — faces paywalls, credential barriers, geographic isolation; cannot access epistemic systems without sacrifice beyond reasonable means
 *   - Peripheral Communities: Secondary victim (powerless/trapped) — geographic, linguistic, or economic isolation from knowledge infrastructure; bears disproportionate extraction
 *   - Epistemic Commons: Tertiary victim (powerless/trapped) — abstract collective good representing the universal human capacity for understanding; contaminated by knowledge scarcity and gatekeeping
 *   - Credential Gatekeepers: Primary beneficiary (institutional/arbitrage) — universities, professional boards, accrediting bodies that control access to labor markets; benefit from credential extraction while appearing neutral
 *   - Information Monopolists: Primary beneficiary (institutional/arbitrage) — journal publishers, textbook companies, data providers; extract rent from knowledge that has marginal reproduction cost near zero
 *   - Established Knowledge Holders: Secondary beneficiary (institutional/arbitrage) — academics, professionals already credentialed; benefit from gatekeeping that protects market position and prestige
 *   - Open Knowledge Movement: Organized agent (organized/constrained) — Creative Commons, open-access journals, Wikipedia editors, OpenStax; building alternative pathways; sees constraint as temporary coordination failure with sunset
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(knowledge_access_inequality, 0.58).
domain_priors:suppression_score(knowledge_access_inequality, 0.62).
domain_priors:theater_ratio(knowledge_access_inequality, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(knowledge_access_inequality, extractiveness, 0.58).
narrative_ontology:constraint_metric(knowledge_access_inequality, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(knowledge_access_inequality, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(knowledge_access_inequality, tangled_rope).
narrative_ontology:human_readable(knowledge_access_inequality, "Knowledge Access Inequality").
narrative_ontology:topic_domain(knowledge_access_inequality, "education/information/economic").

domain_priors:requires_active_enforcement(knowledge_access_inequality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(knowledge_access_inequality, credential_gatekeepers).
narrative_ontology:constraint_beneficiary(knowledge_access_inequality, information_monopolists).
narrative_ontology:constraint_beneficiary(knowledge_access_inequality, established_knowledge_holders).
narrative_ontology:constraint_victim(knowledge_access_inequality, knowledge_seekers_without_means).
narrative_ontology:constraint_victim(knowledge_access_inequality, peripheral_communities).
narrative_ontology:constraint_victim(knowledge_access_inequality, epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ECONOMICALLY EXCLUDED KNOWLEDGE SEEKER (SNARE) — Faces insurmountable barriers to accessing knowledge: paywalled journals, expensive textbooks, credential requirements, geographic isolation. Cannot participate in educational or epistemic systems. Zero degrees of freedom; extraction is total and unavoidable. Bears full cost of knowledge inequality.
constraint_indexing:constraint_classification(knowledge_access_inequality, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CONSTRAINED LEARNER (TANGLED ROPE) — Faces high but surmountable barriers: partial access through public libraries, free-tier resources, informal learning networks. Genuine coordination function exists (knowledge sharing, community education) but extraction mechanisms overlay it (credential premiums, labor market gatekeeping). Can acquire knowledge at significant time/cost/opportunity sacrifice.
constraint_indexing:constraint_classification(knowledge_access_inequality, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: KNOWLEDGE INSTITUTION (ROPE) — Primary beneficiary. Operates access-control systems that generate revenue while coordinating legitimate knowledge distribution functions (peer review, curation, certification). Experiences the constraint as pure coordination with net benefit flow toward this agent. Can arbitrage between access-restricted and open models.
constraint_indexing:constraint_classification(knowledge_access_inequality, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN KNOWLEDGE MOVEMENT (SCAFFOLD) — Organized agents (Creative Commons, open-access journals, Wikipedia, Khan Academy, OpenStax) are building alternative knowledge distribution pathways with explicit sunset logic: as open resources mature and accumulate critical mass, the closed-access extraction mechanism loses competitive advantage. Low effective extraction because organized agents see and are actively building exit paths.
constraint_indexing:constraint_classification(knowledge_access_inequality, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CREDENTIAL SYSTEM (PITON) — Educational credentials once coordinated genuine skill certification; now largely performative in many domains. Expensive degrees signal market-conformity and network access more than knowledge. Theater-heavy ritual (tuition, years of seat-time, accreditation theater) persists through institutional inertia despite cheaper alternatives (online courses, bootcamps, portfolio-based assessment). Declining functional coordination, maintained by regulatory capture and employer lock-in.
constraint_indexing:constraint_classification(knowledge_access_inequality, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE NATURAL LAW (MOUNTAIN) — Risk of naturalizing knowledge access inequality as inherent to education: 'knowledge has always been scarce,' 'curation requires exclusion,' 'credentials must be expensive to mean something.' These framings naturalize what is actually contingent institutional choice. The structural data contradicts the mountain classification — this is not a law of nature but an arrangement of incentives and enforcement mechanisms that the open knowledge movement is actively disrupting.
constraint_indexing:constraint_classification(knowledge_access_inequality, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(knowledge_access_inequality_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(knowledge_access_inequality, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(knowledge_access_inequality, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(knowledge_access_inequality, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(knowledge_access_inequality, TR),
    TR >= 0.70.

:- end_tests(knowledge_access_inequality_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts through multiple mechanisms: information monopoly rents (journals, textbooks), credential premium inflation, market signaling requirements, and institutional gatekeeping. The value is not at maximum (0.66+) because genuine coordination functions persist (knowledge curation, peer review, educational structure, labor market signaling), and because open-knowledge alternatives are eroding extraction capacity. Suppression (0.62): High. Barriers to knowledge access include: paywall economics (knowledge legally behind price walls), credential requirements (structural gatekeeping), geographic/linguistic isolation, time/opportunity costs, internalized beliefs about legitimate knowledge sources, and institutional regulatory capture. Suppression is not total because free alternatives exist (libraries, internet, self-education networks) and suppression is eroding. Theater ratio (0.48): Moderate. Traditional credential systems are increasingly theater-heavy: tuition signals market-conformity more than skill; accreditation theater justifies institutional gatekeeping; research paywalls perform peer review legitimacy while mostly serving publisher extraction. But theater is not at piton levels (0.70+) because genuine educational and epistemic functions persist — some credentials do transfer skill; peer review does catch some errors; curation does add value. The rising trajectory reflects increasing theatrical overlay on real functions.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the powerless victim (Snare) and the institutional beneficiary (Rope) is structural, not perceptual. Both are correctly reading the same constraint: one experiences extraction because they are the target; the other experiences coordination because they are the beneficiary. The gap persists because exit options differ fundamentally — the victim is trapped and cannot leave; the beneficiary can arbitrage between access-restricted and open models. The scaffold perspective (organized agents building alternatives) is crucial: it shows that the Rope/Snare gap is not inherent but historically contingent. As alternatives mature, the snare mechanism (prevention of exit paths) weakens, and victims gain the constrained/mobile exit options necessary to reclassify their perspective. The piton perspective (theatrical credentials) is not separate from the tangled rope — it is the credential layer of the knowledge access constraint. Theater ratio rising while extractiveness rises indicates that the constraint is maintaining itself increasingly through legitimacy theater rather than through direct functional coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (credential gatekeepers, information monopolists) have institutional power and arbitrage options: they can choose between closed and open models, between gatekeeping and alternative curation. Their derived d is low (~0.10-0.15), producing negative or near-zero χ. They experience the constraint as beneficial. Victims (powerless knowledge seekers) are trapped with no arbitrage options: they cannot choose between paywalled and open because they have no means to access paywalled knowledge and may not know about open alternatives. Their derived d is high (~0.90-0.95), producing high χ. They experience the constraint as pure extraction. The organized open knowledge movement has power to create alternatives (d ~0.35-0.45), producing moderate χ — they incur costs in building alternatives but have real agency and visible exit paths. The analytical observer at global/civilizational scope sees the full structure (d ~0.72), producing high χ — the observer recognizes that the constraint is maintained by institutional choice, not natural law. These d values can be confirmed or overridden by directionality_overrides if domain knowledge suggests the automatic derivation is incorrect. For this constraint, no overrides are necessary: the beneficiary/victim structure and exit options clearly determine d.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: This constraint avoids mandatrophy by explicitly maintaining the tension between genuine coordination and asymmetric extraction. Knowledge curation, peer review, educational structure, and credentialing do coordinate real functions — the constraint is not pure Snare. But the distribution of benefits is asymmetric — gatekeepers capture rents while knowledge seekers pay. The beneficiaries/victims/enforcement declarations establish that this is not coordination masquerading as extraction or extraction masquerading as coordination. It is genuinely both: Tangled Rope. The open knowledge movement's perspective (Scaffold) provides the mandatrophy resolution: the constraint's viability depends on information monopoly rents and credential gatekeeping, which are not inherent to education or knowledge distribution but rather contingent institutional arrangements. As alternatives mature, the rents erode and the true coordination function (knowledge sharing, skill transfer, credible certification) can separate from the extraction apparatus (paywalls, credential inflation, institutional theater). The generational sunset logic confirms that this is coordination with embedded extraction, not extraction with coordination theater. If it were pure extraction (Snare), sunset would be impossible — the constraint would need to prevent alternative pathways from forming. Instead, it is actively being displaced by superior coordination mechanisms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    free_access_quality_tradeoff,
    'Does free/open knowledge access reduce epistemic quality through loss of peer review and curation functions, or is the quality loss negligible compared to access gains?',
    'Comparative analysis of citation impact, error rates, and replicability: open-access journals vs paywalled journals; Wikipedia articles vs proprietary encyclopedias; open textbooks vs commercial textbooks. Long-term validation through field-specific outcome metrics.',
    'If quality cost is high: open knowledge movement represents coordination loss (reclassify toward Snare from some perspectives). If negligible: open knowledge movement is pure coordination gain (confirm Scaffold/Rope classification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(free_access_quality_tradeoff, empirical, 'Whether open knowledge access incurs significant quality costs').

omega_variable(
    credential_signaling_vs_skill_transfer,
    'What proportion of credential value derives from skill transfer vs pure signaling (network access, employer filtering, social legitimacy)?',
    'Employer hiring data comparing credential-holders vs portfolio-demonstrated practitioners; wage premiums attributable to skill vs credential; field-specific analysis (software engineering, nursing, trades vs research fields).',
    'If signaling > 70%: credentials are extractive theater (piton classification confirmed). If signaling < 40%: credentials retain genuine coordination function (reclassify toward Rope). Impacts whether credential systems themselves are separable constraints or inseparable from knowledge access inequality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credential_signaling_vs_skill_transfer, empirical, 'Signaling vs skill-transfer proportion in credential value').

omega_variable(
    information_monopoly_sustainability,
    'Can journal paywalls and textbook pricing maintain extraction rents as digital reproduction costs approach zero and alternative curation mechanisms mature?',
    'Market analysis: journal subscription revenue trends, publisher profitability, author flight to open-access venues, institutional subscription cancelations. Correlation with open-access resource maturation.',
    'If monopoly rents erode: scaffold sunset is real (extraction declines over generational timescale). If rents stabilize: institutional capture of credentials/accreditation sustains the constraint indefinitely (reclassify toward Snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(information_monopoly_sustainability, empirical, 'Sustainability of information monopoly rents in digital era').

omega_variable(
    identity_lock_in_credentials,
    'For knowledge seekers with internalized beliefs about credential legitimacy (''real knowledge requires expensive degrees''), is exit capacity constrained by cognitive capture rather than material barriers?',
    'Comparative outcome analysis: learners with material access to open resources who do not use them vs learners who overcome identity/prestige barriers to use them. Psychological/narrative analysis of credential belief persistence post-access.',
    'If cognitive capture is significant: some victims experience identity_locked exit rather than trapped/constrained. Reclassifies the constraint''s suppression mechanism as partially internalized. Mandatrophy analysis shifts: the constraint''s power partly depends on victim belief, not just on material gatekeeping.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_credentials, empirical, 'Whether credential internalization creates identity-lock beyond material barriers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(knowledge_access_inequality, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kai_tr_t0, knowledge_access_inequality, theater_ratio, 0, 0.35).
narrative_ontology:measurement(kai_tr_t5, knowledge_access_inequality, theater_ratio, 5, 0.41).
narrative_ontology:measurement(kai_tr_t10, knowledge_access_inequality, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(kai_be_t0, knowledge_access_inequality, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(kai_be_t5, knowledge_access_inequality, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(kai_be_t10, knowledge_access_inequality, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(knowledge_access_inequality, information_standard).
narrative_ontology:affects_constraint(knowledge_access_inequality, credential_gatekeeping).
narrative_ontology:affects_constraint(knowledge_access_inequality, information_monopoly).
narrative_ontology:affects_constraint(knowledge_access_inequality, epistemic_commons_degradation).

% DUAL FORMULATION NOTE:
% Knowledge access inequality is upstream of multiple institutional constraints. Credential gatekeeping and information monopoly are decomposable as separate constraints with their own ε values and perspectives, but they are structurally coupled through the same institutional mechanisms (paywalls, accreditation capture, market signaling). Each story in this family should link via network.affects_constraints to show the institutional coupling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
