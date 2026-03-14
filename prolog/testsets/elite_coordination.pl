% ============================================================================
% CONSTRAINT STORY: elite_coordination
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_elite_coordination, []).

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
 *   constraint_id: elite_coordination
 *   human_readable: Elite Coordination and Access Gatekeeping
 *   domain: political_economy/institutional_power
 *
 * SUMMARY:
 *   Elite coordination mechanisms enable high-status networks to manage
 *   information, maintain reputational standards, and control access to
 *   institutional positions. This constraint exhibits a hybrid structure:
 *   genuine coordination function (shared knowledge, vetted collaboration)
 *   coexists with asymmetric extraction (access gatekeeping,
 *   network-dependent advancement, systematic exclusion of non-members). The
 *   constraint's extractiveness has increased over the measured interval
 *   (0.42 → 0.58) as gatekeeping mechanisms have become more sophisticated
 *   and institutionalized, while the rhetoric of meritocracy has remained
 *   constant — this gap between claim and practice drives the rising theater
 *   ratio (0.45 → 0.65). Elite networks provide measurable benefits to
 *   members (information advantage, opportunity access, reputational
 *   insurance) but extract these benefits through denial-of-access mechanisms
 *   that systematically disadvantage non-members. The constraint becomes
 *   visible when aspiring outsiders attempt entry: they encounter informal
 *   barriers (lack of 'cultural fit,' unstated credential requirements,
 *   family background expectations) that are more durable than formal
 *   exclusion because they are plausibly deniable as individual shortcomings
 *   rather than structural gatekeeping.
 *
 * KEY AGENTS:
 *   - Established Elite Networks: Primary beneficiary (institutional/arbitrage) — coordinate internal information and maintain member status; extract value through access gatekeeping
 *   - Non-networked Aspiring Members: Primary victim (powerless/trapped) — lack credentials, social capital, and family connections to access networks; face insurmountable barriers to entry
 *   - Marginal Network Participants: Secondary victim (moderate/constrained) — some access but limited status; must overperform to prove network-worthy; face higher exit costs than insiders
 *   - Meritocracy Reform Advocates: Organized agents (organized/constrained) — push for transparent hiring and diversity, but must work within networks; often co-opted by networks adopting reform rhetoric
 *   - Institutional Legitimacy: Abstract victim (powerless/trapped) — bears cost of meritocratic claims unsupported by practice; cannot exit or organize; experiences institutional credibility erosion
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing network-based allocation as inherent human tendency rather than contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(elite_coordination, 0.58).
domain_priors:suppression_score(elite_coordination, 0.52).
domain_priors:theater_ratio(elite_coordination, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(elite_coordination, extractiveness, 0.58).
narrative_ontology:constraint_metric(elite_coordination, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(elite_coordination, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(elite_coordination, tangled_rope).
narrative_ontology:human_readable(elite_coordination, "Elite Coordination and Access Gatekeeping").
narrative_ontology:topic_domain(elite_coordination, "political_economy/institutional_power").

domain_priors:requires_active_enforcement(elite_coordination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(elite_coordination, established_elite_networks).
narrative_ontology:constraint_beneficiary(elite_coordination, incumbent_institutions).
narrative_ontology:constraint_victim(elite_coordination, non_networked_aspiring_members).
narrative_ontology:constraint_victim(elite_coordination, institutional_legitimacy).
narrative_ontology:constraint_victim(elite_coordination, meritocratic_mobility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POWERLESS OUTSIDER (SNARE) — Faces insurmountable barriers to elite networks without prior access or social capital. Trapped by lack of credentials, family connections, and gatekeeping mechanisms. Maximum experienced extraction as elite networks extract value from outsiders' aspirations while systematically denying access. No exit without abandoning career aspirations in constrained sectors.
constraint_indexing:constraint_classification(elite_coordination, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MARGINAL NETWORK PARTICIPANT (TANGLED ROPE) — Some access to elite networks but limited status and influence. Experiences genuine coordination benefits (information sharing, collaboration opportunities) alongside asymmetric extraction (must prove themselves more rigorously than network insiders, face higher exit costs). Constrained by career dependence on network membership while unable to fully shape network rules.
constraint_indexing:constraint_classification(elite_coordination, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ESTABLISHED ELITE NETWORK (ROPE) — Experiences constraint as pure coordination mechanism: sharing information, maintaining reputational standards, and excluding incompetent outsiders. Net beneficiary with arbitrage options (can migrate between networks or exit to alternative status hierarchies). Extraction flows toward this network; coordination benefits internal members.
constraint_indexing:constraint_classification(elite_coordination, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MERITOCRACY REFORM MOVEMENT (TANGLED ROPE) — Organized agents (diversity advocates, transparency advocates) simultaneously benefit from and are constrained by existing elite networks. Must work within the system to reform it, creating asymmetric enforcement burden. Genuine coordination function (improving information flow about outsiders) coexists with extraction (networks co-opt reform rhetoric while preserving gatekeeping).
constraint_indexing:constraint_classification(elite_coordination, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: MERITOCRATIC IDEOLOGY (PITON) — The rhetoric of meritocracy serves as performative covering for network-based allocation. Theater ratio 0.65 reflects the gap between meritocratic claims and network-based practice. The ideology persists through institutional inertia — it legitimizes elite networks while the networks systematically contradict meritocratic principles. Former rope that has degraded into theatrical function.
constraint_indexing:constraint_classification(elite_coordination, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW (FALSE SUMMIT) — From a civilizational view, some elite coordination might appear as natural law: humans inherently form in-groups, information sharing requires trust, hierarchy is immutable. However, the base properties (extractiveness 0.58, suppression 0.52, beneficiaries/victims clearly identified) reveal this as a false summit. Elite coordination naturalizes contingent institutional arrangements. The engine will flag this perspective as a false natural law claim.
constraint_indexing:constraint_classification(elite_coordination, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(elite_coordination_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(elite_coordination, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(elite_coordination, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(elite_coordination, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(elite_coordination, TR),
    TR >= 0.70.

:- end_tests(elite_coordination_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Elite networks capture significant career and opportunity advantages for members while systematically denying access to non-members. The extraction is not maximal because some coordination function is genuine — networks do solve real information problems and maintain collaboration standards. However, the proportion of extraction has increased over the interval as gatekeeping has become more sophisticated (credential inflation, culture-fit filtering) and networks have consolidated institutional power. The measurement trajectory reflects mounting sophistication of exclusion mechanisms. Suppression (0.52): Moderate-high. Significant structural barriers include lack of inherited social capital, geographic isolation from network hubs, credential requirements that correlate with family wealth, and cultural expectations (linguistic codes, leisure activities, family background) that are unstated but rigidly enforced. However, suppression is incomplete — some outsiders do penetrate networks through exceptional performance or fortunate circumstance; alternative institutional pathways (open-source communities, entrepreneurship, non-networked fields) provide partial exit options. Theater ratio (0.65): High and rising. The meritocratic ideology claims that elite positions are earned through talent and demonstrated competence, but empirical evidence consistently shows network membership predicts advancement independent of performance. The theater has increased as networks have become more sophisticated at maintaining the meritocratic facade while the underlying mechanisms have become more extractive. Beneficiaries and victims are clearly identifiable through the advancement asymmetry: network members advance at higher rates with lower performance thresholds than comparable non-members.
 *
 * PERSPECTIVAL GAP:
 *   Elite coordination creates maximum interpretive divergence because the same mechanism (selective information sharing, selective access) is experienced as coordination benefit by insiders and as extraction by outsiders. The network sees 'quality control'; the outsider sees 'gatekeeping.' The established member sees 'earned status'; the non-member sees 'inherited privilege.' The theater ratio rise (0.45 → 0.65) indicates the constraint is becoming increasingly extractive relative to its coordination function — the performative cover story (meritocracy) is diverging from practice as networks become more sophisticated at hiding gatekeeping. This perspectival gap is diagnostic: if all perspectives converged on a single type, the constraint would be either pure coordination (all see Rope) or pure extraction (all see Snare). The range from Rope to Snare across perspectives indicates genuine hybrid structure with high power asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation flows from structural position relative to the extraction mechanism. Established networks (institutional/arbitrage) experience low d because they are beneficiaries with exit options — they can affiliate with alternative networks or exit to other status systems. Their d ≈ 0.15, yielding negative χ (they benefit). Non-networked outsiders (powerless/trapped) experience high d because they are victims with no exit — they cannot obtain network credentials, cannot migrate to alternative pathways easily, and bear full cost of being excluded. Their d ≈ 0.90, yielding maximum χ experienced (high effective extraction). Marginal participants (moderate/constrained) experience intermediate d ≈ 0.55 because they have partial network access but cannot exit without career damage; they benefit somewhat but are extracted from asymmetrically. Meritocracy reformers (organized/constrained) experience d ≈ 0.50-0.60 because they are organized but dependent on the very networks they aim to reform — their exit options are constrained. The analytical observer at civilizational scope experiences d ≈ 0.72 (canonical for analytical), but the false summit perspective suggests the real d should be lower (observer may be outside the extraction flow) or is hidden by the naturalization narrative.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY AVOIDANCE: This story resists mandatrophy by distinguishing genuine coordination benefits from extraction cover stories. The established network's Rope perspective is legitimate — they do coordinate information and maintain collaboration standards. This is NOT a false rope (extraction disguised as coordination) because insiders genuinely receive coordination benefits. The outsider's Snare perspective is legitimate — they face extraction without coordination benefit. The Tangled Rope perspectives (marginal participants, reformers) capture the hybrid mechanism where extraction and coordination coexist asymmetrically. The constraint avoids mandatrophy by accepting that all four types (Rope, Tangled Rope, Snare, Piton) are simultaneously true from different structural positions. The false summit (Mountain) is rejected by the structural data: networks are not natural law; they are contingent institutional arrangements that can be reformed through transparency, open-source alternatives, and deliberate gatekeeping reduction. The classification landscape is resolved not by choosing one type but by mapping the presheaf of types across the observation space.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    network_access_counterfactual,
    'How much of the elite network''s competitive advantage derives from genuine information advantage vs. purely from access gatekeeping?',
    'Comparative analysis of network member performance when information asymmetries are reduced (via open publication, transparent hiring, mentorship programs); tracking of outsider performance when given equivalent information access',
    'If advantage is primarily informational: network functions as rope (genuine coordination). If advantage is purely gatekeeping: network functions as snare (pure extraction with coordination cover story).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_access_counterfactual, empirical, 'Source of network competitive advantage: information vs. gatekeeping').

omega_variable(
    meritocratic_measurement_validity,
    'Are the metrics elite networks use to assess ''merit'' measuring actual competence or are they measuring credential conformity that correlates with network membership?',
    'Independent performance audits of network-selected vs. non-network-selected candidates in identical roles; analysis of credential inflation over time; comparison of predictive validity of network selection criteria vs. alternative assessment methods',
    'If network criteria measure genuine merit: networks are legitimate coordinators. If criteria measure conformity to network norms: extraction mechanism is disguised as quality control.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(meritocratic_measurement_validity, empirical, 'Whether elite network merit criteria measure competence or credential conformity').

omega_variable(
    institutional_legitimacy_time_horizon,
    'At what extraction threshold does institutional legitimacy collapse and elite networks face organizational exit by members or denial of access by external parties?',
    'Historical analysis of institutional crises triggered by network-based allocation failures; measurement of legitimacy decay as extraction becomes visible; tracking of alternative institution formation when primary networks become visibly extractive',
    'If threshold is high: networks can sustain high extraction indefinitely. If threshold is low: extraction mechanisms face forced evolution toward greater transparency or loss of institutional authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_legitimacy_time_horizon, empirical, 'Legitimacy collapse threshold for visibly extractive elite networks').

omega_variable(
    identity_lock_versus_external_constraint,
    'For aspiring elites trapped in networks, how much of the constraint is structural (economic dependence, lack of alternative pathways) vs. identity-locked (internalization of network''s status hierarchy as the only legitimate marker of worth)?',
    'Post-exit analysis of individuals who leave networks: do they reconstruct identity and pursue alternative status markers, or do they maintain psychological dependence on network validation? Comparison of exit outcomes for individuals with vs. without identity fusion.',
    'If primarily structural: constraint is high-suppression snare. If significant identity-lock: constraint maintains control even after structural exit, creating durable extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_versus_external_constraint, empirical, 'Identity-lock vs. structural constraint in elite network dependency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(elite_coordination, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(elit_tr_t0, elite_coordination, theater_ratio, 0, 0.45).
narrative_ontology:measurement(elit_tr_t5, elite_coordination, theater_ratio, 5, 0.58).
narrative_ontology:measurement(elit_tr_t10, elite_coordination, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(elit_be_t0, elite_coordination, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(elit_be_t5, elite_coordination, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(elit_be_t10, elite_coordination, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(elite_coordination, identity_coordination).
narrative_ontology:affects_constraint(elite_coordination, educational_credential_filtering).
narrative_ontology:affects_constraint(elite_coordination, labor_market_stratification).
narrative_ontology:affects_constraint(elite_coordination, institutional_diversity_gap).

% DUAL FORMULATION NOTE:
% Elite coordination operates at multiple scales: within organizations (hiring networks), across sectors (professional guilds), and nationally (elite institution clustering). This story models the general mechanism; domain-specific instances (legal networks, financial networks, academic networks) have their own constraint stories linked via network.affects_constraints. The general story's extractiveness (0.58) represents an average; specific instances vary with the tightness of gatekeeping and availability of alternative pathways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(elite_coordination, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
