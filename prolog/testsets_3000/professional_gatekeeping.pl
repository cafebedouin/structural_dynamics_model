% ============================================================================
% CONSTRAINT STORY: professional_gatekeeping
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_professional_gatekeeping, []).

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
 *   constraint_id: professional_gatekeeping
 *   human_readable: Professional Gatekeeping Mechanisms
 *   domain: labor/professional/institutional
 *
 * SUMMARY:
 *   Professional gatekeeping mechanisms — credentialing requirements,
 *   licensure exams, apprenticeship mandates, degree requirements — create a
 *   structural tension between public protection (coordination function) and
 *   incumbent professional extraction (rent-seeking function). The same
 *   credentialing apparatus can be analyzed from eight distinct perspectives,
 *   each producing a different classification. The constraint exhibits all
 *   characteristics of a tangled rope: genuine coordination function
 *   (ensuring practitioner competence and protecting public safety), active
 *   enforcement (regulatory bodies, professional associations maintaining
 *   requirements), and asymmetric extraction (benefits accrue to incumbents
 *   and credentialing bodies; costs borne by aspirants and the field's
 *   innovation capacity). However, the presence of piton and scaffold
 *   perspectives reveals that much of the credentialing theater is legacy
 *   ritual (exams, apprenticeship hours, degree requirements) while
 *   alternative credentialing pathways (skills assessment, portfolio review,
 *   platform reputation) demonstrate that the coordination function could be
 *   served with lower theater and extraction. The extractiveness measurement
 *   trajectory (0.35 → 0.58 over 20 years) reflects incumbent associations
 *   successfully raising barriers and credentialing bodies expanding
 *   requirements in response to labor market oversupply, increasing the
 *   extraction mechanism even as the public safety benefit remains constant.
 *   The theater ratio increase (0.48 → 0.62) indicates that formal
 *   requirements are becoming increasingly performative relative to actual
 *   competence assessment.
 *
 * KEY AGENTS:
 *   - Aspiring Practitioners: Primary victims (powerless/trapped) — bear full credentialing cost with no exit option; must comply to enter profession
 *   - Incumbent Professionals: Primary beneficiaries (organized/mobile) — gain wage and prestige protection through credential scarcity; collectively maintain gatekeeping mechanism
 *   - Credentialing Bodies: Secondary beneficiary (institutional/arbitrage) — gain institutional legitimacy, control, and revenue from monopoly on credential issuance; have full exit capacity but use gatekeeping as coordination mechanism
 *   - Regulatory Authority: Dual-role institutional actor (powerful/constrained) — mandated to serve public protection but also serves incumbent interests; constrained by competing pressures and political realities
 *   - Adjacent Field Practitioners: Secondary victims (moderate/constrained) — have relevant skills but face retraining barriers; some exit capacity but at high cost
 *   - Alternative Credentialing Movement: Organized agents (organized/constrained) — platforms, skills-based employers, micro-credential systems building temporary alternative pathways; see sunset logic as constraint degrades
 *   - Field Innovation as Abstraction: Systemic victim (powerless/trapped) — credential barriers reduce field mobility and diversity of approaches; cannot exit; abstract good with no advocate
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing institutional arrangements as inherent to expertise itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(professional_gatekeeping, 0.58).
domain_priors:suppression_score(professional_gatekeeping, 0.68).
domain_priors:theater_ratio(professional_gatekeeping, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(professional_gatekeeping, extractiveness, 0.58).
narrative_ontology:constraint_metric(professional_gatekeeping, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(professional_gatekeeping, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(professional_gatekeeping, tangled_rope).
narrative_ontology:human_readable(professional_gatekeeping, "Professional Gatekeeping Mechanisms").
narrative_ontology:topic_domain(professional_gatekeeping, "labor/professional/institutional").

domain_priors:requires_active_enforcement(professional_gatekeeping).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(professional_gatekeeping, incumbent_professionals).
narrative_ontology:constraint_beneficiary(professional_gatekeeping, credentialing_bodies).
narrative_ontology:constraint_victim(professional_gatekeeping, aspiring_practitioners).
narrative_ontology:constraint_victim(professional_gatekeeping, field_innovation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ASPIRING PRACTITIONER (SNARE) — Faces mandatory credentialing requirements, high financial and time barriers to entry, and limited alternative pathways. Cannot practice the profession without credentials; cannot obtain credentials without prohibitive investment. No exit from the requirement; maximum experienced extraction through exclusive access control.
constraint_indexing:constraint_classification(professional_gatekeeping, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ADJACENT FIELD PRACTITIONER (TANGLED ROPE) — Could apply knowledge from related field but faces credentialing barriers requiring return to formal education. Has some exit options (stay in adjacent field, retrain) but at high cost. Benefits from professional standards maintaining field reliability; bears extraction through credential requirement forcing retraining.
constraint_indexing:constraint_classification(professional_gatekeeping, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CREDENTIALING BODY (ROPE) — Sees gatekeeping as coordination: maintaining professional standards, protecting public safety, ensuring competence. Has full exit capacity (could operate differently) but instead uses the constraint as its primary coordination mechanism. Net beneficiary through credential monopoly and institutional legitimacy.
constraint_indexing:constraint_classification(professional_gatekeeping, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INCUMBENT PROFESSIONAL ASSOCIATION (ROPE) — Organized actors who benefit from credential scarcity maintaining wage floors and professional prestige. Perceive gatekeeping as coordination mechanism protecting field standards. Have sufficient collective power to maintain exit option (could lobby for deregulation) but choose enforcement. Experience extraction as minimal — they are net extractors.
constraint_indexing:constraint_classification(professional_gatekeeping, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY AUTHORITY (TANGLED ROPE) — Mandated to ensure public safety through credentialing (coordination function) but also serves incumbent professionals' licensing interests (extraction mechanism). Faces political constraints: cannot easily lower standards without public backlash, cannot easily tighten without industry opposition. Constrained by competing mandates; benefits from gatekeeping through legitimacy and reduced liability.
constraint_indexing:constraint_classification(professional_gatekeeping, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: LEGACY CERTIFICATION RITUAL (PITON) — Many credentialing requirements (licensure exams, apprenticeship hours, degree mandates) persist through institutional inertia despite technological alternatives (skills assessment, demonstrated competence, portfolio review). The certification theater is maintained because alternatives haven't fully replaced it, not because it optimally serves public protection. Theater ratio reflects that much of the credentialing process is performative compliance rather than functional verification.
constraint_indexing:constraint_classification(professional_gatekeeping, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ALTERNATIVE CREDENTIALING MOVEMENT (SCAFFOLD) — Organized agents (online platforms, skills-based hiring, portfolio assessment, micro-credentials) are building temporary alternative pathways to professional practice that bypass traditional gatekeeping. See the constraint as a temporary coordination failure with sunset logic: as employers grow confident in skills-based hiring and platform reputation systems mature, traditional credential requirements lose force. Low effective extraction from this perspective because the coalition has agency and visibility on exit path.
constraint_indexing:constraint_classification(professional_gatekeeping, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal perspective, some form of professional boundary-maintenance is inherent to any knowledge domain: communities must distinguish qualified from unqualified practitioners, and this distinction is a structural feature of expertise itself. This perspective risks naturalizing contingent institutional arrangements (specific credentialing monopolies, exam structures, duration requirements) as immutable laws of how professional knowledge functions.
constraint_indexing:constraint_classification(professional_gatekeeping, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(professional_gatekeeping_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(professional_gatekeeping, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(professional_gatekeeping, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(professional_gatekeeping, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(professional_gatekeeping, TR),
    TR >= 0.70.

:- end_tests(professional_gatekeeping_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Credentialing requirements impose substantial financial and temporal costs on aspirants (degree programs cost $50k-$300k; professional licensing exams cost $500-$5000; total entry investment often 4-8 years post-secondary education). These costs are not proportionate to demonstrated public safety benefits for many professions — barrier levels vary by profession with weak correlation to actual public risk. The trajectory shows extractiveness increasing (0.35→0.58) as incumbent associations raise requirements in response to oversupply. This is extraction acceleration, not coordination deepening. Suppression (0.68): High. Barriers include mandatory credential requirements (cannot legally practice without), high financial cost (excluding low-income entrants), geographic concentration of credentialing bodies (creates inequality of access), time investment (blocking career switches), and credential specificity (low transferability across jurisdictions). These are structural suppressions that aspirants cannot circumvent. Incumbent professionals actively lobby to maintain high barriers. Theater ratio (0.62): Moderate-high. Much credentialing is performative: exams test rote knowledge rather than competence; apprenticeship hour requirements are time-serving rather than skill-targeted; degree programs include irrelevant coursework; continuing education credits often consist of low-value compliance activity. Alternative assessment methods (skills portfolios, demonstrated competence, platform reputation, client outcomes) often correlate better with actual competence than formal credentials. The theater has increased as credentialing shifted from functional skills assessment (apprenticeship model) to formal ritual (degree/exam model). Claimed type (tangled_rope) is diagnostically correct: genuine coordination function (protecting public by vetting practitioners) plus asymmetric extraction (benefits to incumbents through wage/prestige protection and supply restriction). Active enforcement is necessary to maintain the constraint: without licensing boards, exam requirements, and professional associations enforcing credential monopoly, the constraint would collapse.
 *
 * PERSPECTIVAL GAP:
 *   Eight distinct perspectives on the same structural constraint produce classification variance from Mountain to Snare. This variance reflects genuine structural differences in how agents experience the constraint, not disagreement about facts. The aspiring practitioner literally cannot exit (trapped) while the incumbent professional could but doesn't (arbitrage). The credentialing body believes it serves public protection; the aspiring practitioner experiences pure extraction. The alternative credentialing platforms prove the coordination function could be achieved with lower barriers, revealing that current barriers serve extraction in addition to coordination. This perspectival gap is the diagnostic signature of a Tangled Rope — if all perspectives agreed on Snare, it would be pure extraction; if all agreed on Rope, it would be pure coordination. The variance itself signals the hybrid.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim status and exit options. Aspiring practitioners are victims facing trapped exit (no way to practice without credentials) — their d approaches 1.0 (full target), producing maximum experienced extraction χ. Incumbent professionals are beneficiaries with mobile/arbitrage exit (could support deregulation but choose not to) — their d is low (0.1-0.2), producing negative or minimal experienced extraction. Credentialing bodies are beneficiaries with arbitrage exit (could operate differently but benefit from current structure) — their d is institutional/low. Regulatory authorities face constrained exit (mandated to regulate but also pressed by incumbents) — their d is moderately elevated, producing moderate χ. Adjacent field practitioners are victims with constrained exit (could switch but high retraining cost) — their d is moderate-high. Alternative credentialing platforms are organized agents with constrained exit but growing agency — their d decreases over time as alternatives mature. The analytical observer's d is canonical analytical (0.73) but the mountain classification it produces is a false summit (the accessibility_collapse and resistance metrics are low because the constraint is not immutable — alternative credentialing proves this).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that professional gatekeeping simultaneously coordinates (ensures practitioner competence) and extracts (restricts supply, protects incumbent wages, generates credentialing monopoly rent). The mandatrophy resolution is NOT 'which type is correct?' but 'both functions are real, and the constraint's extractiveness reflects their mix.' The aspiring practitioner's Snare perspective is the extractive component made visible. The incumbent professional's Rope perspective is the coordination component made visible. The alternative credentialing movement's Scaffold perspective proves the coordination function could exist with lower extraction — demonstrating that current extraction levels exceed what coordination requires. The piton perspective reveals that much of the credentialing architecture is legacy theater (exams, hour requirements, degree mandates) rather than optimally-designed coordination. The mandatrophy is resolved by accepting that institutional design choices — credential specificity, geographic concentration, retraining barriers, exam structure, degree requirements — determine how much coordination function gets bundled with extraction. Some gatekeeping is inevitable (expertise requires some boundary maintenance); current gatekeeping intensity is contingent (alternative designs are viable). The extractiveness score (0.58) reflects that current design overweights extraction relative to coordination need.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credential_signal_validity,
    'To what extent do formal credentials (degrees, licenses, certifications) actually predict practitioner competence and public safety outcomes versus serving as pure status signaling and access control?',
    'Outcome analysis: correlation studies between credential attainment and competence measures (client satisfaction, error rates, ethics violations, career success); comparison of credentialed vs non-credentialed practitioners in fields with mixed entry pathways; historical analysis of credential requirement introduction vs public safety metric changes',
    'If credentials strongly predict competence: gatekeeping serves genuine coordination function; classification shifts toward Rope across perspectives. If credentials are primarily signaling: gatekeeping is pure extraction; classification shifts toward Snare for aspirants and Piton for legacy requirements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credential_signal_validity, empirical, 'Whether credentials predict competence or serve primarily as status signaling').

omega_variable(
    alternative_pathway_viability,
    'Can skills-based assessment, portfolio review, apprenticeship, and platform reputation systems reliably serve the public protection function that traditional credentialing claims to provide?',
    'Pilot programs with alternative credentialing (apprenticeships, portfolio-based hiring, platform reputation); longitudinal comparison of outcomes; analysis of failure modes in alternative systems; public safety metric correlation',
    'If alternatives are viable: scaffold perspective is structural, sunset is real, current gatekeeping is unjustified extraction. If alternatives fail: gatekeeping remains necessary coordination; current mechanisms may be imperfect but non-optional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_pathway_viability, empirical, 'Whether alternative credentialing systems can serve public protection function').

omega_variable(
    credentialing_cost_distribution_justification,
    'Is the financial and temporal burden of credentialing justified by the public safety benefit produced, or does it primarily redistribute wealth from aspirants to incumbents and credentialing bodies?',
    'Cost-benefit analysis: total cost of credentialing system vs public safety gains; comparison across countries with different requirement levels and entry barriers; analysis of rent extraction vs coordination function by credential type',
    'If justified: suppression levels are acceptable as necessary friction; constraint remains tangled_rope. If unjustified: suppression is pure extraction mechanism; constraint degrades to snare-dominant.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(credentialing_cost_distribution_justification, empirical, 'Whether credentialing costs are proportionate to public safety benefit').

omega_variable(
    regulatory_capture_mechanism,
    'To what extent does incumbent professional control over credentialing bodies create regulatory capture that allows credentialing to function as wage/prestige protection rather than public protection?',
    'Institutional analysis: composition of credentialing board memberships, lobbying influence patterns, credential requirement changes correlated with incumbent wage data; comparison of requirements across jurisdictions; analysis of requirement changes following incumbent organizing vs following public safety incidents',
    'If capture is strong: extraction mechanism is structural; credentialing bodies cannot be reformed to lower barriers. If capture is weak: barriers could be reduced without compromising public protection; current distribution is contingent rather than inevitable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_capture_mechanism, empirical, 'Extent of incumbent professional control over credentialing bodies').

omega_variable(
    identity_lock_duration,
    'For practitioners identity-locked into credentialing requirements (professional identity fused with formal credentials), would removing barriers actually enable career switching or would the identity lock persist as internalized suppression?',
    'Longitudinal analysis of practitioners in fields with recent credential deregulation or alternative pathways; survey of aspiring practitioners'' actual vs stated barriers; analysis of suppression patterns post-exit from active gatekeeping mechanisms',
    'If identity lock is strong: removing barriers alone is insufficient; practitioners carry suppression internalized. If identity lock is weak: barrier removal enables actual mobility; suppression is primarily structural.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_duration, empirical, 'Whether identity-lock suppression persists after barrier removal').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(professional_gatekeeping, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(profgate_tr_t0, professional_gatekeeping, theater_ratio, 0, 0.48).
narrative_ontology:measurement(profgate_tr_t10, professional_gatekeeping, theater_ratio, 10, 0.55).
narrative_ontology:measurement(profgate_tr_t20, professional_gatekeeping, theater_ratio, 20, 0.62).

% Extraction over time
narrative_ontology:measurement(profgate_be_t0, professional_gatekeeping, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(profgate_be_t10, professional_gatekeeping, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(profgate_be_t20, professional_gatekeeping, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(professional_gatekeeping, identity_coordination).
narrative_ontology:affects_constraint(professional_gatekeeping, labor_market_supply_restriction).
narrative_ontology:affects_constraint(professional_gatekeeping, occupational_licensing_regulatory_capture).
narrative_ontology:affects_constraint(professional_gatekeeping, professional_identity_lock).

% DUAL FORMULATION NOTE:
% Professional gatekeeping decomposes into at least three structurally distinct constraints: (1) competence verification (genuine coordination, low ε), (2) supply restriction through credential scarcity (extraction, high ε), (3) incumbent wage/prestige protection (extraction, high ε). This story treats the combined apparatus; decomposition into separate stories would isolate the coordination from the extraction mechanisms and reveal that alternative designs could preserve (1) while reducing (2) and (3).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(professional_gatekeeping, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
