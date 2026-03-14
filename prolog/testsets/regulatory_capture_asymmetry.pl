% ============================================================================
% CONSTRAINT STORY: regulatory_capture_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_regulatory_capture_asymmetry, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: regulatory_capture_asymmetry
 *   human_readable: Regulatory Capture Asymmetry
 *   domain: political_economy/regulation
 *
 * SUMMARY:
 *   Regulatory capture represents an asymmetry between a concentrated,
 *   organized beneficiary (the regulated industry) and a dispersed,
 *   politically weakly organized victim (the general public and regulatory
 *   integrity itself). The constraint operates at the intersection of three
 *   structural facts: (1) regulatory agencies depend on industry expertise
 *   and legislative goodwill to function; (2) the regulated industry has
 *   concentrated incentives and resources to shape rules in its favor; (3)
 *   the diffuse public bearing the costs of lax regulation cannot organize as
 *   a counterweight. The constraint exhibits all six DR types from different
 *   perspectives, revealing how the same structural capture mechanism appears
 *   as pure coordination (rope, from the industry perspective), pure
 *   extraction (snare, from the public perspective), degraded ritual (piton,
 *   from the formal democratic process perspective), mixed
 *   coordination-extraction (tangled_rope, from both the civil society and
 *   captured regulator perspectives), a temporary problem with international
 *   sunset (scaffold, from the global regulatory competition perspective),
 *   and an inevitable law of bureaucratic structure (mountain, from the
 *   analytical naturalization perspective). The theater ratio (0.68) reflects
 *   that formal rulemaking processes — public comment periods, impact
 *   assessments, stakeholder meetings — are substantially performative: they
 *   create legitimacy for outcomes already determined by industry-regulator
 *   negotiation, without systematically changing outcomes. The extractiveness
 *   trajectory (0.42 → 0.58 over the interval) shows capture intensifying as
 *   regulatory complexity increases and industry resources for
 *   influence-seeking grow faster than public interest organization capacity.
 *
 * KEY AGENTS:
 *   - General Public: Primary victim (powerless/trapped) — dispersed, unorganized, bears costs of regulatory failure; cannot exit national jurisdiction
 *   - Regulatory Integrity: Primary victim (institutional/trapped) — abstract collective good; professional norms and enforcement capability degraded by capture
 *   - Regulated Industry: Primary beneficiary (institutional/arbitrage) — concentrated, organized, captures rulemaking through multiple channels (expertise access, revolving door, lobbying)
 *   - Civil Society / Advocacy Organizations: Organized opposition (organized/constrained) — mixed role: coordinate public interest representation but constrained by resource asymmetry and regulatory access barriers
 *   - Captured Regulatory Agency: Institutional actor caught between mandates (institutional/constrained, identity_locked) — bears coordination responsibility (public safety) but career-dependent on industry relationships; professional identity fused with regulated sector
 *   - International Regulatory Competition: Systemic pressure (organized/constrained) — creates generational sunset as global standards tighten and reputational costs of capture increase
 *   - Democratic Legitimacy Process: Formal institutional theater (institutional/arbitrage) — maintains legitimacy through procedural compliance without constraining outcomes; enables extraction through ritualization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(regulatory_capture_asymmetry, 0.58).
domain_priors:suppression_score(regulatory_capture_asymmetry, 0.65).
domain_priors:theater_ratio(regulatory_capture_asymmetry, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(regulatory_capture_asymmetry, extractiveness, 0.58).
narrative_ontology:constraint_metric(regulatory_capture_asymmetry, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(regulatory_capture_asymmetry, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(regulatory_capture_asymmetry, tangled_rope).
narrative_ontology:human_readable(regulatory_capture_asymmetry, "Regulatory Capture Asymmetry").
narrative_ontology:topic_domain(regulatory_capture_asymmetry, "political_economy/regulation").

domain_priors:requires_active_enforcement(regulatory_capture_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(regulatory_capture_asymmetry, regulated_industry).
narrative_ontology:constraint_victim(regulatory_capture_asymmetry, general_public).
narrative_ontology:constraint_victim(regulatory_capture_asymmetry, regulatory_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GENERAL PUBLIC (SNARE) — Powerless citizens cannot exit the regulatory jurisdiction; bear full cost of lax enforcement through reduced safety, environmental contamination, or financial harm. No collective organizing capacity. Maximum experienced extraction — cannot perceive the capture mechanism that operates in bureaucratic chambers far removed from daily awareness.
constraint_indexing:constraint_classification(regulatory_capture_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CIVIL SOCIETY / ADVOCACY (TANGLED ROPE) — Organized agents with genuine coordination function (public interest representation, watchdog oversight) but constrained by resource asymmetry, regulatory access barriers, and litigation costs. Some exit via media and litigation but at high cost. Mixed experience: enabling oversight infrastructure alongside asymmetric extraction through institutional bias.
constraint_indexing:constraint_classification(regulatory_capture_asymmetry, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REGULATED INDUSTRY (ROPE) — Net beneficiary with high exit optionality through revolving door, regulatory arbitrage, and forum shopping. Experiences the constraint as pure coordination: organizing industry standards, accessing regulatory expertise, influencing policy through normal channels. Extraction runs toward this agent — they subsidize their own capture through lobbying that stabilizes favorable rules.
constraint_indexing:constraint_classification(regulatory_capture_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CAPTURED REGULATOR (TANGLED ROPE) — Institutional actor with genuine coordination function (public safety, market integrity) but constrained by career dependence on industry relationships, regulatory budget dependence on industry-friendly legislation, and professional identity fusion with the regulated sector. Can exit through career change but at high personal cost — identity_locked dynamics operate at institutional level. Mixed experience: some enforcement function alongside asymmetric extraction flowing upward to industry.
constraint_indexing:constraint_classification(regulatory_capture_asymmetry, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL REGULATORY COMPETITION (SCAFFOLD) — Jurisdictions with stricter regulatory standards create exit pressures (regulatory arbitrage in reverse): firms face reputational cost or market access barriers in jurisdictions with stronger enforcement. This creates a generational sunset: as global norms tighten (ESG mandates, GDPR, carbon pricing), captured regulators lose extraction value — companies cannot hide behind loose home-jurisdiction rules. Temporary coordination with declining extraction as international standards converge.
constraint_indexing:constraint_classification(regulatory_capture_asymmetry, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: DEMOCRATIC LEGITIMACY THEATER (PITON) — The formal regulatory process (public comment periods, impact assessments, rulemaking procedures) is largely performative. Industry comments are systematically given greater weight; public interest comments are performatively incorporated but rarely change outcomes. The theater persists because it legitimizes regulatory decisions without meaningfully constraining industry influence. High extractiveness despite formal democratic procedures — the ritual maintains institutional inertia without functional constraint.
constraint_indexing:constraint_classification(regulatory_capture_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, regulatory capture appears as an inevitable consequence of information asymmetry: the regulated industry has more knowledge of compliance costs than regulators; hence industry influence is inherent to regulation. This perspective naturalizes capture as a law of bureaucratic structure. However, structural data contradicts this — jurisdictions with strong independence institutions, transparent rulemaking, and whistleblower protections show measurably lower capture. The mountain classification reveals false naturalization of a contingent institutional design.
constraint_indexing:constraint_classification(regulatory_capture_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regulatory_capture_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(regulatory_capture_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(regulatory_capture_asymmetry, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(regulatory_capture_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(regulatory_capture_asymmetry, TR),
    TR >= 0.70.

:- end_tests(regulatory_capture_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Substantial, asymmetric. The regulated industry receives direct benefits — favorable rule interpretation, delayed enforcement, regulatory arbitrage windows — while the public bears costs through reduced safety, environmental damage, financial instability, or public health impacts. The 0.58 value reflects that extraction is significant but not total: some regulatory functions (data gathering, baseline enforcement) persist; some industry preferences are denied. Suppression (0.65): High. Multiple mechanisms prevent public exit or retaliation: jurisdictional lock (cannot move to different regulatory regime without relocating entire enterprise or life), information asymmetry (regulatory proceedings use technical language and occur in venues inaccessible to unorganized citizens), political economy barriers (concentrated industry funding gives outsized political voice relative to dispersed public interest), and structural marginalization of diffuse stakeholders in rulemaking. Theater ratio (0.68): High and rising. Formal rulemaking procedures (public comment, environmental impact assessments, notice-and-comment rulemaking) create legitimacy without constraint — studies show industry comments receive systematic weight advantages; public interest comments are incorporated performatively without outcome changes. The theater increases over the interval as regulatory complexity grows and industry funding for expertise-based comment increases relative to public interest capacity.
 *
 * PERSPECTIVAL GAP:
 *   The regulated industry sees Rope — they view the constraint as pure coordination: standardizing compliance pathways, accessing regulatory expertise, organizing industry best practices. This is their genuine experience: the rulemaking process works for them, enabling coordination at low cost. Civil society sees Tangled Rope — they see both genuine coordination function (regulatory oversight provides baseline safety) and asymmetric extraction (industry bias pervades outcomes). The captured regulator also sees Tangled Rope but with different emphasis: they genuinely coordinate public safety functions while being constrained by industry-dependence and career lock-in; the extraction is experienced as reluctant compromise rather than intentional. The public sees Snare — they have no voice in rulemaking, cannot exit, and bear extraction costs through regulatory failure. International observers see Scaffold — global regulatory competition and reputation costs create generational pressure for tightening, suggesting the capture extractiveness will decline as standards converge. The formalist sees Piton — the democratic legitimacy procedures are theater masking industry domination. The analytical naturalization perspective sees Mountain — capture appears inevitable given information asymmetry — but this is revealed as false naturalization; jurisdictions with strong independence mandates show dramatically lower capture rates.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are determined by each agent's structural position. (1) General public: powerless/trapped → d ≈ 0.95, full target, maximum f(d). (2) Regulated industry: institutional/arbitrage → d ≈ 0.05, full beneficiary, negative f(d), extraction flows toward them. (3) Captured regulator: institutional/constrained + identity_locked → d ≈ 0.40, mixed; the identity lock adds cognitive capture layer (internalized preference for industry success) on top of structural dependence. (4) Civil society: organized/constrained → d ≈ 0.65, moderate target, partial agency through advocacy capacity. (5) International competition: organized/constrained → d ≈ 0.50, symmetric; neither fully captures nor fully escapes, creates pressure for rule change. The perspectival gap is measured by the range of chi values these directionalities produce when multiplied by f(d) and scope σ. The regulated industry experiences χ ≈ -0.05 (subsidized); the public experiences χ ≈ 1.20+ (heavily extracted); the regulator experiences χ ≈ 0.45 (mixed, with internalized identity making extraction feel natural).
 *
 * MANDATROPHY ANALYSIS:
 *   Regulatory capture resolves the mandatrophy by demonstrating that the classification depends entirely on observational position. From the industry perspective, the constraint is pure coordination (Rope) — they genuinely benefit from regulatory clarity and access. From the public perspective, it is pure extraction (Snare) — they bear costs without voice. From the regulator's perspective, it is mixed coordination-extraction (Tangled Rope) with identity-lock elements — the regulator is caught between genuine public safety mandates and professional identity fused with industry success. From the international perspective, it is temporary (Scaffold) — global standards tighten, reducing local extraction value. From the formalist perspective, it is degraded ritual (Piton) — the democratic process persists as theater. The mountain naturalization is false — it treats contingent institutional design (budget dependence, revolving door, information asymmetry) as inevitable laws of nature. The true diagnosis: regulatory capture is a tangled_rope that APPEARS to be rope (to the beneficiary), snare (to the victim), and mountain (to the naturalizing analyst), precisely because institutional design enables the beneficiary to export the extraction cost to the victim while the machinery of legitimacy makes this invisible. The mandatrophy is resolved: all six types are correct perspectival readings of the SAME structural mechanism, viewed from different positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capture_mechanism_attribution,
    'Is capture driven by information asymmetry (industry knows compliance costs regulators cannot), revolving door employment (career incentives), or regulatory resource dependence (agencies funded by industry-friendly legislation)?',
    'Comparative institutional analysis: jurisdictions with high transparency + revolving-door restrictions vs. jurisdictions with low capture; mechanism attribution via natural experiment.',
    'If information asymmetry dominates: capture is structural, difficult to reverse. If revolving-door incentives dominate: conflict-of-interest rules can reduce capture significantly. If funding structure dominates: budget independence can break the cycle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capture_mechanism_attribution, empirical, 'Primary causal mechanism underlying regulatory capture').

omega_variable(
    public_interest_identification,
    'In complex technical domains (pharmaceuticals, financial regulation, environmental standards), can non-industry stakeholders credibly represent the diffuse public interest against concentrated industry expertise?',
    'Analysis of advocacy organization capacity: funding, technical expertise, comment quality in regulatory proceedings; correlation between advocacy capacity and regulatory outcome variance.',
    'If public interest cannot credibly organize: capture appears as inevitable equilibrium. If capacity gaps are institutional rather than fundamental: capacity-building reforms (public funding for advocacy, technical support) could shift outcomes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_interest_identification, empirical, 'Feasibility of effective public interest representation').

omega_variable(
    identity_lock_reversibility,
    'Once a regulator is professionally identity-fused with the regulated industry (career path dependence, peer networks, technical expertise rooted in industry problems), can regulatory independence be recovered through institutional reform or does capture persist through internalized identity?',
    'Longitudinal institutional analysis: regulators with strong independence mandates (e.g., Bank of England post-1997 independence) vs. captured regulators; measurement of post-reform behavior, career trajectories, and enforcement patterns.',
    'If identity-lock is reversible via institutional change: independence mandates can break capture. If identity-lock is persistent: personnel turnover may be necessary condition for capture reversal.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_reversibility, empirical, 'Reversibility of identity-based regulatory capture at institutional level').

omega_variable(
    sunset_plausibility,
    'Can international regulatory competition actually force regulatory tightening, or do captured jurisdictions protect industry by defecting from international standards (regulatory race to the bottom)?',
    'Historical analysis of regulatory harmonization: when do international standards cause regulatory tightening vs. when do they trigger regulatory evasion (relocation, restructuring, jurisdictional shopping)?',
    'If international pressure causes tightening: scaffold perspective is correct, sunset is real. If jurisdictional competition overwhelms harmonization: scaffold is aspirational, capture persists indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_plausibility, empirical, 'Plausibility of international regulatory competition as sunset mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(regulatory_capture_asymmetry, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(regcap_tr_t0, regulatory_capture_asymmetry, theater_ratio, 0, 0.55).
narrative_ontology:measurement(regcap_tr_t10, regulatory_capture_asymmetry, theater_ratio, 10, 0.62).
narrative_ontology:measurement(regcap_tr_t20, regulatory_capture_asymmetry, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(regcap_be_t0, regulatory_capture_asymmetry, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(regcap_be_t10, regulatory_capture_asymmetry, base_extractiveness, 10, 0.51).
narrative_ontology:measurement(regcap_be_t20, regulatory_capture_asymmetry, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(regulatory_capture_asymmetry, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(regulatory_capture_asymmetry, 0.12).
narrative_ontology:affects_constraint(regulatory_capture_asymmetry, revolving_door_career_dynamics).
narrative_ontology:affects_constraint(regulatory_capture_asymmetry, regulatory_expertise_asymmetry).
narrative_ontology:affects_constraint(regulatory_capture_asymmetry, industry_lobbying_resource_concentration).

% DUAL FORMULATION NOTE:
% Regulatory capture asymmetry is upstream of three more specific constraints: the personal incentive structure of the revolving door, the information/expertise asymmetry in technical rulemaking, and the resource concentration enabling industry lobbying. Each has its own extractiveness value and decomposition. The parent constraint (regulatory_capture_asymmetry) operates at the institutional-structural level; the child constraints operationalize specific mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(regulatory_capture_asymmetry, institutional, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
