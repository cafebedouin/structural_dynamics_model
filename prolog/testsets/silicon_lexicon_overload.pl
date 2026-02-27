% ============================================================================
% CONSTRAINT STORY: silicon_lexicon_overload
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_silicon_lexicon_overload, []).

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
 *   constraint_id: silicon_lexicon_overload
 *   human_readable: The Silicon Lexicon (Corporate Tech-Speak)
 *   domain: linguistic/technological/social
 *
 * SUMMARY:
 *   The Silicon Lexicon—corporate tech-speak characterized by terms like
 *   'bandwidth,' 'alignment,' 'synergy,' 'pivot,' 'leverage,' and 'core
 *   competency'—has evolved from specialized technical vocabulary into a
 *   mandatory performance protocol in professional environments. This
 *   constraint exhibits the full range of DR classification depending on the
 *   observer's structural position within the corporate hierarchy and
 *   linguistic community. The lexicon simultaneously serves genuine
 *   coordination functions (enabling precise technical discourse among
 *   engineers and product teams) and functions as an extraction mechanism
 *   (gatekeeping career advancement, status signaling, obscuring actual
 *   business outcomes). The theater ratio has risen from 0.55 to 0.81 over
 *   the interval, indicating increasing dominance of performative over
 *   functional use. Terms that once had specific technical
 *   meaning—'bandwidth' (data transfer capacity), 'alignment' (agreement on
 *   objectives)—have become increasingly abstract and metaphorical, deployed
 *   primarily to signal professional belonging rather than to convey precise
 *   information. The constraint requires active enforcement: organizations
 *   must continually inject new jargon to maintain distinction and avoid
 *   clarity, suggesting Tangled Rope rather than pure Rope (which would
 *   evolve naturally without enforcement).
 *
 * KEY AGENTS:
 *   - Tech Industry Insiders: Primary beneficiaries (institutional/arbitrage) — can code-switch between jargon and plain language, understood as legitimate speakers, benefit from epistemic precision and in-group identity
 *   - Non-Tech Employees: Primary victims (powerless/trapped) — mandatory adoption without comprehension, career penalty for non-fluency, constant status anxiety, forced cognitive labor
 *   - Management Consultants: Secondary beneficiary-victim (moderate/constrained) — benefit from jargon as gatekeeping tool and expertise signal, but also deploy overblown vocabulary that obscures value delivery
 *   - Corporate Communications Department: Institutional ritualist (institutional/arbitrage) — maintains performative vocabulary despite knowing it obscures meaning; sees own output as degraded
 *   - Plain Language Advocates: Organized change agents (organized/constrained) — pushing toward plain-language mandates and accessibility standards; building alternative pathways
 *   - Mid-Career Professionals: Mixed position (moderate/constrained) — benefit from jargon fluency through network access and perceived competence, constrained by continued performance requirements
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(silicon_lexicon_overload, 0.52).
domain_priors:suppression_score(silicon_lexicon_overload, 0.68).
domain_priors:theater_ratio(silicon_lexicon_overload, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(silicon_lexicon_overload, extractiveness, 0.52).
narrative_ontology:constraint_metric(silicon_lexicon_overload, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(silicon_lexicon_overload, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(silicon_lexicon_overload, tangled_rope).
narrative_ontology:human_readable(silicon_lexicon_overload, "The Silicon Lexicon (Corporate Tech-Speak)").
narrative_ontology:topic_domain(silicon_lexicon_overload, "linguistic/technological/social").

domain_priors:requires_active_enforcement(silicon_lexicon_overload).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(silicon_lexicon_overload, tech_industry_insiders).
narrative_ontology:constraint_beneficiary(silicon_lexicon_overload, management_consultants).
narrative_ontology:constraint_beneficiary(silicon_lexicon_overload, executive_class).
narrative_ontology:constraint_victim(silicon_lexicon_overload, linguistic_clarity).
narrative_ontology:constraint_victim(silicon_lexicon_overload, non_tech_employees).
narrative_ontology:constraint_victim(silicon_lexicon_overload, clarity_seekers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-TECH EMPLOYEE (SNARE) — Trapped in mandatory jargon adoption as a condition of employment and social participation in corporate environments. Cannot exit without career penalty. Must decode and perform fluency in specialized vocabulary regardless of comprehension. Maximum extraction: forced cognitive labor, constant status anxiety, incomprehension disguised as professional inadequacy.
constraint_indexing:constraint_classification(silicon_lexicon_overload, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-CAREER PROFESSIONAL (TANGLED ROPE) — Constrained exit: benefits from jargon fluency through improved collaboration within tech-adjacent fields, access to certain networks, and perceived competence. Also bears costs: cognitive load, performance anxiety, reduced communication clarity with non-fluent colleagues. Genuine coordination function (shared vocabulary enables specific technical discourse) but overlaid with asymmetric extraction (fluency confers status and gatekeeping advantage).
constraint_indexing:constraint_classification(silicon_lexicon_overload, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TECH INDUSTRY INSIDERS (ROPE) — Primary beneficiaries with arbitrage exit options (can translate between jargon and plain language, can choose when to deploy specialized vocabulary, understood as legitimate speakers). Experience the lexicon as coordination mechanism enabling precise technical discourse and professional identity. Low extraction experienced by this group — vocabulary serves genuine epistemic and social coordination functions for their domain.
constraint_indexing:constraint_classification(silicon_lexicon_overload, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PLAIN LANGUAGE MOVEMENT (SCAFFOLD) — Organized actors (regulatory bodies, accessibility advocates, internal communications teams) pushing toward plain-language mandates and jargon audits. See the lexicon overload as a solvable coordination failure with a sunset: clear style guides, neurodivergent-friendly communication norms, and cross-functional translation protocols are building alternative pathways. Low theater because alternatives are functionally superior (reduced misunderstanding, faster onboarding, improved accessibility).
constraint_indexing:constraint_classification(silicon_lexicon_overload, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CORPORATE COMMUNICATIONS DEPARTMENT (PITON) — Maintains jargon performance rituals (town halls, mission statements, annual reports) despite knowing the lexicon obscures meaning. Theater ratio extremely high: vocabulary is used to signal alignment and professional identity rather than to convey information. The department sees its own output as degraded — jargon persists through organizational inertia and status signaling, not because it improves communication. Measurement of internal comprehension reveals theater: employees nod at 'synergy initiatives' and 'bandwidth optimization' without shared understanding.
constraint_indexing:constraint_classification(silicon_lexicon_overload, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: MANAGEMENT CONSULTANT CLASS (TANGLED ROPE) — Both benefits and victimizes: benefits from lexicon as gatekeeping tool (specialized vocabulary justifies high billing rates, creates perceived expertise), coordinates genuine (if often trivial) process improvements. Victimizes through overuse of jargon that obscures whether actual value is being delivered. Constrained exit: pivoting away from jargon would undermine professional authority and market positioning. Active enforcement required: continued deployment of neologisms and euphemisms maintains the extraction mechanism.
constraint_indexing:constraint_classification(silicon_lexicon_overload, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, technical jargon is inherent to complex fields: any specialized domain develops vocabulary to describe phenomena its members need to discuss. The lexicon overload might be naturalized as 'inevitable complexity' or 'necessary precision.' However, the structural data contradicts this — the constraint exhibits high suppression (0.68), high theater (0.81), and clear beneficiary/victim structure. The mountain classification fails the accessibility_collapse gate: accessibility to meaning is not collapsed (people can understand plain language explanations of the same concepts), and resistance to the constraint is substantial (plain language advocacy). This is a false summit naturalizing a contingent institutional gatekeeping arrangement.
constraint_indexing:constraint_classification(silicon_lexicon_overload, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(silicon_lexicon_overload_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(silicon_lexicon_overload, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(silicon_lexicon_overload, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(silicon_lexicon_overload, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(silicon_lexicon_overload, TR),
    TR >= 0.70.

:- end_tests(silicon_lexicon_overload_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts cognitive labor from non-fluent employees who must decode jargon, invest time in acquiring fluency, and experience ongoing status anxiety. The extraction is not maximal (0.70+) because some genuine epistemic coordination occurs—tech terminology does enable precise communication in specialized domains. The extraction increases over time (0.32 → 0.52) as jargon proliferates into domains where it serves no technical function (HR, sales, operations). Suppression (0.68): High. Significant barriers to resistance: (1) Career penalty for non-fluency—advancement and credibility depend on vocabulary adoption; (2) Organizational norming—jargon is embedded in internal communications, meetings, feedback, job descriptions; (3) Status signaling lock-in—once jargon becomes the currency of prestige, opting out signals incompetence or outsider status. Some suppression is coordinated (shared vocabulary enables technical work) but substantial suppression is coercive (mandatory adoption by non-specialists). Theater ratio (0.81): Very high. The majority of corporate jargon use is performative rather than functional. Employees use jargon to signal alignment in town halls, strategic planning meetings, and performance reviews where the vocabulary often obscures rather than clarifies. Measurement of comprehension vs. jargon density in internal communications shows high theater: employees report not understanding what was said but nodding in agreement because jargon use signals authority and alignment.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates clear perspectival divergence. Tech insiders see pure Rope (coordination mechanism enabling technical precision). Non-tech employees see Snare (mandatory adoption, career penalty, comprehension barrier). Mid-career professionals see Tangled Rope (genuine coordination benefits mixed with status extraction and continued performance pressure). The plain language movement sees Scaffold (temporary inefficiency being solved by accessibility mandates and style guides). The corporate communications department sees Piton (performative ritual maintaining status distinction despite known ineffectiveness). The analytical observer might naturalize this as 'inevitable complexity in technical fields,' but this mountain classification fails on grounds that plain-language explanations of identical concepts are available, accessible, and often superior in comprehension metrics. The constraint's extractiveness increases over time (from 0.32 to 0.52) because jargon has escaped its original epistemic domain and is now deployed in areas where it serves no technical function—this is Goodhart drift, indicating the constraint has degraded from coordination to pure extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from structural position in the jargon hierarchy. Tech insiders with arbitrage exit options (can choose when to deploy jargon, understand its origins and constraints) derive low d (~0.10-0.20), producing negative or minimal χ — they experience the constraint as coordination, not extraction. Non-tech employees with trapped exit (must adopt jargon, limited ability to refuse without career penalty) derive high d (~0.85-0.95), producing high χ — they experience maximum extraction. Mid-career professionals with constrained exit (benefit from fluency but pressured to maintain performance) derive moderate d (~0.50-0.60). Management consultants with constrained but leverageable exit (can pivot but financial incentives favor continued jargon deployment) derive moderate-high d (~0.55-0.65). The piton perspective derives from the theater gate rather than from high experienced extraction: the communications department sees the constraint as degraded (knows jargon obscures meaning) but maintains it through institutional inertia and status signaling logic.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the classification depends entirely on structural position. The mandatrophy question is not 'is this coordination or extraction?' but 'who experiences it as which?' From the tech insider's perspective, it is legitimate coordination—shared vocabulary enables precise discourse about complex technical problems. From the non-tech employee's perspective, it is pure extraction—vocabulary adoption is mandatory, understanding is not required, and non-fluency signals incompetence. From the management consultant's perspective, it is hybrid extraction layered on coordination—genuine process insights are communicated, but jargon density obscures whether actual value is being delivered, enabling continued high billing. The mandatrophy is resolved not by finding 'the' classification but by recognizing that the constraint's structure enables all these readings simultaneously. The theater ratio (0.81) indicates that performative use dominates, suggesting the constraint has drifted from genuine coordination toward pure extraction and status signaling. The upward trajectory of extractiveness (0.32 → 0.52) and theater (0.55 → 0.81) indicates Goodhart drift: the original coordination function (precise technical vocabulary) has been captured and repurposed for gatekeeping and status signaling, making the constraint increasingly extractive over time.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    jargon_precision_boundary,
    'At what point does specialized vocabulary represent genuine precision gain vs. obfuscation that impedes understanding?',
    'Controlled studies comparing comprehension and task completion time with jargon-heavy vs plain-language explanations of same concepts in corporate settings; measurement of error rates and information retention',
    'If jargon provides genuine precision: more perspectives classify as Rope or Scaffold (coordination function dominates). If obfuscation dominates: more perspectives classify as Snare or Tangled Rope (extraction dominates).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(jargon_precision_boundary, empirical, 'Whether technical jargon provides measurable precision gains or primarily enables gatekeeping').

omega_variable(
    onboarding_cost_asymmetry,
    'Does the onboarding cost of jargon fluency disproportionately affect groups with lower initial exposure (neurodivergent employees, non-native speakers, career-switchers)?',
    'Longitudinal tracking of onboarding timelines, comprehension tests, and psychological safety metrics for employees with different linguistic backgrounds; correlation between jargon exposure pre-hiring and workplace integration success',
    'If costs are symmetric: suppression value should be lower, less evidence of asymmetric extraction. If costs are asymmetric: confirms targeted victimization, supports snare and tangled_rope classifications.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(onboarding_cost_asymmetry, empirical, 'Whether jargon adoption costs fall asymmetrically on vulnerable populations').

omega_variable(
    plain_language_adoption_feasibility,
    'Can large organizations sustain internal communication in plain language while maintaining competitive advantage in external (investor-facing, customer-facing) tech discourse?',
    'Case study analysis of organizations that have adopted internal plain-language mandates; measurement of internal communication effectiveness, employee satisfaction, and competitive market positioning; tracking of investor perception changes',
    'If feasible: scaffold sunset is real and achievable within 5-10 years. If infeasible: jargon constraint is more durable than scaffold perspective suggests; classification may shift toward piton or persistent tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(plain_language_adoption_feasibility, empirical, 'Whether plain-language communication is organizationally sustainable').

omega_variable(
    status_signaling_irreducibility,
    'To what extent is jargon deployment driven by genuine epistemic need vs. pure status signaling and in-group identity formation?',
    'Discourse analysis comparing jargon density in internal vs external communications; measurement of comprehension correlation with business outcomes; psychological studies on status signaling effects of vocabulary choice in corporate hierarchies',
    'If status-signaling is primary driver: theater_ratio should be higher, suppression reflects coercion more than genuine coordination need, classification tips toward Snare and Piton. If epistemic function dominates: extractiveness value should be lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(status_signaling_irreducibility, conceptual, 'Relative weight of status signaling vs epistemic function in jargon deployment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(silicon_lexicon_overload, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sililex_tr_t0, silicon_lexicon_overload, theater_ratio, 0, 0.55).
narrative_ontology:measurement(sililex_tr_t5, silicon_lexicon_overload, theater_ratio, 5, 0.72).
narrative_ontology:measurement(sililex_tr_t10, silicon_lexicon_overload, theater_ratio, 10, 0.81).

% Extraction over time
narrative_ontology:measurement(sililex_be_t0, silicon_lexicon_overload, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(sililex_be_t5, silicon_lexicon_overload, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(sililex_be_t10, silicon_lexicon_overload, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(silicon_lexicon_overload, information_standard).
narrative_ontology:affects_constraint(silicon_lexicon_overload, organizational_clarity_deficit).
narrative_ontology:affects_constraint(silicon_lexicon_overload, neurodivergent_workplace_exclusion).
narrative_ontology:affects_constraint(silicon_lexicon_overload, knowledge_silos_corporate).

% DUAL FORMULATION NOTE:
% The silicon lexicon constraint exists at the intersection of (1) genuine technical vocabulary enabling specialized communication, and (2) performative jargon enabling status gatekeeping. These are structurally distinct mechanisms with different extractiveness profiles. The separation into technical_precision_coordination (ε ≈ 0.15, Rope) and jargon_status_signaling (ε ≈ 0.65, Snare) would be appropriate, but empirically the distinction is difficult to parse because speakers deploy identical vocabulary for both functions. The single-story approach captures the hybrid nature and lets perspectival decomposition reveal the underlying mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(silicon_lexicon_overload, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
