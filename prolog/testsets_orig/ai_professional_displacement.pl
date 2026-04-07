% ============================================================================
% CONSTRAINT STORY: ai_professional_displacement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_professional_displacement, []).

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
 *   constraint_id: ai_professional_displacement
 *   human_readable: AI-Driven Displacement of Entry-Level Professional Pathways
 *   domain: economic/technological
 *
 * SUMMARY:
 *   AI-driven automation of entry-level professional work is creating a
 *   structural barrier to career entry that exhibits all characteristics of a
 *   tangled rope constraint: genuine coordination benefits (improved firm
 *   productivity, faster document processing, better research synthesis)
 *   coexist with asymmetric extraction (wage suppression, skill obsolescence,
 *   blocked pathways). The constraint operates through active enforcement of
 *   credentialing gatekeeping — firms maintain traditional entry-level hiring
 *   and task structures not because they need them but because incumbent
 *   professionals derive status and control from the barrier. New graduates
 *   face maximum extraction (debt serviced while employment prospects
 *   collapse) while firm management experiences pure coordination benefit.
 *   The constraint is not technologically inevitable; alternative
 *   credentialing systems (apprenticeships, AI-assisted skill development,
 *   portfolio-based credentials) exist and are being deployed by
 *   forward-looking organizations. The theater ratio remains relatively low
 *   (0.38) because the constraint is enforced through real economic
 *   mechanisms, not performative ritual — yet. Rising theater indicates
 *   possible piton degradation as the functional justification for
 *   entry-level work erodes and gatekeeping becomes increasingly theatrical.
 *
 * KEY AGENTS:
 *   - New Graduates: Primary victims (powerless/trapped) — bear full extraction through debt servicing and pathway blockage; cannot exit credentialing requirements
 *   - Early-Career Professionals: Secondary victims (moderate/constrained) — partially affected; some mobility through specialization or sector switching but career trajectory compressed
 *   - Professional Services Firm Management: Primary beneficiaries (institutional/arbitrage) — capture coordination gains through AI automation; low suppression; high arbitrage capacity
 *   - Incumbent Senior Partners: Tertiary beneficiaries (powerful/mobile) — maintain cultural control and hierarchical gatekeeping; experience constraint as performative ritual (piton perspective)
 *   - Professional Associations and Regulatory Bodies: Organized agents (organized/constrained) — building alternative credentialing pathways; positioned to sunset traditional entry model
 *   - AI Technology Vendors: Tertiary beneficiaries (organized/constrained) — capture licensing value from automation; suppress open-source alternatives through platform lock-in
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing institutional choice as technological inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_professional_displacement, 0.58).
domain_priors:suppression_score(ai_professional_displacement, 0.65).
domain_priors:theater_ratio(ai_professional_displacement, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_professional_displacement, extractiveness, 0.58).
narrative_ontology:constraint_metric(ai_professional_displacement, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ai_professional_displacement, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_professional_displacement, tangled_rope).
narrative_ontology:human_readable(ai_professional_displacement, "AI-Driven Displacement of Entry-Level Professional Pathways").
narrative_ontology:topic_domain(ai_professional_displacement, "economic/technological").

domain_priors:requires_active_enforcement(ai_professional_displacement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_professional_displacement, incumbent_professionals).
narrative_ontology:constraint_beneficiary(ai_professional_displacement, firm_management).
narrative_ontology:constraint_victim(ai_professional_displacement, new_graduates).
narrative_ontology:constraint_victim(ai_professional_displacement, early_career_professionals).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NEW GRADUATE (SNARE) — No entry point exists. Traditional pathway (perform grunt work, develop skills, advance) has been severed by automation. Cannot exit professional services (social pressure, educational investment, debt), cannot acquire skills without entry-level position, cannot access capital to create alternatives. Maximum extraction: educational debt serviced while employment prospects collapse. Suppression is total — no viable alternative career pathway recognized or supported.
constraint_indexing:constraint_classification(ai_professional_displacement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EARLY-CAREER PROFESSIONAL (TANGLED ROPE) — Some mobility (alternative industries, geographic relocation) but constrained by credential requirements and credential sunk costs. Experiences both coordination benefit (AI tools accelerate legitimate work) and extraction (wage compression, task elimination, accelerated obsolescence of junior skills). Can organize peer pressure and collective job-seeking but faces power imbalance with hiring institutions. Partial exit through skill specialization (AI training, client relationships) available but requires institutional support.
constraint_indexing:constraint_classification(ai_professional_displacement, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FIRM MANAGEMENT (ROPE) — Experiences constraint as pure coordination problem: automating document review, legal research, financial modeling, and case analysis reduces costs and improves throughput. Coordination benefit is real and high — AI tools genuinely improve efficiency. Arbitrage exit is robust (deploy AI in multiple firms, capture margin improvement, exit into cost-leadership strategy). No meaningful suppression from this agent's position. Extraction flow runs away from them (toward shareholders and firm partners). Classification as pure rope masks the extraction happening downstream.
constraint_indexing:constraint_classification(ai_professional_displacement, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PROFESSIONAL ASSOCIATIONS/REGULATORY BODIES (SCAFFOLD) — Bar associations, accounting bodies, and industry groups see a temporary coordination failure with sunset logic: apprenticeship models, mandatory mentorship requirements, AI-assisted learning pathways, and credential-by-portfolio mechanisms are emerging as alternatives to the traditional grunt-work entry model. Suppression is declining as organized agents build new infrastructure. Theater is moderate (certification rituals persist but new pathways bypass them). Sunset estimated at 10-15 years as alternative credentialing systems mature and labor market organizes.
constraint_indexing:constraint_classification(ai_professional_displacement, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INCUMBENT PARTNERS (PITON) — Senior partners in established firms experience the constraint as theatrical maintenance of traditional hierarchy: they benefited from grinding through entry-level work (sunk cost legitimacy), and maintain the barrier even as AI makes it functionally unnecessary. The constraint persists through institutional inertia and credential gatekeeping, not functional necessity. High theater ratio (elaborate onboarding rituals, 'apprenticeship' structures that AI has made performative). Extractive force is declining as next-generation partners question the value, but the structure persists.
constraint_indexing:constraint_classification(ai_professional_displacement, piton,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: AI VENDORS (TANGLED ROPE) — Provide genuine coordination benefit (document automation, contract analysis, research synthesis genuinely improve firm productivity). However, vendors are incentivized to suppress competing alternative models (cheaper open-source tools, internal firm AI development) through licensing lock-in and feature bundling. Active enforcement of proprietary standards masks what could be commodity infrastructure. Extraction is moderate but asymmetric: vendors capture value from the crisis they enable. Suppression is moderate (open-source alternatives exist but face adoption barriers).
constraint_indexing:constraint_classification(ai_professional_displacement, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE SUMMIT (MOUNTAIN) — Risks naturalizing this as inevitable: 'Technological disruption always displaces entry-level work; this is how creative destruction works.' Frames the constraint as inherent to technological progress rather than a contingent institutional arrangement. But the structural data contradicts the mountain classification — the organized response (scaffold perspective), the viable alternative credentialing models, and the policy levers (apprenticeship mandates, credential portability, firm-sponsored training) all indicate this is NOT a natural law. The false summit conceals the choice to maintain the barrier.
constraint_indexing:constraint_classification(ai_professional_displacement, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_professional_displacement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_professional_displacement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_professional_displacement, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_professional_displacement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ai_professional_displacement, TR),
    TR >= 0.70.

:- end_tests(ai_professional_displacement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts through wage suppression (entry-level positions eliminated or downgraded), credential inflation (educational requirements rising while career entry points shrink), and opportunity cost (new graduates forced into adjacent lower-wage sectors or prolonged unemployment). But extraction is not total because new graduates retain human capital and some alternative pathways exist. The value reflects the structural reality: automation genuinely eliminates work, and firms adjust by either (a) consolidating tasks upward (fewer, higher-level positions), (b) shifting costs to junior workers (more competition for fewer positions, wage suppression), or (c) outsourcing via temp/contract labor. Suppression (0.65): High. Multiple barriers reinforce the constraint: (1) credentialing requirements remain high even as entry-level work is eliminated; (2) firms maintain traditional hiring structures for cultural/status reasons; (3) alternative credentialing systems lack institutional recognition; (4) debt burden limits geographic mobility and risk-taking; (5) professional culture stigmatizes non-traditional entry. Theater ratio (0.38): Low-moderate. The constraint is enforced through real economic mechanisms (task elimination, wage competition, debt burden), not primarily through performative ritual. However, theater is rising (measurement trajectory shows 0.32→0.35→0.38) as the functional justification for entry-level apprenticeship erodes and firms maintain hiring structures increasingly for cultural gatekeeping rather than operational necessity.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is extreme. Firm management (institutional/arbitrage) experiences pure coordination: 'We're automating document review and legal research; this improves quality and reduces costs.' New graduates (powerless/trapped) experience pure extraction: 'Entry-level positions disappeared and I cannot accumulate human capital.' The piton perspective (incumbent partners) sees performative gatekeeping: 'We maintain traditional apprenticeship structures for cultural reasons even though AI has made them unnecessary.' The scaffold perspective (organized agents) sees a sunset trajectory: 'Alternative credentialing systems are maturing; in 10-15 years, the traditional bottleneck will be bypassed.' The false mountain perspective risks naturalizing: 'Technological disruption always displaces entry-level work; this is how capitalism works.' The gap reveals that the constraint is not a technology problem — it is a choice problem. AI automation is real, but the decision to eliminate entry-level positions while maintaining credentialing barriers is institutional. Alternative arrangements (mandatory apprenticeships, firm-sponsored training, credential portability, accelerated advancement for high-performers) are structurally feasible but politically blocked.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation follows the structural relationship of each agent to the constraint. New graduates are victims with no exit (trapped) → high d → high f(d) → high experienced extraction. Firm management are beneficiaries with arbitrage exit (can deploy AI strategy globally, pivot business model) → low d → negative f(d) → negative/neutral experienced extraction. Early-career professionals are victims with constrained exit (some geographic/sectoral mobility but credential lock-in) → intermediate d → intermediate f(d) → moderate extraction. Professional associations are organized agents with constrained exit (bound by regulation, member interests) but positioned as architects of alternative credentialing → intermediate d with asymmetric benefit/cost. Incumbent partners benefit but experience the constraint as piton (performative maintenance) → intermediate d masked by theatrical maintenance. The tangled rope classification requires both coordination function (AI genuinely improves firm operations) and asymmetric extraction (benefits concentrated upward, costs concentrated downward), both of which are present. Suppression is independent of directionality — the structural barriers to alternative credentialing (regulatory lock-in, employer preference for traditional credentials, educational system inertia) apply regardless of agent perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in this constraint is the false identification of technological inevitability with institutional necessity. The constraint appears natural ('AI makes entry-level work obsolete, so new graduates must accept worse terms') but is actually contingent ('Firms chose to eliminate entry-level work while maintaining credentialing barriers because it increases extraction'). The resolution is not 'choose between coordination and extraction' but 'recognize that the constraint is structured to extract maximum value from the coordination gain.' Firm management genuinely captures productivity improvement from AI automation — this is real coordination. But instead of sharing the gain (higher entry wages, more stable positions, better training), firms capture it as margin improvement. The mandatrophy is resolved by showing that (1) the coordination function is real (tangled rope, not pure snare), (2) the extraction is active and institutional (not technological), (3) the suppression of alternatives is enforced through credentialing gatekeeping, and (4) organized agents (professional associations, regulatory bodies) are beginning to resolve it by building alternative credentialing systems. The scaffold perspective is not aspirational — it is emerging as a real structural response. The piton perspective confirms that incumbent gatekeeping is increasingly performative (theater rising) rather than functional.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alternative_credentialing_viability,
    'Can alternative credentialing systems (apprenticeships, AI-assisted learning, portfolio-based credentials) actually replace traditional entry-level work as a professional socialization and skill-building pathway?',
    'Longitudinal tracking of outcomes for cohorts credentialed through alternative pathways vs traditional entry-level work; employer acceptance rates; peer quality assessments by senior professionals; career trajectory data at 5-year and 10-year marks',
    'If viable: scaffold sunset timeline is real and constraint degrades over 10-15 years into rope (coordination without extraction). If not viable: tangled rope or snare persists indefinitely; labor market bifurcates into credential-locked entry and AI-assisted incumbents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_credentialing_viability, empirical, 'Whether alternative credentialing can replace traditional entry pathways').

omega_variable(
    firm_incentive_structure,
    'Are firms genuinely indifferent to entry-level junior talent given AI automation, or do they maintain gatekeeping barriers to preserve cultural control and power structure over the junior cohort?',
    'Analysis of hiring patterns in AI-heavy vs AI-light practices; survey data on stated reasons for entry-level hiring; accounting for positions eliminated vs transformed; interviews with hiring partners on training ROI and cultural transmission goals',
    'If indifferent: constraint is purely technological and will optimize toward minimal hiring (full snare for new graduates). If cultural: firms will maintain token entry pathways and extract via underpayment/overwork (tangled rope persists). If hybrid: intermediate outcome.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(firm_incentive_structure, empirical, 'Firm incentive structure regarding junior talent development').

omega_variable(
    labor_surplus_absorption_capacity,
    'What is the absorption capacity of adjacent industries (tech, analytics, government, non-profits) for displaced entry-level professional talent, and what are the credential transfer requirements?',
    'Sector-by-sector hiring demand analysis; credential bridge program success rates; wage outcomes for career switchers; identification of artificial credential barriers vs genuine skill gaps',
    'If high absorption: constraint is temporary (scaffold sunset is real). If low absorption: constraint drives permanent wage suppression or credential inflation in adjacent sectors (snare spreads laterally).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(labor_surplus_absorption_capacity, empirical, 'Adjacent industry absorption capacity for displaced entry-level talent').

omega_variable(
    public_policy_intervention_likelihood,
    'What is the likelihood that policymakers will mandate apprenticeship-style requirements, credential portability, or firm-sponsored training to maintain professional pipeline capacity?',
    'Analysis of regulatory proposals; interviews with bar associations and credential bodies; political economy of professional lobbying; comparison to historical labor market disruptions (e.g., accounting automation, paralegal automation)',
    'If high intervention: constraint is actively being resolved via scaffold mechanism (policy-driven sunset). If low intervention: constraint persists absent market-driven solutions, suggesting structural lock-in.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(public_policy_intervention_likelihood, preference, 'Political likelihood of policy intervention to maintain professional pipelines').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_professional_displacement, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aipd_tr_t0, ai_professional_displacement, theater_ratio, 0, 0.32).
narrative_ontology:measurement(aipd_tr_t3, ai_professional_displacement, theater_ratio, 3, 0.35).
narrative_ontology:measurement(aipd_tr_t6, ai_professional_displacement, theater_ratio, 6, 0.38).

% Extraction over time
narrative_ontology:measurement(aipd_be_t0, ai_professional_displacement, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(aipd_be_t3, ai_professional_displacement, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(aipd_be_t6, ai_professional_displacement, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_professional_displacement, resource_allocation).
narrative_ontology:affects_constraint(ai_professional_displacement, credential_inflation).
narrative_ontology:affects_constraint(ai_professional_displacement, labor_market_inequality).
narrative_ontology:affects_constraint(ai_professional_displacement, professional_pipeline_bottleneck).

% DUAL FORMULATION NOTE:
% AI-driven displacement of entry-level professional work can be decomposed into two constraints with different ε values: (1) the genuine technological displacement of routine cognitive work (ε ≈ 0.15, rope from firm perspective), and (2) the institutional choice to maintain credentialing barriers while eliminating entry pathways (ε ≈ 0.58, tangled rope from graduate perspective). This story models the composite institutional constraint; upstream constraint focuses on technological displacement alone.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_professional_displacement, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
