% ============================================================================
% CONSTRAINT STORY: mirror_of_erised_expectation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mirror_of_erised_expectation, []).

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
 *   constraint_id: mirror_of_erised_expectation
 *   human_readable: The Erised Career/Stability Mirror: Millennial Expectation Trap
 *   domain: psychological/economic/cultural
 *
 * SUMMARY:
 *   The Erised Career/Stability Mirror is a constraint built on internalized
 *   cultural narratives from the 1990s-2000s, where a cohort of young people
 *   (Millennials) absorbed the belief that ordinary people, through merit and
 *   effort, could defeat 'evil' (poverty, precarity, mediocrity) and achieve
 *   stability. This narrative derived from contemporary media (Harry Potter,
 *   The Matrix, Spider-Man origin stories), educational narratives (growth
 *   mindset, diversity initiatives), and economic ideology (the rising tide
 *   lifts all boats). The constraint operates by locking aspirants into
 *   belief in their own meritocratic mobility while the actual institutional
 *   structures (credential inflation, wage stagnation, labor market
 *   arbitrage, precarity normalization) ensure that the payoff never
 *   materializes. The constraint is extractive because the gap between
 *   promised and actual outcomes is systematized and non-random — it extracts
 *   time, emotional labor, debt, and deferred gratification from those who
 *   internalize the narrative most completely. The suppression is high
 *   because alternatives (not pursuing credentials, rejecting the narrative,
 *   organizing collectively) carry social and psychological costs. The
 *   theater ratio has increased over the interval as institutions have
 *   doubled down on performative meritocracy (diversity boards, mentorship
 *   programs, 'culture fit' assessment) while extraction mechanisms have
 *   intensified (credential proliferation, gig-ification of work, benefits
 *   precarity). This constraint is a Snare because the extraction is high,
 *   suppression is severe, and alternatives are effectively blocked by
 *   psychological sunk cost and narrative commitment.
 *
 * KEY AGENTS:
 *   - Millennial Aspirants: Primary victims (powerless/trapped) — locked in by belief in narrative; sunk costs in credentials and identity; psychological shame of non-conformity
 *   - Precarious Knowledge Workers: Secondary victims (moderate/constrained) — possess some skills but face credential devaluation, wage stagnation, benefits precarity, labor arbitrage
 *   - Credential Sellers (Universities, Bootcamps): Primary beneficiaries (institutional/arbitrage) — capture tuition/fees; justify recruitment through narrative; can exit to other markets/cohorts
 *   - Labor Market Arbitrage Employers: Secondary beneficiaries (organized/constrained) — suppress wages and benefits through meritocracy narrative; justify precarity as 'market efficiency' and individual responsibility
 *   - Meritocracy Institutional Apparatus: Enforcement layer (institutional/arbitrage) — maintains narrative performance; generates diversity theater and mentorship while extraction mechanisms run uninterrupted
 *   - Analytical Observer: Risks naturalizing constraint as inevitable feature of markets rather than contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mirror_of_erised_expectation, 0.58).
domain_priors:suppression_score(mirror_of_erised_expectation, 0.68).
domain_priors:theater_ratio(mirror_of_erised_expectation, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mirror_of_erised_expectation, extractiveness, 0.58).
narrative_ontology:constraint_metric(mirror_of_erised_expectation, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(mirror_of_erised_expectation, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mirror_of_erised_expectation, snare).
narrative_ontology:human_readable(mirror_of_erised_expectation, "The Erised Career/Stability Mirror: Millennial Expectation Trap").
narrative_ontology:topic_domain(mirror_of_erised_expectation, "psychological/economic/cultural").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mirror_of_erised_expectation, financial_extraction_agents).
narrative_ontology:constraint_beneficiary(mirror_of_erised_expectation, credential_sellers).
narrative_ontology:constraint_beneficiary(mirror_of_erised_expectation, labor_arbitrage_employers).
narrative_ontology:constraint_victim(mirror_of_erised_expectation, millennial_aspirants).
narrative_ontology:constraint_victim(mirror_of_erised_expectation, precarious_knowledge_workers).
narrative_ontology:constraint_victim(mirror_of_erised_expectation, debt_servicing_cohort).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MILLENNIAL ASPIRANT (SNARE) — Trapped by internalized narrative of magical meritocracy. Exits are blocked: sunk cost in credentials, social shame of 'failure', psychological commitment to the narrative. d≈0.92, f(d)≈1.39, σ=1.0 → χ≈0.81. Maximum extraction through belief system enforcement.
constraint_indexing:constraint_classification(mirror_of_erised_expectation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PRECARIOUS KNOWLEDGE WORKER (TANGLED ROPE) — Constrained by credential investment and local labor market. Does benefit from some coordination: professional networks, skill development, access to meaningful work. But extraction dominates: wage stagnation, gig labor, benefits precarity. d≈0.78, f(d)≈1.12, σ=0.9 → χ≈0.58.
constraint_indexing:constraint_classification(mirror_of_erised_expectation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CREDENTIAL SELLER (ROPE) — Institutional beneficiary with arbitrage exit (can shift curricula, pivot to international markets). Experiences constraint as coordination: the 'meritocracy narrative' solves recruitment and legitimacy problems. Aligns supply (aspirants) with demand (their instruction offerings). d≈0.08, f(d)≈-0.11, σ=1.2 → χ≈-0.09. Net beneficiary; extraction is disguised as coordination service.
constraint_indexing:constraint_classification(mirror_of_erised_expectation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LABOR MARKET ARBITRAGE EMPLOYER (SNARE) — Organized extraction through narrative enforcement. The meritocracy myth justifies wage suppression ('you're lucky to have this opportunity'), benefits reduction ('merit means self-reliance'), and high turnover ('if you're not advancing, you lack talent'). d≈0.22, f(d)≈0.08, σ=1.0 → χ≈0.05. Low effective extraction as computed, but structural extraction is severe — the calculation masks institutional power through arbitrage positioning.
constraint_indexing:constraint_classification(mirror_of_erised_expectation, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: MERITOCRACY INSTITUTIONAL APPARATUS (PITON) — Theater ratio 0.65 reflects that institutional commitment to the meritocracy narrative persists despite contradictory evidence. Diversity initiatives, mentorship programs, and 'culture fit' assessment are performative — they maintain the appearance of fairness while extraction mechanisms (credential inflation, wage stagnation, labor precarity) intensify. The apparatus sees its own maintenance as degraded but necessary.
constraint_indexing:constraint_classification(mirror_of_erised_expectation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER — FALSE SUMMIT (NOT MOUNTAIN) — Risk of naturalizing the constraint as inevitable: 'markets always reward effort; inequality reflects differential talent; mobility is possible for those with merit.' This perspective risks treating the Erised narrative itself as a law of nature rather than as a contingent extraction mechanism. ε=0.58, suppression=0.68, and theater=0.65 contradicts mountain classification. The engine's false summit detector should flag this.
constraint_indexing:constraint_classification(mirror_of_erised_expectation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mirror_of_erised_expectation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(mirror_of_erised_expectation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mirror_of_erised_expectation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(mirror_of_erised_expectation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(mirror_of_erised_expectation, TR),
    TR >= 0.70.

:- end_tests(mirror_of_erised_expectation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts significant value from the victim cohort through deferred wages (credential investment without equivalent labor market return), emotional labor (maintaining belief despite disconfirming evidence), and opportunity cost (time spent on credentialing rather than alternative stability paths). The extraction is not total (some credentials do provide income premium) but is systematic and non-random. Suppression (0.68): High. Multiple barriers prevent exit: (1) Sunk psychological cost — narrative is internalized as identity; (2) Sunk financial cost — debt commitment; (3) Social penalty — rejecting the narrative triggers shame and status loss; (4) Informational suppression — counter-narratives are weaker and arrive later in development; (5) Structural barriers — labor market alternatives (trade, mutual aid, non-credential paths) are themselves suppressed through licensing and credentialism. Theater ratio (0.65): High and rising. The apparatus has shifted from implicit meritocracy belief to explicit performance of meritocracy (DEI initiatives, mentorship, transparent pay bands) while extraction mechanisms have intensified. This is Goodhart drift: the metric (institutional appearance of fairness) has replaced the original function (actual mobility and stability), creating a high theater ratio. The interval shows progression: in 1995, the narrative was largely implicit in culture and barely theatrical (35% theater). By 2025, institutional performance dominates (65% theater), because the contradictions are now visible and require constant performative repair.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates divergent classification from identical structural data. The victim (powerless/trapped) sees a Snare — no way out. The credential seller (institutional/arbitrage) sees a Rope — they're solving a genuine coordination problem: matching aspiring workers with skill development. The employer (organized/constrained) also experiences this as mixed: they benefit from extraction but are partially constrained by reputation and regulatory pressure. The institutional apparatus (piton) sees its own maintenance as degraded but necessary — diversity initiatives are theaters, but the theater is required to maintain the extraction mechanism. The analytical observer risks the false summit — naturalizing the constraint as an inevitable feature of markets rather than a contingent institutional arrangement. The perspectival gap reveals that the constraint is real across all frames, but the beneficiaries' and victims' causal narratives are fundamentally misaligned: beneficiaries narrate it as successful coordination and fair meritocracy; victims experience it as systematic extraction; the institution experiences it as performative repair of a degrading legitimacy.
 *
 * DIRECTIONALITY LOGIC:
 *   Millennial aspirants: Victim + trapped → d≈0.92, f(d)≈1.39. Near-maximum extraction. Identity commitment + sunk costs + social penalty = almost zero exit optionality. Precarious knowledge workers: Victim + constrained → d≈0.78, f(d)≈1.12. High extraction. Some exit optionality (geographic mobility, skill pivots) but constrained by credential lock-in and regional labor markets. Credential sellers: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary. Can exit (pivot to international markets, different student cohorts, online credentials) and capture surplus from aspirant demand. Arbitrage exit is the key: if student demand fell, they could redirect resources; the constraint does not trap them. Employers: Organized + constrained. The calculation is complex: they benefit from extraction but are constrained by reputation, regulatory risk, and (incipient) social pressure. Directionality override may be warranted to reflect that institutional power is higher than the 'moderate/constrained' formula suggests — they have organizational capacity to enforce narrative. Institutional apparatus: Institutional + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary from narrative maintenance, but piton classification comes from theater gate, not from high chi.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint avoids conflating coordination with extraction by distinguishing the institutional narrative from the structural reality. The credential system DOES provide a coordination function — it genuinely matches aspiring workers with skill development and employers with pre-screened labor. This is real. But the extraction mechanism DOMINATES the coordination benefit: credential inflation decouples skills from income premiums, theatrical meritocracy masks wage suppression and benefit precarity, and the gap between narrative promise and actual outcome is systematized. The snare classification holds because suppression (0.68) is high and effective extraction (χ≈0.58 at victim level) exceeds the coordination benefit. A pure Rope would have minimal suppression and visible equal benefit across agent classes; this constraint shows asymmetric benefit and psychological/structural suppression. The Tangled Rope perspective (precarious workers) is valid at the moderate/constrained level, but the powerless/trapped perspective is snare-dominant. The constraint is a snare with coordination overlay, not a coordination mechanism with asymmetric distribution — the direction of extraction is clear (from aspirants toward credential sellers and employers), and the narrative naturalizes it as fair.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    narrative_internalization_threshold,
    'At what age/career stage does the meritocracy narrative lock in as a identity-constraining belief rather than a motivational framework?',
    'Longitudinal psychological studies tracking narrative belief vs. lived career outcomes; cognitive inflexibility measures at different career stages; narrative resistance to contrary evidence',
    'If threshold is early (18-24): narrative is highly constraining, snare classification is robust. If threshold is late or distributed: the constraint is weaker, more agents can exit, classification shifts toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(narrative_internalization_threshold, empirical, 'When narrative belief becomes identity-constraining vs motivational').

omega_variable(
    credential_inflation_ceiling,
    'Is there a point at which credential inflation becomes so severe that even narrative-believing agents cannot service debt through labor income, forcing narrative collapse?',
    'Debt-to-income ratio trends for credential holders; default rates on student/bootcamp loans; labor income percentile distribution vs. credential cohort size over time',
    'If ceiling is approached: cohort will experience forced narrative disconfirmation, potentially enabling exit. If ceiling is infinitely deferred (through credential proliferation and new extraction channels): snare persists indefinitely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(credential_inflation_ceiling, empirical, 'Whether credential inflation has unsustainable debt servicing ceiling').

omega_variable(
    alternative_stability_narrative_adoption,
    'Are counter-narratives (solarpunk, mutual aid, post-credential alternatives) gaining adoption fast enough to compete with the Erised narrative, or does narrative inertia preserve the original framing?',
    'Cultural narrative analysis (media, literature, educational curriculum); survey data on generational belief shifts; adoption rates of alternative credential systems (skill-based hiring, apprenticeships); social movement narrative adoption',
    'If alternative narratives gain traction: younger cohorts may avoid the trap entirely, splitting the constraint into legacy (trapped millennials) and emerging (Gen Z alternatives). If inertia dominates: narrative persists, extraction mechanisms intensify.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_stability_narrative_adoption, empirical, 'Whether alternative stability narratives can displace Erised framing').

omega_variable(
    extractive_agent_narrative_dependence,
    'How dependent is the extraction mechanism on the meritocracy narrative? If the narrative collapses, do extraction agents have non-narrative enforcement tools, or does the constraint dissolve?',
    'Counterfactual analysis of labor markets without meritocracy belief; comparison to institutional contexts where meritocracy narrative is weak (feudal remnants, corruption-endemic systems); regulatory enforcement requirements absent narrative compliance',
    'If extraction is narrative-dependent: constraint can be dissolved through narrative counter-attack. If extraction has autonomous enforcement: narrative is theater masking structural coercion, constraint persists post-collapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extractive_agent_narrative_dependence, conceptual, 'Whether extraction mechanism depends on meritocracy narrative or has independent enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mirror_of_erised_expectation, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eris_tr_t0, mirror_of_erised_expectation, theater_ratio, 0, 0.35).
narrative_ontology:measurement(eris_tr_t15, mirror_of_erised_expectation, theater_ratio, 15, 0.5).
narrative_ontology:measurement(eris_tr_t30, mirror_of_erised_expectation, theater_ratio, 30, 0.65).

% Extraction over time
narrative_ontology:measurement(eris_be_t0, mirror_of_erised_expectation, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(eris_be_t15, mirror_of_erised_expectation, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(eris_be_t30, mirror_of_erised_expectation, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mirror_of_erised_expectation, resource_allocation).
narrative_ontology:affects_constraint(mirror_of_erised_expectation, credential_inflation_spiral).
narrative_ontology:affects_constraint(mirror_of_erised_expectation, gig_labor_precarity_normalization).
narrative_ontology:affects_constraint(mirror_of_erised_expectation, intergenerational_wealth_gap).
narrative_ontology:affects_constraint(mirror_of_erised_expectation, student_debt_servicing).

% DUAL FORMULATION NOTE:
% The Erised expectation constraint operates at the narrative/psychological level. It is upstream of and feeds into credential inflation, precarity normalization, and debt servicing. The constraint could be decomposed into: (1) The narrative itself (belief in magical meritocracy), (2) The institutional enforcement mechanism (credential system, labor market arbitrage), and (3) The psychological lock-in (identity commitment, sunk-cost fallacy). For this version, we treat it as a unified snare where the narrative is the mechanism of suppression. Future decomposition would yield separate stories for institutional arbitrage (institutional ε≈0.25 Tangled Rope) and psychological lock-in (psychological ε≈0.65 Snare).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(mirror_of_erised_expectation, organized, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
