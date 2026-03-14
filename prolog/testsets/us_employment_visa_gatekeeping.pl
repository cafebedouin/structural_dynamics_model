% ============================================================================
% CONSTRAINT STORY: us_employment_visa_gatekeeping
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_employment_visa_gatekeeping, []).

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
 *   constraint_id: us_employment_visa_gatekeeping
 *   human_readable: US Employment Visa Gatekeeping System
 *   domain: labor_migration/economic_regulation
 *
 * SUMMARY:
 *   The US employment visa system (H-1B, EB-3, L-1, O-1, and green card
 *   pathways) operates as a labor market coordination mechanism with embedded
 *   extraction. Nominally, the system coordinates cross-border labor
 *   allocation during skills shortages and protects domestic labor markets
 *   from destabilizing competition. Structurally, it functions as a
 *   gatekeeping apparatus that binds foreign workers to specific employers,
 *   delays permanent settlement through per-country caps, and maintains wage
 *   premiums for protected domestic sectors by constraining labor supply. The
 *   constraint exhibits tangled_rope characteristics: genuine coordination
 *   function (establishing hiring channels, managing integration) coexists
 *   with systematic extraction (employer lock-in, geographic immobility,
 *   lower bargaining power for visa-dependent workers). The system's theater
 *   ratio has increased over 30 years as formal compliance mechanisms (labor
 *   certification, prevailing wage audits) have become more elaborate while
 *   core gatekeeping logic remains unchanged. Foreign skilled workers
 *   experience maximum extraction through structural dependency and
 *   multi-year immigration waits; incumbent domestic workers in protected
 *   sectors experience genuine wage and employment protection;
 *   visa-sponsoring employers experience pure arbitrage benefit through
 *   access to constrained global labor pools; and the bureaucracy that
 *   operates the system sees its own processes as substantially performative
 *   while defending gatekeeping as necessary protection.
 *
 * KEY AGENTS:
 *   - Foreign Skilled Workers: Primary victims (powerless/trapped) — structurally dependent on single employer, multi-year green card delays, constrained wage negotiation and geographic mobility
 *   - Incumbent Protected-Sector Workers: Primary beneficiaries (organized/constrained) — gain wage and employment protection through restricted labor supply; face retraining costs if protections removed
 *   - Visa-Sponsoring Employers (Tech, Finance, Healthcare): Secondary beneficiaries (institutional/arbitrage) — access global talent at constrained wages; minimal cost for sponsorship channels once established
 *   - Non-Sponsoring Domestic Employers: Tertiary victims (institutional/constrained) — cannot access foreign talent pools; face skills gaps and constrained competitiveness; experience cost barriers to visa sponsorship
 *   - Immigration Bureaucracy (USCIS, DOL): Institutional actor (institutional/arbitrage) — maintains gatekeeping authority and regulatory jurisdiction; sees own certification/audit processes as degraded
 *   - Remote-Work and Startup Coalition: Organized agents (organized/constrained) — building alternative pathways (remote employment, international founder visas, visa-bypass strategies) that represent structural sunset mechanisms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_employment_visa_gatekeeping, 0.58).
domain_priors:suppression_score(us_employment_visa_gatekeeping, 0.68).
domain_priors:theater_ratio(us_employment_visa_gatekeeping, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_employment_visa_gatekeeping, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_employment_visa_gatekeeping, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(us_employment_visa_gatekeeping, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_employment_visa_gatekeeping, tangled_rope).
narrative_ontology:human_readable(us_employment_visa_gatekeeping, "US Employment Visa Gatekeeping System").
narrative_ontology:topic_domain(us_employment_visa_gatekeeping, "labor_migration/economic_regulation").

domain_priors:requires_active_enforcement(us_employment_visa_gatekeeping).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_employment_visa_gatekeeping, incumbent_workers_protected_sectors).
narrative_ontology:constraint_beneficiary(us_employment_visa_gatekeeping, visa_sponsoring_employers).
narrative_ontology:constraint_beneficiary(us_employment_visa_gatekeeping, regulatory_bureaucracy).
narrative_ontology:constraint_victim(us_employment_visa_gatekeeping, foreign_skilled_workers).
narrative_ontology:constraint_victim(us_employment_visa_gatekeeping, labor_market_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FOREIGN SKILLED WORKER (SNARE) — Structurally trapped by visa dependency. Cannot work without sponsorship, cannot change employers without restarting immigration process, cannot accumulate residual labor market value. Maximum experienced extraction through employment lock-in, delayed green card pathways (10+ year waits for certain nationals), and employer leverage. No exit options; suppression is structural and comprehensive.
constraint_indexing:constraint_classification(us_employment_visa_gatekeeping, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INCUMBENT PROTECTED-SECTOR WORKERS (TANGLED ROPE) — Genuine coordination benefit: visa restrictions prevent wage competition in certain sectors, protecting employment security and wages. Active enforcement required through labor certification requirements, prevailing wage rules, H-1B lottery caps. But the constraint contains asymmetric extraction: maintains artificial wage floors and employment protections through gatekeeping rather than genuine productivity or skill differentiation. Organized power and constrained exit (retraining costs, geographic dependency) prevent total extraction reversal.
constraint_indexing:constraint_classification(us_employment_visa_gatekeeping, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: VISA-SPONSORING EMPLOYERS (ROPE) — Primary beneficiary through arbitrage. Sponsors can access global talent pools at constrained wages (visa workers cannot negotiate as freely as citizens). The constraint solves coordination problems: establishing sponsorship channels, creating predictable hiring pathways, enabling retention through green card linkage. Pure coordination benefit with minimal extraction cost — employers experience the constraint as enabling rather than restrictive.
constraint_indexing:constraint_classification(us_employment_visa_gatekeeping, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: NON-SPONSORING EMPLOYERS (TANGLED ROPE) — Face constrained exit from visa restrictions: cannot access foreign talent to fill skills gaps, but also benefit indirectly from protected labor pools (upstream suppliers may have lower costs). Extraction flows toward sponsors; non-sponsors experience gatekeeping as a coordination failure they cannot exit from (retraining domestic workers is costly, sponsorship bureaucracy is onerous). Mixed experience of extraction and coordination dependency.
constraint_indexing:constraint_classification(us_employment_visa_gatekeeping, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: IMMIGRATION BUREAUCRATIC SYSTEM (PITON) — Theater ratio high (0.64): formal labor certification, prevailing wage audits, skills assessments are substantially performative. Bureaucratic verification cannot detect actual market labor shortage vs. employer preference for constrained labor; prevailing wage rules are ritualized rather than dynamically responsive. System persists through institutional inertia and path-dependent statutory frameworks (INA of 1990). Agencies see their own gatekeeping as degraded (unable to distinguish genuine need from protectionism) but lack authority to reform. Arbitrary caps (H-1B lottery) are admitted as non-merit-based. Theater persists because statutory authority requires it, not because it functions effectively.
constraint_indexing:constraint_classification(us_employment_visa_gatekeeping, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: STARTUP/REMOTE-WORK COALITION (SCAFFOLD) — Organized agents (tech founders, remote-first companies) see visa gatekeeping as a temporary coordination failure being bypassed through structural change. Remote work from abroad eliminates visa dependency if employment is nominally outside US jurisdiction. Visa alternatives (L-1, O-1 for extraordinary ability) and portable entrepreneurship paths (startup visas, founder visas) represent sunset mechanisms. Constraint experiences low effective extraction because organized agents perceive and are building exit paths. Sunset horizon: 15-20 years as geographic employment arbitrage replaces traditional visa pathways.
constraint_indexing:constraint_classification(us_employment_visa_gatekeeping, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/global scope, the visa system coordinates labor flows while maintaining nationalist extraction: protects domestic labor markets (genuine coordination function) but implements this through gatekeeping that artificially constrains global labor allocation efficiency (asymmetric extraction toward native workers and incumbent industries). The constraint is analytically coherent as tangled_rope: both coordination (preventing labor market collapse in protected sectors) and extraction (preventing wage competition and geographic mobility). Not a mountain because the coordination function is genuine but value-laden (protecting locals vs. efficient allocation); not a pure rope because extraction is structural, not incidental.
constraint_indexing:constraint_classification(us_employment_visa_gatekeeping, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_employment_visa_gatekeeping_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_employment_visa_gatekeeping, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_employment_visa_gatekeeping, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_employment_visa_gatekeeping, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_employment_visa_gatekeeping, TR),
    TR >= 0.70.

:- end_tests(us_employment_visa_gatekeeping_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint imposes measurable costs on visa-dependent workers through employer lock-in, delayed family reunification, reduced bargaining power, and multi-year immigration queues (effective economic extraction). However, the extraction is not total (not a pure snare at 0.72+) because: (1) visa pathways do provide access to US employment that would be otherwise unavailable, (2) green card pathways, while delayed, are genuine rather than perpetual, (3) some visa categories (L-1, O-1) offer more mobility than H-1B. The 0.58 value reflects that the system extracts substantial value from labor supply constraints while retaining a coordination function. Rising from 0.35 to 0.58 over the 30-year interval reflects increased bureaucratic elaboration and decreased actual visa availability per applicant (H-1B caps effective since 2004). Suppression (0.68): High. Structural barriers include: statutory employer sponsorship requirement (cannot switch jobs freely), per-country caps creating multi-year waits (particularly for Indian and Chinese nationals), single-visa-at-a-time constraint (cannot explore alternatives while in queue), family reunification delays, credential recognition barriers, and immigration status uncertainty. These barriers are comprehensive and externally enforced (statutory, not negotiable). Theater ratio (0.64): Moderately high. Labor certification and prevailing wage determinations are substantially performative: officials cannot effectively distinguish genuine labor shortage from employer preference, prevailing wage determinations lag actual market rates, audit rates are low relative to visa volume. The bureaucratic apparatus maintains legitimacy through apparent rigor (formal attestations, wage audits, skills testing) while core gatekeeping logic (per-country caps, lottery systems) is admittedly arbitrary and non-merit-based. Theater has increased as statutory requirements (PERM labor certification) have become more elaborate without improving actual shortage detection.
 *
 * PERSPECTIVAL GAP:
 *   The foreign skilled worker sees a snare: employer dependency, delayed green cards, constrained bargaining power, and multi-year immigration queues create a locked-in extraction mechanism with minimal coordination benefit to the worker themselves. Incumbent domestic workers see tangled rope: genuine wage and employment protection (coordination benefit) coexists with knowledge that this protection depends on continuing gatekeeping (asymmetric extraction toward protected workers, away from foreign workers and employers seeking unrestricted access). Visa-sponsoring employers see rope: the system solves the coordination problem of establishing hiring channels, managing integration, and retaining talent through green card linkage — pure coordination benefit with minimal cost. Immigration bureaucrats see piton: the formal certification and audit machinery is substantially performative (cannot effectively verify labor shortage), yet they maintain authority and jurisdiction through perpetuating the ritual. Remote-work and startup founders see scaffold: visa gatekeeping is being structurally bypassed through geographic employment arbitrage, founder visa categories, and remote work agreements that shift employment nominally outside US borders. This sunset mechanism is real and visible to organized agents but invisible or irrelevant to trapped workers and formal bureaucracy. The analytical observer sees tangled rope at civilizational scale: both coordination (protecting domestic labor stability) and extraction (preventing efficient global labor allocation) are structurally real and value-laden; the observer cannot dissolve this tension within the existing framework.
 *
 * DIRECTIONALITY LOGIC:
 *   The extraction flow in this constraint runs from foreign workers → visa sponsors and protected domestic sectors. Visa sponsors (institutional/arbitrage) have low directionality (d ≈ 0.15) because they control sponsorship as a voluntary mechanism and derive net benefit. Protected domestic workers (organized/constrained) have moderate directionality (d ≈ 0.55) because they benefit from the constraint but face retraining costs if it is removed — their protection is real but contingent on others remaining constrained. Foreign workers (powerless/trapped) have maximum directionality (d ≈ 0.95) because they bear comprehensive structural constraints with minimal exit options. Immigration bureaucracy (institutional/arbitrage) has low directionality (d ≈ 0.20) because they maintain regulatory authority and extract rents through jurisdiction maintenance. This distribution produces the observed perspectival gap: agents with low d experience the constraint as coordinating or beneficial (rope, scaffold), while agents with high d experience it as extractive (snare).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy by maintaining a genuine coordination function within a tangled-rope structure. The coordination benefit is real: the system does establish hiring channels, manage labor market integration, and provide some immigration pathway (albeit delayed and constrained). The extraction is also real: the system maintains artificial wage premiums through supply constraints, employer lock-in, and delayed settlement. The tangled-rope classification resolves the potential confusion (is this coordination or extraction?) by declaring it both. The perspectival gap confirms this: powerless trapped agents see extraction (snare); beneficiaries see coordination (rope); organized agents with partial exit see both (tangled rope); the analytical observer sees both as structurally real (tangled rope at civilizational scale). The false summit risk is captured in the bureaucratic perspective: the immigration system's self-understanding as a sophisticated merit-based allocation mechanism (mountain narrative of objective gatekeeping) is contradicted by acknowledged arbitrariness (H-1B lottery, per-country caps that have no relationship to actual labor shortage). The analytical perspective avoids naturalizing the constraint as inherent to international labor mobility (which would be a false mountain) by showing that alternative coordination mechanisms (remote work, international founder visas, portable benefits) are structurally feasible and are being built by organized agents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_labor_shortage_vs_preference,
    'How to distinguish genuine skills shortage requiring visa access from employer preference for cheaper constrained labor?',
    'Comparative wage analysis: if visa workers earn substantially below native-worker market rates for identical roles, preference mechanism is likely operative. Regional labor-market tightness indices: measure native unemployment vs. job openings by skill level and geography.',
    'If genuine shortage: visa restriction is supply-side coordination problem (Rope from analyst perspective). If preference: visa restriction is demand-side extraction mechanism (Snare from analyst perspective). Distinction changes classification from rope-heavy to snare-heavy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_labor_shortage_vs_preference, empirical, 'Whether labor shortages are genuine or employer preference-driven').

omega_variable(
    lock_in_mechanism_specificity,
    'How much of the foreign worker''s extraction is due to visa dependency vs. general immigrant labor market disadvantage (language, networks, credential recognition)?',
    'Comparative outcome analysis: measure wage growth, mobility, career progression for visa-sponsored workers vs. native workers and vs. immigrants in similarly constrained non-visa visa categories. Control for skill level and field.',
    'If primarily visa-specific: snare classification is correct; visa reform would substantially improve outcomes. If general immigrant disadvantage: visa specificity may be overstated; reform might have limited effect on relative extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lock_in_mechanism_specificity, empirical, 'Visa-specific vs. general immigrant labor disadvantage').

omega_variable(
    downstream_innovation_impact,
    'Does restricting visa access reduce US innovation and competitiveness, converting the snare into a self-harming constraint?',
    'Patent citation analysis: compare innovation output per capita in visa-restrictive vs. visa-permissive sectors. Startup formation rates and founder immigration status. Brain drain to competitors (Canada, Australia, EU).',
    'If innovation loss exceeds protectionist gain: constraint satisfies snare criteria structurally but fails on strategic interest (extraction becomes self-defeating). May justify reclassification as piton (theater persists despite loss of function) or justify scaffold sunset mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(downstream_innovation_impact, empirical, 'Whether visa restrictions damage US innovation and competitiveness').

omega_variable(
    green_card_queue_pathology,
    'Do per-country green card caps and employment-based immigration queues (10+ year waits for certain nationals) represent gatekeeping extraction or backend labor policy design?',
    'Historical legislative intent analysis: did caps result from deliberate protectionist intent or from statutory accident (per-country equity provisions designed for family reunification, repurposed for employment)? Compare to competing jurisdictions'' green card pathways.',
    'If intentional extraction: component of snare mechanism. If accidental: reveals piton dynamics (procedural theater persisting despite acknowledged dysfunction). Affects whether reform requires active protectionist interest removal or regulatory modernization.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(green_card_queue_pathology, conceptual, 'Green card queue delays as intentional or accidental policy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_employment_visa_gatekeeping, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(visagate_tr_t0, us_employment_visa_gatekeeping, theater_ratio, 0, 0.48).
narrative_ontology:measurement(visagate_tr_t15, us_employment_visa_gatekeeping, theater_ratio, 15, 0.58).
narrative_ontology:measurement(visagate_tr_t30, us_employment_visa_gatekeeping, theater_ratio, 30, 0.64).
narrative_ontology:measurement(visagate_tr_t10, us_employment_visa_gatekeeping, theater_ratio, 10, 0.54).

% Extraction over time
narrative_ontology:measurement(visagate_be_t0, us_employment_visa_gatekeeping, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(visagate_be_t15, us_employment_visa_gatekeeping, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(visagate_be_t30, us_employment_visa_gatekeeping, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(visagate_be_t10, us_employment_visa_gatekeeping, base_extractiveness, 10, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_employment_visa_gatekeeping, resource_allocation).
narrative_ontology:affects_constraint(us_employment_visa_gatekeeping, us_startup_founder_visa_pathways).
narrative_ontology:affects_constraint(us_employment_visa_gatekeeping, remote_work_employment_jurisdiction).
narrative_ontology:affects_constraint(us_employment_visa_gatekeeping, brain_drain_to_competing_jurisdictions).
narrative_ontology:affects_constraint(us_employment_visa_gatekeeping, protected_sector_wage_levels).

% DUAL FORMULATION NOTE:
% The US employment visa system decomposes into structurally distinct constraints: (1) green-card-pathway immigration delay (ε ≈ 0.72, pure snare for employment-based categories), (2) H-1B labor certification and employer lock-in (ε ≈ 0.58, tangled rope), (3) L-1/O-1 visa flexibility for specialized categories (ε ≈ 0.25, rope), (4) remote work and geographic arbitrage bypass mechanisms (ε ≈ 0.15, scaffold with sunset). This story focuses on the integrated system (ε ≈ 0.58, tangled rope) and links to the downstream constraints that are emerging as the system's limitations become apparent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_employment_visa_gatekeeping, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
