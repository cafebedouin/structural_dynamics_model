% ============================================================================
% CONSTRAINT STORY: school_choice_system
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_school_choice_system, []).

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
 *   constraint_id: school_choice_system
 *   human_readable: School Choice System: Coordination and Asymmetric Extraction
 *   domain: education/policy
 *
 * SUMMARY:
 *   School choice systems (charter schools, vouchers, open enrollment across
 *   district lines, selective schools within districts) represent a policy
 *   mechanism for decoupling school assignment from neighborhood residence.
 *   The stated coordination function is genuine: rigid assignment to failing
 *   neighborhood schools prevents families from accessing alternatives and
 *   prevents successful schools from expanding. However, the choice
 *   architecture simultaneously implements an extraction mechanism that sorts
 *   families by ability to navigate the system, concentrates resources in
 *   schools serving advantaged populations, and reduces funding available to
 *   high-need students. The constraint exhibits all characteristics of
 *   Tangled Rope: real coordination benefits for mobile, informed families;
 *   real extraction from trapped, immobile families; active enforcement
 *   through admissions policies, funding allocation, and application
 *   requirements; and a theater ratio that has increased over time as choice
 *   systems have emphasized aspiration and meritocratic framing while sorting
 *   intensifies. The extractiveness trajectory shows acceleration: initial
 *   choice systems were relatively low-extraction (0.28), creating genuine
 *   new options for constrained middle-class families. As systems matured,
 *   cream-skimming effects accumulated, peer composition sorting intensified,
 *   and funding mechanisms produced cascading effects (fixed costs remaining
 *   in public schools while per-pupil funding declined), raising
 *   extractiveness to current levels (0.52). Theater has similarly increased
 *   as choice systems rely on transparency reports and merit-based framing to
 *   justify outcomes that are increasingly determined by family background
 *   and ability to navigate applications.
 *
 * KEY AGENTS:
 *   - Low-income families: Primary victims (powerless/trapped) — Cannot exercise choice due to transportation, information, application barriers, and lack of slots in quality schools; experience choice rhetoric without choice reality
 *   - Middle-class families with resources: Secondary beneficiaries (moderate/constrained) — Gain genuine choice options but at high search/application/relocation cost; mixed experience
 *   - Advantaged families: Primary beneficiaries (powerful/arbitrage) — Gain selective school access and can arbitrage through property markets; net beneficiaries with minimal extraction cost
 *   - Charter operators: Secondary beneficiaries (institutional/arbitrage) — Gain access to public funding and growth capital; operate in advantaged market position
 *   - Traditional public school system: Primary victim (institutional/constrained) — Faces declining per-pupil funding, residualization to highest-need students, and institutional legitimacy loss
 *   - Students with disabilities: Primary victims (powerless/trapped) — Systematically excluded or under-served by choice schools despite legal access; bear extraction cost through reduced service availability
 *   - Analytical observer: Sees full structure (analytical/analytical) — Recognizes genuine coordination benefit alongside extraction mechanism that targets powerless agents
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(school_choice_system, 0.52).
domain_priors:suppression_score(school_choice_system, 0.58).
domain_priors:theater_ratio(school_choice_system, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(school_choice_system, extractiveness, 0.52).
narrative_ontology:constraint_metric(school_choice_system, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(school_choice_system, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(school_choice_system, tangled_rope).
narrative_ontology:human_readable(school_choice_system, "School Choice System: Coordination and Asymmetric Extraction").
narrative_ontology:topic_domain(school_choice_system, "education/policy").

domain_priors:requires_active_enforcement(school_choice_system).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(school_choice_system, advantaged_families).
narrative_ontology:constraint_beneficiary(school_choice_system, charter_operators).
narrative_ontology:constraint_beneficiary(school_choice_system, selective_schools).
narrative_ontology:constraint_victim(school_choice_system, low_income_families).
narrative_ontology:constraint_victim(school_choice_system, public_school_funding).
narrative_ontology:constraint_victim(school_choice_system, students_with_disabilities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME FAMILY (SNARE) — Structurally trapped by transportation, information, and application barriers. School choice rhetoric promises agency but delivers extraction: the family cannot exercise choice due to work schedules, lack of transportation, inability to navigate application systems, or unavailability of slots in quality schools near them. Maximum suppression; no exit options; zero perceived coordination benefit.
constraint_indexing:constraint_classification(school_choice_system, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: MOBILE MIDDLE-CLASS FAMILY (TANGLED ROPE) — Faces high but surmountable costs to exercise choice: time investment in research, application competition, possible relocation. Experiences genuine coordination benefit (expanded options if they win the competitive process) alongside extraction (time, emotional labor, status anxiety, social sorting). Constrained exit — can participate but at significant cost.
constraint_indexing:constraint_classification(school_choice_system, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CHARTER SCHOOL OPERATOR (ROPE) — Experiences choice system as pure coordination mechanism for their benefit. Gains access to students, public funding, operational autonomy, and growth capital through choice architecture. Sees the system as solving a genuine problem: providing alternative to failing traditional public schools. Arbitrage options (can exit to private sector, other jurisdictions); net beneficiary.
constraint_indexing:constraint_classification(school_choice_system, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: ADVANTAGED FAMILY (TANGLED ROPE) — Experiences choice system as producing genuine coordination value (true expansion of options, real alternatives to assigned school) while simultaneously extracting from the system they use. Benefits from selective school access while profiting from property value appreciation in high-demand school zones. Arbitrage options; experiences minimal suppression; significant net benefit with some extraction embedded.
constraint_indexing:constraint_classification(school_choice_system, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: PUBLIC SCHOOL DISTRICT (PITON) — Maintains institutional legitimacy through choice-responsive rhetoric and selective school programs while core function degrades. Per-pupil funding declines as choice diverts resources; schools become sorted by family demographic; district serves increasingly immobile families (lowest-income, highest-need) with declining capacity. Theater ratio high: choice reports, selective enrollment policies, charter authorization mimics functional responsiveness while structural sorting intensifies. Trapped by statutory authority; cannot exit.
constraint_indexing:constraint_classification(school_choice_system, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Views choice system as genuinely solving a coordination problem (assignment to neighborhood schools is inflexible, preventing response to demographic change or school quality variation) while simultaneously implementing a mechanism that extracts from powerless agents (low-income families, students with disabilities, English language learners). Sees the extraction as the primary outcome: choice systems sort families by ability to navigate complexity, concentrate resources in popular schools, and reduce funding for high-need populations. The coordination benefit is real but dwarfed by the extraction mechanism.
constraint_indexing:constraint_classification(school_choice_system, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(school_choice_system_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(school_choice_system, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(school_choice_system, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(school_choice_system, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(school_choice_system, TR),
    TR >= 0.70.

:- end_tests(school_choice_system_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high and increasing. The trajectory from 0.28 to 0.52 over 15 years reflects the maturation of cream-skimming effects and funding spiral impacts. Initial choice systems did solve real coordination problems (inflexible assignment, mismatch between families and neighborhood schools) with moderate extraction. As systems expanded, the sorting mechanism became dominant: families with information, time, and social capital could navigate choice; families without these resources could not. Funding followed students, but fixed costs in traditional public schools remained, creating a negative spiral. Current extractiveness reflects that choice systems primarily redistribute access toward advantaged families rather than expand total opportunity. Suppression (0.58): Moderate-high and stable. Multiple barriers prevent low-income families from exercising choice: transportation costs, application complexity, information asymmetries, lack of availability in desired schools, implicit/explicit enrollment barriers for high-need students. These barriers are not absolute (some low-income families do exercise choice) but are sufficiently high that the distribution of choice-takers is sharply skewed toward advantaged families. Theater ratio (0.65): Moderate-high and increasing. Choice systems maintain substantial performative activity: school choice reports, transparency metrics, merit-based admissions rhetoric, equity commitments despite systematic sorting. The theater serves to legitimate outcomes determined largely by family background. As the gap between stated meritocratic principles and sorting-based outcomes has widened, theater has increased to maintain institutional legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence driven by structural position. Low-income families see Snare: choice rhetoric without choice reality, pure extraction of hope without delivery. Middle-class families see Tangled Rope: genuine new options gained at high cost. Advantaged families see Rope: coordination benefits with minimal personal cost. Charter operators see Rope: pure coordination benefit from access to public funding and advantaged student demographics. Public school system sees Piton: degraded core function maintained through selective enrollment theater. Analytical observer sees Tangled Rope: real coordination benefit (alternatives to inflexible assignment) alongside real extraction mechanism (sorting by family resources). The perspectival gap reveals that 'choice' is not a neutral policy mechanism but a sorting device: it benefits families with resources to navigate it and extracts from families without those resources.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position (beneficiary vs victim) and exit options. Low-income families are victims with trapped exit (high d ~0.90), experiencing maximum extractiveness from the system. Middle-class families are both victims (high application costs, social anxiety) and partial beneficiaries (real options if they win the competition), with constrained exit (high cost but possible), yielding moderate d (~0.60). Advantaged families are beneficiaries with arbitrage exit (can leverage choice system or exit to private schools), yielding low d (~0.20). Charter operators are beneficiaries with arbitrage exit (can exit to other jurisdictions or private sector), yielding low d (~0.15). Public school system is a victim with constrained exit (cannot exit assignment role but can strategically serve choice-advantaged populations), yielding high d (~0.75). The analytical observer has analytical exit (can observe from outside the system) with d derived from the aggregate extraction pattern (~0.72), showing that the system primarily targets powerless agents. This directionality structure explains why Snare dominates from the powerless perspective while Rope dominates from the beneficiary perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that choice systems exhibit genuine coordination benefits (solving the real problem of inflexible assignment) while simultaneously implementing an extraction mechanism (sorting by family resources) that targets powerless agents. The false summit would be naturalizing choice as either 'pure coordination' (ignoring the sorting mechanism) or 'pure extraction' (ignoring the real alternatives created). The analytical classification as Tangled Rope reflects that both functions are structural: choice systems do expand options for some families AND do sort families by ability to navigate the system. The Tangled Rope classification prevents both errors: it rejects the libertarian framing that choice is pure coordination (ignoring distributional effects) and the leftist framing that it is pure extraction (ignoring that some families genuinely benefit). The reality is that choice systems coordinate school allocation while extracting from families without navigation capacity, making them structurally hybrid. The increasing extractiveness trajectory (0.28 → 0.52) shows that as these systems mature, the extraction mechanism becomes more prominent: initial choice creates genuine new options; sustained choice sorts by family resources; mature choice systems concentrate resources in advantaged schools while funding traditional public schools serving residual populations. The mandatrophy is thus resolved not by claiming one type is 'correct' but by showing that Tangled Rope captures the structure: genuine coordination asymmetrically benefiting advantaged families, financed through extraction from powerless families.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    information_access_sufficiency,
    'Do transparent school performance data and choice information systems actually eliminate information barriers for low-income families, or do structural barriers (language, time poverty, trust) prevent utilization?',
    'Comparison of choice utilization rates by income/language across transparent vs opaque choice systems; exit surveys of families who did not exercise choice despite availability',
    'If information sufficient: suppression ≤ 0.40, reclassify as Rope from low-income perspective. If barriers persist: suppression remains high, confirms Snare classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(information_access_sufficiency, empirical, 'Whether information transparency eliminates low-income choice barriers').

omega_variable(
    cream_skimming_mechanism,
    'Do charter schools and selective programs systematically attract above-average students through explicit selection, application complexity, or peer effects, thereby mechanically reducing peer quality in traditional public schools?',
    'Longitudinal student demographic tracking; comparison of peer achievement distributions in choice schools vs public schools controlling for initial selection criteria; simulation of funding flow under counterfactual no-choice assignment',
    'If systematic cream skimming confirmed: extractiveness should increase to 0.65+; mechanism is structural, not aspirational. If cream skimming minimal: choice system may genuinely improve average outcomes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cream_skimming_mechanism, empirical, 'Extent of systematic selection bias by charter and selective schools').

omega_variable(
    funding_mechanism_neutrality,
    'Does per-pupil funding truly follow students to new schools, or do fixed costs in traditional public schools create a funding death spiral where choice systems drain resources faster than enrollment declines?',
    'Comparative financial analysis of per-pupil funding trends in high-choice vs low-choice districts; accounting for fixed vs variable cost structures',
    'If funding is neutral: Rope classification becomes more defensible. If funding creates death spiral: extractiveness increases, Tangled Rope confirmed, theater ratio may be understated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(funding_mechanism_neutrality, empirical, 'Whether per-pupil funding adequately follows students across school systems').

omega_variable(
    special_needs_access_asymmetry,
    'Do charter schools and selective programs systematically under-serve students with disabilities, English language learners, and high-cost special needs despite legal access requirements?',
    'Comparison of special needs population representation in choice schools vs public school baseline; analysis of explicit vs implicit barriers (enrollment caps, lack of services, application requirements)',
    'If systematic exclusion confirmed: victims should include ''students_with_disabilities'' and ''english_learners'' as primary categories. Extraction mechanism targets most vulnerable; suppression increases; Snare classification becomes more prominent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(special_needs_access_asymmetry, empirical, 'Whether choice schools systematically under-serve students with special needs').

omega_variable(
    choice_system_counterfactual_outcome,
    'If choice systems were removed, would student outcomes improve (due to funding consolidation and reduced sorting) or deteriorate (due to loss of alternatives and flexibility)?',
    'Comparison of jurisdictions with declining vs expanding choice; longitudinal outcome tracking; simulation of resource reallocation under assignment vs choice',
    'If outcomes improve without choice: Snare classification dominates; constraint is pure extraction. If outcomes deteriorate: Rope classification gains ground; constraint solves real coordination problem at extraction cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(choice_system_counterfactual_outcome, conceptual, 'Counterfactual outcome comparison: choice vs no-choice systems').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(school_choice_system, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(schoolchoice_tr_t0, school_choice_system, theater_ratio, 0, 0.35).
narrative_ontology:measurement(schoolchoice_tr_t5, school_choice_system, theater_ratio, 5, 0.5).
narrative_ontology:measurement(schoolchoice_tr_t10, school_choice_system, theater_ratio, 10, 0.65).
narrative_ontology:measurement(schoolchoice_tr_t15, school_choice_system, theater_ratio, 15, 0.72).

% Extraction over time
narrative_ontology:measurement(schoolchoice_be_t0, school_choice_system, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(schoolchoice_be_t5, school_choice_system, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(schoolchoice_be_t10, school_choice_system, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(schoolchoice_be_t15, school_choice_system, base_extractiveness, 15, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(school_choice_system, resource_allocation).
narrative_ontology:affects_constraint(school_choice_system, school_funding_distribution).
narrative_ontology:affects_constraint(school_choice_system, residential_segregation_amplification).

% DUAL FORMULATION NOTE:
% School choice systems operate as a distinct constraint from the underlying school assignment mechanism. The choice architecture itself is decomposed from the funding mechanism (school_funding_distribution) and the residential segregation feedback (residential_segregation_amplification). Each story has its own extractiveness: assignment rigidity (low ε), choice sorting (moderate-high ε), funding dynamics (high ε), segregation feedback (high ε). Linked through network effects: choice → funding extraction → segregation → reduced public school capacity → increased extraction from immobile families.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(school_choice_system, moderate, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
