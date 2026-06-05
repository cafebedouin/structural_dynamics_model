% ============================================================================
% CONSTRAINT STORY: unconditional_income_support__dependency_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE_KERNEL_READING]
% ============================================================================

:- module(constraint_unconditional_income_support__dependency_trap_reading, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: unconditional_income_support__dependency_trap_reading
 *   human_readable: Unconditional Income Support as Incentive-Distorting Dependency Trap
 *   domain: political_economy/welfare_state/income_redistribution
 *
 * SUMMARY:
 *   This constraint story instantiates the dependency-trap reading of
 *   unconditional income support: a policy mechanism that claims to provide
 *   autonomy-enabling universality but structurally redistributes resources
 *   upward (to non-needy recipients and the political class) while harming
 *   the poorest through program substitution and employment disincentives.
 *   The reading foregrounds the fiscal and behavioral mechanisms by which UBI
 *   operates as a snare—a pure extraction constraint disguised as pure
 *   coordination. The working poor face maximum extraction: their specialized
 *   programs (housing assistance, childcare subsidies, targeted EITC
 *   supplements) worth $18,000–$24,000 annually are consolidated into a flat
 *   $12,000 UBI payment, with no offsetting gains. The employment
 *   disincentive (-3.2% from AEI meta-analysis) reduces labor supply, tax
 *   revenue, and wage growth, increasing the extraction burden on remaining
 *   workers. Middle and upper-class recipients capture pure transfer benefit
 *   ($12,000 annually with no behavioral response or program loss). The
 *   political class benefits from the theater of universality—claiming
 *   progressive redistribution while consolidating power over benefit
 *   administration and eliminating the specialized advocacy structures that
 *   defended targeted programs. The universality framing obscures the
 *   downward redistribution of resources from working poor to non-needy.
 *
 * KEY AGENTS:
 *   - Working Poor (powerless/trapped): Primary victims. Lose specialized programs worth $18k–$24k annually, replaced by flat $12k UBI. Face employment disincentive reducing labor income further. No exit option from income support dependency.
 *   - Middle/Upper Class Recipients (institutional/arbitrage): Primary beneficiaries. Receive $12k annually unconditionally. See policy as coordination enabling autonomy. High exit capacity (political leverage). No reduction in other benefits.
 *   - UBI Advocacy Coalition (institutional/arbitrage): Secondary beneficiaries. Capture political capital from universality framing. Advocates include progressive organizations and tech-sector interests. Benefit from program consolidation reducing specialized advocacy capacity.
 *   - Taxpayer Base (moderate/constrained): Bear $1.4 trillion net cost. Constrained by political minority status and legal tax obligation. Experience employment disincentive reducing tax-base recovery. Extraction hidden through cost-shifting to future periods.
 *   - Targeted Program Beneficiaries (moderate/constrained): Experience mixed effects. Gain administrative simplification and dignity from single payment; lose specialized support worth 50–100% more than UBI replacement value.
 *   - Political Class (institutional/arbitrage): Consolidate administrative power over benefits. Benefit from theater of universality without need to justify trade-offs. Exit capacity through policy shifts.
 *   - Analytical Observer: Identifies the constraint as a ratcheting mechanism—once consolidated, program return is structurally impossible, locking in poverty maintenance at new baseline.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__dependency_trap_reading, 0.65).
domain_priors:suppression_score(unconditional_income_support__dependency_trap_reading, 0.68).
domain_priors:theater_ratio(unconditional_income_support__dependency_trap_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__dependency_trap_reading, snare).
narrative_ontology:human_readable(unconditional_income_support__dependency_trap_reading, "Unconditional Income Support as Incentive-Distorting Dependency Trap").
narrative_ontology:topic_domain(unconditional_income_support__dependency_trap_reading, "political_economy/welfare_state/income_redistribution").

domain_priors:requires_active_enforcement(unconditional_income_support__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__dependency_trap_reading, '7328e35d-7b3b-4130-afa4-62d7909aea9a').
narrative_ontology:cs_kernel_codification('7328e35d-7b3b-4130-afa4-62d7909aea9a', distributed).
narrative_ontology:cs_authority_grounding('7328e35d-7b3b-4130-afa4-62d7909aea9a', extraction).
narrative_ontology:cs_reading_relation('7328e35d-7b3b-4130-afa4-62d7909aea9a', unconditional_income_support__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('7328e35d-7b3b-4130-afa4-62d7909aea9a', unconditional_income_support__universality_paradox_reading, coexists_with).
narrative_ontology:cs_axiom('7328e35d-7b3b-4130-afa4-62d7909aea9a', foundational, employment_response_is_harm).
narrative_ontology:cs_axiom_status(employment_response_is_harm, holdable).
narrative_ontology:cs_axiom_grounding('7328e35d-7b3b-4130-afa4-62d7909aea9a', employment_response_is_harm, empirically_contingent).
narrative_ontology:cs_axiom('7328e35d-7b3b-4130-afa4-62d7909aea9a', foundational, program_substitution_redistributes_downward).
narrative_ontology:cs_axiom_status(program_substitution_redistributes_downward, holdable).
narrative_ontology:cs_axiom_grounding('7328e35d-7b3b-4130-afa4-62d7909aea9a', program_substitution_redistributes_downward, empirically_contingent).
narrative_ontology:cs_reference_frame('7328e35d-7b3b-4130-afa4-62d7909aea9a', targeted_welfare_administration).
narrative_ontology:cs_drift_state('7328e35d-7b3b-4130-afa4-62d7909aea9a', contemporary_ubi_implementation_phase, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('7328e35d-7b3b-4130-afa4-62d7909aea9a', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(unconditional_income_support__dependency_trap_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__dependency_trap_reading, middle_upper_class_recipients).
narrative_ontology:constraint_beneficiary(unconditional_income_support__dependency_trap_reading, ubi_advocacy_coalition).
narrative_ontology:constraint_beneficiary(unconditional_income_support__dependency_trap_reading, political_class_consolidating_power).
narrative_ontology:constraint_victim(unconditional_income_support__dependency_trap_reading, working_poor).
narrative_ontology:constraint_victim(unconditional_income_support__dependency_trap_reading, taxpayer_base).
narrative_ontology:constraint_victim(unconditional_income_support__dependency_trap_reading, targeted_program_beneficiaries).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WORKING POOR (SNARE) — Face the most severe extraction. UBI replaces targeted programs worth significantly more: housing assistance, EITC supplements, childcare subsidies worth $18,000–$24,000 annually are replaced by flat $12,000 UBI payment. Trapped by dependency on public income support with no realistic exit path. Employment disincentive at -3.2% from AEI meta-analysis reduces labor income, worsening net position. Extraction is pure: constraint redistributes their targeted aid upward while keeping them income-dependent and lowering absolute purchasing power.
constraint_indexing:constraint_classification(unconditional_income_support__dependency_trap_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MIDDLE/UPPER CLASS RECIPIENTS + UBI ADVOCACY (ROPE) — Receive unconditional income support despite having no need, capturing pure transfer benefit. See the policy as coordination mechanism enabling autonomous economic participation. Possess arbitrage exit (can exit UBI framework politically through coalition leverage). Net beneficiary: receive $12,000 annually with no reduction in other benefits or income. Political capital from 'universality' framing justifies expansion. Constraint functions as pure coordination from their perspective—transfers are framed as legitimate recognition of autonomy and dignity, not extraction.
constraint_indexing:constraint_classification(unconditional_income_support__dependency_trap_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: TAXPAYER BASE (SNARE) — Bears net cost of $1.4 trillion after offsets (replacing SNAP, housing vouchers, TANF, disability supplements). Constrained by political impossibility of exit—tax obligation enforced by law, voting minority status in redistributive coalition. Employment disincentive creates deadweight loss: reduced labor supply among recipients suppresses wage growth and tax base growth, increasing per-capita tax burden. Extraction mechanism: progressively higher tax rates on earned income to fund unconditional transfers to both needy and non-needy, with employment disincentive reducing tax base recovery. Effective extraction hidden by cost-shifting to future periods (intergenerational debt).
constraint_indexing:constraint_classification(unconditional_income_support__dependency_trap_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: TARGETED PROGRAM BENEFICIARIES (TANGLED ROPE) — Experience mixed coordination and extraction. UBI consolidates multiple targeted programs, enabling some administrative coordination (one payment system vs. five). But extraction dominates: housing-assistance beneficiary loses $8,000/year in housing subsidy, childcare beneficiary loses $6,000/year in childcare support. Consolidation carries genuine administrative benefit (dignity of single payment, reduced stigma, no means-testing surveillance) but at cost of losing specialized support. Generational time horizon: UBI advocates frame this as progressive consolidation; beneficiaries experience it as dismantling of targeted protections. Constraint exhibits both real coordination function (administrative simplification, reduced stigma) and severe extraction (loss of specialized support worth 50–100% more than flat payment).
constraint_indexing:constraint_classification(unconditional_income_support__dependency_trap_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INSTITUTIONAL POLITICAL CLASS (PITON) — The 'universality' framing is substantially performative. Universal programs are politically easier to expand than targeted ones (middle-class buy-in), but universality masks severe redistribution downward from poor to non-poor. Theater ratio (0.42) reflects that political rhetoric of 'unconditional support for all' obscures actual mechanism of program substitution that harms the poorest. Institutional actors benefit from theater: can claim progressive redistribution while actually consolidating power over social benefits administration. Exit mechanism: political, via coalition shifts. Piton classification reflects that the universality performance becomes the political reality—defenders point to 'everyone receives' without acknowledging the net-harm structure for targeted beneficiaries.
constraint_indexing:constraint_classification(unconditional_income_support__dependency_trap_reading, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / DEPENDENCY TRAP READING (SNARE) — From civilizational scope, the constraint appears as a systematic mechanism that trades targeted aid for unconditional but universalized poverty maintenance. The 'incentive distortion' is not incidental but structural: employment disincentive (-3.2%) is necessary to justify the fiscal cost, and the fiscal cost is necessary to fund non-needy recipients. Civilizationally, this reading sees the constraint as a ratcheting mechanism: once UBI is established, the employment disincentive increases political demand for additional universal income support (cycle deepens dependency), and the tax base compression makes return to targeted aid politically impossible. Snare classification reflects the structural impossibility of exit for affected populations.
constraint_indexing:constraint_classification(unconditional_income_support__dependency_trap_reading, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unconditional_income_support__dependency_trap_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(unconditional_income_support__dependency_trap_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(unconditional_income_support__dependency_trap_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(unconditional_income_support__dependency_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(unconditional_income_support__dependency_trap_reading, TR),
    TR >= 0.70.

:- end_tests(unconditional_income_support__dependency_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65, rising to 0.65 by interval end): High and stable. Base value reflects the net transfer from working poor to non-needy: working poor lose $6,000–$12,000 annually in specialized program value (difference between $12k UBI and $18k–$24k targeted aid), while non-needy gain $12,000 with no offset. Employment disincentive (-3.2%) suppresses wages and tax revenue by an estimated $180–$240 billion annually—this is pure deadweight loss, not transfer, and therefore counts as extractive overhead. The stable trajectory (rather than rising) reflects that employment disincentive is set by UBI level and behavioral response at implementation; it does not accumulate further. Suppression (0.68, rising from 0.55): High and rising. Initial suppression (0.55) reflects that working poor face barriers to exit income support (job scarcity, skill gaps, family obligations) but can in principle find employment. Rising trajectory (0.55→0.62→0.68) reflects that employment disincentive increases structural suppression: as labor supply contracts economy-wide (due to UBI), job availability declines, making exit through employment increasingly costly. Suppression encodes both the intentional policy constraint (flat payment regardless of need) and the secondary behavioral constraint (employment disincentive reducing exit capacity). Theater ratio (0.42, rising from 0.28): Moderate, rising. Base value (0.28) reflects that UBI itself is relatively non-performative—it is a cash transfer, not a ritual. Rising trajectory (0.28→0.35→0.42) reflects that universality framing becomes increasingly performative as implementation reveals trade-offs. Early stage: 'universal benefit' framing is credible. Mid stage: evidence of program substitution and employment effects emerges; universality framing must work harder to mask redistribution downward. Late stage: theater becomes explicit in political defense of consolidation against calls for program restoration. The rising theater ratio indicates a Snare that must increasingly rely on ideological performance to sustain its extraction mechanism.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates extreme perspectival divergence. Working poor see a Snare (pure extraction via program substitution + employment disincentive). Middle/upper class see a Rope (pure coordination enabling autonomy). Taxpayers see a Snare (extraction via hidden cost and tax-base compression). Targeted program beneficiaries see a Tangled Rope (mixed administrative coordination + program loss). Political class see a Piton (performative universality hiding consolidation of power). Analytical observer sees a Snare with ratchet dynamics (lock-in preventing reversal). The perspectival gap is maximal: the constraint is simultaneously experienced as coordination, extraction, degraded ritual, and structural lock-in. This gap is not measurement uncertainty—it is structural. Different agents occupy genuinely different causal positions relative to the extraction mechanism. The largest gap is between beneficiaries (rope/piton) and victims (snare). This gap is diagnostic of the constraint's core structure: it functions as pure coordination for those who benefit and pure extraction for those who lose targeted support.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) measures each agent's structural position relative to the extraction flow. Working poor: d ≈ 0.92 (near-full target). They receive UBI but lose more targeted aid; employment disincentive reduces their labor income further. All flows run toward extraction. Beneficiary status is ambiguous (they do receive income support) but overwhelmed by victim status (specialized support loss dominates). Middle/upper class: d ≈ 0.08 (near-full beneficiary). They receive UBI with no program loss, no employment response, no tax increase concentrated on them. All flows run toward benefit. Institutional arbitrage exit makes them beneficiaries structurally. Taxpayers (moderate/constrained): d ≈ 0.78 (near-full target). Net cost ($1.4 trillion) is borne by tax increases. Employment disincentive reduces tax-base recovery. Constrained exit (legal tax obligation, minority voting status) makes them targets despite moderate power. Political class: d ≈ 0.15 (moderate beneficiary). They capture administrative consolidation benefit and political capital from universality framing. Institutional arbitrage exit enables them to benefit. Targeted program beneficiaries: d ≈ 0.65 (near-equal mixed position). They lose specialized support (victim status, d↑) but gain administrative simplification (modest beneficiary status, d↓). Net position is approximately balanced toward targets due to severity of program-loss magnitude. The d values are derived from beneficiary/victim declarations, power atoms, and exit options per the framework specifications. No overrides needed—the structural data is unambiguous.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint instantiates a critical mandatrophy: the dependency-trap reading claims that unconditional income support functions as a Snare (pure extraction) because it eliminates specialized support while generating employment disincentive. The freedom-floor reading claims that unconditional income support functions as a Rope or Tangled Rope (coordination with some extraction) because removal of coercive labor-market participation improves autonomy and dignity regardless of employment response. The mandatrophy is not ambiguity in measurement or observation—it is irreducible disagreement about whether employment response is a design feature (freedom reading: acceptable trade-off for autonomy) or a design flaw (dependency-trap reading: makes the program extractive). The resolution requires normative commitment: Which matters more—employment levels or autonomy from coercion? The empirical fact of -3.2% employment response does not resolve the mandatrophy; it is interpreted through the normative lens of each reading. The dependency-trap reading uses employment response as evidence of extraction (the program harms the poor by reducing their income through employment loss). The freedom-floor reading uses autonomy gains as evidence of benefit (the program improves the poor's position by removing coercive labor participation). Both readings are internally coherent and empirically grounded. The Snare classification in this story reflects the dependency-trap axiom (employment response is a harm indicator). A separate story instantiating the freedom-floor reading would classify the same constraint as Rope or Tangled Rope, reflecting the freedom axiom (autonomy is a benefit indicator). Both stories can coexist in the corpus because they are readings of the same contested kernel, not contradictory claims about an identical constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    employment_disincentive_magnitude,
    'Is the -3.2% employment effect from AEI meta-analysis accurate, or does it reflect publication bias toward negative findings and selection effects from study design?',
    'Large-scale RCT with full randomization, long-term follow-up (5+ years post-program), cross-domain labor market data (formal, informal, gig), and pre-specified outcome metrics published before analysis',
    'If accurate: -3.2% employment reduction translates to $180–$240 billion annual deadweight loss in foregone wages and tax revenue. Snare classification confirmed. If biased downward (true effect 0–1%): employment cost is minimal, and program functions more like Rope or Tangled Rope depending on program design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(employment_disincentive_magnitude, empirical, 'Magnitude and direction of employment disincentive from UBI').

omega_variable(
    program_substitution_vs_expansion,
    'Are targeted programs actually eliminated (program substitution reading) or do both UBI and targeted aid coexist (expansion reading)?',
    'Historical budget analysis: post-UBI implementation, do targeted programs (SNAP, housing, childcare) receive continued funding at baseline levels, or are they explicitly eliminated/defunded? Track legislative intent through bill text and appropriations.',
    'If substitution (this reading): net cost absorbed by working poor through loss of specialized support. If expansion (sibling reading): UBI becomes additive entitlement, and extraction mechanism is purely tax-base compression, not program replacement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(program_substitution_vs_expansion, empirical, 'Whether UBI replaces or supplements existing targeted programs').

omega_variable(
    distributional_incidence_across_income_quintiles,
    'What is the actual net distribution of benefits and costs by income quintile? Do non-needy recipients in top quintiles receive more than bottom quintile loses?',
    'Microsimulation using tax data and program participation rates: compute net transfer (UBI received minus incremental tax paid) for each decile; compare to baseline targeted program distribution. Include indirect effects (wage suppression, inflation, tax-base effects).',
    'If top quintile nets $2,000+ annually while bottom quintile nets $-4,000 (program loss > UBI gain): confirms snare classification with upward redistribution. If quintiles net approximately equally: reading collapses to Rope or freedom-floor reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(distributional_incidence_across_income_quintiles, empirical, 'Net distributional incidence of UBI across income quintiles').

omega_variable(
    sibling_reading_kernel_contest,
    'Does the dependency-trap reading''s core axiom (employment response dominates, making unconditional support a net poverty trap) logically foreclose the freedom-floor reading''s core axiom (removal of labor coercion increases autonomy regardless of employment response)?',
    'Conceptual analysis: can both axioms be true simultaneously in a single normative framework? If an agent has reduced employment but increased autonomy (due to lower survival anxiety), which value dominates? How do different normative frameworks rank employment vs. autonomy?',
    'If axioms foreclose each other: the two readings are incompatible, and one must be rejected. If coexistent: the two readings represent genuinely different value priorities, and the kernel contest is irresolvable (conceptual omega, not empirical).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_kernel_contest, conceptual, 'Whether dependency-trap and freedom-floor readings foreclose each other axiomatically').

omega_variable(
    political_reversibility_of_program_consolidation,
    'Once targeted programs are consolidated into UBI, can they be re-established? Or does program consolidation create political lock-in that makes return to specialized support structurally impossible?',
    'Comparative institutional analysis: study past welfare consolidations (PRWORA 1996, welfare-to-work transitions) and reversibility. Track whether programs once consolidated can be disaggregated. Examine veto-point structure in legislatures post-consolidation.',
    'If irreversible: the constraint is a ratchet—it locks in upward redistribution and prevents future program specialization. If reversible: the constraint is a temporary policy choice, not structural. Affects assessment of whether this is a Snare (irreversible extraction) or a Scaffold (temporary with exit path).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_reversibility_of_program_consolidation, empirical, 'Political reversibility of program consolidation into UBI').

omega_variable(
    universality_as_committer_framing,
    'This reading instantiates one committer frame (dependency trap via program replacement). The freedom-floor reading instantiates another (autonomy floor via coercion removal). Does the universality-paradox reading constitute a third committer frame, or is it a meta-reading that observes the contest between the first two?',
    'Examine the universality-paradox reading''s core claim: if it argues that UBI''s cross-ideological appeal masks incompatible implementation paths, it is claiming that the kernel (unconditional income support) admits multiple readings that are genuinely incompatible at implementation time, not merely normatively different. This suggests the universality-paradox is a meta-reading observing the underspecification of the kernel itself.',
    'If meta-reading: the constraint is more fundamentally a problem of kernel ambiguity than a choice between readings. If co-equal reading: all three readings are perspectival choices at the same logical level. Affects how the engine models the contest structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(universality_as_committer_framing, conceptual, 'Whether universality-paradox reading is meta-reading or co-equal sibling').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__dependency_trap_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uis_dep_theater_t0, unconditional_income_support__dependency_trap_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(uis_dep_theater_t3, unconditional_income_support__dependency_trap_reading, theater_ratio, 3, 0.35).
narrative_ontology:measurement(uis_dep_theater_t6, unconditional_income_support__dependency_trap_reading, theater_ratio, 6, 0.42).

% Extraction over time
narrative_ontology:measurement(uis_dep_extractiveness_t0, unconditional_income_support__dependency_trap_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(uis_dep_extractiveness_t3, unconditional_income_support__dependency_trap_reading, base_extractiveness, 3, 0.58).
narrative_ontology:measurement(uis_dep_extractiveness_t6, unconditional_income_support__dependency_trap_reading, base_extractiveness, 6, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(uis_dep_suppression_t0, unconditional_income_support__dependency_trap_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(uis_dep_suppression_t3, unconditional_income_support__dependency_trap_reading, suppression_requirement, 3, 0.62).
narrative_ontology:measurement(uis_dep_suppression_t6, unconditional_income_support__dependency_trap_reading, suppression_requirement, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__dependency_trap_reading, resource_allocation).
narrative_ontology:affects_constraint(unconditional_income_support__dependency_trap_reading, unconditional_income_support__freedom_floor_reading).
narrative_ontology:affects_constraint(unconditional_income_support__dependency_trap_reading, unconditional_income_support__universality_paradox_reading).
narrative_ontology:affects_constraint(unconditional_income_support__dependency_trap_reading, labor_market_employment_subsidy_tradeoff).
narrative_ontology:affects_constraint(unconditional_income_support__dependency_trap_reading, welfare_program_consolidation_lock_in).

% DUAL FORMULATION NOTE:
% The unconditional_income_support kernel admits three structurally distinct readings, each with different epsilon values and classification types. This story (dependency_trap_reading, ε=0.65, Snare) models the constraint as extractive via program substitution and employment disincentive. The freedom_floor_reading (ε=0.30, Rope) models the constraint as coordination enabling autonomy. The universality_paradox_reading (ε=0.52, Tangled Rope) models the constraint as political ambiguity masking incompatible implementation paths. All three stories share the same base policy mechanism (cash transfers without conditions) but diverge on whether the mechanism primarily coordinates or extracts, and on whether employment response is a harm or acceptable trade-off. They are not alternative measurements of one constraint; they are genuinely different normative readings of one contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
