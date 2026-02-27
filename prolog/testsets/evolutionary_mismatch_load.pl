% ============================================================================
% CONSTRAINT STORY: evolutionary_mismatch_load
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_evolutionary_mismatch_load, []).

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
 *   constraint_id: evolutionary_mismatch_load
 *   human_readable: The Paleolithic Circuit Break
 *   domain: biological/technological/social
 *
 * SUMMARY:
 *   The evolutionary mismatch load is a structural constraint operating
 *   across biological, technological, and social domains. Human neurobiology,
 *   metabolism, and social instincts were sculpted by natural selection in
 *   ancestral environments characterized by scarcity, face-to-face groups,
 *   physical movement, circadian synchronization, and delayed gratification.
 *   The modern technological and economic landscape inverts nearly every
 *   parameter: abundance of high-calorie processed foods engineered for
 *   supernormal stimuli, digital platforms extending tribal instincts to
 *   millions of strangers, sedentary work and leisure, artificial lighting
 *   disrupting circadian rhythms, and immediate gratification as a business
 *   model. The constraint manifests across multiple victim populations
 *   (individuals suffering metabolic dysfunction, obesity, and attention
 *   fragmentation; social cohesion commons fragmented by algorithmic
 *   polarization; cognitive commons degraded by constant interruption) and
 *   beneficiary populations (food manufacturers, tech platforms, advertising
 *   networks profiting from attention capture and food sales). The constraint
 *   exhibits all six DR types from different observer positions, making it a
 *   diagnostic exemplar for how evolutionary biology, economics, and
 *   technology interact to create structural extraction mechanisms.
 *
 * KEY AGENTS:
 *   - Individual Consumer: Powerless/trapped victim. Biologically wired for reward-seeking in scarcity; caught between personal agency limits and engineered supernormal stimuli.
 *   - Social Cohesion Commons: Powerless/trapped abstract collective. Tribal attention architecture hijacked by digital platforms; bears full cost of algorithmic polarization and viral outrage.
 *   - Attention Economy Beneficiary (Tech/Food/Advertising): Institutional/arbitrage. Captures value from human attention and consumption; designs systems that exploit mismatch load.
 *   - Public Health Authority: Moderate/constrained. Sees the mismatch as a public health problem requiring environmental intervention; constrained by industry resistance and budget limits.
 *   - Countermeasure Coalition (Wellness/Ancestral-Health Movements): Organized/constrained. Builds alternative pathways (local food, digital minimalism, ancestral practices); perceives sunset clause as these norms mature.
 *   - Wellness Theater Industry: Organized/constrained. Maintains performative solutions (fitness, supplements, biohacking) that address symptoms while reinforcing extraction mechanisms.
 *   - Workplace (Employer-Employee): Moderate/constrained. Both parties experience coordination problem (getting work done) and extraction problem (using mismatch vulnerabilities to extract surplus labor).
 *   - Analytical Observer: Analytical/analytical. Risks naturalizing contingent architectural choices (high-fructose corn syrup, algorithmic feeds, sedentary work) as immutable biological constraints.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(evolutionary_mismatch_load, 0.58).
domain_priors:suppression_score(evolutionary_mismatch_load, 0.68).
domain_priors:theater_ratio(evolutionary_mismatch_load, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(evolutionary_mismatch_load, extractiveness, 0.58).
narrative_ontology:constraint_metric(evolutionary_mismatch_load, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(evolutionary_mismatch_load, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(evolutionary_mismatch_load, tangled_rope).
narrative_ontology:human_readable(evolutionary_mismatch_load, "The Paleolithic Circuit Break").
narrative_ontology:topic_domain(evolutionary_mismatch_load, "biological/technological/social").

domain_priors:requires_active_enforcement(evolutionary_mismatch_load).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(evolutionary_mismatch_load, attention_economy_platforms).
narrative_ontology:constraint_beneficiary(evolutionary_mismatch_load, processed_food_manufacturers).
narrative_ontology:constraint_beneficiary(evolutionary_mismatch_load, sedentary_lifestyle_vendors).
narrative_ontology:constraint_victim(evolutionary_mismatch_load, metabolic_health).
narrative_ontology:constraint_victim(evolutionary_mismatch_load, cognitive_capacity).
narrative_ontology:constraint_victim(evolutionary_mismatch_load, social_cohesion).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL CONSUMER — Powerless and trapped. Human reward systems evolved for scarcity; high-calorie foods and social signals were survival adaptations. In environments of abundance and engineered palatability, these same systems drive obesity, metabolic dysfunction, and compulsive consumption. Individual exit is exceptionally costly: resisting engineered food rewards requires continuous willpower against systems explicitly designed to hijack reward pathways. Reorganizing social life away from digital attention capture requires institutional support the individual cannot unilaterally create. Maximum experienced extraction.
constraint_indexing:constraint_classification(evolutionary_mismatch_load, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SOCIAL COHESION COMMONS — Abstract collective good with no exit and no advocate. Paleolithic attention architecture was optimized for face-to-face groups of 50-150 (Dunbar's number). Digital platforms extend this architecture to millions of strangers, triggering tribal instincts that produce outrage, polarization, and ingroup/outgroup violence. The commons cannot organize or exit — it bears the full cost of viral rage dynamics without benefit. Maximum extraction with zero agency.
constraint_indexing:constraint_classification(evolutionary_mismatch_load, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: ATTENTION ECONOMY BENEFICIARY (INSTITUTIONAL) — Tech platforms, food corporations, and advertising networks perceive this constraint as coordination: they are solving the problem of capturing and monetizing human attention. The evolutionary mismatch is invisible to them as an extraction mechanism — they see product engagement, food sales, and profitable ad targeting. Their arbitrage exit option (can redeploy capital elsewhere if this market saturates) and institutional power position them as net beneficiaries. They experience this as Rope: coordination around scalable value capture.
constraint_indexing:constraint_classification(evolutionary_mismatch_load, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PUBLIC HEALTH AUTHORITY — Moderate power, constrained exit. Public health sees the mismatch as a genuine coordination problem: individual behavior change is impossible without environmental intervention, yet the beneficiary institutions resist regulation. Public health has agency (can design policy, fund research) but lacks exit options — cannot withdraw from managing the consequences without harm to constituents. Experiences both coordination function (designing obesity interventions) and asymmetric extraction (bearing costs of failed interventions while food companies retain profits). Tangled Rope classification reflects this hybrid structure.
constraint_indexing:constraint_classification(evolutionary_mismatch_load, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: COUNTERMEASURE COALITION — Organized actors (public health movements, biohacking communities, digital minimalism advocates, paleo/ancestral health groups) experience the mismatch as a temporary coordination failure with a sunset clause. They perceive alternative pathways: local food systems, screen-free communities, attention-aware interface design, and rewilding of human attention architecture. These are not permanent solutions but transition mechanisms. Theater is moderate (countermeasures involve performative wellness signaling) but sunset is visible: as ancestral-aligned practices become mainstream norms, the extraction mechanism weakens. Low effective extraction because this perspective has agency and sees an exit path.
constraint_indexing:constraint_classification(evolutionary_mismatch_load, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: WELLNESS THEATER INDUSTRY — Organized actors (fitness influencers, supplement companies, biohacking gurus) maintain elaborate performance around 'optimizing' the mismatch without resolving it. Crossfit, intermittent fasting, nootropics, and ancestral diet communities are substantially performative — they address symptoms rather than structural causes. The theater persists through institutional inertia (people continue because peers continue) despite low functional verification that these interventions scale beyond individual practitioners. Theater ratio is high; the primary function (selling products and status signals) is distinct from the stated function (resolving evolutionary mismatch).
constraint_indexing:constraint_classification(evolutionary_mismatch_load, piton,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: WORKPLACE DYNAMIC — Employers and employees both experience this constraint as tangled rope at the workplace level. Employees' circadian rhythms, attention spans, and social bonding needs evolved for different work structures; employers design for efficiency using these mismatches as leverage (surveillance, always-on culture, open offices disrupting focus). Neither party can exit the constraint easily (employees need income, employers depend on coordinated labor), but both experience extraction (overwork, burnout, productivity decline). The constraint is simultaneously coordination (solving the problem of getting work done) and extraction (using mismatch vulnerabilities to extract surplus labor).
constraint_indexing:constraint_classification(evolutionary_mismatch_load, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW — From a civilizational/universal perspective, the mismatch between ancestral adaptation and modern environment is an immutable feature of human evolution: we cannot redesign our neurobiology faster than our environment changes, and the gap is a structural constant. This perspective sees the mismatch as Mountain. However, the structural data contradicts this — the mismatch is not unchangeable (human neuroplasticity, environmental design choices, institutional arrangements are all malleable) and is contingent on specific technological and economic choices (high-fructose corn syrup, algorithmic attention capture, sedentary work design). The analytical observer risks naturalizing contingent arrangements as biological law.
constraint_indexing:constraint_classification(evolutionary_mismatch_load, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(evolutionary_mismatch_load_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(evolutionary_mismatch_load, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(evolutionary_mismatch_load, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(evolutionary_mismatch_load, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(evolutionary_mismatch_load, TR),
    TR >= 0.70.

:- end_tests(evolutionary_mismatch_load_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.58): High-moderate. The beneficiary institutions (food manufacturers, tech platforms) capture measurable surplus from the mismatch: food companies extract profit margins from engineered hyperpalatable products; tech platforms extract advertiser revenue from attention capture; workplace employers extract surplus labor by exploiting attention fragmentation and circadian misalignment. However, the extraction is not maximal (0.70+) because much of the surplus is reinvested in maintaining the consumer base rather than pure rent-seeking, and because individual behavioral responses (dietary changes, digital minimalism, work-life boundaries) do reduce the extraction pressure at the margin. Suppression (0.68): High. Multiple barriers prevent exit: biological reward systems are not easily overridden; food environment is designed for addiction; digital platforms are structurally addictive; career success depends on participating in attention economy; social signaling is mediated through platform participation. However, suppression is not total (0.90+) because some populations do achieve sustained behavioral changes through community support, institutional redesign, and explicit cognitive effort. Theater ratio (0.55): Moderate. Significant performative components exist (wellness industry, biohacking, fitness culture) but are not dominant. The core extraction mechanisms (engineered food, algorithmic feeds, sedentary work) operate primarily through direct reward hijacking rather than performative theater. Claimed type (Tangled Rope): The constraint exhibits both genuine coordination functions (platforms do enable real communication, processed food does provide reliable calories, employers do coordinate labor) and asymmetric extraction (mismatch vulnerabilities are exploited, not incidentally mitigated). The active enforcement requirement is met through continuous technological and product design choices that maintain the mismatch environment.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full range of DR classification from a single set of base properties, revealing that the mismatch is both objective and perspectival. The beneficiary institution sees coordination (Rope) — they are genuinely solving problems of scale and consumer access. The individual victim sees a trap (Snare) — they experience biological imperatives they cannot override. The public health authority sees a mixed coordination-extraction problem (Tangled Rope) — both functions are structurally present. The organized countermeasure movement sees a temporary problem (Scaffold) — alternative pathways exist with a sunset clause. The wellness industry sees their own degraded practice (Piton) — solutions that worked at small scale don't scale, but the theater persists. The civilizational observer risks seeing an immutable natural law (Mountain) — the claim that 'humans can never adapt fast enough' — but the structural data reveals this as a false summit: neuroplasticity, institutional redesign, and technological architecture choices are all malleable. The perspectival gap encodes the fundamental insight that the mismatch is not a property of human biology alone (which is ancient and constant) but of the interaction between human biology and modern architectures (which are recent and contingent).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) maps each agent's structural position in the extraction flow. The individual consumer and social cohesion commons are trapped with no exit and no organizational power — they derive d ≈ 0.95 (maximum targeting), producing high f(d) ≈ 1.42, and experience maximum extraction. The institutional beneficiary (tech/food) has arbitrage exit options (can redeploy capital) and institutional power — they derive d ≈ 0.05 (maximum beneficiary positioning), producing negative f(d) ≈ -0.12, and experience negative effective extraction (they profit). The public health authority has constrained exit (cannot ignore the problem) and moderate power — they derive d ≈ 0.55 (symmetric), producing f(d) ≈ 0.75, and experience moderate extraction balanced against some coordination benefit. The countermeasure coalition has constrained exit but organizational power and visible sunset — they derive d ≈ 0.45 (moderate targeting), producing f(d) ≈ 0.55. The derivation chain priority: (1) Explicit override if declared, (2) Structural derivation from beneficiary/victim status plus exit options, (3) Canonical fallback if no beneficiary/victim data. In this case, all directionality values are structural derivations from the declared beneficiaries/victims and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint resolves the core mandatrophy by distinguishing extraction from coordination at the structural level. The beneficiary institutions claim the constraint is pure coordination (Rope: 'We're solving the problem of delivering value at scale'). The framework reveals this as Tangled Rope: genuine coordination functions exist (platforms do enable real communication, food systems do provide reliable calories), but they are inseparable from asymmetric extraction (mismatch vulnerabilities are actively exploited, not incidentally present). The resolution mechanism is the requirement of BOTH beneficiary AND victim declarations in the tangled_rope type: you cannot claim Rope (pure coordination) if you have declared victims; if victims are structurally present, the constraint is hybrid, and the active enforcement requirement forces acknowledgment that coordination is maintained through coercion. The institutional beneficiary would fail to satisfy the Rope gates if a victim population is declared. The false natural law (mountain perspective) is exposed by asking: If this is an immutable biological law, why does it persist through specific institutional and technological choices? Why can individuals escape it through environmental change? Why do designer communities achieve different outcomes? These questions reveal that what appears as 'nature' is actually 'engineered architecture mimicking nature,' and the mismatch is persistent but not immutable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    neuroplasticity_ceiling,
    'What is the timescale of human neural and metabolic adaptation to novel environments? Can developmental plasticity in digital natives produce genuine rewiring that neutralizes mismatch load, or is neuroplasticity bounded by fundamental constraints?',
    'Longitudinal neuroscience studies of attention span, impulse control, and metabolic health in digital-native cohorts vs ancestral-like control environments; comparative analysis of neuroimaging metrics across age cohorts with differential digital exposure',
    'If adaptation is rapid and deep: the mismatch is temporary (Scaffold). If bounded by developmental windows: the mismatch is structural and persistent (Snare/Tangled Rope). If irreversible: the mismatch approaches Mountain classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neuroplasticity_ceiling, empirical, 'Timescale and depth of human neural adaptation to novel environments').

omega_variable(
    technological_reversibility,
    'Can technological and institutional architectures designed around human evolutionary constraints (ancestral alignment, low-stimulation environments, local-scale coordination) scale to support modern populations, or do they inevitably break down at large scale?',
    'Comparative case studies of intentional communities, digital minimalist networks, ancestral-health adopters; analysis of scaling limits and failure modes; economic modeling of whether ancestral-aligned production systems can sustain technological civilization',
    'If reversible and scalable: Scaffold perspective is structural (genuine sunset exists). If requires permanent sacrifice of modern tools: mismatch is an insoluble design constraint (Mountain). If partially reversible at local scale: hybrid landscape with islands of escape but global persistence of extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(technological_reversibility, conceptual, 'Scalability of technological designs aligned with evolutionary constraints').

omega_variable(
    extraction_intentionality,
    'Is the evolutionary mismatch load primarily driven by intentional exploitation (food/tech companies actively engineering for addiction) or by emergent misalignment (companies optimize for engagement without explicit manipulation)?',
    'Internal corporate documentation analysis (emails, product design docs, neuroscience hiring); neuroscientist employment patterns in tech and food industries; comparison of design outcomes with addiction science literature; behavioral economic analysis of profit-maximizing strategies',
    'If intentional: classification is Snare/Tangled Rope from all perspectives (deliberate extraction). If emergent misalignment: classification shifts toward Tangled Rope/Scaffold (tragic coordination failure, potentially remediable). This is critical for legal and policy response — intentional extraction justifies coercive correction; emergent misalignment justifies coordination redesign.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_intentionality, empirical, 'Degree of intentional exploitation vs emergent misalignment in mismatch-driving technologies').

omega_variable(
    metabolic_irreversibility,
    'How much of the obesity and metabolic dysfunction load is reversible through environmental change vs locked in by early-life epigenetic programming?',
    'Longitudinal metabolic studies of individuals relocating to low-calorie-abundance environments; epigenetic analysis of metabolic gene expression in high-calorie vs ancestral-diet cohorts; intergenerational tracking of health outcomes',
    'If highly reversible: individuals can exit the trap (exit options upgrade from trapped to constrained). If locked in: the mismatch becomes a permanent metabolic burden (trapped exits remain stable). Affects the biographical time horizon classification for powerless agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metabolic_irreversibility, empirical, 'Reversibility of metabolic dysfunction under environmental change').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(evolutionary_mismatch_load, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(evomis_theater_paleolithic, evolutionary_mismatch_load, theater_ratio, 0, 0.2).
narrative_ontology:measurement(evomis_theater_postindustrial, evolutionary_mismatch_load, theater_ratio, 50, 0.4).
narrative_ontology:measurement(evomis_theater_digital_present, evolutionary_mismatch_load, theater_ratio, 100, 0.55).

% Extraction over time
narrative_ontology:measurement(evomis_extract_paleolithic, evolutionary_mismatch_load, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(evomis_extract_postindustrial, evolutionary_mismatch_load, base_extractiveness, 50, 0.45).
narrative_ontology:measurement(evomis_extract_digital_present, evolutionary_mismatch_load, base_extractiveness, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(evolutionary_mismatch_load, global_infrastructure).
narrative_ontology:affects_constraint(evolutionary_mismatch_load, attention_fragmentation_economy).
narrative_ontology:affects_constraint(evolutionary_mismatch_load, metabolic_dysregulation_infrastructure).
narrative_ontology:affects_constraint(evolutionary_mismatch_load, sedentary_work_constraint).
narrative_ontology:affects_constraint(evolutionary_mismatch_load, social_tribalism_amplification).

% DUAL FORMULATION NOTE:
% The evolutionary mismatch load decomposes into multiple downstream constraints that share a common upstream structural cause (the interaction between evolved human traits and modern architectures) but have distinct ε values and extraction mechanisms. Attention fragmentation is primarily a supernormal stimulus problem (ε ≈ 0.45); metabolic dysregulation is primarily an engineered food problem (ε ≈ 0.62); sedentary work is primarily an incentive problem (ε ≈ 0.38); social tribalism amplification is primarily an algorithmic problem (ε ≈ 0.55). All four are affected by the core mismatch but each represents a distinct structural constraint with its own perspectives and measurement data. The upstream mismatch is the shared root; the downstream constraints are the domain-specific instantiations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(evolutionary_mismatch_load, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
