% ============================================================================
% CONSTRAINT STORY: sotu_1981_reagan_categorical_block_grant_conversion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1981_reagan_categorical_block_grant_conversion, []).

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
 *   constraint_id: sotu_1981_reagan_categorical_block_grant_conversion
 *   human_readable: Conversion of Categorical Federal Grants to Block Grants (1981-1985)
 *   domain: social_policy/federalism/administrative_reform
 *
 * SUMMARY:
 *   The conversion of categorical federal grant programs to block grants
 *   (1981-1985, initiated in Reagan's 1981 State of the Union address)
 *   consolidates multiple narrowly-targeted federal programs (CDBG, SMSA, job
 *   training, social services) into flexible allocations to states and
 *   localities, eliminating federal mandates and performance requirements in
 *   exchange for lump-sum funding. The constraint exhibits classic
 *   tangled-rope structure: genuine coordination benefit (reduced federal
 *   reporting overhead, restored state discretionary authority) is paired
 *   with asymmetric extraction (federal program protections eliminated;
 *   vulnerable populations in low-capacity jurisdictions lose guaranteed
 *   service levels). The constraint functions as a subsidiarity mechanism,
 *   relocating decision-making authority from federal agencies to state
 *   legislatures, but the distribution of benefits and costs is highly
 *   uneven. Wealthy jurisdictions with existing administrative capacity and
 *   strong fiscal positions benefit from flexibility without losing service
 *   quality. Low-capacity, low-wealth jurisdictions lose federal program
 *   floors and face state budget pressure, resulting in service degradation.
 *   Federal program administrators experience suppression of position and
 *   authority. The false summit challenge emerges in the federalism rhetoric:
 *   the subsidiarity principle ('decisions closest to the people') is invoked
 *   as natural law, but the structural data reveals that 'closest to the
 *   people' often means 'most vulnerable to state budget cuts' and that the
 *   beneficiary group is state officials, not dispersed citizens.
 *
 * KEY AGENTS:
 *   - State Governors and State Legislatures: Primary beneficiaries (institutional/arbitrage) — capture discretionary authority over block grant allocations, eliminate federal compliance requirements, restore fiscal flexibility. Net gainers throughout the interval.
 *   - Federal Program Administrators (Career Civil Service): Primary victims (powerless/constrained) — lose program supervision authority, face position elimination, experience suppression of oversight function. Constrained by civil service protections but limited job market alternatives.
 *   - Vulnerable Program Recipients in Low-Capacity Jurisdictions: Primary victims (powerless/trapped) — lack exit options; bear extraction risk through loss of federal service guarantees and floor protection. Trapped by geography and circumstance.
 *   - Wealthy/High-Capacity Jurisdictions: Secondary beneficiaries (powerful/constrained) — benefit from flexibility without losing administrative capacity to implement programs effectively. Constrained by need to maintain political legitimacy but in position to arbitrage state flexibility against local resources.
 *   - State Budget Coalitions (National Governors Association, State Legislators Association): Organized beneficiaries (organized/mobile) — mobilize to capture block grant policy, coordinate state position, build advocacy for flexible funding. Mobile through inter-state coordination and federal policy engagement.
 *   - Federal Oversight Agencies (OMB, HHS Program Offices): Secondary actors (institutional/constrained) — initially suppressed by loss of categorical program authority; gradually build performance-based accountability mechanisms as alternative oversight mode. Constrained by political direction to devolve authority but capable of developing new oversight technologies.
 *   - Advocacy Coalitions for Vulnerable Populations (civil rights organizations, poverty advocates): Secondary victims (organized/mobile) — initially organized to oppose block grants; transition toward monitoring for floor erosion and inter-state equity. Mobile through advocacy and litigation strategy.
 *   - Analytical Observer (Federalism Doctrine): Risk of naturalizing contingent institutional choice as immutable principle; false summit candidate.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1981_reagan_categorical_block_grant_conversion, 0.38).
domain_priors:suppression_score(sotu_1981_reagan_categorical_block_grant_conversion, 0.42).
domain_priors:theater_ratio(sotu_1981_reagan_categorical_block_grant_conversion, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1981_reagan_categorical_block_grant_conversion, extractiveness, 0.38).
narrative_ontology:constraint_metric(sotu_1981_reagan_categorical_block_grant_conversion, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(sotu_1981_reagan_categorical_block_grant_conversion, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1981_reagan_categorical_block_grant_conversion, tangled_rope).
narrative_ontology:human_readable(sotu_1981_reagan_categorical_block_grant_conversion, "Conversion of Categorical Federal Grants to Block Grants (1981-1985)").
narrative_ontology:topic_domain(sotu_1981_reagan_categorical_block_grant_conversion, "social_policy/federalism/administrative_reform").

domain_priors:requires_active_enforcement(sotu_1981_reagan_categorical_block_grant_conversion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1981_reagan_categorical_block_grant_conversion, state_governors).
narrative_ontology:constraint_beneficiary(sotu_1981_reagan_categorical_block_grant_conversion, state_legislatures).
narrative_ontology:constraint_beneficiary(sotu_1981_reagan_categorical_block_grant_conversion, local_budget_officials).
narrative_ontology:constraint_victim(sotu_1981_reagan_categorical_block_grant_conversion, federal_program_administrators).
narrative_ontology:constraint_victim(sotu_1981_reagan_categorical_block_grant_conversion, low_capacity_jurisdictions).
narrative_ontology:constraint_victim(sotu_1981_reagan_categorical_block_grant_conversion, vulnerable_program_recipients).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VULNERABLE RECIPIENTS (SNARE) — Cannot exit local jurisdiction without migration. Bear full extraction risk if state reallocates block grant funds away from their program category. No federal guarantee of service level or funding floor. Trapped by geography and circumstance; suppression through dependence on local discretion.
constraint_indexing:constraint_classification(sotu_1981_reagan_categorical_block_grant_conversion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: FEDERAL PROGRAM ADMINISTRATORS (SNARE) — Career federal employees face elimination of positions, program supervision authority, and technical oversight roles. Exit options constrained by civil service protections but limited job market for specialized grant administration expertise. Extraction flows through loss of authority and institutional position, not income.
constraint_indexing:constraint_classification(sotu_1981_reagan_categorical_block_grant_conversion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: STATE GOVERNORS/LEGISLATURES (ROPE) — Net beneficiaries experience the constraint as coordination: block grants eliminate federal reporting overhead, restore fiscal discretion, and enable state-level strategic allocation. Benefits from restored authority and administrative efficiency. Can arbitrage between categorical program structures and state priorities. Extraction runs toward this agent.
constraint_indexing:constraint_classification(sotu_1981_reagan_categorical_block_grant_conversion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: WEALTHY/HIGH-CAPACITY JURISDICTIONS (TANGLED ROPE) — Experience genuine coordination benefit (reduced federal compliance burden, restored discretion) AND extract from less wealthy jurisdictions. With existing fiscal capacity and professional staff, they benefit from flexibility without losing program quality. They bear some constraint (must manage new complexity) but net beneficiary. Extraction flows from low-capacity neighbors who lose federal protections.
constraint_indexing:constraint_classification(sotu_1981_reagan_categorical_block_grant_conversion, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: STATE BUDGET COALITIONS (TANGLED ROPE) — Organized state officials mobilize to capture block grant flexibility for state priorities; simultaneously experience pressure to defend program standards when others reduce services. Mixed coordination (solve state fiscal matching problems) and extraction (states with weaker advocacy lose protection). Generational time horizon reflects the structural redistribution of power from federal to state over decades. Mobile exit through policy advocacy and inter-state coordination.
constraint_indexing:constraint_classification(sotu_1981_reagan_categorical_block_grant_conversion, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: FEDERAL OVERSIGHT AGENCIES (SCAFFOLD) — Federal oversight bodies (inspector generals, program evaluators) initially experience suppression of monitoring authority but gradually build performance-based accountability mechanisms as block grant outcomes become visible. Theater high initially (competitive federalism narrative); declines as genuine comparative state performance data emerges. Sunset: federal monitoring authority migrates from categorical control to outcome-based peer review (1985-1995).
constraint_indexing:constraint_classification(sotu_1981_reagan_categorical_block_grant_conversion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: SUBSIDIARITY DOCTRINE (PITON) — The federalism rhetoric of 'decisions closest to the people' persists as institutional legitimation long after the administrative function (accountability and equity protection) has atrophied. Theater ratio high: the principle is invoked to justify divergent outcomes that violate its own logic (wealthy areas flourish, poor areas deteriorate). The principle has become performative justification for centralized discretionary authority at the state level rather than true decentralization.
constraint_indexing:constraint_classification(sotu_1981_reagan_categorical_block_grant_conversion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / FEDERALISM NATURAL LAW (MOUNTAIN) — From a civilizational perspective, the constraint appears as an immutable structural principle of federal systems: authority necessarily concentrates at the tier with capacity to collect revenue (federal) OR diffuses to the tier with capacity to deliver services (local). Block grants represent a recalibration toward diffusion, but the underlying dynamic is 'natural' — any federal system will oscillate between centralization and devolution. However, the structural data reveals this as a false summit: identifiable beneficiaries (state officials, wealthy jurisdictions) and victims (program recipients, federal administrators) exist, suggesting the choice is contingent, not inevitable.
constraint_indexing:constraint_classification(sotu_1981_reagan_categorical_block_grant_conversion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1981_reagan_categorical_block_grant_conversion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1981_reagan_categorical_block_grant_conversion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1981_reagan_categorical_block_grant_conversion, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1981_reagan_categorical_block_grant_conversion, TR),
    TR >= 0.70.

:- end_tests(sotu_1981_reagan_categorical_block_grant_conversion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate, reflecting that the constraint has genuine coordination components (reduced federal overhead, restored state discretion) but paired with real extraction from vulnerable populations and federal employees. The trajectory shows increasing extractiveness over the interval (0.22 → 0.38) as the full impact of service devolution without federal floors becomes evident. This is not maximum-extraction snare (ε ≤ 0.25) but tangled rope (χ ≥ 0.40) because beneficiaries (state officials) genuinely experience reduced administrative burden and coordination improvement. Suppression (0.42): Moderate-high. Federal program managers face authority suppression through role elimination. Vulnerable populations face suppression through loss of federal program guarantees and exposure to state budget pressure. Low-capacity jurisdictions face suppression through administrative complexity they lack capacity to manage. However, suppression is not total (≥ 0.60): state officials retain administrative infrastructure and fiscal tools; federal employees retain civil service protections; some federal oversight continues through performance monitoring. Theater ratio (0.55): Moderate-high. The subsidiarity principle is invoked throughout the policy rhetoric ('decisions closest to the people,' 'restore local discretion') but the actual implementation prioritizes fiscal constraint and reduction of federal obligations over local decision-making. Theater increases over the interval (0.35 → 0.55) as the gap between subsidiarity rhetoric and budget-pressure-driven cuts becomes apparent.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a dramatic perspectival gap across the agent population. State officials and governors classify it as Rope (coordination benefit, restored discretion) or even Mountain (treated as natural subsidiarity principle). Federal administrators and program staff classify it as Snare (suppression of position and authority). Vulnerable recipients in low-capacity jurisdictions classify it as Snare (loss of guaranteed services). Wealthy jurisdictions and high-capacity states classify it as Tangled Rope (genuine coordination benefit mixed with advantage over low-capacity neighbors). Organized advocacy coalitions initially classify it as Snare but shift toward Tangled Rope as they adapt to monitoring within the new structure. The analytical observer classifies it as Mountain (federalism natural law) — but the structural data reveals this as a false summit, since identifiable beneficiaries and victims exist. The perspectival gap is not a matter of measurement error or observer subjectivity; it reflects genuine structural differences in how agents experience the constraint. State officials genuinely do experience reduced administrative burden. Vulnerable recipients genuinely do experience loss of service guarantees. Both are true simultaneously. The constraint is tangled rope precisely because it creates coordination benefit for one group while extracting from another.
 *
 * DIRECTIONALITY LOGIC:
 *   The chi formula χ = ε × f(d) × σ(S) applies differentially across the agent population. State governors/legislatures are beneficiaries with arbitrage-exit options: their d-value is low (d ≈ 0.15), yielding negative f(d) ≈ -0.01, so their experienced extractiveness is negative (they benefit). Federal administrators are victims with constrained exit: their d-value is high (d ≈ 0.75), yielding f(d) ≈ 1.08, so their experienced extractiveness is high (0.38 × 1.08 × 1.0 ≈ 0.41). Vulnerable recipients in low-capacity jurisdictions are victims with trapped exit: their d-value is very high (d ≈ 0.90), yielding f(d) ≈ 1.35, so their experienced extractiveness is very high (0.38 × 1.35 × 0.9 ≈ 0.46, since regional scope σ=0.9). High-capacity jurisdictions are beneficiaries with constrained exit (they benefit but must manage complexity): their d-value is moderate (d ≈ 0.35), yielding f(d) ≈ 0.40, so their experienced extractiveness is moderate (0.38 × 0.40 × 1.0 ≈ 0.15). The directionality logic reveals that the constraint's impact is highly position-dependent: the same policy shift delivers benefits to state officials (negative χ), moderate burden to high-capacity jurisdictions (positive but low χ), and severe extraction to vulnerable populations in low-capacity jurisdictions (high χ). This position-dependency is the signature of tangled rope: genuine coordination benefit paired with asymmetric extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by distinguishing genuine subsidiarity (decentralized authority paired with local coordination) from fiscal devolution (authority without resources). The ε value (0.38) reflects that the constraint is neither pure coordination (Rope, ε ≤ 0.45) nor pure extraction (Snare, ε ≥ 0.46) — it achieves genuine coordination benefit (reduced federal overhead, restored state authority) while simultaneously enabling extraction from vulnerable populations. The mandatrophy is resolved by noting that the two gains (coordination and extraction) flow to different agent groups: states gain coordination efficiency; vulnerable populations lose protection. The constraint is not ambiguous about which type to assign; rather, it demonstrates that a single policy mechanism can be rope from one position and snare from another. The analytical observer's temptation to classify it as mountain (immutable federalism principle) is diagnosed as false summit — the subsidiarity doctrine is invoked post-hoc to naturalize a contingent policy choice that could have been implemented differently (e.g., block grants WITH federal service floors, or devolution WITH capacity-building for low-income jurisdictions). The presence of identifiable beneficiaries (state officials) and documented victims (vulnerable populations in low-capacity jurisdictions) rules out genuine mountain classification. The mandatrophy is fully resolved: the constraint is tangled rope because it genuinely coordinates state-level resource allocation (eliminating federal reporting overhead) while asymmetrically extracting from vulnerable populations through loss of federal guarantees.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    federal_vs_state_capacity_mismatch,
    'Does the shift from federal to state administration represent genuine devolution to more capable local actors, or does it move authority away from actors with greater technical capacity and toward political-discretionary actors?',
    'Comparative analysis of federal vs state administrative capacity (staffing, expertise, oversight infrastructure) before and after conversion; examination of whether defects in federal administration were genuine incapacity or political constraints on enforcement.',
    'If federal was genuinely less capable: block grants represent functional improvement (Rope perspective strengthens). If federal had greater capacity but faced political constraints: block grants represent extraction mechanism that escapes accountability (Snare perspective strengthens).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(federal_vs_state_capacity_mismatch, empirical, 'Whether federal or state administration has superior capacity for categorical program delivery').

omega_variable(
    equity_floor_erosion_mechanism,
    'What proportion of block grant reallocation away from categorical program categories reflects genuine state priority-setting versus budget pressure-driven cuts to vulnerable populations?',
    'Longitudinal tracking of block grant budget allocations by state; time-series comparison with categorical program recipients in low-capacity jurisdictions; survey of state budget pressures during recession or fiscal stress periods.',
    'If reallocation driven by state priorities: mixed-case tangled rope (extraction exists but is paired with coordination benefit). If reallocation driven by fiscal pressure: transitions toward snare (suppression of recipient protections).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equity_floor_erosion_mechanism, empirical, 'Whether block grant reallocation reflects genuine devolution or fiscal pressure-driven program cuts').

omega_variable(
    administrative_overhead_reduction_validity,
    'Does elimination of federal categorical program administration genuinely reduce total administrative overhead, or does it shift overhead from federal to state/local tiers without reduction?',
    'Accounting analysis comparing federal grant administration costs (pre-conversion) to combined state/local administration costs (post-conversion), including: federal staff reduction, state staffing additions, compliance/reporting infrastructure shifts, and duplication of specialized expertise.',
    'If genuinely reduces total overhead: rope perspective (coordination efficiency) confirmed. If overhead merely shifts: theater ratio increases and constraint reclassifies toward tangled rope or piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(administrative_overhead_reduction_validity, empirical, 'Whether block grants genuinely reduce or merely relocate administrative overhead').

omega_variable(
    competitive_federalism_race_to_bottom,
    'Does inter-state competition for block grant flexibility trigger a race-to-the-bottom dynamic where states compete by cutting program protections, or does competition drive innovation in service delivery?',
    'Comparative state policy analysis tracking program eligibility, benefit levels, and service quality 1980-1990; identification of whether policy changes cluster toward cutbacks or innovation; welfare migration data examining if mobile populations move toward high-service or low-cost states.',
    'If race-to-the-bottom: snare and tangled rope perspectives confirmed (extraction mechanism); suppression through inter-state competition. If innovation: rope perspective strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competitive_federalism_race_to_bottom, empirical, 'Whether competitive federalism produces program degradation or service innovation').

omega_variable(
    false_summit_subsidiarity_doctrine,
    'Is the subsidiarity principle invoked to justify the block grant conversion a genuine commitment to localized decision-making, or a rhetorical cover for fiscal conservatism and reduction of federal program guarantees?',
    'Discourse analysis of policy rhetoric vs. actual implementation outcomes; examination of whether subsidiarity is applied consistently (decisions stay local when local choices favor expansion) or selectively (federal override when local choices cost federal budget); historical examination of prior federalism debates to assess whether subsidiarity doctrine is applied consistently across policy domains.',
    'If genuine subsidiarity commitment: constraint classified as rope (coordination) or scaffold (temporary transition). If rhetorical cover: constraint classified as snare (extraction under subsidiarity framing) or piton (performative invocation of principle).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_subsidiarity_doctrine, conceptual, 'Whether subsidiarity is genuinely applied principle or cover story for fiscal reduction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1981_reagan_categorical_block_grant_conversion, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(blockgrant_tr_t0, sotu_1981_reagan_categorical_block_grant_conversion, theater_ratio, 0, 0.35).
narrative_ontology:measurement(blockgrant_tr_t2, sotu_1981_reagan_categorical_block_grant_conversion, theater_ratio, 2, 0.48).
narrative_ontology:measurement(blockgrant_tr_t4, sotu_1981_reagan_categorical_block_grant_conversion, theater_ratio, 4, 0.55).

% Extraction over time
narrative_ontology:measurement(blockgrant_be_t0, sotu_1981_reagan_categorical_block_grant_conversion, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(blockgrant_be_t2, sotu_1981_reagan_categorical_block_grant_conversion, base_extractiveness, 2, 0.3).
narrative_ontology:measurement(blockgrant_be_t4, sotu_1981_reagan_categorical_block_grant_conversion, base_extractiveness, 4, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1981_reagan_categorical_block_grant_conversion, resource_allocation).
narrative_ontology:affects_constraint(sotu_1981_reagan_categorical_block_grant_conversion, medicaid_block_grant_caps).
narrative_ontology:affects_constraint(sotu_1981_reagan_categorical_block_grant_conversion, welfare_devolution_fiscal_pressure).
narrative_ontology:affects_constraint(sotu_1981_reagan_categorical_block_grant_conversion, state_administrative_capacity_divergence).

% DUAL FORMULATION NOTE:
% This constraint decomposes into three downstream narratives: (1) Medicaid block grant implementation (ε ≈ 0.50, tangled rope with explicit floor protection) — coordination benefit with capped extraction; (2) welfare program devolution absent floor (ε ≈ 0.65, snare) — extraction mechanism with minimal coordination; (3) state administrative capacity divergence (ε ≈ 0.35, rope/piton mixture) — coordination at capable states, piton at others. The parent constraint (0.38) represents the average across heterogeneous implementation. Downstream stories show that the constraint's actual impact depends critically on whether federal floors are preserved (Medicaid case) or eliminated (AFDC case).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1981_reagan_categorical_block_grant_conversion, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
