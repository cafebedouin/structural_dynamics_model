% ============================================================================
% CONSTRAINT STORY: mental_health_treatment_access
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mental_health_treatment_access, []).

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
 *   constraint_id: mental_health_treatment_access
 *   human_readable: Mental Health Treatment Access Constraint
 *   domain: healthcare/social_policy
 *
 * SUMMARY:
 *   Mental health treatment access operates as a hybrid
 *   coordination-extraction mechanism in which genuine coordination functions
 *   (insurance pooling, provider networks, medication standardization)
 *   coexist with asymmetric extraction mechanisms targeting economically
 *   vulnerable populations. The constraint demonstrates how access barriers
 *   operate through multiple structural channels: insurance design
 *   (copayment/deductible structures that price marginalized patients out of
 *   care), provider supply (psychiatrist concentration in wealthy urban
 *   markets), regulatory frameworks (licensure requirements that limit non-MD
 *   providers), and stigma-driven epistemic barriers. The distinguishing
 *   feature of this constraint is that the extraction is not binary —
 *   patients across income levels experience mixed coordination-extraction
 *   rather than clean victim/beneficiary division. Insurance provides genuine
 *   catastrophic risk pooling while simultaneously enabling cost-shifting to
 *   patients with greatest need and least ability to pay. This mixed
 *   character makes Tangled Rope the appropriate classification. The theater
 *   ratio (0.55) reflects moderate performativity: public mental health
 *   systems provide nominal services under severe funding constraints;
 *   insurance marketing emphasizes coverage breadth while policy incentivizes
 *   utilization restriction; treatment guidelines exist but operate as
 *   symbolic rather than binding directives due to resource constraints.
 *
 * KEY AGENTS:
 *   - Uninsured/Underinsured Patients: Primary victims (powerless/trapped) — face maximum barriers; no insurance buffer and no ability to pay out-of-pocket
 *   - Low-Income Insured Patients: Secondary victims (moderate/constrained) — have nominal coverage but cost-sharing mechanisms price them out of effective utilization
 *   - Rural Populations: Tertiary victims (moderate/constrained) — geographic isolation compounds provider scarcity and creates multi-layered access barriers
 *   - Insurance Companies: Primary beneficiaries (institutional/arbitrage) — extract through premium collection, cost-control mechanisms, and selective coverage
 *   - Pharmaceutical Manufacturers: Secondary beneficiaries (powerful/mobile) — benefit from treatment protocol standardization and captured regulatory environment
 *   - In-Network Psychiatrists: Mixed (powerful/mobile) — experience coordination benefits (patient referrals, EHR integration) alongside extraction (reimbursement rates below market clearing)
 *   - Public Mental Health System: Vestigial actor (institutional/arbitrage) — maintains theatrical presence while actual capacity degrades; functions as pressure valve for system rather than primary care provider
 *   - Advocacy Coalition: Organized challengers (organized/constrained) — attempt to coordinate patient/provider interests against insurance industry extraction; constrained by asymmetric power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mental_health_treatment_access, 0.58).
domain_priors:suppression_score(mental_health_treatment_access, 0.68).
domain_priors:theater_ratio(mental_health_treatment_access, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mental_health_treatment_access, extractiveness, 0.58).
narrative_ontology:constraint_metric(mental_health_treatment_access, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(mental_health_treatment_access, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mental_health_treatment_access, tangled_rope).
narrative_ontology:human_readable(mental_health_treatment_access, "Mental Health Treatment Access Constraint").
narrative_ontology:topic_domain(mental_health_treatment_access, "healthcare/social_policy").

domain_priors:requires_active_enforcement(mental_health_treatment_access).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mental_health_treatment_access, insurance_companies).
narrative_ontology:constraint_beneficiary(mental_health_treatment_access, pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(mental_health_treatment_access, healthcare_administrators).
narrative_ontology:constraint_victim(mental_health_treatment_access, low_income_patients).
narrative_ontology:constraint_victim(mental_health_treatment_access, uninsured_populations).
narrative_ontology:constraint_victim(mental_health_treatment_access, rural_populations).
narrative_ontology:constraint_victim(mental_health_treatment_access, public_mental_health_system).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNINSURED PATIENT IN CRISIS (SNARE) — No insurance, no savings, no exit from acute mental health crisis. Cannot delay treatment seeking. Faces either debt accumulation or untreated deterioration. System extracts maximum value through emergency room costs and debt traps. No coordination benefit experienced.
constraint_indexing:constraint_classification(mental_health_treatment_access, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INSURED PATIENT WITH HIGH COPAYS (TANGLED ROPE) — Has formal access but faces high deductibles and copayment barriers. Receives genuine coordination benefit (insurance pool reduces catastrophic risk) alongside asymmetric extraction (cost-sharing designed to limit utilization). Constrained by financial burden and information asymmetry about treatment options.
constraint_indexing:constraint_classification(mental_health_treatment_access, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSURANCE COMPANY (ROPE) — Experiences mental health coverage as a pure coordination mechanism: pooling risk enables sustainable coverage model. Arbitrage options available (exit specific markets, adjust coverage levels). Benefits from constraint through premium collection and cost control mechanisms. Coordination with employers and regulators enables continued operation.
constraint_indexing:constraint_classification(mental_health_treatment_access, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PSYCHIATRIST IN NETWORK (TANGLED ROPE) — Experiences genuine coordination (referral networks, EHR integration, payment structures) alongside extraction (insurance reimbursement rates far below out-of-pocket fees, administrative burden). Mobile through private practice option but constrained by insurance network participation being required for patient volume. Mixed experience: coordination benefits outweighed by extraction mechanisms.
constraint_indexing:constraint_classification(mental_health_treatment_access, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: PUBLIC MENTAL HEALTH SYSTEM (PITON) — Formally coordinates safety net care but operates as largely theatrical vestige: underfunded, understaffed, with long waitlists and fragmented services. The institutional structure persists through regulatory requirement and political narrative ('we have a public system') rather than functional capacity. Theater ratio high because funding constraints force provision of nominal services with minimal actual therapeutic impact.
constraint_indexing:constraint_classification(mental_health_treatment_access, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ADVOCACY COALITION (TANGLED ROPE) — Organized patient and provider groups see genuine coordination function (advocacy increases access for marginalized populations) alongside extraction (professional gatekeeping, insurance industry captures policy conversations). Constrained by asymmetric power but possess agency through organizing. Classification reflects mixed experience: real collective benefits alongside structural extraction.
constraint_indexing:constraint_classification(mental_health_treatment_access, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — May perceive mental health access barriers as inherent to scarcity: limited psychiatrists, expensive medications, training requirements create immutable constraints on availability. From this view, the constraint reflects irreducible physical and knowledge limitations. However, structural data contradicts this — the barriers are partly contingent on insurance design, reimbursement policy, and regulatory structure, not natural scarcity. This perspective risks naturalizing policy choices as laws of nature.
constraint_indexing:constraint_classification(mental_health_treatment_access, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mental_health_treatment_access_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(mental_health_treatment_access, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mental_health_treatment_access, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(mental_health_treatment_access, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(mental_health_treatment_access, TR),
    TR >= 0.70.

:- end_tests(mental_health_treatment_access_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts through multiple channels: insurance copayments create price barriers for low-income patients; psychiatric pharmaceutical pricing is among the highest in healthcare; administrative burden on providers reduces access capacity; public system underfunding forces rationing. The value reflects that extraction is substantial but not maximal — some genuine coordination (insurance pools) persists, and providers maintain ethical norms that reduce pure profit extraction. Theater has remained relatively stable (0.48→0.55) because both public and private systems maintain symbolic investment in access while actual capacity constrains utilization. Suppression (0.68): High. Barriers are substantial: (1) Material — insurance copayments, out-of-pocket costs, geographic provider scarcity; (2) Epistemic — complex insurance coverage rules, lack of information about options; (3) Cognitive — stigma, shame, identity-lock through internalized mental health stigma. This multi-layered suppression explains why even nominally insured patients frequently forgo treatment.
 *
 * PERSPECTIVAL GAP:
 *   This constraint's perspectival gap is exceptionally wide — from Snare (uninsured) to Rope (insurer) to Piton (public system) within the same constraint architecture. This diversity signals that the constraint mechanism is structurally asymmetric: it successfully coordinates risk pools while simultaneously enabling extraction from the most vulnerable agents. The gap reveals how beneficiary/victim declarations map to real material flows: insurers aggregate premiums from insured populations and use administrative/utilization mechanisms to restrict payouts to high-cost patient groups. The public system's Piton classification is particularly revealing — it demonstrates how theatrical maintenance ('we have a safety net') can coexist with functional degradation (starvation of actual care capacity). The analytical perspective's mountain is a false summit: mental health access barriers appear immutable only if policy design is invisible.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (insurance companies, pharma manufacturers, healthcare administrators) experience low directionality values (d ≈ 0.05-0.20) because the constraint subsidizes their operations — premiums flow to them, utilization controls protect margins, regulatory capture enables price-setting power. Victims (uninsured, low-income, rural populations) experience high directionality (d ≈ 0.85-0.95) because the constraint extracts from them through barriers to access. Mixed agents (in-network psychiatrists, advocacy coalitions) experience moderate directionality (d ≈ 0.45-0.65) reflecting their hybrid position. The sigmoid f(d) scales these into experienced extractiveness multipliers. Institutional beneficiaries with arbitrage options achieve negative effective extraction (subsidy), while powerless victims experience maximum χ. This mapping is what produces the perspectival divergence: the same ε (0.58) becomes different χ values across the index space.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy by clearly establishing that genuine coordination (insurance risk pooling, provider networks) coexists with asymmetric extraction (cost-shifting to vulnerable populations, utilization controls, provider margin compression). The Tangled Rope classification is justified by: (1) beneficiary declaration (insurance companies benefit from the system), (2) victim declaration (uninsured and underinsured populations bear costs), (3) active enforcement requirement (insurance mechanisms require continuous administrative enforcement to restrict utilization and shift costs). The constraint resolves mandatrophy by refusing to reduce to pure extraction or pure coordination — both mechanisms operate simultaneously and are structurally necessary to each other. The insurance pool cannot function without cost controls (extraction), and the extraction cannot operate without the pooling fiction (coordination). The Piton classification for the public system is mandatrophy-safe because it explicitly identifies theater as the mechanism of inertial persistence, not as a form of genuine coordination. Theater_ratio (0.55) indicates moderate performativity, allowing the public system to maintain symbolic validity while functional capacity degrades.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_vs_episodic_barriers,
    'Are access barriers primarily structural (insurance design, provider supply, reimbursement) or episodic (temporary treatment gaps during crises)?',
    'Longitudinal tracking of access patterns; analysis of whether barriers persist during non-crisis periods; comparison of access rates pre/post policy changes',
    'If structural: constraint operates continuously as Tangled Rope. If episodic: extraction is intermittent, reclassify as Scaffold with crisis cycles. Mandatrophy hinges on this distinction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_episodic_barriers, empirical, 'Whether access barriers are structural or episodic').

omega_variable(
    benefit_attribution_ambiguity,
    'Do insurance mechanisms genuinely coordinate mental health risk or primarily serve as extraction vehicles disguised as coordination?',
    'Analysis of insurance pool cross-subsidization; comparison of outcomes for insured vs uninsured patients; measurement of administrative overhead as proportion of premiums; evaluation of whether insurance reduces catastrophic outcomes or primarily redistributes costs',
    'If genuine coordination: Tangled Rope classification confirmed. If primarily extraction: reclassify as Snare. The distinction determines whether insurance reform can preserve coordination while reducing extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(benefit_attribution_ambiguity, empirical, 'Whether insurance mechanisms coordinate or extract').

omega_variable(
    pharmaceutical_innovation_tradeoff,
    'Does high pharmaceutical pricing (and resulting profit-driven R&D incentives) produce net gain in treatment options that outweighs extraction from patients, or does pricing exceed marginal benefit?',
    'Analysis of R&D productivity vs pricing correlation; measurement of access rates under alternative pricing models; tracking of innovation in low-profit treatment areas',
    'If pricing justified: constraint is legitimate Tangled Rope with unavoidable extraction cost. If pricing exceeds marginal benefit: extraction is exploitative, reclassify segments as Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(pharmaceutical_innovation_tradeoff, preference, 'Whether pharmaceutical pricing extracts value or funds necessary innovation').

omega_variable(
    rural_psychiatrist_scarcity,
    'Is the shortage of psychiatrists in rural areas a natural law (training requirements, patient density constraints) or a policy artifact (reimbursement incentives concentrate providers in urban areas)?',
    'Comparative analysis of provider distribution vs potential demand; examination of reimbursement gradient effects; historical analysis of rural access during periods of different payment structures',
    'If natural law: mountain classification for rural access barrier. If policy artifact: Tangled Rope/Snare classification for rural populations indicates extractive design choice. Sunset potential depends on this resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rural_psychiatrist_scarcity, empirical, 'Whether rural psychiatrist shortage is natural or policy-driven').

omega_variable(
    identity_lock_in_stigma,
    'To what extent do patients experience mental health treatment barriers as identity-locked (shame/stigma prevents help-seeking) versus trapped (material barriers prevent access)?',
    'Post-destigmatization intervention analysis; measurement of help-seeking behavior changes following awareness campaigns; exit surveys of patients who overcome stigma vs those with material barriers',
    'If primarily identity-locked: constraints operate through cognitive capture, not structural barriers. Therapeutic intervention can shift classification. If primarily trapped: structural barriers dominate, therapeutic intervention insufficient. Mixed: requires decomposition into separate stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_stigma, empirical, 'Whether patient barriers are identity-locked or structurally trapped').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mental_health_treatment_access, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mhta_tr_t0, mental_health_treatment_access, theater_ratio, 0, 0.48).
narrative_ontology:measurement(mhta_tr_t10, mental_health_treatment_access, theater_ratio, 10, 0.52).
narrative_ontology:measurement(mhta_tr_t20, mental_health_treatment_access, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(mhta_be_t0, mental_health_treatment_access, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(mhta_be_t10, mental_health_treatment_access, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(mhta_be_t20, mental_health_treatment_access, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mental_health_treatment_access, resource_allocation).
narrative_ontology:affects_constraint(mental_health_treatment_access, pharmaceutical_pricing_power).
narrative_ontology:affects_constraint(mental_health_treatment_access, psychiatrist_labor_supply).
narrative_ontology:affects_constraint(mental_health_treatment_access, insurance_industry_regulatory_capture).

% DUAL FORMULATION NOTE:
% Mental health treatment access is a constraint family with three distinct structural stories: (1) insurance_coordination_mechanism (ε≈0.30, Rope) — the genuine pooling function; (2) cost_shifting_extraction (ε≈0.72, Snare) — the asymmetric burden on uninsured/underinsured; (3) public_system_degradation (ε≈0.15, Piton) — theatrical safety net. This story integrates all three into a single Tangled Rope by declaring beneficiaries and victims from the unified constraint architecture. The family decomposition would separate coordination from extraction, but the policy relevance is in the hybrid.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(mental_health_treatment_access, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
