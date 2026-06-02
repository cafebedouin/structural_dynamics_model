% ============================================================================
% CONSTRAINT STORY: brazil_hiv_vtn_elimination
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_brazil_hiv_vtn_elimination, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: brazil_hiv_vtn_elimination
 *   human_readable: Brazil's Program for Eliminating Vertical HIV Transmission
 *   domain: social/public_health
 *
 * SUMMARY:
 *   Brazil's Program for Eliminating Vertical HIV Transmission represents a
 *   well-functioning public health coordination mechanism that has achieved
 *   near-elimination of mother-to-child HIV transmission through integrated
 *   antiretroviral therapy, prenatal care, institutional delivery, and infant
 *   prophylaxis protocols. The constraint structures coordination among
 *   HIV-positive pregnant women, healthcare providers, and institutional
 *   actors (Ministry of Health, international partners) to prevent neonatal
 *   infection. Unlike many health interventions that embed asymmetric
 *   extraction (where beneficiaries are charged, access is unequal, or
 *   implementation burden falls on the powerless), Brazil's program is
 *   structured as near-pure coordination: pregnant women access treatment
 *   voluntarily and benefit directly; providers gain clarity and
 *   evidence-based protocols; institutions gain policy prestige and
 *   international funding. The program's theater ratio (0.38) reflects
 *   moderate performative content — surveillance reporting to international
 *   bodies, statistical targets for program justification — but this is lower
 *   than typical public health programs because the biological outcome
 *   (undetectable = untransmittable viral loads) provides transparent
 *   verification independent of institutional rhetoric. The program exhibits
 *   temporal improvement: theater_ratio declining from 0.52 to 0.38,
 *   extractiveness declining from 0.28 to 0.22, indicating that early
 *   performative overhead has decreased as the program matured from emergency
 *   response (early 2000s) to institutionalized routine care (2015+). This
 *   downward trajectory suggests the program is transitioning from a
 *   temporary problem (maternal transmission as public health emergency)
 *   toward a solved coordination challenge (routine management of HIV in
 *   pregnancy).
 *
 * KEY AGENTS:
 *   - HIV-Positive Pregnant Women: Primary beneficiaries (powerless/mobile) — direct access to life-saving antiretroviral therapy and elimination of transmission risk to newborn; no victimization
 *   - Healthcare Providers: Secondary beneficiary (moderate/constrained) — gain standardized protocols, reduced clinical uncertainty, career prestige from participation in global success story
 *   - Brazilian Ministry of Health: Institutional beneficiary (institutional/arbitrage) — gains international recognition, funding, policy influence, leadership role in global HIV response
 *   - UNAIDS, PEPFAR, Global Fund: International institutional partners (institutional/arbitrage) — benefit from visible success story, maintained engagement in Brazil's health system
 *   - Community-Based HIV/AIDS Organizations: Organized actors (organized/constrained) — provide advocacy, patient support, surveillance input; see program as temporary (scaffold) until elimination is achieved and maintenance mode begins
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — observes both the biological constraint (viral transmission physics) and the institutional constraint (program coordination requirements)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(brazil_hiv_vtn_elimination, 0.22).
domain_priors:suppression_score(brazil_hiv_vtn_elimination, 0.35).
domain_priors:theater_ratio(brazil_hiv_vtn_elimination, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(brazil_hiv_vtn_elimination, extractiveness, 0.22).
narrative_ontology:constraint_metric(brazil_hiv_vtn_elimination, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(brazil_hiv_vtn_elimination, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(brazil_hiv_vtn_elimination, rope).
narrative_ontology:human_readable(brazil_hiv_vtn_elimination, "Brazil's Program for Eliminating Vertical HIV Transmission").
narrative_ontology:topic_domain(brazil_hiv_vtn_elimination, "social/public_health").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(brazil_hiv_vtn_elimination, hiv_positive_pregnant_women).
narrative_ontology:constraint_beneficiary(brazil_hiv_vtn_elimination, newborns_at_risk).
narrative_ontology:constraint_beneficiary(brazil_hiv_vtn_elimination, healthcare_system).
narrative_ontology:constraint_beneficiary(brazil_hiv_vtn_elimination, public_health_infrastructure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HIV-POSITIVE PREGNANT WOMAN (ROPE) — Mobile within Brazil's healthcare system; can access antiretroviral therapy, prenatal care, and delivery protocols. The constraint coordinates her medical management with neonatal protection. Benefits directly from the program through access to treatment and elimination of transmission risk. No victimization — the coordination serves her interests and her child's welfare.
constraint_indexing:constraint_classification(brazil_hiv_vtn_elimination, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: HEALTHCARE PROVIDERS (ROPE) — Constrained by protocol requirements but benefit from the coordination framework. The program provides standardized treatment guidelines, reduces clinical uncertainty, and enables specialization. Low extraction — providers experience the constraint as enabling their work, not as coercive burden. Some career advancement benefit from participation in an internationally-recognized success story.
constraint_indexing:constraint_classification(brazil_hiv_vtn_elimination, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: BRAZILIAN MINISTRY OF HEALTH / UNAIDS (ROPE) — Institutional beneficiary with full arbitrage options. The program generates international recognition, funding flows, technical assistance, and policy prestige. Coordinates global HIV response norms while maintaining domestic control. No extraction — the institutional actors experience pure coordination benefit and leadership position.
constraint_indexing:constraint_classification(brazil_hiv_vtn_elimination, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COMMUNITY-BASED ORGANIZATIONS (SCAFFOLD) — Organized agents (NGOs, patient advocacy groups) see the program as a temporary solution requiring sustained effort. Low effective extraction (χ ≤ 0.30) because these groups have agency and see an exit path: once vertical transmission is genuinely eliminated (not just in statistics but in achieved infant outcomes), the acute phase of the program becomes routine, and community monitoring shifts to maintenance. Theater is low (< 0.70) — the program's coordination function is transparent.
constraint_indexing:constraint_classification(brazil_hiv_vtn_elimination, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, the biological fact of mother-to-child HIV transmission is an irreducible constraint: without antiretroviral intervention, pregnant women with undetectable viral loads cannot transmit HIV to their infants. This is a near-universal law of virology, verified across all populations and contexts. However, the classification here is contested — the biological constraint is real, but the 'constraint' in the Deferential Realism sense is actually the institutional and behavioral program required to achieve the biological outcome, not the biology itself.
constraint_indexing:constraint_classification(brazil_hiv_vtn_elimination, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(brazil_hiv_vtn_elimination_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(brazil_hiv_vtn_elimination, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(brazil_hiv_vtn_elimination, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(brazil_hiv_vtn_elimination_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.22): Low. The program exhibits minimal extraction relative to coordination benefit. Pregnant women are not charged for treatment; providers are not exploited for their labor; institutions gain prestige but do not extract rents at the expense of other programs. The modest extractiveness reflects resource concentration on a specific population (pregnant women with HIV), which is not inherently extractive but does represent resource allocation choices that benefit one group over others. Suppression (0.35): Moderate. Barriers exist to program engagement: residual stigma around HIV status, geographic access challenges in remote areas (Amazon, Nordeste), undocumented migrant women's reluctance to engage with health system, and partner resistance to disclosure. However, suppression is not severe — Brazil's universal healthcare system (SUS) guarantees access, medication is free, and stigma has declined substantially over the interval. Theater ratio (0.38): Moderate-low. The program maintains some performative content: international reporting targets, statistical milestones, surveillance data for policy justification. However, the biological outcome (undetectable viral loads) provides verifiable feedback independent of institutional messaging, keeping theater relatively low compared to many public health interventions that rely on proxy metrics.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is minimal — most agents perceive the program as genuine coordination rather than extraction. Pregnant women and providers experience direct benefit. The institutional actors experience prestige and funding benefit. Community organizations see a temporary problem being solved. The analytical observer risks seeing the biological constraint (irreversibility of untransmittable status) as an immutable natural law, but the structural data shows it's actually a highly successful institutional-behavioral coordination mechanism — the biology is settled; the constraint is the program required to implement it. The classification gap is between rope (all pragmatic perspectives) and mountain (analytical view naturalizing the biology), revealing that the 'immutable natural law' framing is actually a false summit naturalizing the contingent institutional success.
 *
 * DIRECTIONALITY LOGIC:
 *   Pregnant women (powerless/mobile) derive low d values because they are beneficiaries accessing voluntary treatment with exit options (can seek care elsewhere, can choose institutional delivery). Providers (moderate/constrained) have low d because they benefit from protocols and prestige despite protocol constraints. Institutional actors (institutional/arbitrage) have near-zero or negative d because they are primary beneficiaries with full exit options. Community organizations (organized/constrained) have moderate d because they are partially constrained by dependence on program infrastructure but have agency through advocacy. All d values are on the beneficiary side of the spectrum (d < 0.50), producing low or negative effective extraction χ. This is the signature of pure coordination: no agent perceives themselves as victimized, and the constraint's existence benefits all participants.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    elimination_definition_threshold,
    'What constitutes ''elimination'' of vertical transmission: zero clinical cases, < 0.01% transmission rate, or sustained absence in routine surveillance?',
    'Comparative analysis of elimination definitions across TB, polio, and malaria eradication programs; Brazil-specific surveillance data on perinatally-acquired HIV cases over 10-year window',
    'If zero-tolerance (no clinical cases): program must maintain high suppression and theater to sustain reporting intensity. If statistical threshold (< 0.01%): program can transition to lower-intensity maintenance surveillance and reduce extraction burden on providers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(elimination_definition_threshold, conceptual, 'Definition of elimination status').

omega_variable(
    treatment_coverage_ceiling,
    'Can Brazil achieve and sustain > 95% antiretroviral coverage among pregnant women with HIV, or does structural inequality (poverty, geographic access, undocumented migrants) impose a coverage ceiling below 95%?',
    'Longitudinal analysis of ART coverage by socioeconomic status, region, and documentation status; modeling of access barriers in Amazon and Nordeste regions; comparison with coverage ceilings in sub-Saharan Africa programs',
    'If ceiling exists below 95%: program classification shifts from pure rope toward tangled_rope (benefiting those accessed, extracting via exclusion from those not accessed). If ceiling is crossed and sustained: rope classification confirmed for all relevant populations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treatment_coverage_ceiling, empirical, 'Achievable antiretroviral coverage ceiling').

omega_variable(
    institutional_dependency_vs_autonomy,
    'Is Brazil''s program structurally dependent on international funding (PEPFAR, Global Fund) in ways that constrain policy autonomy, or has domestic commitment and financing achieved genuine self-sufficiency?',
    'Analysis of budget composition (domestic vs. international), policy decision-making authority (WHO/UNAIDS guidance vs. Ministry of Health autonomy), and counterfactual scenarios if funding were withdrawn',
    'If dependent: institutional beneficiary (Ministry of Health) is also partially victim of directionality constraints (reduced d). If autonomous: rope classification confirmed; institutional actor experiences no extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_dependency_vs_autonomy, empirical, 'Institutional financial and policy autonomy').

omega_variable(
    stigma_suppression_dynamics,
    'Does the program''s suppression value (0.35) reflect genuine barriers to women''s engagement or does it reflect the program''s residual role in normalizing HIV status during pregnancy?',
    'Qualitative research on pregnant women''s experience of stigma (health worker treatment, family disclosure, employment discrimination); longitudinal tracking of program participation as stigma measure changes independently',
    'If suppression is residual (post-normalization): theater_ratio should be lower, extractiveness lower. If suppression reflects active stigma: suppression value is accurate and may increase if social narratives shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stigma_suppression_dynamics, empirical, 'Stigma as program suppression component').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(brazil_hiv_vtn_elimination, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(brazil_hiv_tr_t0, brazil_hiv_vtn_elimination, theater_ratio, 0, 0.52).
narrative_ontology:measurement(brazil_hiv_tr_t5, brazil_hiv_vtn_elimination, theater_ratio, 5, 0.45).
narrative_ontology:measurement(brazil_hiv_tr_t10, brazil_hiv_vtn_elimination, theater_ratio, 10, 0.38).

% Extraction over time
narrative_ontology:measurement(brazil_hiv_be_t0, brazil_hiv_vtn_elimination, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(brazil_hiv_be_t5, brazil_hiv_vtn_elimination, base_extractiveness, 5, 0.24).
narrative_ontology:measurement(brazil_hiv_be_t10, brazil_hiv_vtn_elimination, base_extractiveness, 10, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(brazil_hiv_vtn_elimination, enforcement_mechanism).
narrative_ontology:affects_constraint(brazil_hiv_vtn_elimination, brazil_hiv_treatment_access).
narrative_ontology:affects_constraint(brazil_hiv_vtn_elimination, maternal_mortality_reduction).

% DUAL FORMULATION NOTE:
% This constraint represents the institutional coordination mechanism that implements the biological imperative (undetectable = untransmittable). Upstream constraints include treatment access barriers and maternal mortality reduction frameworks. The elimination program is downstream of these and represents the convergence point where biology, public health policy, and institutional capacity align.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
