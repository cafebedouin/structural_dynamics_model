% ============================================================================
% CONSTRAINT STORY: reproductive_medicine_gatekeeping
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reproductive_medicine_gatekeeping, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: reproductive_medicine_gatekeeping
 *   human_readable: Reproductive Medicine Access Gatekeeping
 *   domain: healthcare/reproductive_medicine/access_control
 *
 * SUMMARY:
 *   Reproductive medicine gatekeeping constrains access to fertility
 *   treatment through a combination of institutional control (clinic
 *   licensing, professional monopoly), economic barriers (treatment costs
 *   $12k-$20k per cycle, typically uninsured), and legitimacy control
 *   (medical authority over which reproductive technologies are
 *   'acceptable'). The constraint exhibits genuine coordination functions —
 *   medical screening prevents harm, informed consent processes protect
 *   autonomy, outcome tracking improves protocols — while simultaneously
 *   extracting through monopoly pricing, eligibility barriers, and access
 *   delay. The tension between these is not resolvable by removing one side:
 *   eliminating all gatekeeping would sacrifice safety coordination, but full
 *   gatekeeping enforces extraction. The constraint's extractiveness has
 *   increased over the interval (0.35 → 0.58) as treatment costs have risen
 *   and decentralized alternatives have not yet scaled sufficiently to offer
 *   real exit options. Theater ratio has risen (0.42 → 0.58) as regulatory
 *   activity has increased without demonstrably improving outcomes,
 *   indicating Goodhart drift where compliance with licensing and ethics
 *   protocols becomes performative rather than functionally oriented toward
 *   patient benefit.
 *
 * KEY AGENTS:
 *   - Infertile Individuals: Primary victims (powerless/trapped) — face biological urgency, cost barriers, and identity lock to medicalized reproduction pathway
 *   - Fertility Clinic System: Primary beneficiary (institutional/arbitrage) — captures fee-for-service revenue, professional authority, and control over reproductive technology access
 *   - Reproductive Technology Industry: Secondary beneficiary (institutional/arbitrage) — manufactures equipment, develops protocols, profits from standardized procedures
 *   - Patient Advocacy Coalition: Organized victims (organized/constrained) — have challenged gatekeeping through insurance reform and legislative action but cannot unilaterally override clinical control
 *   - Regulatory Framework: Institutional actor (institutional/arbitrage) — maintains gatekeeping through licensing, embryo research restrictions, and clinic oversight, increasingly through theater
 *   - Decentralized Reproductive Tech Movement: Emerging alternative (organized/mobile) — DIY sperm banks, genetic screening apps, direct-to-consumer gamete preservation, medical tourism networks building parallel pathways
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees the constraint as irreducibly hybrid coordination-extraction, not resolvable by choosing one pole
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reproductive_medicine_gatekeeping, 0.58).
domain_priors:suppression_score(reproductive_medicine_gatekeeping, 0.65).
domain_priors:theater_ratio(reproductive_medicine_gatekeeping, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reproductive_medicine_gatekeeping, extractiveness, 0.58).
narrative_ontology:constraint_metric(reproductive_medicine_gatekeeping, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(reproductive_medicine_gatekeeping, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reproductive_medicine_gatekeeping, tangled_rope).
narrative_ontology:human_readable(reproductive_medicine_gatekeeping, "Reproductive Medicine Access Gatekeeping").
narrative_ontology:topic_domain(reproductive_medicine_gatekeeping, "healthcare/reproductive_medicine/access_control").

domain_priors:requires_active_enforcement(reproductive_medicine_gatekeeping).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reproductive_medicine_gatekeeping, fertility_specialists).
narrative_ontology:constraint_beneficiary(reproductive_medicine_gatekeeping, reproductive_technology_industry).
narrative_ontology:constraint_beneficiary(reproductive_medicine_gatekeeping, institutional_medical_systems).
narrative_ontology:constraint_victim(reproductive_medicine_gatekeeping, infertile_individuals).
narrative_ontology:constraint_victim(reproductive_medicine_gatekeeping, economically_marginalized_populations).
narrative_ontology:constraint_victim(reproductive_medicine_gatekeeping, reproductive_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INFERTILE INDIVIDUAL (SNARE) — Trapped by biological urgency (reproductive window), economic dependency (treatment costs $12k-$20k per cycle), and institutional monopoly on effective technologies. No exit option: cannot obtain reproductive autonomy outside the medical gatekeeping system. Maximum extraction experienced — the constraint extracts time, money, and psychological burden.
constraint_indexing:constraint_classification(reproductive_medicine_gatekeeping, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PATIENT SEEKING FERTILITY TREATMENT (TANGLED ROPE / IDENTITY-LOCKED) — Structurally mobile (could pursue unregulated providers, DIY protocols, medical tourism) but identity-fused with the aspiration for biological parenthood and medicalized legitimacy. The gate coordinates genuine care coordination (medical screening, safety protocols, success tracking) while simultaneously extracting through cost barriers, wait times, and eligibility criteria. Identity lock means the patient cannot perceive the exit options (unregulated providers, alternative kinship models) as valid alternatives — only the medical pathway 'counts' as real parenthood.
constraint_indexing:constraint_classification(reproductive_medicine_gatekeeping, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: FERTILITY CLINIC SYSTEM (ROPE) — Experiences the constraint as coordination: clinical protocols protect patient safety, standardize outcome reporting, and enable quality improvement. The gatekeeping coordinates legitimate medical functions (screening for contraindications, counseling, protocol compliance). Net beneficiary — extraction flows toward this institutional actor through fee-for-service revenue and professional authority. Arbitrage exit: can shift to adjacent reproductive services (surrogacy, gamete banking, wellness counseling) if regulatory environment shifts.
constraint_indexing:constraint_classification(reproductive_medicine_gatekeeping, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PATIENT ADVOCACY COALITION (TANGLED ROPE) — Organized actors (RESOLVE, patient support groups) have challenged gatekeeping through insurance reform, legislative action, and direct advocacy. They experience both genuine coordination (clinics need feedback on patient experience) and extraction (their organizing labor is often unpaid; clinics use patient stories for legitimacy without ceding control). Constrained exit: can organize political pressure but cannot unilaterally override clinical protocols.
constraint_indexing:constraint_classification(reproductive_medicine_gatekeeping, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY FRAMEWORK (PITON) — Medical licensing, fertility clinic oversight, and embryo research restrictions persist through institutional inertia despite contested empirical justification. Theater ratio is high: much regulatory activity is performative (ethics committees that rubber-stamp standard protocols, licensing requirements that don't measurably improve outcomes). The framework maintains gatekeeping primarily because alternatives haven't fully replaced it, not because regulation demonstrably protects patients. Theater has increased as direct-to-consumer fertility services (genetic screening, AI matching) bypass traditional clinic gatekeeping.
constraint_indexing:constraint_classification(reproductive_medicine_gatekeeping, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: DECENTRALIZED REPRODUCTIVE TECH MOVEMENT (SCAFFOLD) — Organized actors (DIY sperm banks, genetic screening apps, direct-to-consumer gamete freezing, medical tourism networks) are building parallel pathways that bypass traditional clinic gatekeeping. These alternatives have genuine coordination functions (home insemination coordination, genetic counseling via digital platforms, international clinic matching) but are temporary scaffolds — as regulation catches up or platforms consolidate, new gatekeeping emerges. Sunset logic: distributed reproductive tech initially bypasses the constraint but will eventually spawn new institutional gates as insurance, liability, and legitimacy pressures rebuild bottlenecks.
constraint_indexing:constraint_classification(reproductive_medicine_gatekeeping, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational perspective, reproductive medicine gatekeeping simultaneously coordinates (safety, outcome standardization, psychological support) and extracts (cost barriers, eligibility criteria, monopoly on legitimacy). The constraint is neither pure coordination nor pure extraction — it is a hybrid where extraction is bundled with genuine coordination function. The tension between these is irreducible: eliminating all barriers would sacrifice safety coordination; full extraction would eliminate coordination benefits.
constraint_indexing:constraint_classification(reproductive_medicine_gatekeeping, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reproductive_medicine_gatekeeping_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(reproductive_medicine_gatekeeping, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(reproductive_medicine_gatekeeping, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(reproductive_medicine_gatekeeping, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(reproductive_medicine_gatekeeping, TR),
    TR >= 0.70.

:- end_tests(reproductive_medicine_gatekeeping_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts through monopoly pricing ($15k-$20k median per cycle cost, typically uninsured), time barriers (2-6 month clinic wait times), eligibility gatekeeping (age limits, marital status requirements in some jurisdictions, insurance requirements), and psychological extraction (repeated failure cycles). The extraction is real and significant. However, the constraint is not pure extraction (snare) because genuine coordination benefits exist: medical screening prevents adverse outcomes, counseling reduces psychological harm, outcome tracking enables protocol improvement. The 0.58 value reflects that coordination and extraction are bundled. Suppression (0.65): High. Multiple barriers operate simultaneously: cost ($15k-$20k), time (biological window + treatment duration), medical access (clinic scarcity in many regions), and psychological burden (hope cycles, grief). Suppression is not total — some individuals can access clinics, and medical tourism provides partial exit — but barriers are substantial. Theater ratio (0.58): Moderate-high. Much clinic activity is genuinely functional (safety screening, outcome tracking), but significant theater exists: ethics committees that rubber-stamp standard protocols, licensing requirements that don't measurably improve outcomes, and increasingly, direct-to-consumer genetic screening that clinics incorporate without clinical validation. The ratio reflects the boundary between genuine medical coordination and performative legitimacy maintenance.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between the beneficiary (clinic system) and victim (infertile individual) views. The clinic system's Rope classification reflects genuine coordination: they are solving legitimate medical problems. The infertile individual's Snare classification reflects genuine extraction: they face maximum barriers and no exit. Neither perspective is false — the constraint truly has both coordination and extraction elements. The gap reveals that neither perspective is complete. The analytical observer's Tangled Rope reconciles both: the constraint bundles real coordination with real extraction, and these are not separable. The identity-locked perspective is particularly diagnostic: it shows how the victim can intellectually recognize the constraint's extraction while being unable to exercise exit options because their identity is constituted through the medicalized pathway. This is the mechanism by which high-extraction constraints persist — the victim experiences the constraint as inevitable not because barriers are insurmountable (they are not — unregulated alternatives exist) but because accepting these alternatives would require abandoning their identity as someone seeking biological parenthood through legitimate medical means.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) for each perspective are derived from the agent's structural relationship to the extraction flow. Infertile individuals as trapped agents with victim status → high d → high f(d) → maximum experienced extractiveness. Identity-locked patients have structurally mobile exit options (medical tourism, unregulated providers, alternative kinship) but their identity fusion prevents exercise of these options → intermediate d → high experienced extractiveness despite exit availability. Clinic systems as beneficiaries with arbitrage options → low d → low/negative experienced extractiveness (they experience the constraint as beneficial coordination). Patient advocacy coalition as organized victims with constrained (political) exit → intermediate-high d → moderate-high extractiveness. Regulatory framework as institutional beneficiary with arbitrage → low d. Decentralized movement as organized agents with mobile exit options → low d despite victim/challenger status (they have exit paths and are building them). Analytical observer at civilizational scale with analytical exit → moderate d reflecting balanced coordination-extraction structure. The directionality pipeline correctly captures why the same structural constraint is experienced as Snare by powerless agents, Rope by beneficiaries, and Tangled Rope by intermediate actors.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that reproductive medicine gatekeeping is structurally Tangled Rope — not pure coordination (Rope) because of asymmetric extraction through cost, time, and access barriers; not pure extraction (Snare) because genuine medical coordination benefits exist. The false summit temptation is to classify as Mountain ('medical gatekeeping is inherent to safe reproduction') or as Rope ('clinics are just coordinating care'). The false snare temptation is to ignore the genuine coordination benefits. The mandatrophy resolves by: (1) measuring extracted value accurately (cost barriers, time delays, foregone alternatives), (2) measuring coordination value accurately (safety outcomes, success rates, psychological support), (3) recognizing that extraction AND coordination are both structural features, not artifacts of framing, and (4) rejecting the false choice between 'it's pure coordination' or 'it's pure extraction.' The constraint IS both, and this irreducible hybridity is what makes it Tangled Rope. The remedial implication is that improving the constraint does not mean eliminating gatekeeping (which would lose coordination benefits) or accepting current gatekeeping (which extracts asymmetrically). Instead, it means restructuring so that coordination benefits are decoupled from extraction — a genuine problem that Scaffold and decentralized-movement perspectives show is partially solvable through parallel institutions that provide coordination without monopoly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    medical_necessity_boundary,
    'What portion of fertility clinic gatekeeping reflects medical necessity (patient safety, outcome verification) versus professional rent-seeking?',
    'Comparative analysis of outcomes in regulated vs unregulated settings; international variation in clinic licensing requirements; randomized trials of reduced gatekeeping (e.g., home insemination protocols with digital medical supervision)',
    'If medical necessity dominant (>70%): constraint reclassifies as Rope or reduced-extractiveness Tangled Rope. If rent-seeking dominant (>50%): constraint is high-extractiveness Snare with theatrical legitimacy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(medical_necessity_boundary, empirical, 'Medical necessity vs professional rent-seeking in clinic gatekeeping').

omega_variable(
    identity_lock_persistence,
    'Can infertile individuals adopt alternative kinship models (adoption, co-parenting, non-biological family) if the medical pathway fails, or is the identity lock to biological parenthood structurally irreversible?',
    'Longitudinal tracking of identity narratives in patients denied clinic access; ethnographic analysis of alternative kinship construction; survey of identity flexibility before vs after failed treatment cycles',
    'If identity lock reversible: exit options upgrade from trapped to constrained or mobile. If irreversible: patients'' identity-locked status is structural feature of the constraint, not perceptual artifact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_persistence, empirical, 'Whether identity lock to biological parenthood is reversible').

omega_variable(
    regulatory_outcome_correlation,
    'Do stricter fertility clinic regulations (embryo screening limits, provider licensing, patient consent protocols) actually improve patient outcomes or just increase theater?',
    'Meta-analysis of regulatory variation across jurisdictions and outcome measures (live birth rates, adverse events, psychological harm, cost per successful pregnancy); temporal analysis of outcome changes following regulatory tightening',
    'If regulation improves outcomes: piton classification is too harsh; constraint has genuine coordination function. If no correlation: regulation is pure theater maintaining gatekeeping; piton classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_outcome_correlation, empirical, 'Whether regulation improves patient outcomes').

omega_variable(
    decentralized_tech_sustainability,
    'Can decentralized reproductive technologies (DIY banks, genetic apps, medical tourism networks) maintain low gatekeeping as they scale, or do they inevitably consolidate and reproduce the gatekeeper problem?',
    'Historical trajectory of similar decentralized technologies (DNA testing, mental health apps, telemedicine); analysis of scaling pressures (liability, insurance integration, regulatory capture) in reproductive tech platforms',
    'If consolidation inevitable: scaffold perspective is aspirational; decentralization merely delays gatekeeping reconsolidation. If sustainability possible: genuine sunset to extraction is achievable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decentralized_tech_sustainability, empirical, 'Whether decentralized reproductive tech can avoid gatekeeper consolidation').

omega_variable(
    suppression_mechanism_internalization,
    'Is measured suppression (0.65) structural (cost, time, medical requirements) or internalized (patients have internalized the legitimacy of clinical gatekeeping as necessary)?',
    'Pre-exit vs post-exit suppression trajectories: if patients using unregulated pathways report reduced suppression, it is structural; if suppression persists after exiting official channels, it is internalized. Comparison of suppression experience in medical-tourism seekers vs domestic-clinic patients.',
    'If structural: suppression can be reduced by policy change. If internalized: suppression carriers persist even after barrier removal — constraint''s extractive power exceeds the measured suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs internalized nature of reproductive medicine suppression').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reproductive_medicine_gatekeeping, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(repro_tr_t0, reproductive_medicine_gatekeeping, theater_ratio, 0, 0.42).
narrative_ontology:measurement(repro_tr_t10, reproductive_medicine_gatekeeping, theater_ratio, 10, 0.5).
narrative_ontology:measurement(repro_tr_t20, reproductive_medicine_gatekeeping, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(repro_be_t0, reproductive_medicine_gatekeeping, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(repro_be_t10, reproductive_medicine_gatekeeping, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(repro_be_t20, reproductive_medicine_gatekeeping, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reproductive_medicine_gatekeeping, resource_allocation).
narrative_ontology:boltzmann_floor_override(reproductive_medicine_gatekeeping, 0.18).
narrative_ontology:affects_constraint(reproductive_medicine_gatekeeping, maternal_mortality_gatekeeping).
narrative_ontology:affects_constraint(reproductive_medicine_gatekeeping, contraceptive_access_control).
narrative_ontology:affects_constraint(reproductive_medicine_gatekeeping, reproductive_autonomy_bodily).

% DUAL FORMULATION NOTE:
% Reproductive medicine gatekeeping has upstream constraints (contraceptive access control, reproductive autonomy bodily) that feed into it. It affects downstream constraints in maternal mortality gatekeeping (where gatekeeping in fertility treatment creates selection effects in pregnancy outcomes). These are separate constraint stories because they have different ε values and different beneficiary/victim structures, but they form a reproductive medicine constraint family linked by institutional coupling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reproductive_medicine_gatekeeping, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
