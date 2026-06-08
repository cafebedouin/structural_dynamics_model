% ============================================================================
% CONSTRAINT STORY: never_practiced_cohort_formation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_never_practiced_cohort_formation, []).

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
 *   constraint_id: never_practiced_cohort_formation
 *   human_readable: Never-Practiced Cohort Formation in Medical Workforce
 *   domain: health_workforce_economics/organizational_behavior/gender_labor
 *
 * SUMMARY:
 *   The never-practiced cohort — physicians who complete residency training
 *   but never enter clinical practice — represents a complete loss of
 *   training investment at the point of workforce entry. At 11% of residency
 *   graduates, this cohort reveals a fundamental dysfunction in the medical
 *   education pipeline: the system certifies physicians as competent and
 *   ready for practice, yet a substantial minority never practice at all.
 *   This constraint is structurally downstream of administrative burden
 *   extraction (the upstream constraint): many never-practiced physicians
 *   cite administrative burden, EHR demands, and reimbursement pressures as
 *   reasons for non-entry, suggesting that the practice environment has
 *   become untenable before they even begin. The constraint exhibits piton
 *   characteristics from the accreditation system's perspective: the original
 *   function (producing practicing physicians) has atrophied into measuring
 *   proxy goals (board pass rates, competency milestones, rotation
 *   completion), and the system maintains the certification ritual even as it
 *   decouples from workforce outcomes. The theater_ratio (0.78) reflects that
 *   accreditation measures training completion but not workforce entry — an
 *   11% never-practiced rate indicates the measurement theater has lost touch
 *   with the system's ostensible purpose. The constraint also demonstrates
 *   identity_locked binding: physicians who complete 7-11 years of medical
 *   training have fused their identity with the physician role, making
 *   non-clinical careers literally unthinkable from within that frame even as
 *   structural barriers make practice untenable. This is cognitive capture at
 *   the biographical scale: the agent is structurally mobile (has degree,
 *   license, transferable skills) but functionally trapped by identity
 *   fusion.
 *
 * KEY AGENTS:
 *   - Never-Practiced Physicians: Primary victims (powerless/identity_locked) — bear full cost of training investment, debt burden, and identity crisis; cannot exit because identity is fused with physician role despite structural barriers to practice
 *   - Residency Graduates Considering Exit: Secondary victims (moderate/constrained) — face high exit costs but can imagine alternatives; experience mixed coordination (skill development) and extraction (debt, path dependency)
 *   - Residency Programs: Primary beneficiaries (institutional/arbitrage) — receive Medicare GME funding and resident labor regardless of graduate outcomes; no accountability for workforce entry rates
 *   - Medical Education Accreditation System: Institutional actor (institutional/mobile) — maintains certification ritual that measures training completion but not workforce entry; piton perspective reveals atrophied function
 *   - Specialty Boards: Secondary beneficiaries (institutional/arbitrage) — collect certification fees and maintain credential monopoly; no incentive to track workforce entry
 *   - Public Training Investment: Abstract victim (powerless/trapped) — taxpayer-funded GME subsidies produce non-practicing physicians; no mechanism to recover investment or redirect training
 *   - Healthcare Access Communities: Secondary victims (powerless/trapped) — underserved populations bear cost of physician shortage exacerbated by never-practiced cohort
 *   - Workforce Reform Coalition: Organized agents (organized/constrained) — building alternative pathways with sunset logic (team-based care, scope expansion, loan forgiveness)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(never_practiced_cohort_formation, 0.68).
domain_priors:suppression_score(never_practiced_cohort_formation, 0.42).
domain_priors:theater_ratio(never_practiced_cohort_formation, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(never_practiced_cohort_formation, extractiveness, 0.68).
narrative_ontology:constraint_metric(never_practiced_cohort_formation, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(never_practiced_cohort_formation, theater_ratio, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(never_practiced_cohort_formation, piton).
narrative_ontology:human_readable(never_practiced_cohort_formation, "Never-Practiced Cohort Formation in Medical Workforce").
narrative_ontology:topic_domain(never_practiced_cohort_formation, "health_workforce_economics/organizational_behavior/gender_labor").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(never_practiced_cohort_formation, residency_programs).
narrative_ontology:constraint_beneficiary(never_practiced_cohort_formation, medical_education_institutions).
narrative_ontology:constraint_beneficiary(never_practiced_cohort_formation, specialty_boards).
narrative_ontology:constraint_victim(never_practiced_cohort_formation, never_practiced_physicians).
narrative_ontology:constraint_victim(never_practiced_cohort_formation, public_training_investment).
narrative_ontology:constraint_victim(never_practiced_cohort_formation, healthcare_access_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NEVER-PRACTICED PHYSICIAN (SNARE) — Identity-locked rather than structurally trapped: has medical degree and license (structural mobility) but identity constituted through 7-11 years of medical training makes non-clinical career literally unthinkable from within the frame. Exit would require abandoning physician identity entirely. Experiences maximum extraction: sunk cost of training, debt burden, social expectation, no alternative pathway that preserves identity investment. The binding mechanism is cognitive — the agent cannot see exit because their self-concept is fused with clinical practice even as structural barriers (administrative burden, reimbursement cuts, workplace conditions) make practice untenable.
constraint_indexing:constraint_classification(never_practiced_cohort_formation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: RESIDENCY GRADUATE CONSIDERING EXIT (TANGLED ROPE) — Constrained by debt, credential specificity, and career path lock-in, but not identity-locked (can imagine alternative careers). Experiences mixed coordination and extraction: residency training provided genuine skill development and credential (coordination function) but also created path dependency and debt burden that makes exit costly (extraction). The constraint both enables (medical expertise) and traps (narrow credential, high exit cost). Moderate effective extraction because agent has some agency and can see exit options, even if expensive.
constraint_indexing:constraint_classification(never_practiced_cohort_formation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: RESIDENCY PROGRAM (ROPE) — Benefits from training pipeline regardless of graduate outcomes: receives Medicare GME funding, maintains teaching mission, fills service needs during training. Whether graduates practice or not does not affect program revenue or accreditation. Experiences constraint as pure coordination: training physicians is the institutional mandate, and the system delivers credentials on schedule. Net beneficiary — extraction flows toward this agent (training subsidies, resident labor) not away.
constraint_indexing:constraint_classification(never_practiced_cohort_formation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MEDICAL EDUCATION ACCREDITATION SYSTEM (PITON) — The system measures training completion (board pass rates, program accreditation, competency milestones) but not workforce entry or retention. The original function (ensuring physician competence) has atrophied into measuring proxy goals (test scores, procedure logs, rotation completion). An 11% never-practiced rate reveals the measurement theater: the system certifies physicians who never practice, indicating the certification ritual has decoupled from workforce function. Maintained through institutional inertia and stakeholder investment in the credential pipeline, not because it produces practicing physicians.
constraint_indexing:constraint_classification(never_practiced_cohort_formation, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: WORKFORCE REFORM COALITION (SCAFFOLD) — Organized stakeholders (primary care advocacy groups, rural health coalitions, loan forgiveness programs, scope-of-practice expansion advocates) see the never-practiced cohort as a symptom of a transitional crisis in physician workforce design. Building alternative pathways: team-based care models, mid-level provider expansion, loan forgiveness tied to service commitments, administrative burden reduction initiatives. These reforms carry implicit sunset logic: as alternative care models mature and administrative extraction decreases, the structural barriers that create the never-practiced cohort should diminish. Estimated sunset: 15-25 years for workforce model transition.
constraint_indexing:constraint_classification(never_practiced_cohort_formation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the never-practiced cohort represents a coordination failure (training pipeline misaligned with practice environment) layered with extraction (debt burden, credential lock-in, administrative burden from upstream constraint). The system coordinates training but extracts from trainees through structural barriers that make practice untenable for a substantial minority. Not a mountain (the 11% rate is contingent on institutional arrangements, not a natural law of medical training) and not pure extraction (genuine skill development occurs). Tangled rope: real coordination function with asymmetric extraction embedded in the same structure.
constraint_indexing:constraint_classification(never_practiced_cohort_formation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(never_practiced_cohort_formation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(never_practiced_cohort_formation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(never_practiced_cohort_formation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(never_practiced_cohort_formation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(never_practiced_cohort_formation, TR),
    TR >= 0.70.

:- end_tests(never_practiced_cohort_formation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The never-practiced cohort bears substantial extraction: 7-11 years of training investment, median debt burden of $200k-$300k, opportunity cost of alternative careers, social and familial expectation, and identity investment — all with zero return in the form of clinical practice. The extraction is not total (training provides transferable skills and credential has some non-clinical value) but is severe enough to represent near-complete loss of training ROI from the individual and public investment perspectives. The value reflects that extraction accumulates over the training period and crystallizes at the point of non-entry. Suppression (0.42): Moderate. Barriers to workforce entry include debt burden (cannot afford to exit medicine entirely), credential specificity (medical degree has limited transferability), identity lock (physician identity makes non-clinical career unthinkable), and upstream administrative burden (practice environment is untenable). But suppression is not total: some physicians do exit to non-clinical careers (pharma, consulting, policy), and alternative pathways exist even if costly. The moderate value reflects that exit is possible but requires abandoning identity investment and accepting credential devaluation. Theater ratio (0.78): High. The accreditation system measures training completion (board pass rates, competency milestones, procedure logs) but not workforce entry or retention. An 11% never-practiced rate reveals the measurement theater: the system certifies physicians as ready for practice who never practice, indicating certification has decoupled from workforce function. The theater has increased over the interval as administrative burden and practice environment deterioration have made the gap between certification and practice viability wider. The high theater_ratio is the primary signal for piton classification: the function (producing practicing physicians) has atrophied while the ritual (certification) persists.
 *
 * PERSPECTIVAL GAP:
 *   The never-practiced cohort demonstrates how the same structural phenomenon appears differently depending on the observer's position. The never-practiced physician experiences a snare: identity-locked by 7-11 years of training, unable to exit because their self-concept is fused with the physician role even as structural barriers make practice untenable. The residency graduate considering exit experiences tangled_rope: genuine skill development (coordination) layered with debt burden and path dependency (extraction). The residency program experiences rope: training physicians is the institutional mandate, and the system delivers credentials on schedule regardless of graduate outcomes — the program is a net beneficiary of GME funding and resident labor. The accreditation system experiences piton: the original function (ensuring competent practicing physicians) has atrophied into measuring proxy goals (test scores, rotation completion), and the certification ritual persists through institutional inertia despite an 11% never-practiced rate revealing functional decoupling. The workforce reform coalition experiences scaffold: the never-practiced cohort is a symptom of a transitional crisis, and alternative care models (team-based care, mid-level expansion, administrative burden reduction) carry sunset logic. The analytical observer sees tangled_rope at the civilizational scale: real coordination (skill development) with asymmetric extraction (debt, credential lock-in, administrative burden) embedded in the same structure. The perspectival gap is not about disagreement over facts but about structural position: beneficiaries see coordination, victims see extraction, and the system maintaining the ritual sees its own degraded function.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position and beneficiary/victim declarations. Never-practiced physicians are victims with identity_locked exit options: high d (near 1.0) because they bear maximum extraction (training investment loss, debt burden, identity crisis) and cannot exit due to cognitive binding. The engine amplifies their experienced extraction through the exit modulation term. Residency graduates considering exit are victims with constrained exit options: moderate-high d (0.6-0.7) because they bear extraction but have some agency and can see exit paths even if costly. Residency programs are beneficiaries with arbitrage exit options: low d (near 0.0) because they capture GME funding and resident labor regardless of graduate outcomes — extraction flows toward them, not away. The accreditation system is a beneficiary with mobile exit options: low d because it maintains institutional position and credential monopoly without accountability for workforce outcomes. The piton classification for the accreditation system derives from the theater gate (theater_ratio > 0.7) rather than from high experienced extraction — the system sees its own degraded function. The analytical observer at civilizational scale sees tangled_rope: moderate d (0.4-0.5) reflecting mixed coordination and extraction, with the balance determined by whether the training investment produces transferable value (coordination) or merely creates path dependency (extraction). The upstream constraint (administrative_burden_extraction) contributes to never-practiced cohort formation by making the practice environment untenable, raising effective extraction for those who complete training but face insurmountable barriers to entry.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The never-practiced cohort reveals that the medical education system's mandate (producing competent practicing physicians) has outlived its function in the current form. The system continues to certify physicians at historical rates while an 11% never-practiced rate indicates the certification ritual has decoupled from workforce outcomes. The mandate persists because stakeholders benefit from the training pipeline (residency programs receive GME funding, specialty boards collect certification fees, medical schools maintain enrollment) regardless of whether graduates practice. The constraint is maintained theatrically: accreditation measures training completion but not workforce entry, allowing the system to claim success (high board pass rates, accredited programs) while ignoring the workforce failure (never-practiced cohort, physician shortage in underserved areas). The mandatrophy is resolved not by eliminating the training pipeline but by recognizing that the current certification model serves institutional interests (beneficiaries) rather than workforce needs (victims). The scaffold perspective (workforce reform coalition) represents an attempt to resolve the mandatrophy through alternative pathways: team-based care models that reduce physician bottleneck, mid-level provider expansion that creates alternative credentials, loan forgiveness programs that tie training investment to service commitments, and administrative burden reduction that makes practice viable. These reforms carry implicit sunset logic: as they mature, the structural barriers creating the never-practiced cohort should diminish, and the certification ritual should re-couple with workforce function. The piton classification captures the current state: a degraded system maintained through inertia and stakeholder investment, not because it produces practicing physicians.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_vs_structural_binding,
    'What proportion of the never-practiced cohort is identity-locked (cannot imagine non-clinical career) versus structurally trapped (can imagine alternatives but faces insurmountable barriers)?',
    'Longitudinal interviews with never-practiced physicians; analysis of career transition attempts and psychological framing of exit decisions; comparison of debt burden and alternative career exploration between those who exit immediately post-residency versus those who attempt practice first.',
    'If primarily identity-locked: the binding mechanism is cognitive and could be addressed through earlier career counseling and identity diversification during training. If primarily structural: the barriers are material (debt, credential specificity, administrative burden) and require policy intervention. Mixed binding suggests both interventions needed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_vs_structural_binding, empirical, 'Whether never-practiced cohort binding is cognitive or structural').

omega_variable(
    gender_differential_mechanism,
    'Does the never-practiced rate differ by gender, and if so, is the mechanism differential identity-lock (women physicians more likely to fuse identity with caregiving roles that conflict with practice demands) or differential structural barriers (workplace discrimination, work-life balance constraints)?',
    'Gender-stratified analysis of never-practiced rates; qualitative analysis of stated reasons for non-entry; comparison with gender differentials in other high-credential professions with similar training investments.',
    'If identity mechanism: interventions target professional identity formation during training. If structural mechanism: interventions target workplace conditions and discrimination. If both: reveals that gender operates through multiple channels in physician workforce retention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gender_differential_mechanism, empirical, 'Gender differential in never-practiced cohort formation mechanism').

omega_variable(
    administrative_burden_causality,
    'What proportion of never-practiced cohort formation is causally downstream of administrative burden extraction (the upstream constraint) versus independent factors (debt, lifestyle preferences, alternative career opportunities)?',
    'Natural experiment: compare never-practiced rates in health systems with high versus low administrative burden; longitudinal tracking of residency graduates'' stated reasons for non-entry; correlation analysis between administrative burden metrics and workforce entry rates by specialty and region.',
    'If administrative burden is primary driver: reducing upstream extraction (EHR optimization, prior authorization reform, billing simplification) should reduce never-practiced rate. If independent factors dominate: workforce entry problem requires different interventions (debt relief, training redesign, alternative career pathways).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(administrative_burden_causality, empirical, 'Causal contribution of administrative burden to never-practiced cohort').

omega_variable(
    piton_vs_tangled_rope_threshold,
    'At what never-practiced rate does the medical education system cross from tangled_rope (coordination with embedded extraction) to piton (atrophied function maintained theatrically)? Is 11% above or below that threshold?',
    'Historical analysis of workforce entry rates in other credentialing systems (law, engineering, PhD programs); identification of threshold at which credential production decouples from workforce function; stakeholder analysis of whether accreditation bodies treat never-practiced rate as a problem or ignore it.',
    'If 11% is below threshold: system is tangled_rope with fixable coordination problems. If 11% is above threshold: system is piton, and the certification ritual has decoupled from workforce production. Threshold location determines whether reform or replacement is appropriate intervention.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(piton_vs_tangled_rope_threshold, conceptual, 'Threshold for piton classification in credentialing systems').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(never_practiced_cohort_formation, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npc_theater_1990, never_practiced_cohort_formation, theater_ratio, 0, 0.55).
narrative_ontology:measurement(npc_theater_1995, never_practiced_cohort_formation, theater_ratio, 5, 0.62).
narrative_ontology:measurement(npc_theater_2000, never_practiced_cohort_formation, theater_ratio, 10, 0.68).
narrative_ontology:measurement(npc_theater_2005, never_practiced_cohort_formation, theater_ratio, 15, 0.73).
narrative_ontology:measurement(npc_theater_2010, never_practiced_cohort_formation, theater_ratio, 20, 0.76).
narrative_ontology:measurement(npc_theater_2015, never_practiced_cohort_formation, theater_ratio, 25, 0.78).

% Extraction over time
narrative_ontology:measurement(npc_extract_1990, never_practiced_cohort_formation, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(npc_extract_1995, never_practiced_cohort_formation, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(npc_extract_2000, never_practiced_cohort_formation, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(npc_extract_2005, never_practiced_cohort_formation, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(npc_extract_2010, never_practiced_cohort_formation, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(npc_extract_2015, never_practiced_cohort_formation, base_extractiveness, 25, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(never_practiced_cohort_formation, identity_coordination).

% DUAL FORMULATION NOTE:
% The never-practiced cohort is structurally downstream of administrative_burden_extraction: many physicians cite administrative burden, EHR demands, and reimbursement pressures as reasons for never entering practice. The upstream constraint makes the practice environment untenable before workforce entry, contributing to the 11% never-practiced rate. However, the never-practiced cohort also has independent drivers (debt burden, credential specificity, identity lock, alternative career opportunities) that would produce some non-entry rate even if administrative burden were eliminated. The two constraints are linked but have distinct extractiveness values: administrative_burden_extraction measures the ongoing extraction from practicing physicians, while never_practiced_cohort_formation measures the complete loss of training investment at the point of non-entry.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(never_practiced_cohort_formation, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
