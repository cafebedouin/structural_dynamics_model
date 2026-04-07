% ============================================================================
% CONSTRAINT STORY: vaccine_mandates_school_entry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandates_school_entry, []).

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
 *   constraint_id: vaccine_mandates_school_entry
 *   human_readable: Vaccine Mandates for School Entry
 *   domain: public_health/education/governance
 *
 * SUMMARY:
 *   Vaccine mandates for school entry represent a coordination mechanism with
 *   asymmetric extraction: the constraint solves the collective action
 *   problem of preventing vaccine-preventable disease outbreaks through herd
 *   immunity, but imposes costs on agents who did not choose participation.
 *   The constraint's classification as Tangled Rope reflects this hybrid
 *   nature. From the unvaccinated family's perspective, the constraint
 *   functions as a Snare — binary choice between vaccination or school
 *   exclusion with suppression mechanisms (social penalty, lost educational
 *   access, employment effects on parents) that make exit prohibitively
 *   costly. From the public health authority's perspective, it is pure
 *   coordination — solving disease prevention. From the immunocompromised
 *   student's perspective, the mandate mobilizes their vulnerability to
 *   justify coerced participation by others. The analytical observer sees the
 *   genuine tension: the coordination benefit (herd immunity) is real, but so
 *   is the extraction (coerced medical intervention without proportional
 *   voice). The constraint exhibits all signatures of Tangled Rope: active
 *   enforcement (legal/policy requirements), genuine coordination function
 *   (disease prevention), asymmetric extraction (burden concentrated on
 *   hesitant families), and both beneficiaries (vaccinated students, public
 *   health institutions) and victims (unvaccinated families) clearly
 *   identifiable.
 *
 * KEY AGENTS:
 *   - Unvaccinated Children and Families: Primary victims (powerless/trapped) — face binary choice: vaccination or school exclusion with severe suppression mechanisms
 *   - Vaccine-Hesitant Parents: Secondary victims (moderate/constrained) — experience both coordination benefit and extraction; some exit options exist but are costly
 *   - Vaccinated Student Population: Primary beneficiaries (moderate/mobile) — benefit from herd immunity and disease prevention; bear minimal cost
 *   - Public Health Authority: Institutional beneficiary (institutional/arbitrage) — solves coordination problem; can arbitrage between mandate configurations
 *   - Immunocompromised Students: Vulnerable population (moderate/constrained) — benefit from herd immunity but their vulnerability is mobilized to justify mandates
 *   - School Administrative System: Institutional maintainer (institutional/arbitrage) — implements mandates through institutional routine; sees reduced functional necessity over time
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — recognizes both coordination and extraction functions as irreducible
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandates_school_entry, 0.58).
domain_priors:suppression_score(vaccine_mandates_school_entry, 0.62).
domain_priors:theater_ratio(vaccine_mandates_school_entry, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandates_school_entry, extractiveness, 0.58).
narrative_ontology:constraint_metric(vaccine_mandates_school_entry, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(vaccine_mandates_school_entry, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandates_school_entry, tangled_rope).
narrative_ontology:human_readable(vaccine_mandates_school_entry, "Vaccine Mandates for School Entry").
narrative_ontology:topic_domain(vaccine_mandates_school_entry, "public_health/education/governance").

domain_priors:requires_active_enforcement(vaccine_mandates_school_entry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandates_school_entry, vaccinated_student_population).
narrative_ontology:constraint_beneficiary(vaccine_mandates_school_entry, public_health_institutions).
narrative_ontology:constraint_beneficiary(vaccine_mandates_school_entry, school_administrators).
narrative_ontology:constraint_victim(vaccine_mandates_school_entry, unvaccinated_families).
narrative_ontology:constraint_victim(vaccine_mandates_school_entry, vaccine_hesitant_parents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNVACCINATED CHILD (SNARE) — The child faces a binary: vaccination or school exclusion. Parents cannot opt out without bearing severe costs: homeschooling burdens, lost educational access, social stigma. Exit is structurally available but suppression mechanisms (social pressure, educational access restrictions, employment prospects for parents) make it prohibitive. Experienced extraction is maximal from this position — the constraint permits no genuine alternatives.
constraint_indexing:constraint_classification(vaccine_mandates_school_entry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: VACCINE-HESITANT PARENT (TANGLED ROPE) — Faces genuine coordination problem: disease transmission prevention benefits the broader student population. But extraction is asymmetric — this agent's medical autonomy is subordinated to collective benefit without proportional voice in the mandate design. Exit options exist (private schools, religious exemptions where available, homeschooling) but are costly. Experiences both coordination (shared disease prevention) and extraction (coerced participation).
constraint_indexing:constraint_classification(vaccine_mandates_school_entry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PUBLIC HEALTH AUTHORITY (ROPE) — Solves coordination problem: preventing vaccine-preventable disease outbreaks requires population-level immunization. The mandate is the mechanism. Experiences the constraint as pure coordination — the extraction benefits flow toward this agent, but are justified by disease prevention. Can exit easily (remove mandate) and can arbitrage between different policy configurations (mandate strength, exemption breadth). Net beneficiary.
constraint_indexing:constraint_classification(vaccine_mandates_school_entry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: IMMUNOCOMPROMISED STUDENT (TANGLED ROPE) — Benefits from vaccination of others (herd immunity protection). But also bears extraction: their medical condition becomes grounds for treating others' vaccination as a public good enforceable on everyone. Their vulnerability is mobilized to justify mandates that primarily benefit the broader population, not them. Exit via segregation (remote learning, specialized schools) is available but costly.
constraint_indexing:constraint_classification(vaccine_mandates_school_entry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: SCHOOL ADMINISTRATIVE SYSTEM (PITON) — Mandatory vaccination requirements persist in school entry protocols through institutional inertia. Schools implement mandates because they are law/policy, not because the administration has tested whether alternatives (testing, optional vaccination, risk-stratified policies) would work as well. Theater ratio indicates performative compliance: the mandate is maintained and documented but its justification (preventing school closure, protecting students) has shifted as disease risk profiles changed. The administrative routine persists despite reduced functional necessity.
constraint_indexing:constraint_classification(vaccine_mandates_school_entry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (CIVILIZATIONAL) — From a civilizational perspective, vaccine mandates for school entry represent a genuine coordination mechanism (herd immunity prevents disease spread) combined with asymmetric extraction (some agents bear costs they did not choose). The mandate is neither pure coordination nor pure extraction, but a hybrid where the coordination benefit is real and the asymmetric cost distribution is also real. This is the canonical Tangled Rope signature: both functions present, both measurable, both irreducible.
constraint_indexing:constraint_classification(vaccine_mandates_school_entry, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandates_school_entry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(vaccine_mandates_school_entry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(vaccine_mandates_school_entry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandates_school_entry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(vaccine_mandates_school_entry, TR),
    TR >= 0.70.

:- end_tests(vaccine_mandates_school_entry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint does extract from vaccine-hesitant families — it imposes medical intervention without their consent and restricts educational access as enforcement. But extractiveness is not as high as pure coercion would suggest (0.75+) because the coordination benefit (disease prevention, herd immunity) is genuine and measurable. The value reflects that extraction is real but justified by legitimate coordination function. Suppression (0.62): High. Barriers to exit include social stigma, loss of educational access, career consequences for parents, and in many jurisdictions, absence of meaningful exemptions. But suppression is not total (0.85+) because some families do opt out (private schools, homeschooling, jurisdictions with exemptions), though at substantial cost. Theater ratio (0.45): Moderate. The mandate has real functional content — vaccination does prevent disease transmission. But theater is increasing (per measurements) as disease risk profiles change while mandate justifications persist unchanged. The performative component grows as the functional necessity diminishes.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence. The powerless, trapped agent (unvaccinated child) experiences pure Snare — coercion with no meaningful exit. The public health authority experiences pure Rope — solving a coordination problem. The moderate, constrained agents (vaccine-hesitant parents, immunocompromised students) experience Tangled Rope — both coordination benefit and extraction. The school administrative system experiences Piton — maintenance of a policy through inertia despite reduced functional necessity. The analytical observer sees Tangled Rope as the appropriate classification. The gap between powerless and institutional perspectives is maximal: one sees extraction, the other sees coordination. This is the canonical perspectival gap that defines Tangled Rope: the same structural phenomenon appears as either coordination or extraction depending on the observer's structural position relative to the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position in the extraction flow. Unvaccinated families are victims — they bear costs (medical intervention, educational exclusion, social penalty) without proportional benefit. Vaccination protects them individually, but the mandate imposes it regardless of their consent or their individual risk-benefit calculation. They derive d ≈ 0.88 (high target): beneficiary status would lower d; victim status with trapped exit raises it. Public health authority is institutional beneficiary with arbitrage exit — they benefit from disease prevention (their institutional mission) and can arbitrage between different policy configurations (mandate strength, exemption breadth). They derive d ≈ 0.10 (low target/beneficiary): beneficiary status lowers d; arbitrage exit lowers it further. Vaccine-hesitant parents are victims with constrained exit — they cannot leave without cost, but can organize and change minds if evidence shifts. They derive d ≈ 0.65 (moderate-high target). Immunocompromised students are beneficiaries (herd immunity protects them) but constrained victims (their vulnerability is mobilized to justify coercion on others). Their d ≈ 0.55 (near-symmetric): beneficiary and victim statuses balance.
 *
 * MANDATROPHY ANALYSIS:
 *   PRELIMINARY (mandatrophy_resolved: false). The constraint does not yet resolve the mandatrophy because the empirical question of disease risk threshold remains open. If current disease prevalence is near zero in vaccinated populations, the mandate functions as extraction with health coordination as cover story — it would shift toward pure Snare (ε increases, χ increases, classification narrows). If disease risk remains significant and alternative mechanisms are insufficient, the mandate is genuine Tangled Rope (ε stable or increases, but justified by real coordination benefit). The mandatrophy is resolved when: (1) disease epidemiology clarifies whether current risk justifies coercion, (2) alternative mechanisms are systematically tested and their sufficiency demonstrated or falsified, and (3) identity-lock mechanisms are distinguished from rational choice hesitation. Until these empirical questions are answered, the classification as Tangled Rope remains analytical rather than definitively structural. The theater ratio's upward trajectory suggests the functional necessity may be declining even as the policy persists, which would shift the classification toward Piton (degraded institutional maintenance) — this trajectory should trigger mandate review or sunset provisions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_disease_risk_threshold,
    'At what disease prevalence and severity threshold does mandatory vaccination shift from coordination to extraction?',
    'Comparative analysis of disease epidemiology at mandate introduction vs current conditions; mapping of mandate strength to actual outbreak risk; cohort studies of health outcomes with/without mandate',
    'If current disease risk is near zero: mandate appears extractive (coordination benefit is phantom). If disease risk remains significant: mandate appears justified as coordination mechanism. Classification shifts from Snare toward Rope as risk justifies coercion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(empirical_disease_risk_threshold, empirical, 'Disease risk threshold for mandate justification').

omega_variable(
    alternative_mitigation_sufficiency,
    'Would alternative non-mandatory mechanisms (rapid testing, voluntary vaccination, risk-stratified policies) achieve equivalent herd immunity and disease prevention outcomes?',
    'Comparative policy analysis across jurisdictions with different mandate strictness; modeling of alternative protocols against actual outbreak data; cost-benefit analysis of mandate overhead vs alternative mechanisms',
    'If alternatives sufficient: mandate is extraction using health coordination as cover story. If alternatives insufficient: mandate is genuine coordination necessity. Directionally shifts classification from Snare toward Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_mitigation_sufficiency, empirical, 'Whether non-mandatory alternatives achieve equivalent disease prevention').

omega_variable(
    identity_lock_vs_rational_choice,
    'Is vaccine hesitation driven by rational cost-benefit calculation relative to alternative health strategies, or by identity-locked commitment to bodily autonomy frames?',
    'Qualitative analysis of hesitancy narratives; comparison of hesitant vs non-hesitant populations'' underlying beliefs about medical autonomy, trust in institutions, and risk tolerance; follow-up on vaccine-hesitant individuals who change their minds — what caused the shift?',
    'If rational: agents are constrained but not identity-locked; exit options exist at measurable cost. If identity-locked: exit would require identity rupture; suppression is internalized. Changes exit_options classification for vaccine-hesitant parents from constrained to potentially identity_locked.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_rational_choice, empirical, 'Vaccine hesitation as rational choice vs identity commitment').

omega_variable(
    exemption_accessibility_mechanism,
    'Do exemptions (medical, religious, philosophical where available) function as genuine exit options or as performative releases that preserve the mandate''s coercive structure?',
    'Audit of exemption approval rates and processes; comparison of exemption availability across jurisdictions; analysis of whether exemption paths are publicized and accessible or obscured; follow-up with families who attempted exemption — how many succeeded, how many faced barriers?',
    'If exemptions are genuine: suppression metric decreases, exit options shift from trapped to constrained. If exemptions are performative (rarely approved, deliberately obscured): suppression remains high, exit options stay trapped. Determines whether powerless agent perspective should classify as Snare (true) or Tangled Rope (conditional).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exemption_accessibility_mechanism, empirical, 'Whether exemptions provide genuine exit or performative relief').

omega_variable(
    institutional_mandate_persistence_driver,
    'Is vaccine mandate persistence driven by genuine ongoing disease threat, by institutional path-dependence, or by political commitment to the policy regardless of epidemiology?',
    'Content analysis of public health communications — frequency of disease-risk justification vs policy-consistency framing; policy reviews comparing mandate timing to disease epidemiology; interviews with public health officials about mandate necessity; analysis of mandate changes following disease risk shifts',
    'If disease-driven: Tangled Rope classification correct. If path-dependent or political: classification degrades toward Piton (performative maintenance) or pure Snare (extraction justified by outdated rationale). Theater ratio should increase over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_mandate_persistence_driver, empirical, 'Mandate persistence driver: epidemiology vs institutional inertia vs political commitment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandates_school_entry, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vax_mandate_theater_t0, vaccine_mandates_school_entry, theater_ratio, 0, 0.25).
narrative_ontology:measurement(vax_mandate_theater_t3, vaccine_mandates_school_entry, theater_ratio, 3, 0.35).
narrative_ontology:measurement(vax_mandate_theater_t6, vaccine_mandates_school_entry, theater_ratio, 6, 0.45).

% Extraction over time
narrative_ontology:measurement(vax_mandate_extractiveness_t0, vaccine_mandates_school_entry, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(vax_mandate_extractiveness_t3, vaccine_mandates_school_entry, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(vax_mandate_extractiveness_t6, vaccine_mandates_school_entry, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandates_school_entry, enforcement_mechanism).
narrative_ontology:affects_constraint(vaccine_mandates_school_entry, herd_immunity_threshold).
narrative_ontology:affects_constraint(vaccine_mandates_school_entry, medical_autonomy_vs_collective_health).
narrative_ontology:affects_constraint(vaccine_mandates_school_entry, school_access_as_public_good).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vaccine_mandates_school_entry, powerless, 0.88).
constraint_indexing:directionality_override(vaccine_mandates_school_entry, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
