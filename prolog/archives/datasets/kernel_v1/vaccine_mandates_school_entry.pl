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
 *   Vaccine mandates for school entry exemplify tangled rope classification:
 *   the constraint solves a genuine collective action problem (preventing
 *   vaccine-preventable disease outbreaks through population-level herd
 *   immunity) while simultaneously imposing costs on agents who did not
 *   choose participation in the system. The constraint has a real
 *   coordination function (outbreak suppression benefits all students,
 *   including those who cannot be vaccinated due to age or medical
 *   contraindication), which distinguishes it from pure extraction. However,
 *   the constraint also exhibits asymmetric extraction: families with vaccine
 *   hesitation or medical concerns face suppression (loss of public school
 *   access) that is not offset by proportional coordination benefit from
 *   their perspective. The mandates operate through institutional enforcement
 *   mechanisms (school enrollment conditioning) rather than through pure
 *   incentive alignment. The constraint's evolution shows increasing
 *   extractiveness as vaccine resistance increases (requiring heightened
 *   enforcement) and increasing suppression as exemption policies tighten in
 *   response to outbreaks. The false summit risk is high: epidemiological
 *   framing ('herd immunity is biologically necessary') naturalizes what is
 *   actually an institutional choice among enforcement alternatives (mandates
 *   vs incentives vs targeted strategies).
 *
 * KEY AGENTS:
 *   - Unvaccinated Families: Primary victims (powerless/trapped) — forced choice between vaccination or school exclusion; no realistic exit options in most jurisdictions
 *   - Vaccine-Hesitant Parents: Secondary victims (moderate/identity_locked) — have structural exit options but experience them as identity-violating; identity fused with vaccine skepticism makes coordination frame invisible
 *   - School District Administrators: Institutional coordinators (moderate/constrained) — experience genuine coordination necessity but also enforcement burden; must implement mandates or face state sanctions
 *   - Public Health Authority: Primary beneficiary (institutional/arbitrage) — experiences mandate as pure coordination tool; can arbitrage between policies and jurisdictions
 *   - Unvaccinated Disease-Vulnerable Populations: Paradoxical victim-beneficiary (powerless/trapped) — protected by herd immunity coordination but may be harmed by mandate's suppression of alternative accommodations (remote learning, isolation, targeted protection)
 *   - Immunocompromised Students: Paradoxical beneficiary-victim (powerless/trapped) — theoretically protected by mandate-driven herd immunity but may lack individualized accommodations if mandate forces in-person attendance
 *   - Public Health Surveillance System: Institutional infrastructure (organized/constrained) — temporarily activated by mandate; should deprecate as herd immunity reaches sustainability threshold
 *   - Epidemiological Community: Analytical observers at risk of false summit (analytical/analytical) — risk naturalizing institutional choice (mandate strategy) as biological necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandates_school_entry, 0.52).
domain_priors:suppression_score(vaccine_mandates_school_entry, 0.65).
domain_priors:theater_ratio(vaccine_mandates_school_entry, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandates_school_entry, extractiveness, 0.52).
narrative_ontology:constraint_metric(vaccine_mandates_school_entry, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(vaccine_mandates_school_entry, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandates_school_entry, tangled_rope).
narrative_ontology:human_readable(vaccine_mandates_school_entry, "Vaccine Mandates for School Entry").
narrative_ontology:topic_domain(vaccine_mandates_school_entry, "public_health/education/governance").

domain_priors:requires_active_enforcement(vaccine_mandates_school_entry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandates_school_entry, unvaccinated_disease_vulnerable_populations).
narrative_ontology:constraint_beneficiary(vaccine_mandates_school_entry, public_health_system).
narrative_ontology:constraint_victim(vaccine_mandates_school_entry, vaccine_hesitant_families).
narrative_ontology:constraint_victim(vaccine_mandates_school_entry, immunocompromised_students).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNVACCINATED FAMILY (SNARE) — Powerless agents with no exit: forced choice between vaccination against parental belief/medical concern or exclusion from public school. No alternative schools available in most jurisdictions; homeschooling requires parental capacity (economic, educational, time). Suppression is structural — the mandate removes the option, not merely makes it costly. Maximum extraction from this perspective.
constraint_indexing:constraint_classification(vaccine_mandates_school_entry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SCHOOL DISTRICT ADMINISTRATOR (TANGLED ROPE) — Faces genuine coordination necessity (preventing outbreaks protects all students) but also faces extraction burden: enforcement labor, exemption processing, documentation, parental conflict management, legal liability. Benefits from coordinated herd immunity; bears costs of enforcement. Moderate power with constrained exit — can advocate for policy changes but must ultimately implement mandates or face state sanctions.
constraint_indexing:constraint_classification(vaccine_mandates_school_entry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PUBLIC HEALTH AUTHORITY (ROPE) — Experiences the mandate as pure coordination: aggregated vaccination reduces outbreak risk across the population, enabling unconstrained school operation and reduced surveillance burden. Can arbitrage between jurisdictions and policies. Benefits substantially exceed costs — the mandate is fundamentally their tool for solving collective action. Institutional power with high exit optionality (can modify mandate, enforce selectively, grant exemptions).
constraint_indexing:constraint_classification(vaccine_mandates_school_entry, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PUBLIC HEALTH SURVEILLANCE SYSTEM (SCAFFOLD) — Organized infrastructure designed as temporary support for outbreak prevention. Mandate activates surveillance, outbreak response protocols, and vaccination coordination. Scaffold classification reflects the sunset logic: as vaccination rates reach herd immunity thresholds (estimated 85-95% depending on pathogen), the mandate theoretically becomes unnecessary — surveillance detects outbreaks reliably without mandate enforcement. Theater remains low because the coordinating function is genuine. Sunset conditional on sustained high vaccination rates.
constraint_indexing:constraint_classification(vaccine_mandates_school_entry, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: VACCINE-HESITANT PARENT (TANGLED ROPE, identity_locked) — Distinct from the powerless family perspective. This agent has structural exit options (private school, homeschooling, medical exemptions if qualified) but experiences them as unthinkable because parental identity is fused with vaccine skepticism. The parent's self-concept as a protective, informed parent is constituted through the belief that mandatory vaccination violates autonomy and represents captured regulatory capture. Exit (accepting the mandate as legitimate coordination) would require abandoning the identity frame, not just paying a material cost. Structurally mobile but functionally locked. Coordination benefits exist (outbreak prevention) but are invisibilized by identity frame.
constraint_indexing:constraint_classification(vaccine_mandates_school_entry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 6: EPIDEMIOLOGICAL INEVITABILITY (MOUNTAIN) — Views vaccine mandates as following necessarily from basic epidemic dynamics: once pathogen transmissibility and disease severity are known, herd immunity thresholds are mathematically determined. From this perspective, the mandate is not a constructed constraint but a natural law of disease dynamics — it simply instantiates what the biology requires. Emergence appears natural. However, this perspective naturalizes several contingent institutional choices: (1) whether the disease is endemic or eradicable; (2) whether mandate is the chosen enforcement mechanism vs alternatives (voluntary incentives, school-linked clinics); (3) whether exemptions are permitted; (4) whether benefit-risk calculus favors universal vs targeted mandates. False summit candidate.
constraint_indexing:constraint_classification(vaccine_mandates_school_entry, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: SMALLPOX MANDATE PRECEDENT (PITON) — Vaccine mandates for school entry have 150+ year institutional precedent (Jacobson v. Massachusetts, 1905; smallpox eradication). The smallpox mandate was highly functional — mandatory vaccination achieved herd immunity rapidly and eradicated the disease. Contemporary mandates inherit this institutional framing ('we've done this before, it works') but often apply to diseases with much lower mortality/transmission than smallpox. The theater increases as the pathogens change — mandate ritual persists from institutional inertia even when epidemiological case is weaker. Infrastructure degraded relative to original function but maintained through precedent and organizational form.
constraint_indexing:constraint_classification(vaccine_mandates_school_entry, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

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
 *   Extractiveness (0.52): Moderate-high. The mandate does solve a genuine collective action problem (herd immunity coordination), preventing the ~2-4% free-rider extraction that would occur if vaccination were purely voluntary and disease-vulnerable agents remained unprotected. However, the enforcement mechanism imposes costs on hesitant families that exceed the coordination benefit they perceive. The extractiveness trajectory shows growth over time (0.35 → 0.52) as vaccine resistance hardens and exemption policies tighten, requiring more intensive enforcement. Suppression (0.65): High. The constraint removes the option of unvaccinated school attendance; it does not merely make it costly. Alternative pathways (homeschooling, private schools) exist in principle but are practically inaccessible for many families (economic, educational, geographic barriers). The suppression trajectory shows growth (0.45 → 0.65) as exemption categories narrow and enforcement intensifies. Theater ratio (0.38): Moderate-low. The coordination function is genuine — vaccination does prevent outbreaks and enable school operation without constant disease management. Unlike pure theater constraints, the mandate's machinery (vaccination verification, exemption processing) serves a functional purpose beyond performative display. The theater component reflects procedures (documentation, exemption justification) that exceed minimum necessary enforcement, but the core function is real.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival divergence. The unvaccinated family experiencing snare sees coercion without benefit. The vaccine-hesitant parent experiencing tangled_rope (identity_locked) sees extraction of autonomy within an invisible coordination frame. The school administrator experiencing tangled_rope sees genuine coordination necessity paired with enforcement cost. The public health authority experiencing rope sees pure coordination with minimal cost. The immunocompromised student occupies an internal paradox: theoretically a beneficiary of herd immunity but potentially harmed by mandate inflexibility that eliminates remote learning options. The epidemiological observer risks mountain classification by naturalizing the mandate as a requirement of disease dynamics, when the actual requirement is merely 'achieve herd immunity' — the mandate is one institutional strategy for achieving that among several alternatives.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d value) is derived from the agent's structural relationship to the extraction flow. Unvaccinated families are full victims (d ≈ 0.95 → f(d) ≈ 1.42) with no exit: high experienced extraction. Vaccine-hesitant parents with identity lock are victims with nominal exit options (d ≈ 0.88 → f(d) ≈ 1.25) that they cannot psychologically access: high experienced extraction despite structural mobility. School administrators are mixed (d ≈ 0.55 → f(d) ≈ 0.75): bearing enforcement costs but also benefiting from outbreak prevention and reduced absenteeism. Public health authorities are full beneficiaries (d ≈ 0.05 → f(d) ≈ -0.12): the mandate is their extraction-reducing mechanism, producing negative effective chi for their perspective. Immunocompromised students have ambiguous directionality: nominally beneficiaries (protected by herd immunity) but potentially harmed by mandate's suppression of accommodation pathways, complicating d assignment. The epidemic observer's d is analytical (d ≈ 0.73 → f(d) ≈ 1.15), not empirically grounded in beneficiary/victim status, allowing the false summit risk to surface.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by clarifying what the mandate coordinates and what it extracts. The coordination function is genuine: vaccine-preventable disease outbreaks harm all students, including those who cannot be vaccinated; herd immunity through vaccination protects the population. The mandate achieves herd immunity at a faster rate than voluntary vaccination alone would. This is real coordination, not theater — the constraint solves a collective action problem. However, the mandate also extracts from hesitant families by removing their exit option (unvaccinated school attendance) without offering alternative accommodation. The extraction is not contingent on the coordination: even if exemptions were freely granted and the mandate had no teeth, the coordination function (herd immunity from voluntary vaccination) would remain. Conversely, even if the mandate were perfectly fair (imposing equal costs on all), it would still coordinate herd immunity. The tangled rope classification correctly separates these: the constraint has both a coordination function (genuine) and an extraction mechanism (asymmetric enforcement). Neither dominates; both are structurally necessary to the constraint's operation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exemption_compliance_threshold,
    'At what exemption rate does the mandate lose its coordination function and become pure extraction?',
    'Comparative analysis across jurisdictions: correlate exemption rates with actual outbreak prevention, herd immunity maintenance, and disease incidence. Identify threshold where coordination breaks down.',
    'If threshold < 5% exemptions: mandate requires near-universal compliance; high suppression justified. If threshold > 15%: mandate''s coordination claim weakens; classification drifts toward snare (enforcement without function).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exemption_compliance_threshold, empirical, 'Exemption rate at which mandate loses coordination function').

omega_variable(
    identity_lock_vs_rational_hesitation,
    'Does vaccine hesitation in mandate-resistant populations reflect genuine medical/autonomy reasoning (rational constrained agents) or identity-fused rejection (identity_locked agents)?',
    'Qualitative analysis of parental narratives and decision-making; longitudinal tracking of individuals who change vaccine stance; comparison of hesitancy persistence vs evidence presentation. Distinguish cognitive capture from autonomy concerns.',
    'If predominantly identity_locked: the constraint may be working as designed (enforcing coordination despite identity resistance), and classification as tangled_rope is appropriate. If predominantly rational hesitation: suppression measure understates agent agency, and snare classification may be more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_rational_hesitation, conceptual, 'Whether hesitation is identity-fused or rationally justified').

omega_variable(
    natural_law_vs_institutional_choice,
    'Is the mandate a natural consequence of epidemiological law (mountain), or a particular institutional choice among alternatives (tangled_rope/snare)?',
    'Historical comparison of disease control strategies (quarantine, isolation, voluntary vaccination with incentives, school-linked clinics, targeted high-risk mandates). If alternatives achieved similar outcomes at lower suppression cost, the mandate was institutional choice, not natural law.',
    'If natural law: mountain classification confirmed; mandate is immutable given the disease biology. If institutional choice: false summit triggered; classification drifts toward tangled_rope or snare depending on alternative availability and enforcement intensity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_institutional_choice, conceptual, 'Whether mandate follows from epidemiological necessity or institutional preference').

omega_variable(
    immunocompromised_paradox,
    'Are immunocompromised students genuinely protected by mandate-driven herd immunity, or does the mandate''s suppression of alternative protection pathways (remote learning, individualized accommodations) actually harm them?',
    'Health outcome analysis: track infection rates, hospitalization, and mortality in immunocompromised students before and after mandate implementation. Compare to alternative protection strategies (remote option, smaller cohort isolation, targeted testing). Identify whether mandate coordination benefit accrues to the vulnerable population.',
    'If protected: mandate''s beneficiary claim (includes immunocompromised) is validated. If harmed by mandate inflexibility: mandate''s victim designation is internally inconsistent; immunocompromised become victims alongside hesitant families. Classification may shift if mandate increases net harm to the vulnerable population it claims to protect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immunocompromised_paradox, empirical, 'Whether mandate protects or harms immunocompromised students').

omega_variable(
    enforcement_mechanism_substitution,
    'Could the coordination function (herd immunity through vaccination) be achieved with lower suppression via alternative enforcement: incentives, school-linked clinics, targeted mandates for high-risk groups, voluntary high-confidence campaigns?',
    'Comparative jurisdiction analysis: compare outcomes (vaccination rates, outbreak prevention, community trust) between mandate jurisdictions and alternative-mechanism jurisdictions. Identify whether lower-suppression alternatives achieve equivalent coordination.',
    'If alternatives achieve equivalent outcomes: mandate''s suppression level is excessive relative to coordination necessity; extraction component increases. Classification remains tangled_rope but with emphasis on unnecessary extraction. If mandates are uniquely effective: suppression is justified coordination cost; extraction component is legitimate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_mechanism_substitution, empirical, 'Whether lower-suppression mechanisms achieve equivalent coordination').

omega_variable(
    temporal_mandate_necessity,
    'At what vaccination coverage threshold does the mandate''s coordination function deprecate, allowing sunset to activate?',
    'Epidemiological modeling and empirical validation: estimate herd immunity threshold for target pathogen, track vaccination coverage over time, correlate with outbreak suppression. Identify when outbreak risk drops below mandate necessity threshold.',
    'If sunset threshold is reachable: scaffold classification is valid. If coverage plateaus below threshold indefinitely: mandate becomes permanent extraction mechanism; classification drifts toward tangled_rope or snare. Sunset clause reality determines functional constraint type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_mandate_necessity, empirical, 'Vaccination coverage threshold enabling mandate sunset').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandates_school_entry, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vmse_tr_t0, vaccine_mandates_school_entry, theater_ratio, 0, 0.32).
narrative_ontology:measurement(vmse_tr_t10, vaccine_mandates_school_entry, theater_ratio, 10, 0.36).
narrative_ontology:measurement(vmse_tr_t20, vaccine_mandates_school_entry, theater_ratio, 20, 0.38).

% Extraction over time
narrative_ontology:measurement(vmse_be_t0, vaccine_mandates_school_entry, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(vmse_be_t10, vaccine_mandates_school_entry, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(vmse_be_t20, vaccine_mandates_school_entry, base_extractiveness, 20, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(vmse_su_t0, vaccine_mandates_school_entry, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(vmse_su_t10, vaccine_mandates_school_entry, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(vmse_su_t20, vaccine_mandates_school_entry, suppression_requirement, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandates_school_entry, attachment_coordination).
narrative_ontology:affects_constraint(vaccine_mandates_school_entry, herd_immunity_threshold_dynamics).
narrative_ontology:affects_constraint(vaccine_mandates_school_entry, exemption_policy_extraction_ratchet).
narrative_ontology:affects_constraint(vaccine_mandates_school_entry, vaccine_hesitancy_identity_lock).

% DUAL FORMULATION NOTE:
% Vaccine mandates decompose into multiple structurally distinct constraints: (1) vaccination coordination (achieving herd immunity through behavioral incentive), with ε ≈ 0.15-0.25; (2) mandate enforcement (removing school access to unvaccinated students), with ε ≈ 0.55-0.70; (3) exemption policy (medical vs philosophical distinction), with ε varying by exemption category; (4) identity-lock dynamics in vaccine hesitancy (independent of mandate coercion). This story focuses on the mandate enforcement constraint (item 2). Upstream constraint (item 1) is lower-extraction (pure coordination); downstream constraints (items 3-4) have higher extraction and different victim sets. The three linked stories together form the vaccine-mandate family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
