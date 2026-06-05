% ============================================================================
% CONSTRAINT STORY: public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_health_primary, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: public_health_primary
 *   human_readable: State Compulsion for Collective Harm Prevention (Public Health Primary Reading)
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint represents ONE reading of the contested kernel
 *   'coercion_legitimacy_boundary' — the unstabilized constitutional claim
 *   about when state coercion is justified. The public_health_primary reading
 *   answers: state may compel medical intervention when collective
 *   harm-prevention outweighs individual autonomy. This is ONE claim among at
 *   least three live positions: bodily_autonomy_primary (autonomy is
 *   foundational, coercion is rarely justified) and proportionality_reading
 *   (coercion is justified only when narrowly tailored, time-bounded, and
 *   proportionate to threat). This story instantiates ONLY the
 *   public_health_primary reading as a clean constraint with stable
 *   extractiveness. It does not average over readings or hedge across them.
 *   The sibling readings are separate constraint stories with their own
 *   extractiveness values. The constraint describes the structural
 *   relationship between state authority (beneficiary, arbitrage position),
 *   unvaccinated individuals (victim, trapped position), and collective
 *   protection (beneficiary, constrained position) under the assumption that
 *   collective harm-prevention is the primary legitimating principle.
 *   Extractiveness rises from 0.35 (early pandemic, voluntary compliance
 *   phase) to 0.62 (peak enforcement, mandatory vaccination deadlines,
 *   employment restrictions) then declines to 0.58 (endemic phase,
 *   enforcement maintenance through inertia rather than acute threat).
 *   Theater ratio rises from 0.32 (early pandemic, genuine emergency
 *   conditions, minimal bureaucratic theater) to 0.55 (late pandemic,
 *   continued emergency justification despite endemic equilibrium, increasing
 *   performative content). The constraint exhibits a Scaffold structure
 *   during acute pandemic (time-bounded, legitimate emergency coercion)
 *   drifting toward Piton structure during endemic phase (emergency powers
 *   maintained through institutional inertia, theater rising, functional
 *   justification declining).
 *
 * KEY AGENTS:
 *   - Unvaccinated Individuals: Primary victim (powerless/trapped) — subject to compulsory medical intervention, employment restrictions, movement constraints. No negotiation over terms. Experience maximum effective extraction.
 *   - Public Health Authority: Primary beneficiary (institutional/arbitrage) — gains enforcement legitimacy, coordination mechanism to reach herd immunity thresholds, expanded regulatory capacity. Derives benefit from the constraint mechanism itself.
 *   - Immunocompromised Populations: Secondary beneficiary (moderate/constrained) — protected via mandate-driven herd immunity. Also constrained by paternalistic state apparatus and contingent on mandate continuation. Mixed extraction and benefit.
 *   - Medical Autonomy Principle: Victim of principle subordination (powerful/mobile) — extracted from (recalibrated subordinate to public health), but retains generational recovery via informed consent doctrine and medical ethics boards. Negotiated tension rather than destruction.
 *   - Public Health Governance Coalition: Organized framers (organized/constrained) — design mandates as time-bounded Scaffold structures with explicit sunset logic. Constrained by scientific evidence requirements and proportionality accountability.
 *   - Emergency Powers Framework: Institutional actor (institutional/arbitrage) — legacy legal structure for martial law, emergency declarations, temporary constitutional suspension. Maintains performative emergency justification; drifts toward Piton as functional emergency ends but legal framework persists.
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional choice (public health primary) as an immutable feature of social organization (Mountain). False summit candidate.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_primary, 0.62).
domain_priors:suppression_score(public_health_primary, 0.68).
domain_priors:theater_ratio(public_health_primary, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_primary, extractiveness, 0.62).
narrative_ontology:constraint_metric(public_health_primary, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(public_health_primary, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_primary, tangled_rope).
narrative_ontology:human_readable(public_health_primary, "State Compulsion for Collective Harm Prevention (Public Health Primary Reading)").
narrative_ontology:topic_domain(public_health_primary, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(public_health_primary, formalized).
narrative_ontology:cs_authority_grounding(public_health_primary, lineage).
narrative_ontology:cs_interpretation_layer_present(public_health_primary).
narrative_ontology:cs_kernel_id(public_health_primary, coercion_legitimacy_boundary).
narrative_ontology:cs_reading_relation(public_health_primary, bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation(public_health_primary, proportionality_reading, influences).
narrative_ontology:cs_axiom(public_health_primary, foundational, collective_harm_prevention_primacy).
narrative_ontology:cs_axiom_status(collective_harm_prevention_primacy, holdable).
narrative_ontology:cs_axiom(public_health_primary, foundational, autonomy_subordinable_to_aggregate_benefit).
narrative_ontology:cs_axiom_status(autonomy_subordinable_to_aggregate_benefit, holdable).
narrative_ontology:cs_reference_frame(public_health_primary, public_health_emergency_authority).
narrative_ontology:cs_drift_state(public_health_primary, endemic_equilibrium, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_primary, immunocompromised_populations).
narrative_ontology:constraint_beneficiary(public_health_primary, public_health_apparatus).
narrative_ontology:constraint_victim(public_health_primary, unvaccinated_individuals).
narrative_ontology:constraint_victim(public_health_primary, medical_autonomy_principle).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNVACCINATED INDIVIDUAL (SNARE) — Faces legal mandate with minimal exit options (relocation, medical exemption narrowly construed, or violation with legal penalty). State enforcement apparatus compels medical intervention; alternatives are suppressed. The individual experiences maximum extraction: bodily autonomy is subordinated to collective harm-prevention calculus with no negotiation. No residual coordination benefit perceived at this position.
constraint_indexing:constraint_classification(public_health_primary, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PUBLIC HEALTH AUTHORITY (ROPE) — Experiences the constraint as legitimate coordination mechanism: mandates solve collective action problems (herd immunity thresholds) that voluntary compliance cannot reach. The authority has enforcement discretion (arbitrage) and derives legitimacy from harm-prevention function. Extraction toward this agent is the mechanism's entire purpose — perceived as justified coordination, not unjust extraction.
constraint_indexing:constraint_classification(public_health_primary, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: IMMUNOCOMPROMISED POPULATION (TANGLED ROPE) — Benefits from the mandate (protection via others' vaccination) but experiences real constraints: reliance on paternalistic state apparatus, limited voice in design of thresholds or timing, and vulnerability to mandate withdrawal. Mixed extraction and coordination — simultaneously protected and subject to state decisions that constrain their autonomy in other contexts (movement, employment contingency).
constraint_indexing:constraint_classification(public_health_primary, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MEDICAL AUTONOMY PRINCIPLE (TANGLED ROPE) — As a principle, experiences extraction (subordination to public health calculus) but retains generational recovery: informed consent doctrine, patient rights movements, and medical ethics boards represent ongoing coordination to preserve autonomy alongside collective protection. The principle is not destroyed by the mandate but recalibrated — negotiated tension rather than one-sided extraction.
constraint_indexing:constraint_classification(public_health_primary, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: PUBLIC HEALTH GOVERNANCE COALITION (SCAFFOLD) — Organized actors (epidemiologists, ethics committees, legislative bodies) see mandates as temporary coordination structures with sunset logic: effective during acute pandemic conditions, scaling down as transmission declines, sunset toward voluntary compliance as herd immunity stabilizes or endemic equilibrium is reached. Enforcement is explicitly time-bounded relative to the threat. Theater is moderate — scientific justification is required, not performative ritual.
constraint_indexing:constraint_classification(public_health_primary, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: EMERGENCY POWERS FRAMEWORK (PITON) — The legal apparatus for emergency mandates (martial law, emergency declarations, temporary suspension of normal constitutional limits) persists through institutional inertia long after acute conditions end. Theater ratio high: mandates maintain performative emergency justification even as threats are endemic or manageable. The mechanism that was deployed as Scaffold (time-bounded, sunset-oriented) drifts toward Piton (maintained through inertia and legal theater despite functional termination of the original emergency).
constraint_indexing:constraint_classification(public_health_primary, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some tension between individual autonomy and collective harm-prevention is inherent to any social order: no pure autonomy exists in interdependent systems, and no collective good is achievable without some constraint on individual choice. This perspective sees the boundary as an immutable feature of social organization, not a contingent policy decision. However, the structural data contradicts the mountain classification — the engine's false summit detector will identify this as naturalization of a contested institutional choice.
constraint_indexing:constraint_classification(public_health_primary, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_health_primary_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(public_health_primary, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(public_health_primary, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(public_health_primary, TR),
    TR >= 0.70.

:- end_tests(public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): Moderate-high. This reading directly subordinates individual medical autonomy to collective harm-prevention, creating significant extraction from unvaccinated individuals (compulsion, employment restrictions, movement constraints). However, extractiveness is not maximal (snare-level ≥0.70) because: (1) the extraction is bounded in scope (medical intervention only, not confiscatory), (2) it has temporal structure (nominally time-bounded to acute emergency, though this decays), (3) the beneficiary (collective protection) is partially genuine (not pure rent-seeking), and (4) some coordination function exists (vaccination mandates do solve herd immunity collective action problems that voluntary compliance cannot reach). The reading acknowledges that extraction exists and is justified — it does not deny the extraction, it justifies it. This is the Tangled Rope signature: genuine coordination function AND asymmetric extraction AND enforcement, with extractiveness moderate enough to admit the coordination claim. Suppression (0.68): High. Barriers to exit from the mandate are substantial: legal penalties for non-compliance, employment termination, school exclusion, travel restrictions, medical exemption narrowly construed. However, suppression is not total (not 0.85+) because relocation and exemption options exist, however costly. Unvaccinated individuals retain some agency (illegal non-compliance, appeal to exemption criteria, relocation), distinguishing this from pure physical confinement (trapped suppression ≥0.80). Theater ratio (0.48): Moderate. At peak emergency (time_point 2-4), theater is relatively low — mandates are justified by genuine epidemiological evidence (case rates, hospitalization burden, mortality data). As the constraint persists into endemic phase (time_point 6+), theater rises: the same legal mandates are maintained despite reduced mortality threat, justified through inertia and residual risk rather than acute emergency. The Scaffold → Piton drift is visible in the rising theater: the constraint begins with genuine functional justification and drifts toward performative maintenance. Claimed type: Tangled Rope. The constraint manifests both genuine coordination (solving collective action problem of herd immunity) and asymmetric extraction (subordinating individual autonomy to collective benefit). The boundary between justified coordination and unjustified extraction is the central uncertainty — omega variables document this.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the perspectival multiplicity of the coercion_legitimacy_boundary kernel. From the public health authority's position, mandates are legitimate coordination (Rope) — they solve collective action problems and are experienced as justified authority, not unjust extraction. From the unvaccinated individual's position, the same constraint is Snare — compulsion with no exit and no perceived benefit. From the immunocompromised population's position, it is Tangled Rope — simultaneous protection and paternalistic constraint. From the medical autonomy principle's position, it is Tangled Rope — the principle is subordinated but retains recovery capacity through informed consent doctrine. From the governance coalition's position, it is Scaffold — time-bounded emergency coordination with explicit sunset logic. From the emergency powers framework's position, it is Piton — persisting through institutional inertia, increasing theater. From the civilizational analytical position, it risks appearing as Mountain (immutable feature of social organization) — the false summit detector reveals this as naturalization of a contingent institutional choice. The perspectival gap between the authority's Rope and the individual's Snare is the constraint's core diagnostic signal: it reveals that the same mechanism is experienced as coordination by the beneficiary and extraction by the victim. This is the defining Tangled Rope property.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) flows from beneficiary/victim declarations and exit options. Unvaccinated individuals are declared as victims with trapped exit options — they face compulsory intervention with minimal alternatives (relocation, exemption narrowly construed, or legal violation). The trapped exit combined with victim status produces high d (≈0.90+), corresponding to maximum experienced extractiveness. Public health authority is declared as beneficiary with arbitrage options — they can choose enforcement timing, threshold, duration, and have multiple pathways to accomplish harm-prevention (vaccine mandates, quarantine, treatment protocols). Beneficiary status with arbitrage exit produces low d (≈0.15), corresponding to institutional gain without subjective extraction cost. Immunocompromised populations are beneficiaries with constrained exit — they benefit from herd immunity but have no choice about the mandate's design or continuation; this produces moderate d (≈0.45-0.55). The medical autonomy principle is nominally powerful/mobile but experiences extraction through subordination; powerful status with mobile exit produces d≈0.48, mid-range, reflecting that the principle retains generational recovery capacity (not destroyed, but subordinated). Each perspective's effective extractiveness chi is computed as χ = ε × f(d) × σ(S), where f(d) is the sigmoid directionality function and σ(S) is the scope modifier. At national scope (σ=1.0), trapped individuals experience chi ≈ 0.62 × f(0.92) × 1.0 ≈ 0.62 × 1.39 ≈ 0.86 (Snare range); institutional beneficiary experiences chi ≈ 0.62 × f(0.15) × 1.0 ≈ 0.62 × -0.01 ≈ -0.01 (institutional gain); moderate constrained beneficiary experiences chi ≈ 0.62 × f(0.50) × 1.0 ≈ 0.62 × 0.65 ≈ 0.40 (Tangled Rope range). The perspectival gap is large: same base extractiveness, different experienced extraction across positions.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved through kernel reading specification. The classical mandatrophy question ('Is this Tangled Rope or Snare?') is dissolved by recognizing that the answer depends on which reading of the coercion_legitimacy_boundary kernel is adopted. Under the public_health_primary reading, the constraint is legitimately Tangled Rope: genuine coordination function (herd immunity threshold) is coupled with asymmetric extraction (individual autonomy subordination), and both elements are justified by harm-prevention principle. Under the bodily_autonomy_primary reading, the same constraint would be Snare: the extraction is unjustified because autonomy is foundational, and the coordination claim is secondary. Under the proportionality_reading, the constraint is conditionally Tangled Rope: legitimate when proportionate and time-bounded, degrading toward Snare when theater rises or sunset logic is abandoned. The mandatrophy is not a classification failure — it is a kernel reading feature. The constraint correctly belongs in Tangled Rope under the public_health_primary framing; it would belong in Snare under the bodily_autonomy_primary framing. Both are analytically sound within their respective readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_threshold_ambiguity,
    'What level and type of collective harm justifies compulsory medical intervention? Is the threshold disease mortality rate, hospitalization burden, or transmission risk?',
    'Comparative analysis: threshold applied in pandemic (COVID-19) vs. endemic (influenza) vs. rare disease (polio) contexts. Empirical measurement of harm levels at which mandates were deployed vs. at which they were rescinded. Normative analysis of whether thresholds are codified or discretionary.',
    'If threshold is high and empirically grounded: constraint is legitimate Tangled Rope (genuine coordination with extraction justified by harm magnitude). If threshold is low or discretionary: constraint drifts toward Snare (extraction masked by harm-prevention framing). Classification sensitivity to threshold uncertainty is high.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(harm_threshold_ambiguity, empirical, 'Definitional ambiguity in harm thresholds that trigger mandates').

omega_variable(
    autonomy_principle_subordination,
    'This reading subordinates medical autonomy to collective harm-prevention. What defines the scope of this subordination? Does it extend to all medical interventions, only vaccines, or only infectious disease prevention?',
    'Judicial precedent analysis (Jacobson v. Massachusetts scope, contemporary interpretations). Comparative constitutional law: how different frameworks define justified medical coercion boundaries. Expert testimony on medical ethics boundaries for autonomy suspension.',
    'If scope is narrowly tailored to transmissible disease: subordination is limited, and constraint remains Tangled Rope (bounded extraction). If scope is expansive (compulsory surgery, medication, organ donation): extraction becomes severe, constraint drifts toward Snare or Pure Extraction. This reading''s coherence depends on scope limitation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_principle_subordination, conceptual, 'Scope of medical autonomy subordination under collective harm prevention').

omega_variable(
    sibling_reading_foreclosure,
    'Does this reading (public health primary) logically foreclose the bodily_autonomy_primary reading, or do both remain live positions that can coexist across different institutional frameworks?',
    'Constitutional analysis: can a single legal order hold both ''collective harm justifies medical coercion'' AND ''bodily autonomy is inviolable except in narrowest circumstances''? Jurisdictional comparison: cases where frameworks have privileged one reading over the other. Normative analysis: whether the readings foreclose or influence each other.',
    'If foreclosure: this reading''s core claim (harm-prevention justifies coercion) directly contradicts bodily_autonomy_primary''s core (autonomy is foundational). No single framework could hold both; kernel readings represent genuinely incompatible premises. If coexists_with: both remain live, and the framework difference is where they are applied (emergency contexts vs. routine medicine), not whether both can be law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether this reading forecloses the bodily_autonomy_primary sibling reading').

omega_variable(
    extraction_mechanism_legitimacy,
    'Is the extraction from unvaccinated individuals a justified cost of collective protection, or an unjustified subordination of the individual to the state apparatus?',
    'Proportionality analysis: cost to individual (medical intervention, employment restrictions, movement constraints) vs. benefit to collective (prevented deaths, hospitalization reduction). Empirical measurement of harm prevented vs. harm caused by enforcement (coercion-related medical refusal, trust erosion, downstream vaccine hesitancy). Normative comparison with other domains where state compels medical action (quarantine, public health inspection).',
    'If justified: constraint is legitimate Tangled Rope (both coordination and extraction present, extraction is justified by harm-prevention magnitude). If unjustified: constraint is Snare (extraction masked by legitimate-sounding harm-prevention rationale). Classification hinges on whether the proportionality analysis produces genuine balancing or one-directional subordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_mechanism_legitimacy, preference, 'Whether extraction from unvaccinated individuals is proportional to collective benefit').

omega_variable(
    kernel_reading_identity,
    'This constraint instantiates ONE reading of the contested coercion_legitimacy_boundary kernel. The kernel is the unstabilized claim: ''When does state coercion become legitimate?'' This reading answers: ''When collective harm-prevention outweighs individual autonomy.'' Sibling reading bodily_autonomy_primary answers: ''Rarely; individual bodily autonomy is foundational.'' Sibling proportionality_reading answers: ''Only when narrowly tailored, time-bounded, and proportionate.'' How do these readings relate structurally?',
    'Jurisprudential and constitutional analysis. The relations are declared in cs_structure.reading_relations. Axiom analysis: what foundational premise distinguishes this reading from each sibling? See cs_structure.axioms.',
    'Kernel reading classification is the foundation for understanding how this constraint relates to its siblings. Misidentifying the relation (as forecloses when it should be influences, etc.) produces incorrect predictions about institutional change and doctrine evolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Kernel identity and reading relations for public_health_primary vs. bodily_autonomy_primary and proportionality_reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_primary, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(publ_tr_t0, public_health_primary, theater_ratio, 0, 0.32).
narrative_ontology:measurement(publ_tr_t2, public_health_primary, theater_ratio, 2, 0.4).
narrative_ontology:measurement(publ_tr_t4, public_health_primary, theater_ratio, 4, 0.48).
narrative_ontology:measurement(publ_tr_t6, public_health_primary, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(publ_be_t0, public_health_primary, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(publ_be_t2, public_health_primary, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(publ_be_t4, public_health_primary, base_extractiveness, 4, 0.62).
narrative_ontology:measurement(publ_be_t6, public_health_primary, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(public_health_primary, bodily_autonomy_primary).
narrative_ontology:affects_constraint(public_health_primary, proportionality_reading).

% DUAL FORMULATION NOTE:
% The coercion_legitimacy_boundary kernel decomposes into three constraint stories representing three live readings of the same contested constitutional principle. public_health_primary (this story) prioritizes collective harm-prevention; bodily_autonomy_primary prioritizes individual medical autonomy; proportionality_reading seeks to balance both through narrow tailoring and time-bounding. Each reading has its own extractiveness, beneficiaries, victims, and perspectives. The network.affects_constraints links represent that each reading influences the others' institutional feasibility and legitimacy claims without foreclosing them. Under the public_health_primary reading, the constraint moves toward Snare as proportionality constraints are removed (wider scope, longer duration, higher barriers to exemption). Under the bodily_autonomy_primary reading, the constraint remains Snare throughout. The proportionality_reading keeps the constraint as bounded Tangled Rope by maintaining sunset logic and narrow tailoring.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
