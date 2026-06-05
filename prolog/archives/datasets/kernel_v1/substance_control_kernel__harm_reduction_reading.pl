% ============================================================================
% CONSTRAINT STORY: substance_control_kernel__harm_reduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_kernel__harm_reduction_reading, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: substance_control_kernel__harm_reduction_reading
 *   human_readable: Substance Use as Health Condition (Harm Reduction Reading)
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   The harm reduction reading of substance control positions substance use
 *   as a health condition requiring pragmatic intervention to reduce harms
 *   (overdose death, disease transmission, social marginalization)
 *   independent of use cessation. This reading emerged in the 1980s-90s as a
 *   response to HIV/AIDS and overdose mortality in the context of ineffective
 *   criminal prohibition. It decriminalizes users, repositions them as health
 *   clients, and shifts state authority from law enforcement to public health
 *   systems. The constraint exhibits the full range of indexical
 *   classifications depending on observer position: users experience
 *   decriminalization (removing snare of criminal enforcement) but
 *   re-criminalization through paternalistic health control (tangled rope);
 *   public health systems see coordination and institutional expansion
 *   (rope); marginalized communities experience harm mitigation layered on
 *   structural inequality (tangled rope); reform coalitions see a temporary
 *   transition with a sunset (scaffold); pharmaceutical industries benefit
 *   from stable patient populations requiring lifelong medication (rope);
 *   criminal justice systems experience degraded enforcement function
 *   (piton); analytical observers risk naturalizing therapeutic surveillance
 *   as inherent to medicine (false summit mountain). The constraint is ONE
 *   READING of the contested substance_control_kernel. Sibling readings
 *   include the prohibition reading (substance use is criminal behavior
 *   requiring law enforcement) and legalization reading (substance use is
 *   private behavior requiring no state intervention). The harm reduction
 *   reading coexists with both siblings in contemporary policy discourse
 *   while influencing their scope and legitimacy conditions.
 *
 * KEY AGENTS:
 *   - Substance Users: Primary victims under identity_locked exit (powerless/biographical) — benefit from decriminalization and overdose prevention but subject to paternalistic health control and identity fusion with treatment role
 *   - Public Health Systems: Primary beneficiaries (institutional/immediate) — capture expanded institutional authority, funding, and clearly defined client population; experience constraint as coordination mechanism
 *   - Marginalized Communities: Secondary victims (moderate/generational) — benefit from reduced incarceration but face surveillance, mandatory treatment, pathologization of poverty
 *   - Drug Policy Reform Coalition: Organized advocates (organized/biographical) — see constraint as temporary transition with sunset; have exit options and genuine service provision function
 *   - Pharmaceutical Industry: Institutional beneficiary (powerful/generational) — benefits from lifetime medication dependency; market is stabilized without addressing oversupply
 *   - Criminal Justice System: Degraded institutional actor (institutional/civilizational) — loses primary function to health systems; maintains performative enforcement role
 *   - Analytical Observer: Civilizational perspective (analytical/civilizational) — risks naturalizing paternalistic therapeutic control as inherent medical necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_kernel__harm_reduction_reading, 0.38).
domain_priors:suppression_score(substance_control_kernel__harm_reduction_reading, 0.48).
domain_priors:theater_ratio(substance_control_kernel__harm_reduction_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_kernel__harm_reduction_reading, "Substance Use as Health Condition (Harm Reduction Reading)").
narrative_ontology:topic_domain(substance_control_kernel__harm_reduction_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_kernel__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__harm_reduction_reading, 'harm-reduction-kernel-reading-2026').
narrative_ontology:cs_kernel_codification('harm-reduction-kernel-reading-2026', distributed).
narrative_ontology:cs_authority_grounding('harm-reduction-kernel-reading-2026', extraction).
narrative_ontology:cs_interpretation_layer_present('harm-reduction-kernel-reading-2026').
narrative_ontology:cs_reading_relation('harm-reduction-kernel-reading-2026', substance_control_kernel__prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('harm-reduction-kernel-reading-2026', substance_control_kernel__legalization_reading, influences).
narrative_ontology:cs_axiom('harm-reduction-kernel-reading-2026', foundational, substance_use_is_health_condition).
narrative_ontology:cs_axiom_status(substance_use_is_health_condition, holdable).
narrative_ontology:cs_axiom_grounding('harm-reduction-kernel-reading-2026', substance_use_is_health_condition, empirically_contingent).
narrative_ontology:cs_axiom('harm-reduction-kernel-reading-2026', foundational, health_authority_supersedes_criminal_authority).
narrative_ontology:cs_axiom_status(health_authority_supersedes_criminal_authority, holdable).
narrative_ontology:cs_axiom_grounding('harm-reduction-kernel-reading-2026', health_authority_supersedes_criminal_authority, deontological).
narrative_ontology:cs_axiom('harm-reduction-kernel-reading-2026', secondary, therapeutic_paternalism_is_acceptable_cost).
narrative_ontology:cs_axiom_status(therapeutic_paternalism_is_acceptable_cost, holdable).
narrative_ontology:cs_axiom_grounding('harm-reduction-kernel-reading-2026', therapeutic_paternalism_is_acceptable_cost, instrumental).
narrative_ontology:cs_reference_frame('harm-reduction-kernel-reading-2026', public_health_harm_mitigation).
narrative_ontology:cs_drift_state('harm-reduction-kernel-reading-2026', contemporary_pharmaceutical_market_capture, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('harm-reduction-kernel-reading-2026', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__harm_reduction_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, public_health_systems).
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, drug_policy_reformers).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, substance_users).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, marginalized_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBSTANCE USER (TANGLED ROPE) — The harm reduction reading decriminalizes users, positioning them as health clients rather than criminals. This removes the snare of criminal enforcement but replaces it with paternalistic medical supervision, surveillance via service systems, and identity fusion with 'addict' clinical category. The user benefits from overdose prevention and disease mitigation but remains subject to mandated treatment, behavioral monitoring, and therapeutic control. The binding is now cognitive (identity as 'person in recovery') rather than purely legal, making exit identity-locked rather than trapped.
constraint_indexing:constraint_classification(substance_control_kernel__harm_reduction_reading, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: PUBLIC HEALTH SYSTEM (ROPE) — The harm reduction reading shifts institutional role from law enforcement to service provision. The public health system coordinates overdose response, medication-assisted treatment (MAT), housing support, and disease prevention. This constraint appears as a coordination mechanism: the system benefits from a clearly defined client population, secured funding, and expanded institutional authority. Effective extraction is low — the system genuinely provides services and has no incentive to withhold them. The 'extraction' is the institutional expansion and resource capture, not revenue or coercive control.
constraint_indexing:constraint_classification(substance_control_kernel__harm_reduction_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: MARGINALIZED COMMUNITIES (TANGLED ROPE) — Harm reduction in resource-poor settings creates genuine benefits (overdose prevention kits, naloxone access, hepatitis screening) but layers paternalistic intervention on top of pre-existing inequality. Communities benefit from reduced incarceration but face increased social service surveillance, mandatory treatment requirements for housing/employment, and therapeutic criminalizing of poverty and trauma. The constraint coordinates harm mitigation with extraction of behavioral compliance and identity categories that pathologize structural disadvantage.
constraint_indexing:constraint_classification(substance_control_kernel__harm_reduction_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: DRUG POLICY REFORM COALITION (SCAFFOLD) — Organized harm reduction advocates (treatment providers, peer-led organizations, public health advocates) see the constraint as temporary: a transition mechanism from criminal prohibition toward post-prohibition governance. The coalition has exit options (can shift resources, mobilize constituencies, build alternative models) and sees a sunset: as harm reduction norms mature and supply-side policy shifts, the paternalistic health control layer becomes negotiable. Theater ratio is moderate because the coalition maintains genuine service delivery alongside advocacy.
constraint_indexing:constraint_classification(substance_control_kernel__harm_reduction_reading, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: PHARMACEUTICAL INDUSTRY (ROPE) — From this perspective, harm reduction is a coordination mechanism that stabilizes the opioid market without addressing oversupply. Medication-assisted treatment (methadone, buprenorphine) creates a stable patient base with lifetime treatment requirements. The industry benefits from the constraint — it locks users into pharmaceutical dependency while appearing to provide harm reduction. Extraction is present but appears as service coordination, making the classification rope rather than snare.
constraint_indexing:constraint_classification(substance_control_kernel__harm_reduction_reading, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: CRIMINAL JUSTICE SYSTEM (PITON) — The harm reduction reading degrades the criminal system's primary function (enforcement) but does not eliminate its role. Criminal enforcement remains for supply-side actors and property crime, creating a partially functional but increasingly theatrical system. The judiciary maintains nominal authority over drug offenses while public health systems handle the client population. The system persists through institutional inertia — it cannot fully exit (constitutional mandate for law enforcement) but has lost functional primacy (health systems now manage the core population). Theater ratio is high because criminal drug enforcement becomes performative: processing cases that health systems have already redirected.
constraint_indexing:constraint_classification(substance_control_kernel__harm_reduction_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LIMIT VIEW (MOUNTAIN) — From the civilizational perspective, the harm reduction reading naturalizes paternalistic health control as inevitable: substance use disorders are chronic medical conditions requiring lifelong management, just as diabetes requires lifelong treatment. This perspective frames therapeutic surveillance as inherent to medicine, not as a form of control. However, the structural data — extractiveness, suppression, and the identity-locking mechanism — suggests this is a false summit: the 'medical inevitability' framing naturalizes contingent institutional arrangements (lifelong pharmaceutical dependency, mandated treatment, behavioral monitoring) as natural law.
constraint_indexing:constraint_classification(substance_control_kernel__harm_reduction_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_kernel__harm_reduction_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(substance_control_kernel__harm_reduction_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(substance_control_kernel__harm_reduction_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(substance_control_kernel__harm_reduction_reading, TR),
    TR >= 0.70.

:- end_tests(substance_control_kernel__harm_reduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The harm reduction reading reduces extraction compared to criminal prohibition (which would be snare, ε ≥ 0.66) because users are no longer criminal victims and enforcement overhead decreases. However, extractiveness does not reach pure coordination (rope, ε ≤ 0.35) because the paternalistic health control layer — mandatory treatment, behavioral monitoring, identity categorization — creates persistent extraction from users and marginalized communities. The extractiveness trajectory from 0.28 to 0.38 reflects increasing pharmaceutical dependency locking and therapeutic scope creep as harm reduction systems mature. Suppression (0.48): Moderate. The suppression comes from mandated treatment access, service-system gatekeeping, behavioral monitoring in exchange for housing/employment, and identity-locked binding to treatment role. However, suppression is lower than criminal prohibition (would be 0.60+) because services are genuinely beneficial and users have some choice in treatment modality and timing. The trajectory is flat at 0.48 after t=5 because the suppressive architecture stabilizes once systems mature. Theater ratio (0.55): Moderate. Harm reduction has higher functionality than criminal enforcement (lower theater) but faces performative elements: compliance-driven metrics, therapeutic documentation rituals, treatment outcome reporting that doesn't capture user-defined success. The rising trajectory (0.38 to 0.55) reflects increasing administrative theater as systems professionalize and funding requires demonstrable outcomes.
 *
 * PERSPECTIVAL GAP:
 *   The harm reduction reading produces maximal perspectival divergence from different observer positions. Users see removal of criminal snare (escape from police harassment, arrest risk, carceral consequence) but replacement with health snare (mandatory treatment, behavioral control through service access). Public health systems see coordination (solving overdose epidemic, reducing disease transmission). Marginalized communities see extraction masquerading as help (pathologizing poverty, imposing therapeutic compliance). Reform coalitions see temporary transition (sunset as legalization and supply-side policy shift). Pharmaceutical interests see stable market expansion (lifetime treatment populations). Criminal justice sees functional degradation (loss of primary enforcement role). The analytical observer risks seeing immutable medical necessity (therapeutic control is inherent to treating chronic medical conditions) when the structural data reveals contingency (identity_locked binding, suppression architecture, theater ratio all point to constructed institutional arrangements rather than natural law). This perspectival gap between the analytical mountain and the user tangled_rope reveals the false summit: treating substance use as exclusively a health condition (natural law reading) naturalizes what is actually a contested policy choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's structural relationship to this specific constraint. Users are defined as victims under the harm reduction reading (decriminalization removes criminal victim status but therapeutic control replaces it) with identity_locked exit options → high d (0.80+) → high f(d) → high experienced extraction despite moderate base extractiveness. Public health systems are beneficiaries (institutional expansion, funding, client population) with arbitrage exit options → low d (0.15) → negative f(d) → negative or low experienced extraction. Marginalized communities are victims with constrained exit (can access services but with mandatory compliance strings) → high-moderate d (0.68) → moderate-high f(d). Reform coalitions are organized beneficiaries (advancing their policy agenda) with mobile exit → moderate-low d (0.35) → moderate f(d). Pharmaceutical industry is beneficiary with arbitrage exit → very low d (0.12) → negative f(d), but empirical χ is still positive because institutional coordination produces net benefit. Criminal justice is neither beneficiary nor victim but functionally displaced → moderate d (0.55) → moderate f(d), producing piton classification from institutional perspective (low functional significance). Analytical observer treating substance use as natural law creates a false summit: d would be 0.72 (observer position), but the beneficiary/victim structure contradicts mountain classification.
 *
 * MANDATROPHY ANALYSIS:
 *   THERAPEUTIC CONTROL PARADOX: The harm reduction reading resolves mandatrophy by distinguishing genuine harm mitigation (overdose prevention, disease management) from paternalistic control (mandatory treatment, behavioral monitoring, identity fusion). The mandatrophy — is this coordination or extraction? — cannot be resolved at a single observational level because different agents experience genuine benefits alongside genuine extraction. Users benefit from overdose prevention but suffer identity_locked binding. Public health systems provide real services but expand institutional authority. Communities get disease prevention but face surveillance. The reading's resolution is that harm reduction IS a tangled_rope: it coordinates genuine harm reduction (coordination function) with paternalistic therapeutic control (extraction function). The presence of both simultaneously is not a bug to be eliminated but a structural feature to be managed. The exit for mandatrophy is the scaffold perspective: as post-prohibition governance matures, the paternalistic layer becomes negotiable (sunset gate toward either legalization or higher-autonomy harm reduction models).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_locked_versus_constrained_ambiguity,
    'Is the binding mechanism in harm reduction contexts primarily cognitive (identity fusion with treatment/recovery role) or structural (economic dependency on services)?',
    'Longitudinal ethnographic study of users who exit treatment systems: persistent behavior change and identity maintenance (indicates internalized binding) versus rapid behavioral reversion when service access removed (indicates structural binding only)',
    'If primarily cognitive: exit_options remains identity_locked; classification remains tangled_rope from powerless perspective. If primarily structural: reclassify to constrained; may shift perspectives downward in extracted-ness. If mixed: current modeling is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_versus_constrained_ambiguity, empirical, 'Whether harm reduction binding is cognitive or structural').

omega_variable(
    pharmaceutical_dependency_intentionality,
    'Does medication-assisted treatment (MAT) intentionally lock users into lifetime pharmaceutical dependency, or is this an unintended side effect of evidence-based practice?',
    'Policy analysis of MAT protocol design; comparison with precedent medical conditions (insulin diabetes vs MAT opioid use) to identify differential treatment assumptions; historical analysis of pharmaceutical industry involvement in MAT protocol development',
    'If intentional: pharmaceutical perspective reclassifies as snare; extractiveness increases. If unintended: classification remains rope; extractiveness is justified harm mitigation cost. If mixed: current model accurate — rope with extraction elements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pharmaceutical_dependency_intentionality, empirical, 'Whether MAT pharmaceutical dependency is intentional design').

omega_variable(
    supply_chain_criminalization_persistent,
    'In harm reduction jurisdictions, does supply-side criminalization remain structurally inseparable from user-side decriminalization, or can supply criminalization be relaxed while maintaining harm reduction?',
    'Comparative policy analysis (Portugal vs Netherlands vs Canada vs Oregon): jurisdictions attempting varying supply-side policies while maintaining harm reduction; longitudinal tracking of interdiction, pricing, purity control, and overdose rates',
    'If inseparable: harm reduction users remain embedded in criminalized supply systems; extractiveness cannot be reduced below current level. If separable: full decriminalization pathway exists; current harm reduction is transitional (scaffold gate confirmed). If partially separable: current tangled_rope classification from user perspective is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supply_chain_criminalization_persistent, empirical, 'Whether supply-side criminalization is separable from user-side harm reduction').

omega_variable(
    therapeutic_control_scope_boundary,
    'What distinguishes legitimate health intervention in harm reduction from therapeutic overreach/paternalism in resource-poor settings?',
    'Comparative ethnography: harm reduction systems with explicit user-centered governance versus top-down clinical governance; measure of user-initiated versus system-initiated treatment modifications; tracking of therapeutic scope creep over time',
    'If boundary is clear and maintainable: current suppression (0.48) is legitimate health system overhead. If boundary is routinely crossed: suppression is artificially low; should increase to 0.60+. If boundary is context-dependent: suppression varies by implementation; current score is aggregate across heterogeneous systems.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(therapeutic_control_scope_boundary, empirical, 'Boundary between harm reduction and therapeutic overreach').

omega_variable(
    kernel_revision_competing_readings_live,
    'Within contemporary substance policy discourse, are the prohibition reading and legalization reading still live alternative kernels, or has harm reduction established hegemony over the kernel?',
    'Discourse analysis: institutional commitments to each reading (prohibition: federal DEA, international treaty enforcement; legalization: state-level decriminalization, cannabis legalization; harm reduction: public health systems, treatment provision); tracking of resource allocation and policy priority across three readings over time',
    'If all three remain live: kernel is truly contested; all three readings coexist in different jurisdictions (current modeling). If harm reduction has captured the kernel: other readings become secondary interpretations rather than equal alternatives (reclassify to coexists_with + influences rather than pure coexist). If prohibition or legalization resurges: drift_state direction and magnitude change.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_revision_competing_readings_live, conceptual, 'Whether the substance control kernel remains contested or harm reduction has established primacy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__harm_reduction_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(harm_red_theater_t0, substance_control_kernel__harm_reduction_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(harm_red_theater_t5, substance_control_kernel__harm_reduction_reading, theater_ratio, 5, 0.48).
narrative_ontology:measurement(harm_red_theater_t10, substance_control_kernel__harm_reduction_reading, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(harm_red_extractiveness_t0, substance_control_kernel__harm_reduction_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(harm_red_extractiveness_t5, substance_control_kernel__harm_reduction_reading, base_extractiveness, 5, 0.33).
narrative_ontology:measurement(harm_red_extractiveness_t10, substance_control_kernel__harm_reduction_reading, base_extractiveness, 10, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(harm_red_suppression_t0, substance_control_kernel__harm_reduction_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(harm_red_suppression_t5, substance_control_kernel__harm_reduction_reading, suppression_requirement, 5, 0.48).
narrative_ontology:measurement(harm_red_suppression_t10, substance_control_kernel__harm_reduction_reading, suppression_requirement, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__harm_reduction_reading, resource_allocation).
narrative_ontology:affects_constraint(substance_control_kernel__harm_reduction_reading, substance_control_kernel__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_kernel__harm_reduction_reading, substance_control_kernel__legalization_reading).

% DUAL FORMULATION NOTE:
% The harm reduction reading is one of three structurally distinct constraints within the substance_control_kernel. All three readings share the same kernel (state authority over substance use) but produce different extractiveness values and structural configurations. This story models the harm reduction reading exclusively. The prohibition reading (ε ≈ 0.72, snare) and legalization reading (ε ≈ 0.15, rope) are separate constraints with different beneficiary/victim structures. All three are linked via network.affects_constraints to document kernel family membership and to enable contamination analysis across reading positions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_kernel__harm_reduction_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
