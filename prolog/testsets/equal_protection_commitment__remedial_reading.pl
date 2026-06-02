% ============================================================================
% CONSTRAINT STORY: equal_protection_commitment__remedial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_commitment__remedial_reading, []).

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
 *   constraint_id: equal_protection_commitment__remedial_reading
 *   human_readable: Equal Protection Remedial Reading: Race-Conscious Dismantling of Caste Subordination
 *   domain: constitutional_law/political_philosophy/social_policy
 *
 * SUMMARY:
 *   The remedial reading of equal protection constitutes one coherent
 *   interpretation of the Fourteenth Amendment's guarantee of equal
 *   protection of the laws: the clause forbids perpetuation of caste-like
 *   systems of racial subordination and affirmatively permits race-conscious
 *   measures to dismantle that subordination. This reading is structurally
 *   distinct from the colorblind reading (which forbids all
 *   race-consciousness) and the diversity reading (which permits
 *   race-consciousness to achieve educational/institutional diversity goals).
 *   The remedial reading grounds equal protection in the constitutional
 *   commitment to dismantle the specific harm of racial caste subordination —
 *   the system by which law and custom embedded one racial group in permanent
 *   disadvantage. Race-conscious remedy is not incidental but central:
 *   subordination is group-level and inherited; it cannot be remedied by
 *   colorblind rules that treat individuals as if they enter from equal
 *   positions when the caste system has already allocated unequal starting
 *   points across generations. The constraint exhibits high extractiveness
 *   (0.52) because the beneficiary/victim structure inverts depending on
 *   observer position. From the standpoint of historically subordinated
 *   groups, remedial measures are liberation. From the standpoint of
 *   historically privileged groups denied preferential access, remedial
 *   measures are extraction. From the state's standpoint, remedial authority
 *   is granted discretion. The suppression measurement (0.45) reflects that
 *   racial caste subordination operates through both explicit legal rules and
 *   self-perpetuating cultural/economic patterns — remedial authority must
 *   overcome both. Theater ratio (0.38) reflects that the remedial reading's
 *   functional purpose is transparent: dismantle caste subordination. The
 *   constraint's classification as tangled_rope (rather than pure snare or
 *   rope) reflects that genuine coordination function (dismantling
 *   subordination) coexists with asymmetric extraction (some groups bear
 *   remedial costs while others receive remedial benefits).
 *
 * KEY AGENTS:
 *   - Historically Subordinated Racial Groups: Primary victim of caste system (powerless/trapped) — bear multi-generational inherited disadvantage; primary beneficiary of remedial measures (gains substantive equality)
 *   - Historically Privileged Racial Groups: Beneficiary of caste system's inherited advantages (moderate/constrained) — experience remedial measures as extraction when denied preferential access; constrained by remedial doctrine
 *   - State Remedial Authority: Institutional actor (institutional/arbitrage) — granted discretion to design and implement race-conscious remedial programs; benefits from expanded authority
 *   - Civil Rights Organizations: Organized agents (organized/constrained) — perceive genuine coordination function alongside extraction costs; constrained by political opposition
 *   - Colorblind Equal Protection Doctrine: The victim set — remedial reading inverts colorblind logic by treating colorblindness as perpetuation mechanism rather than solution
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing caste subordination as immutable when remedial reading may actually construct its perpetuation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_commitment__remedial_reading, 0.52).
domain_priors:suppression_score(equal_protection_commitment__remedial_reading, 0.45).
domain_priors:theater_ratio(equal_protection_commitment__remedial_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__remedial_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_commitment__remedial_reading, "Equal Protection Remedial Reading: Race-Conscious Dismantling of Caste Subordination").
narrative_ontology:topic_domain(equal_protection_commitment__remedial_reading, "constitutional_law/political_philosophy/social_policy").

domain_priors:requires_active_enforcement(equal_protection_commitment__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__remedial_reading, '00d93b95-c1e8-4493-ab5f-701bc51cc35c').
narrative_ontology:cs_kernel_codification('00d93b95-c1e8-4493-ab5f-701bc51cc35c', fixed_text).
narrative_ontology:cs_authority_grounding('00d93b95-c1e8-4493-ab5f-701bc51cc35c', lineage).
narrative_ontology:cs_interpretation_layer_present('00d93b95-c1e8-4493-ab5f-701bc51cc35c').
narrative_ontology:cs_reading_relation('00d93b95-c1e8-4493-ab5f-701bc51cc35c', equal_protection_commitment__colorblind_reading, coexists_with).
narrative_ontology:cs_reading_relation('00d93b95-c1e8-4493-ab5f-701bc51cc35c', equal_protection_commitment__diversity_reading, influences).
narrative_ontology:cs_axiom('00d93b95-c1e8-4493-ab5f-701bc51cc35c', foundational, racial_caste_subordination_is_primary_harm).
narrative_ontology:cs_axiom_status(racial_caste_subordination_is_primary_harm, holdable).
narrative_ontology:cs_axiom_grounding('00d93b95-c1e8-4493-ab5f-701bc51cc35c', racial_caste_subordination_is_primary_harm, empirically_contingent).
narrative_ontology:cs_axiom('00d93b95-c1e8-4493-ab5f-701bc51cc35c', foundational, colorblindness_perpetuates_subordination).
narrative_ontology:cs_axiom_status(colorblindness_perpetuates_subordination, holdable).
narrative_ontology:cs_axiom_grounding('00d93b95-c1e8-4493-ab5f-701bc51cc35c', colorblindness_perpetuates_subordination, empirically_contingent).
narrative_ontology:cs_axiom('00d93b95-c1e8-4493-ab5f-701bc51cc35c', secondary, race_consciousness_required_for_remedy).
narrative_ontology:cs_axiom_status(race_consciousness_required_for_remedy, holdable).
narrative_ontology:cs_axiom_grounding('00d93b95-c1e8-4493-ab5f-701bc51cc35c', race_consciousness_required_for_remedy, instrumental).
narrative_ontology:cs_reference_frame('00d93b95-c1e8-4493-ab5f-701bc51cc35c', post_reconstruction_caste_system_remediation).
narrative_ontology:cs_drift_state('00d93b95-c1e8-4493-ab5f-701bc51cc35c', contemporary_judicial_retrenchment_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('00d93b95-c1e8-4493-ab5f-701bc51cc35c', '').
narrative_ontology:cs_kernel_id(equal_protection_commitment__remedial_reading, equal_protection_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_commitment__remedial_reading, historically_subordinated_racial_groups).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__remedial_reading, state_remedial_authority).
narrative_ontology:constraint_victim(equal_protection_commitment__remedial_reading, historically_privileged_racial_groups).
narrative_ontology:constraint_victim(equal_protection_commitment__remedial_reading, colorblind_equal_protection_doctrine).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HISTORICALLY SUBORDINATED GROUPS (SNARE) — Trapped in multi-generational subordination with no exit via individual merit or neutral rules. The caste system's suppression is structural and self-reinforcing. Race-conscious remedies are perceived as the only mechanism to escape the trap — neutral rules perpetuate subordination. Maximum extraction from the constraint of colorblind doctrine; race-conscious remedy is perceived as liberation mechanism, not as extraction.
constraint_indexing:constraint_classification(equal_protection_commitment__remedial_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CIVIL RIGHTS ORGANIZATIONS (TANGLED ROPE) — Organized agents perceive genuine coordination function (dismantling caste) alongside extraction costs (litigation burden, political resistance, implementation complexity). Constrained by political opposition and judicial doctrine shifts. The remedial reading offers both mechanism for coordination and contested resource allocation.
constraint_indexing:constraint_classification(equal_protection_commitment__remedial_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE REMEDIAL AUTHORITY (ROPE) — Institutional actors implementing race-conscious remedies perceive pure coordination: allocating resources to achieve legislative remedial purpose (dismantling caste). The remedial reading grants state actors authority to design and execute remedial programs. Net beneficiary via expanded discretion — exit is via political pressure, not constraint.
constraint_indexing:constraint_classification(equal_protection_commitment__remedial_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: HISTORICALLY PRIVILEGED GROUPS (TANGLED ROPE) — Experience the remedial reading as extraction: denied preferential access, subject to scrutiny of unearned advantage. But the constraint also offers coordination benefit: explicit acknowledgment of subordination structure creates legitimacy for addressing it systemically. Constrained by legal doctrine; cannot exit without dismantling the remedial frame itself.
constraint_indexing:constraint_classification(equal_protection_commitment__remedial_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the remedial reading treats multi-generational group subordination as an immutable structural fact: caste harms are inherited, persist across generations, and cannot be remedied by colorblind rules because the subordination is not colorblind. The observer perceives the caste structure itself as the binding natural law — the remedial reading merely acknowledges this immutable fact. Engine false-summit detection will evaluate whether this naturalization is accurate or whether the caste structure is actually a contingent institutional arrangement (i.e., whether the remedial reading itself is constructing the caste naturalization).
constraint_indexing:constraint_classification(equal_protection_commitment__remedial_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: TRANSITIONAL REMEDIAL PROGRAMS (SCAFFOLD) — Race-conscious remedies are understood within the remedial reading as temporary: once substantive equality is achieved and caste subordination is dismantled, the basis for race-conscious remediation disappears. Sunset clause is built into the remedial logic itself — remedies sunset when the condition (subordination) is cured. Theater ratio is lower here because the functional purpose (dismantling caste) is transparent.
constraint_indexing:constraint_classification(equal_protection_commitment__remedial_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_commitment__remedial_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(equal_protection_commitment__remedial_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(equal_protection_commitment__remedial_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_commitment__remedial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equal_protection_commitment__remedial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): High-moderate. The remedial reading creates asymmetric benefits and costs. Historically subordinated groups receive remedial access (benefits); historically privileged groups denied preferential access (costs). State remedial authority gains discretionary power (benefits). The constraint is extraction from the standpoint of privileged groups and liberation from the standpoint of subordinated groups. The 0.52 value reflects that the extraction is real (privileged groups do bear costs) but bounded (costs are limited to preferential access, not subsistence or citizenship). Suppression (0.45): Moderate. Caste subordination operates through both explicit rules and self-reinforcing patterns. Remedial authority must overcome cultural stigma, economic feedback loops, and political opposition. But suppression is not total — the remedial reading explicitly acknowledges and names the subordination structure, which is itself a reduction in suppression compared to colorblind doctrine that denies the structure. Theater ratio (0.38): Low-moderate. The remedial reading's functional purpose is transparent: dismantle caste subordination. Implementation may include performative elements (symbolic remedies, visibility theater), but the core logic is explicit and tied to a named harm. The rising trajectory reflects increasing judicial scrutiny and political contestation over remedial programs, which increases the performative content over time.
 *
 * PERSPECTIVAL GAP:
 *   The maximum gap is between the historical-subordinated-group perspective (snare: colorblind doctrine is the extraction trap; remedial reading is liberation) and the analytical-natural-law perspective (mountain: caste subordination is immutable structure). If the analytical observer is correct and subordination is inherent/unchangeable, then remedial action is merely performance theater (piton-like). If the subordinated-group observer is correct and subordination is remediable, then the remedial reading is genuine tangled_rope. The perspectival gap reveals the kernel ambiguity: equal protection is underspecified on whether group subordination is inherent (favoring remedial reading as acknowledgment of natural law) or contingent (favoring remedial reading as active remedy for constructed structure).
 *
 * DIRECTIONALITY LOGIC:
 *   The remedial reading inverts standard beneficiary/victim analysis depending on which harm is the reference point. If the reference point is individual fairness (colorblind principle), then remedial measures extract from historically privileged individuals by denying them race-neutral preference. If the reference point is group subordination (the remedial premise), then colorblind doctrine perpetuates extraction by refusing to remediate inherited disadvantage. Directionality derives from this inversion: d values shift depending on whether the observer locates the primary harm in individual race-consciousness or in group-level caste perpetuation. The engine computes d from beneficiary/victim declarations: historically subordinated groups are beneficiaries of remedial measures (low d → low chi); historically privileged groups denied preferential access are victims of remedial measures (high d → high chi); the colorblind doctrine itself enters the victim set under the remedial reading because colorblindness is diagnosed as perpetuation mechanism. This is not a contradiction but a perspectival feature: the same structural element (colorblind equal protection doctrine) is beneficiary from the colorblind reading's perspective (protects neutral principle) and victim from the remedial reading's perspective (perpetuates subordination).
 *
 * MANDATROPHY ANALYSIS:
 *   The remedial reading resolves mandatrophy by showing that the two primary alternative readings (colorblind and remedial) measure DIFFERENT primary harms and therefore legitimately classify the constraint differently. Colorblind reading treats race-consciousness itself as the harm and perceives equal protection as requiring colorblindness (rope/mountain). Remedial reading treats caste subordination as the harm and perceives equal protection as requiring race-conscious remedy (tangled_rope). Both are internally coherent — the constraint is not unstable, it is ambiguously specified. The mandatrophy is dissolved by recognizing that the kernel (equal protection clause) genuinely permits both readings, and the political question is which harm is primary. The analytical observer's false-summit risk (perceiving subordination as immutable law) is the real mandatrophy problem: it naturalizes what the remedial reading treats as remediable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    caste_subordination_inherence,
    'Is multi-generational racial subordination (caste) an inherent structural feature of American society requiring ongoing race-conscious remedy, or a contingent institutional arrangement that can be dismantled via sustained remedial effort?',
    'Longitudinal measurement: does substantive equality (income, wealth, health, educational attainment, incarceration, political representation parity) approach convergence as remedial policies mature? Historical comparison: did societies with comparable historical subordination structures dismantle caste via colorblind rules or require race-conscious remedy?',
    'If inherent: mountain classification is accurate; remedial reading is acknowledgment of natural law. If contingent: mountain is false summit; remedial reading constructs the perpetuation it claims to acknowledge. Classification shifts from mountain (immutable) to tangled_rope (remediable via active enforcement).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(caste_subordination_inherence, empirical, 'Whether multi-generational racial subordination is inherent or contingent').

omega_variable(
    colorblind_doctrine_incompleteness,
    'Can neutral rules, applied equally regardless of race, dismantle inherited subordination without perpetuating it, or does colorblindness systematically freeze subordination into place by refusing to acknowledge or remediate group-level harm?',
    'Counterfactual analysis: comparison of subordination reduction under colorblind regimes vs remedial-reading regimes controlling for time interval and remedial intensity. Mechanism analysis: does colorblind doctrine fail to identify feedback loops that perpetuate subordination (segregation → resource inequality → test-score gaps → meritocratic exclusion → segregation)?',
    'If colorblindness is sufficient: remedial reading''s core premise (colorblindness perpetuates subordination) fails; colorblind reading becomes the natural law. If colorblindness is insufficient: remedial reading''s premise holds; remedial doctrine is required mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colorblind_doctrine_incompleteness, empirical, 'Whether colorblind rules can dismantle inherited subordination').

omega_variable(
    remedial_extraction_boundary,
    'Where is the line between legitimate remedial extraction (denying unearned advantage to historically privileged groups) and unjust extraction (imposing costs on individuals for inherited privilege they did not personally choose or enforce)?',
    'Jurisprudential analysis: do courts distinguish remedial from punitive when evaluating race-conscious programs? Empirical analysis: what is the causal chain from individual beneficiary to group advantage to current individual disadvantage? Can that chain be severed without race-conscious remedy?',
    'If extraction boundary is clear and localized: remedial reading is coherent tangled_rope (bounded extraction for bounded harm). If boundary is ambiguous or diffuse: remedial reading becomes snare (extraction justified as remedy). If extraction is not justified: remedial reading reverts to colorblind reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedial_extraction_boundary, conceptual, 'Boundary between legitimate remedial extraction and unjust imposition').

omega_variable(
    remedial_authority_capture,
    'Can state actors implementing race-conscious remedies be trusted to target remediation toward actual caste subordination, or will remedial authority be captured by bureaucratic interests and converted into pure extraction masked as remedy?',
    'Institutional analysis: what checks constrain remedial authority? Do implementation patterns match legislative intent? Are remedial programs evaluated against their stated goal (dismantling subordination) or converted to proxy measures (numeric targets divorced from substantive equality)?',
    'If authority is reliably constrained: remedial reading is tangled_rope. If authority is routinely captured: remedial reading becomes snare (extraction via bureaucratic discretion). Classification shifts based on empirical institutional strength, not doctrine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedial_authority_capture, empirical, 'Whether remedial authority remains accountable to stated remedial purpose').

omega_variable(
    kernel_reading_ambiguity,
    'The equal protection clause permits both the colorblind reading (no race-conscious action) and the remedial reading (race-conscious remedy for caste subordination). Is this dual legitimacy a feature (both readings remain live options) or a flaw (the kernel is insufficiently specified)?',
    'Hermeneutic analysis: does the constitutional text, original understanding, or precedent favor one reading over the other? Or does the kernel deliberately preserve ambiguity to allow democratic processes to choose? Historical analysis: did framers intend colorblindness or remedial authority or both?',
    'If feature: coexists_with relation to colorblind reading is correct. If flaw: one reading forecloses the other, and the constraint is not a coexistence but a contest. If deliberate ambiguity: the kernel itself exhibits drift (interpretation varies as political composition shifts).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether equal protection kernel permits or requires dual readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__remedial_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eqprot_rem_tr_t0, equal_protection_commitment__remedial_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(eqprot_rem_tr_t10, equal_protection_commitment__remedial_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(eqprot_rem_tr_t20, equal_protection_commitment__remedial_reading, theater_ratio, 20, 0.38).

% Extraction over time
narrative_ontology:measurement(eqprot_rem_be_t0, equal_protection_commitment__remedial_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(eqprot_rem_be_t10, equal_protection_commitment__remedial_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(eqprot_rem_be_t20, equal_protection_commitment__remedial_reading, base_extractiveness, 20, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(eqprot_rem_su_t0, equal_protection_commitment__remedial_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(eqprot_rem_su_t10, equal_protection_commitment__remedial_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(eqprot_rem_su_t20, equal_protection_commitment__remedial_reading, suppression_requirement, 20, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_commitment__remedial_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_commitment__remedial_reading, equal_protection_commitment__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_commitment__remedial_reading, equal_protection_commitment__diversity_reading).

% DUAL FORMULATION NOTE:
% The equal protection commitment decomposes into three structurally distinct constraint stories corresponding to three live readings of the Fourteenth Amendment's equal protection clause. Each reading has different ε (remedial ε=0.52, colorblind ε≈0.25-0.35 depending on measurement of perpetuation cost, diversity ε≈0.30-0.40). The readings are not alternative measurements of one constraint — they are genuinely different constraints grounded in different primary harms (caste subordination vs. individual race-consciousness vs. institutional diversity). They coexist in contemporary constitutional law because the kernel is ambiguous. All three stories must be written; the network links them as members of the equal_protection_commitment family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
