% ============================================================================
% CONSTRAINT STORY: remedial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_remedial_reading, []).

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
 *   constraint_id: remedial_reading
 *   human_readable: Remedial Reading of Equal Protection: Race-Conscious Remediation of Historical Group Subordination
 *   domain: constitutional_law/education_policy/equal_protection
 *
 * SUMMARY:
 *   The remedial reading of equal protection represents one competing
 *   interpretation of the Fourteenth Amendment's guarantee of equal
 *   protection of the laws. This reading asserts that achieving substantive
 *   equality requires the state to recognize and remedy the effects of
 *   historical group subordination through race-conscious remedial measures.
 *   The constraint is a tangled_rope because it simultaneously coordinates
 *   group-level remedy (genuine coordination function) and extracts costs
 *   from individual members of non-preferred groups (asymmetric extraction).
 *   The reading competes within a single constitutional kernel with the
 *   colorblind reading (which denies that race-conscious measures can ever
 *   satisfy equal protection) and the diversity reading (which justifies
 *   race-consciousness for institutional diversity rather than historical
 *   remedy). This story instantiates ONE of these readings — the remedial
 *   reading — and documents its structural properties, not as the truth about
 *   equal protection, but as one coherent, defended interpretation that legal
 *   and political communities hold.
 *
 * KEY AGENTS:
 *   - Historically marginalized racial groups: Primary beneficiaries (organized/constrained) — seeking group-level remedy and recognition of systematic subordination
 *   - Individual members of non-preferred groups: Primary victims (moderate/constrained) — face exclusion or disadvantage in admissions/hiring as a cost of group remediation
 *   - Elite selective institutions: Secondary actor (powerful/arbitrage) — implement remedial race-consciousness with exit options and sunset horizon
 *   - Race-neutrality doctrine: Institutional principle (institutional/arbitrage) — formally maintained but functionally degraded to piton status
 *   - Historically marginalized groups without remedy access: Secondary victims (powerless/trapped) — bear backlash costs without accessing remedy benefits
 *   - Analytical observer: Sees full hybrid structure (analytical/analytical) — observes genuine coordination and genuine extraction simultaneously
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(remedial_reading, 0.58).
domain_priors:suppression_score(remedial_reading, 0.48).
domain_priors:theater_ratio(remedial_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(remedial_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(remedial_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(remedial_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(remedial_reading, tangled_rope).
narrative_ontology:human_readable(remedial_reading, "Remedial Reading of Equal Protection: Race-Conscious Remediation of Historical Group Subordination").
narrative_ontology:topic_domain(remedial_reading, "constitutional_law/education_policy/equal_protection").

domain_priors:requires_active_enforcement(remedial_reading).
narrative_ontology:has_sunset_clause(remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(remedial_reading, 'd29a568c-fe45-4304-bd11-5b6498e46b05').
narrative_ontology:cs_created_at('d29a568c-fe45-4304-bd11-5b6498e46b05', '').
narrative_ontology:cs_kernel_codification('d29a568c-fe45-4304-bd11-5b6498e46b05', fixed_text).
narrative_ontology:cs_authority_grounding('d29a568c-fe45-4304-bd11-5b6498e46b05', lineage).
narrative_ontology:cs_interpretation_layer_present('d29a568c-fe45-4304-bd11-5b6498e46b05').
narrative_ontology:cs_kernel_id(remedial_reading, equal_protection_clause).
narrative_ontology:cs_reading_relation('d29a568c-fe45-4304-bd11-5b6498e46b05', colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('d29a568c-fe45-4304-bd11-5b6498e46b05', diversity_reading, influences).
narrative_ontology:cs_axiom('d29a568c-fe45-4304-bd11-5b6498e46b05', foundational, group_subordination_remedial_necessity).
narrative_ontology:cs_axiom_status(group_subordination_remedial_necessity, holdable).
narrative_ontology:cs_axiom('d29a568c-fe45-4304-bd11-5b6498e46b05', foundational, race_consciousness_permissible_for_remediation).
narrative_ontology:cs_axiom_status(race_consciousness_permissible_for_remediation, holdable).
narrative_ontology:cs_reference_frame('d29a568c-fe45-4304-bd11-5b6498e46b05', remedial_equal_protection_framework).
narrative_ontology:cs_drift_state('d29a568c-fe45-4304-bd11-5b6498e46b05', contemporary_conservative_retrenchment, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(remedial_reading, historically_marginalized_racial_groups).
narrative_ontology:constraint_victim(remedial_reading, individual_members_non_preferred_groups).
narrative_ontology:constraint_victim(remedial_reading, institutional_race_neutrality_doctrine).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HISTORICALLY MARGINALIZED GROUPS (ROPE) — Organized groups pursuing remedy through litigation and policy advocacy. The constraint coordinates their collective action: recognizing race in remediation enables group-level redress rather than atomized individual claims that cannot address systemic subordination. This perspective experiences the constraint primarily as coordination — the tool that makes group-level remedy possible. Exit costs exist (dependence on judicial support, legislative political will) but organized groups retain significant agency.
constraint_indexing:constraint_classification(remedial_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: INDIVIDUAL MEMBERS OF NON-PREFERRED GROUPS (TANGLED ROPE) — Face exclusion or disadvantage in admissions, hiring, or contracting as a direct result of remedial classification. These individuals are trapped in a hybrid: they benefit from a rule-of-law constraint that ensures the remedy is formally tied to historical subordination (not arbitrary), but they bear a specific cost (individual exclusion) for group-level wrongs they did not commit. The constraint coordinates remedy and extracts from this group simultaneously. Medium power level reflects that these individuals can (and do) litigate, but litigating against a remedy framed as correcting historical group wrongs is costly and often unsuccessful.
constraint_indexing:constraint_classification(remedial_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MARGINALIZED GROUPS WITHOUT ACCESS TO REMEDY (SNARE) — Many members of historically marginalized groups remain trapped in segregated, under-resourced schools or lack access to selective institutions altogether, regardless of remedial policies. These groups experience the constraint as pure extraction: they bear the social cost of the remedy (backlash, resentment) without accessing its benefits (selective institution admission, contracting opportunity). Powerless to exit; trapped within the very subordination the remedy claims to address.
constraint_indexing:constraint_classification(remedial_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 4: ELITE INSTITUTIONS (SCAFFOLD) — Universities and selective employers use remedial race-conscious admissions as a temporary mechanism to address historical exclusion while building diverse cohorts. Institutions have arbitrage options (they can exit by claiming changed circumstances, or by substituting other remedial proxies like class-based affirmative action). The constraint is low-extraction for this agent because they retain control and can modify remedial terms. The sunset logic is explicit: institutions frame remediation as ending when demographic parity is achieved or when race-neutral alternatives suffice. Powerful agents with exit paths experience this as temporary support.
constraint_indexing:constraint_classification(remedial_reading, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: RACE-NEUTRALITY DOCTRINE AS INSTITUTIONAL ARTIFACT (PITON) — The formal legal principle of 'race-blindness' in equal protection jurisprudence persists as a doctrinal framework even as it is explicitly subordinated to remedial necessity. This doctrine is maintained institutionally through precedent, but its functional authority has degraded — it now serves primarily as the foil against which remedial necessity is justified. The doctrine is not abolished (it resurfaces in attacks on remedial race-consciousness) but it no longer governs primary institutional action. Theater ratio reflects this: the doctrine maintains formal presence but functions mainly rhetorically. Institutional actors (courts, educational systems) treat race-neutral principle as aspirational rather than operative.
constraint_indexing:constraint_classification(remedial_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/analytical position, the remedial reading instantiates a genuine constitutional hybrid: it coordinates the state's legitimate interest in remedying past systemic subordination (genuine coordination function) while simultaneously extracting a cost from individuals classified by race (asymmetric extraction tied to membership in non-preferred group). The constraint requires active enforcement (courts must distinguish remedial from invidious classification) and has explicit time-bound character (remedy dissolves when subordination is redressed). This perspectival position sees the full hybrid structure without the political commitments that lead other positions to deny either the coordination function or the extraction.
constraint_indexing:constraint_classification(remedial_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(remedial_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(remedial_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(remedial_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(remedial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(remedial_reading, TR),
    TR >= 0.70.

:- end_tests(remedial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The remedial reading mandates state action based on race classification and imposes costs on individuals in non-preferred groups. The extractiveness is not higher (e.g., 0.72) because remedial framing provides genuine coordination function — the state is solving a real coordination problem (how to remedy group-level subordination that individual-level equal-protection claims cannot address). The extractiveness reflects the cost to non-preferred individuals plus the administrative burden of implementation, constrained by the requirement that remediation be tailored to identified past harm (not arbitrary race preference). Suppression (0.48): Moderate. Non-preferred individuals face significant barriers to exit (they cannot un-classify from their racial assignment; they cannot individually remedy historical group wrongs; they face legal doctrinal barriers to challenging remedial measures). However, suppression is not higher because institutional mechanisms exist (litigation, legislative change) and because the constraint is explicitly time-bound — agents can anticipate an exit date when remediation sunsets. Barriers are real but not absolute. Theater ratio (0.35): Low-to-moderate. The remedial reading is relatively transparent about its function: state action is explicitly justified as remedying identified historical subordination, backed by factual records of past discrimination. This is not primarily performative — the constraint's legitimacy depends on credible connection to actual historical harms. Theater is not zero because remedial measures can become disconnected from actual remedial purpose (benefiting advantaged members of preferred groups while failing to reach disadvantaged members of subordinated groups), and because the measure's sunset clause is often nominal (rarely enforced).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. Historically marginalized groups see rope (pure coordination function — the tool that makes group-level redress possible). Non-preferred individuals see tangled_rope (genuine remedy coordination mixed with their individual exclusion). Marginalized groups without remedy access see snare (extraction without benefit). Elite institutions see scaffold (temporary support with clear exit). The race-neutrality doctrine sees itself as piton (degraded principle maintained by inertia). The analytical observer sees tangled_rope (genuine hybrid of coordination and extraction). The largest gap is between the organized beneficiary groups (rope) and the non-preferred individuals (tangled_rope) — both experience the same constraint but with opposite power asymmetries. This gap reveals the remedial reading's core tension: it solves a group-level coordination problem but imposes individual costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position. Historically marginalized groups classified as beneficiaries with constrained exit options yield moderate-low d (they benefit, but benefit is constrained by exit barriers — remedy dependence on institutional/political support). Individual non-preferred groups classified as victims with constrained exit yield moderate-high d (they bear costs and face barriers to exit, but retain some agency through litigation and political channels). Elite institutions with arbitrage options (can exit, can substitute other remedial proxies) yield low d despite victim classification of the race-neutrality doctrine. The powerless perspective (trapped, no arbitrage) yields maximum d. The analytical observer, positioned neutrally, yields canonical d for analytical power atom (~0.73). The directionality variation across perspectives produces variance in computed chi values despite constant ε, reflecting how different agents experience the same extractive/coordinative mechanism asymmetrically.
 *
 * MANDATROPHY ANALYSIS:
 *   REMEDIAL READING MANDATROPHY: The constraint resolves mandatrophy by explicitly embracing hybrid classification (tangled_rope) — it is neither pure coordination (purely beneficial) nor pure extraction (purely extractive). The reading acknowledges that remedial race-consciousness performs a genuine coordination function (enables group-level remedy) while simultaneously imposing individual costs (exclusion of non-preferred individuals). The mandatrophy is not 'is this coordination or extraction?' but 'given that it is both, what are the criteria for when the coordination value exceeds the extraction cost?' This is resolvable only through substantive commitment to a theory of remedial justice — the reading must specify what historical wrongs are being remedied, what suffices as remedy, and when remediation is complete. Without such specification, the constraint risks perpetual extraction disguised as coordination. The analytical observer's perspective reveals this mandatrophy most clearly: from outside the political commitments of any faction, the constraint shows both genuine remedial coordination and real individual extraction simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    remediation_completion_threshold,
    'What metric determines when historical group subordination has been adequately remedied and the race-conscious constraint should sunset?',
    'Specification of measurable criteria (demographic parity in selective institutions? Equal wealth accumulation? Equal life outcomes? Equal opportunity for social mobility?). Different criteria yield different sunset timelines and different assessments of extraction duration.',
    'If criterion is demographic parity: sunset 15-30 years (moderate extraction duration). If criterion is equal life outcomes or wealth accumulation: sunset 75+ years (severe, multi-generational extraction). If no clear criterion: perpetual extraction dynamic or risk of abandonment before actual remediation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(remediation_completion_threshold, conceptual, 'Metric and threshold for determining when remediation is complete').

omega_variable(
    individual_versus_group_remedy_asymmetry,
    'Can individual non-preferred-group members legitimately bear remedial costs for group-level historical wrongs they did not commit or benefit from?',
    'Philosophical analysis of group responsibility, causal chains of subordination, and benefit principle. This is not empirically resolvable — different frameworks (forward-looking remedial justice, backward-looking reparations, benefit-based fairness) yield incommensurable answers.',
    'If asymmetry is justified (group remediation requires some individual cost): tangled_rope classification holds; extraction is legitimate. If asymmetry is unjustified (individuals cannot be taxed for group wrongs): snare classification for non-preferred individuals; extraction is extractive regardless of remedial purpose. This omega determines whether the constraint is genuinely hybrid or disguised snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(individual_versus_group_remedy_asymmetry, preference, 'Legitimacy of individual bearing costs for group-level historical remedy').

omega_variable(
    race_essentialist_versus_social_construct_framings,
    'Does the remedial reading rely on race as an essential category (groups are naturally bounded and persist over time) or as a socially constructed classification (groups are administratively created and historically contingent)?',
    'Genealogical analysis of how remedial policy operationalizes ''race.'' If relying on essentialist frames, the constraint risks naturalizing race categories that were created through subordination. If relying on social-construct frames, the constraint risks losing coherence (if race is constructed, on what grounds do remedies target ''groups'' that are not natural kinds?).',
    'Essentialist framing: remedial reading maintains clarity but risks reifying the very categories that enabled subordination. Constructionist framing: remedial reading avoids essentialism but must specify which historical group classification to remedy and risks incoherence in identifying beneficiaries. Either choice produces tension with claims about why remediation targets groups rather than individuals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(race_essentialist_versus_social_construct_framings, conceptual, 'Whether remedial reading treats race as essential or socially constructed category').

omega_variable(
    forward_versus_backward_looking_remediation,
    'Does remedial race-consciousness aim to correct past injustice (backward-looking) or to prevent future subordination (forward-looking) or both?',
    'Doctrinal analysis of remedial purpose statements in case law and policy. The two aims are structurally different: backward-looking remedy is time-limited (past is fixed); forward-looking remedy is indefinite (future subordination risk persists indefinitely).',
    'Backward-looking only: sunset logic is clear (remedy ends when past injustice is addressed). Forward-looking only: sunset is incoherent (subordination risk never fully disappears). Mixed: ambiguous about when remedial constraint is no longer necessary. The reading''s sunset clause claim depends on this distinction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(forward_versus_backward_looking_remediation, conceptual, 'Whether remediation targets past injustice or future subordination risk or both').

omega_variable(
    kernel_reading_contest,
    'Is this reading (remedial race-consciousness) of equal protection logically foreclosed by the colorblind reading, or do they coexist as live political-constitutional positions held by different judicial and political factions?',
    'Constitutional and philosophical analysis. If colorblindness is accepted as a core constitutional principle (race cannot be considered ever), then remedial race-consciousness is foreclosed — you cannot simultaneously accept colorblindness and race-conscious remedy. If colorblindness is treated as a policy preference or historical doctrine that can be overridden by remedial necessity, then coexistence is possible.',
    'If foreclosed: only one reading can prevail in a coherent constitutional framework — the engine''s false-summit detection and omega resolution mechanisms should classify this as impossible coexistence. If coexistent: the readings represent a genuine constitutional impasse requiring political resolution, not logical resolution. This omega documents the kernel''s fundamental contestedness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Logical relationship between remedial and colorblind readings of equal protection').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(remedial_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(reme_tr_t0, remedial_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(reme_tr_t5, remedial_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(reme_tr_t10, remedial_reading, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(reme_be_t0, remedial_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(reme_be_t5, remedial_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(reme_be_t10, remedial_reading, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(remedial_reading, resource_allocation).
narrative_ontology:affects_constraint(remedial_reading, colorblind_reading).
narrative_ontology:affects_constraint(remedial_reading, diversity_reading).

% DUAL FORMULATION NOTE:
% The equal_protection_clause kernel decomposes into three distinct constraint stories, one per reading. Each reading instantiates a different ε, different beneficiary/victim structure, and different classification. The remedial_reading story treats race-conscious remediation as a tangled_rope coordinating group-level remedy and extracting from non-preferred individuals. The colorblind_reading (separate story) treats race-consciousness as a snare of arbitrary discrimination. The diversity_reading (separate story) treats race-consciousness as a rope enabling institutional diversity benefit. All three link via network.affects_constraints: they compete for interpretive authority over the same constitutional text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(remedial_reading, powerful, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
