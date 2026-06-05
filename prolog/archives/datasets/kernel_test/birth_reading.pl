% ============================================================================
% CONSTRAINT STORY: birth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_birth_reading, []).

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
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: birth_reading
 *   human_readable: Moral Personhood Begins at Birth (Birth-Reading Constraint)
 *   domain: moral_philosophy/bioethics/constitutional_law
 *
 * SUMMARY:
 *   The birth-reading is ONE interpretation of the personhood-boundary kernel
 *   — a foundational and contested commitment that different traditions,
 *   legal systems, and moral frameworks read differently. This reading
 *   instantiates the claim that moral personhood begins at birth (or at
 *   delivery/emergence from the pregnant person's body), that the fetus does
 *   not hold independent moral or legal rights during gestation, and that the
 *   pregnant person retains sole rights-holding status and authority over
 *   reproductive decisions throughout pregnancy. Under this reading, abortion
 *   at any stage of pregnancy is permissible as an exercise of the pregnant
 *   person's bodily autonomy and reproductive self-determination. The reading
 *   is not a claim about fetal biology (neurodevelopment, sentience,
 *   viability) but about the grounds of moral status — and it asserts that
 *   whatever morally relevant properties the fetus has, they do not generate
 *   independent claims on the pregnant person's body. The birth-reading has
 *   low extractiveness (0.12) because it is primarily a coordination
 *   mechanism, not an extraction mechanism. It aligns medical, legal, and
 *   moral authority around the pregnant person's recognized personhood and
 *   autonomy, with minimal coercive overhead. However, the reading admits
 *   perspectival variation: for agents whose identity is fused with pro-life
 *   conviction (perspective 3), the framework creates a piton-like constraint
 *   — legally permissive but performatively burdensome and
 *   identity-alienating. For economically dependent pregnant persons subject
 *   to relational coercion (perspective 4), the formal legal permissiveness
 *   masks material snare-like suppression. The reading forecloses the
 *   conception-reading (personhood at conception) within any single legal
 *   framework, but coexists with the viability-reading to the extent that
 *   both can acknowledge fetal interests while differing on whether those
 *   interests are rights-bearing before birth.
 *
 * KEY AGENTS:
 *   - Pregnant Person: Primary beneficiary (moderate/mobile) — recognized as sole rights-holder with bodily autonomy throughout gestation; benefits from legal/medical coordination around their authority
 *   - State Legal Authority: Institutional coordinator (institutional/arbitrage) — establishes personhood boundary and aligns medical/legal systems; low extraction from the coordination function itself
 *   - Pro-Life Moral Adherent: Secondary agent with identity lock (moderate/identity_locked) — permitted private conscience but institutionally unsupported; experiences piton-like constraint (performative adherence without legitimacy)
 *   - Economically Dependent Pregnant Person in Coercive Context: Victim (powerless/trapped) — formal legal rights voided by material conditions; experiences snare-like suppression despite legal permissiveness
 *   - Medical/Legal Institutional Apparatus: Enforcer (institutional/arbitrage) — operationalizes the birth-reading through medical protocols, legal standards, and access procedures; benefits from clear demarcation
 *   - Analytical Observer: Civilian-scope perspective (analytical/analytical) — sees the birth-reading as a coherent axiom-set within a commitment system; notes the foreclosure relation with conception-reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(birth_reading, 0.12).
domain_priors:suppression_score(birth_reading, 0.18).
domain_priors:theater_ratio(birth_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(birth_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(birth_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(birth_reading, theater_ratio, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(birth_reading, rope).
narrative_ontology:human_readable(birth_reading, "Moral Personhood Begins at Birth (Birth-Reading Constraint)").
narrative_ontology:topic_domain(birth_reading, "moral_philosophy/bioethics/constitutional_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(birth_reading, '7af3f0ae-55b0-4810-aec9-3bb3edc4fa4d').
narrative_ontology:cs_created_at('7af3f0ae-55b0-4810-aec9-3bb3edc4fa4d', '').
narrative_ontology:cs_kernel_codification('7af3f0ae-55b0-4810-aec9-3bb3edc4fa4d', fixed_text).
narrative_ontology:cs_authority_grounding('7af3f0ae-55b0-4810-aec9-3bb3edc4fa4d', lineage).
narrative_ontology:cs_interpretation_layer_present('7af3f0ae-55b0-4810-aec9-3bb3edc4fa4d').
narrative_ontology:cs_kernel_id(birth_reading, personhood_boundary).
narrative_ontology:cs_reading_relation('7af3f0ae-55b0-4810-aec9-3bb3edc4fa4d', conception_reading, forecloses).
narrative_ontology:cs_reading_relation('7af3f0ae-55b0-4810-aec9-3bb3edc4fa4d', viability_reading, coexists_with).
narrative_ontology:cs_axiom('7af3f0ae-55b0-4810-aec9-3bb3edc4fa4d', foundational, bodily_autonomy_foundational).
narrative_ontology:cs_axiom_status(bodily_autonomy_foundational, holdable).
narrative_ontology:cs_axiom_grounding('7af3f0ae-55b0-4810-aec9-3bb3edc4fa4d', bodily_autonomy_foundational, deontological).
narrative_ontology:cs_axiom('7af3f0ae-55b0-4810-aec9-3bb3edc4fa4d', foundational, personhood_begins_at_birth).
narrative_ontology:cs_axiom_status(personhood_begins_at_birth, holdable).
narrative_ontology:cs_axiom_grounding('7af3f0ae-55b0-4810-aec9-3bb3edc4fa4d', personhood_begins_at_birth, deontological).
narrative_ontology:cs_reference_frame('7af3f0ae-55b0-4810-aec9-3bb3edc4fa4d', pregnant_person_as_sole_rights_bearer).
narrative_ontology:cs_drift_state('7af3f0ae-55b0-4810-aec9-3bb3edc4fa4d', contemporary_post_roe_era, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(birth_reading, pregnant_person).
narrative_ontology:constraint_beneficiary(birth_reading, individual_conscience).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PREGNANT PERSON (ROPE) — Under the birth-reading framework, the pregnant person retains sole rights-holding status and bodily autonomy throughout gestation. The constraint enables coordination between the pregnant person's reproductive autonomy and medical-legal recognition without extractive asymmetry. Low suppression; the pregnant person can exercise conscience and choice across the full abortion window. Genuine coordination function: medical systems, legal frameworks, and conscience align around the pregnant person's authority.
constraint_indexing:constraint_classification(birth_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 2: STATE LEGAL AUTHORITY (ROPE) — States adopting the birth-reading framework coordinate the legal status of personhood with the pregnant person's rights-holding. Low extraction: the state's interest in potential life is recognized but subordinated to the pregnant person's established personhood and bodily autonomy. The coordination is genuine (medical/legal alignment) and coercive overhead is minimal (enforcement is low-suppression default).
constraint_indexing:constraint_classification(birth_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: PRO-LIFE ADHERENT WITH BIRTH-READING CONSCIENCE (PITON) — Agent whose religious or moral identity is fused with the conviction that the fetus is a person from conception. Under the birth-reading framework, this identity is permitted privately (low suppression; no legal coercion) but institutionally unsupported and politically marginalized. Theater_ratio: high (performative adherence to birth-reading norms in public while maintaining private conviction; the constraint's accommodation creates a performance/belief gap). Piton classification: the constraint maintains itself through institutional habit and procedural inertia, not because it satisfies the agent's actual moral conviction. The agent is identity-locked (exit would require abandoning foundational moral identity); the constraint provides legal permission but not legitimacy within the agent's frame.
constraint_indexing:constraint_classification(birth_reading, piton,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 4: ECONOMICALLY DEPENDENT PREGNANT PERSON IN COERCIVE FAMILY CONTEXT (SNARE) — Agent whose formal legal rights under the birth-reading framework are voided by economic dependency, family coercion, or medical gatekeeping. The birth-reading's permissive legal stance offers no material support for exit from coercive reproductive control. Suppression: high (economic, relational, medical barriers override formal legal autonomy). Theater_ratio: low (the constraint's performative element is minimal when suppression is total — coercion operates directly). Effective extraction: moderate-to-high (the agent's reproductive autonomy is extracted regardless of the birth-reading's legal permissiveness, because material conditions foreclose choice).
constraint_indexing:constraint_classification(birth_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (ROPE FROM COMMITMENT-SYSTEM VIEW) — The birth-reading is a coherent axiom-set within a commitment system grounding personhood in birth/sentience/relational engagement. The system's coordination function is genuine: it aligns legal personhood with biological/relational markers and centers pregnant person autonomy. The extraction mechanism is minimal (low chi from this vantage). However, the analytical observer notes that this reading forecloses the sibling conception-reading within any single legal framework — they cannot coexist in the same jurisdiction's personhood definition. This is a real logical constraint, not empirical ambiguity.
constraint_indexing:constraint_classification(birth_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(birth_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(birth_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(birth_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(birth_reading, TR),
    TR >= 0.70.

:- end_tests(birth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. The birth-reading's primary function is coordination, not extraction. It aligns legal personhood, medical authority, and pregnant-person autonomy around a coherent framework. The minimal extractiveness reflects that the constraint benefits the pregnant person (beneficiary status) and institutional actors (who gain clear demarcation), while imposing low suppression on those who comply. The slight positive value (not zero) accounts for the internalized performance burden and institutional inertia in healthcare and legal systems. Suppression (0.18): Low. The birth-reading permits individual conscience to adopt stricter standards privately without legal enforcement. The suppression value reflects institutional barriers to certain practices (e.g., restrictions on medication abortion) rather than the reading itself — which is minimally coercive. Theater ratio (0.25): Low-moderate. The reading's enforcement is primarily through legal/medical coordination and documentation, not through performative ritual. The slight theater reflects institutional procedures and bureaucratic handling that are somewhat decoupled from the reading's actual function.
 *
 * PERSPECTIVAL GAP:
 *   The birth-reading exhibits a significant perspectival gap between institutional and identity-locked perspectives. Institutional actors (state, medical system, legal framework) experience the reading as a coordination mechanism that enables clear operational authority and unified procedures — they see Rope. Identity-locked agents (pro-life adherents) experience the same legal framework as a constraint on their moral conviction — they see Piton (permissive but performatively burdensome). Economically dependent pregnant persons experience the reading as an abstract guarantee that masks material coercion — they see Snare. The analytical observer sees all three perspectives as structurally coherent yet mutually incommensurable, indicating that the reading's actual function varies sharply by agent position. This gap reveals that the reading's 'coordination' is really coordination among institutional actors and mobile agents, not universally coordinate-able.
 *
 * DIRECTIONALITY LOGIC:
 *   The birth-reading's beneficiaries are the pregnant person and institutional actors (medical/legal systems) who gain clear operational demarcation. Victims are primarily identity-locked agents whose moral conviction conflicts with the legal framework, and economically dependent pregnant persons whose formal rights are masked by material suppression. The analytical directionality (perspective 5) is neutral — the observer sees the reading as coherent without adopting it. Directionality derivation for each perspective: (1) Pregnant person (moderate/mobile): beneficiary status + mobile exit = low d → low chi. (2) Institutional (institutional/arbitrage): beneficiary status + arbitrage = very low d → negative chi (coordinates their interests). (3) Identity-locked (moderate/identity_locked): victim status + identity lock = elevated d → moderate chi (constrained by conviction, not material barriers). (4) Trapped economically dependent (powerless/trapped): victim status + trapped exit = very high d → high chi (snare structure overrides legal permissiveness).
 *
 * MANDATROPHY ANALYSIS:
 *   The birth-reading avoids mandatrophy by defining extraction minimally and coordination genuinely. It acknowledges that the fetus has morally relevant properties (potential, developing capacities, genetic uniqueness) WITHOUT granting the fetus rights-bearing status or claims on the pregnant person's body. This is analytically clean: the reading does not deny fetal interests; it subordinates those interests to the pregnant person's recognized personhood and bodily autonomy. The fetus is not denied moral consideration — it is denied independent moral status. This distinction permits the reading to coexist with viability-reading (which can also subordinate fetal interests while recognizing increasing moral weight), but it genuinely forecloses the conception-reading (which asserts independent fetal moral status from conception). The mandatrophy is resolved through clear axiom-set definition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    birth_as_demarcation_justification,
    'What justifies birth (delivery/emergence from the body) as the demarcation point for personhood, rather than earlier markers (sentience, viability, conception) or later ones (consciousness, relationality)?',
    'Conceptual analysis of what grounds moral status: if grounded in sentience, viability is more defensible; if grounded in genetic uniqueness, conception is more defensible; if grounded in social recognition, relationality might be more defensible. Identify which normative premise the birth-reading''s justification rests on, and whether competing premises are genuinely incommensurable or merely empirically uncertain.',
    'If birth is justified by sentience alone: viability-reading becomes more defensible (sentience emerges earlier). If birth is justified by bodily autonomy + separation: the reading forecloses fetal-personhood readings completely (as it claims). If birth is justified by social convention: the reading is contingent, not foundational (shifts from axiom to empirical fact).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(birth_as_demarcation_justification, conceptual, 'Justification for birth as the demarcation of personhood').

omega_variable(
    fetal_interests_recognition_gap,
    'Does the birth-reading''s framework eliminate genuine fetal interests (e.g., avoidance of pain, continuation of biological processes) or merely subordinate those interests to the pregnant person''s rights?',
    'Distinguish between: (a) denying that fetus has experienceable interests at all (strong reading); (b) acknowledging fetal interests but denying they are moral claims on the pregnant person (moderate reading). Examine how the reading handles third-trimester abortion scenarios and late-term fetal pain.',
    'If strong reading: the birth-reading forecloses all fetal-interest-respecting readings. If moderate reading: the birth-reading coexists with readings that respect fetal interests while centering pregnant-person autonomy (influence relation, not foreclosure). This affects how the reading relates to the viability-reading sibling.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fetal_interests_recognition_gap, conceptual, 'Whether birth-reading acknowledges or denies fetal interests as morally relevant').

omega_variable(
    private_conscience_vs_public_coherence,
    'Can adherents of birth-reading coherently maintain private moral beliefs that diverge from the public legal framework (e.g., believing abortion is wrong while affirming legal personhood begins at birth)?',
    'Examine how the birth-reading framework treats conscience accommodation: does it permit private ethical commitments to stricter standards without requiring legal enforcement? Does institutional coherence require uniform commitment or permit pluralism?',
    'If coherence requires public-private alignment: theater_ratio and identity_locked classification in perspective 3 are unsustainable long-term. If pluralism is permitted: the piton perspective (con-science-locked within permissive framework) is structurally stable but performatively burdensome. This affects the reading''s long-term institutional trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(private_conscience_vs_public_coherence, conceptual, 'Coherence of private moral belief with public legal framework').

omega_variable(
    materiality_gap_personhood_privilege,
    'Does the birth-reading''s formal legal equality (pregnant person as sole rights-holder) create or mask class-stratified realities in which economically privileged pregnant persons exercise autonomy while economically dependent ones do not?',
    'Empirical tracking of abortion access and rates by economic class, family structure, and relational coercion context. Does the birth-reading''s permissive legal stance correlate with greater autonomy for all classes, or does it conceal class-stratified suppression?',
    'If access is class-stratified: the birth-reading has a hidden snare layer (perspective 4) that is not visible in the legal text. The constraint''s actual extractiveness varies by social position. If access is equalized: the reading''s rope classification holds across positions. This affects whether the reading''s purported coordination function is real or ideological.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(materiality_gap_personhood_privilege, empirical, 'Class-stratified realities masked by formal legal equality').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(birth_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(birt_tr_t0, birth_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(birt_tr_t20, birth_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(birt_tr_t40, birth_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(birt_be_t0, birth_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(birt_be_t20, birth_reading, base_extractiveness, 20, 0.12).
narrative_ontology:measurement(birt_be_t40, birth_reading, base_extractiveness, 40, 0.13).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(birth_reading, identity_coordination).
narrative_ontology:affects_constraint(birth_reading, conception_reading).
narrative_ontology:affects_constraint(birth_reading, viability_reading).

% DUAL FORMULATION NOTE:
% The birth-reading, conception-reading, and viability-reading form a constraint family over the personhood-boundary kernel. Each reading has its own ε value, axiom-set, and perspectival structure. They are not alternative measurements of one constraint — they are structurally distinct constraints grounded in different normative premises. The birth-reading has ε=0.12 (coordination-dominated); the conception-reading has higher ε reflecting asymmetric extraction against pregnant persons; the viability-reading occupies intermediate ε. All three link via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
