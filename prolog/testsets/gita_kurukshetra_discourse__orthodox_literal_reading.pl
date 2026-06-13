% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra_discourse__orthodox_literal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-13
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gita_kurukshetra_discourse__orthodox_literal_reading, []).

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
 *   constraint_id: gita_kurukshetra_discourse__orthodox_literal_reading
 *   human_readable: Gita Kurukshetra Discourse (Orthodox Literal Reading): Caste Duty and Righteous War
 *   domain: religious/textual/ethical
 *
 * SUMMARY:
 *   The Bhagavad Gita, composed between the 5th and 2nd centuries BCE, frames
 *   Krishna's discourse to the warrior Arjuna on the Kurukshetra battlefield
 *   as the solution to a dharmic crisis. The orthodox literal reading
 *   interprets this text as a cosmic mandate: caste hierarchy is divinely
 *   ordained (varna svadharma), a warrior's duty to fight is absolute and
 *   karmically blameless, and the Brahmin priestly class retains monopoly on
 *   authentic interpretation. This reading has served as textual foundation
 *   for caste legitimation, justification of warrior violence, suppression of
 *   heterodox religions, and subordination of women for over two millennia.
 *   The constraint emerges through a specific hermeneutical choice: treating
 *   the text as timeless revelation rather than historical composition, and
 *   reading caste assignment as cosmic rather than constructed. This is one
 *   reading of a contested kernel; the Gita itself is ambiguous about whether
 *   Krishna mandates caste hierarchy or transcends it through devotion. The
 *   orthodox literal reading forecloses the universalist devotional reading
 *   by anchoring authority in duty-based hierarchy; it coexists with the
 *   Gandhian allegorical reading (held by different communities) but
 *   suppresses it institutionally.
 *
 * KEY AGENTS:
 *   - Brahmin priestly class: agenda-setter, interprets the text, maintains ritual monopoly, benefits from caste hierarchy legitimation
 *   - Kshatriya warrior class: primary beneficiary, justified in violence by dharmic duty, exempt from karma of killing
 *   - Shudra servile class: primary victims, locked in hierarchy as cosmic obligation, denied interpretive voice
 *   - Women under patriarchal duty: secondary victims, excluded from direct textual authority, bound by derived duties
 *   - Heterodox practitioners (Buddhist, Jain, Shaivite): excluded, suppressed as adharmic, competing ethical frameworks
 *   - Gandhian and universalist interpreters: excluded from authority structure, their readings institutionally marginalized
 *   - Modern scholars: observers, can historicize the text and reveal its contingency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__orthodox_literal_reading, 0.81).
domain_priors:suppression_score(gita_kurukshetra_discourse__orthodox_literal_reading, 0.87).
domain_priors:theater_ratio(gita_kurukshetra_discourse__orthodox_literal_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 0.87).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__orthodox_literal_reading, tangled_rope).
narrative_ontology:human_readable(gita_kurukshetra_discourse__orthodox_literal_reading, "Gita Kurukshetra Discourse (Orthodox Literal Reading): Caste Duty and Righteous War").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__orthodox_literal_reading, "religious/textual/ethical").

domain_priors:requires_active_enforcement(gita_kurukshetra_discourse__orthodox_literal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__orthodox_literal_reading, 'b7963a37-bd0d-4260-b603-576cd21d6a95').
narrative_ontology:cs_kernel_codification('b7963a37-bd0d-4260-b603-576cd21d6a95', fixed_text).
narrative_ontology:cs_authority_grounding('b7963a37-bd0d-4260-b603-576cd21d6a95', extraction).
narrative_ontology:cs_interpretation_layer_present('b7963a37-bd0d-4260-b603-576cd21d6a95').
narrative_ontology:cs_reading_relation('b7963a37-bd0d-4260-b603-576cd21d6a95', gita_kurukshetra_discourse__gandhian_allegorical_reading, coexists_with).
narrative_ontology:cs_reading_relation('b7963a37-bd0d-4260-b603-576cd21d6a95', gita_kurukshetra_discourse__universalist_devotional_reading, forecloses).
narrative_ontology:cs_axiom('b7963a37-bd0d-4260-b603-576cd21d6a95', foundational, caste_hierarchy_divinely_mandated).
narrative_ontology:cs_axiom_status(caste_hierarchy_divinely_mandated, holdable).
narrative_ontology:cs_axiom_grounding('b7963a37-bd0d-4260-b603-576cd21d6a95', caste_hierarchy_divinely_mandated, deontological).
narrative_ontology:cs_axiom('b7963a37-bd0d-4260-b603-576cd21d6a95', foundational, dharma_duty_absolute_over_kinship).
narrative_ontology:cs_axiom_status(dharma_duty_absolute_over_kinship, holdable).
narrative_ontology:cs_axiom_grounding('b7963a37-bd0d-4260-b603-576cd21d6a95', dharma_duty_absolute_over_kinship, deontological).
narrative_ontology:cs_axiom('b7963a37-bd0d-4260-b603-576cd21d6a95', secondary, brahmin_interpretive_monopoly_legitimate).
narrative_ontology:cs_axiom_status(brahmin_interpretive_monopoly_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('b7963a37-bd0d-4260-b603-576cd21d6a95', brahmin_interpretive_monopoly_legitimate, conventional).
narrative_ontology:cs_reference_frame('b7963a37-bd0d-4260-b603-576cd21d6a95', vedic_cosmic_order).
narrative_ontology:cs_drift_state('b7963a37-bd0d-4260-b603-576cd21d6a95', contemporary_post_independence_india, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b7963a37-bd0d-4260-b603-576cd21d6a95', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__orthodox_literal_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, brahmin_priestly_class).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, kshatriya_warrior_class).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, vaishya_merchant_class).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, vedic_order_doctrine).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, shudra_servile_class).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, women_under_patriarchal_duty).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, heterodox_religious_practitioners).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, war_casualties_labeled_righteous).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__orthodox_literal_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(gita_kurukshetra_discourse__orthodox_literal_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gita_kurukshetra_discourse__orthodox_literal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gita_kurukshetra_discourse__orthodox_literal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gita_kurukshetra_discourse__orthodox_literal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.81) because the constraint transfers labor, obedience, and moral accountability upward through the hierarchy in exchange for assignment to a fixed role that cannot be changed through effort or choice. The extraction grows over the interval (0.72 to 0.81) as institutional elaboration deepens—the brahminical tradition developed increasingly intricate commentaries that locked the literal reading into place and made alternative readings appear heterodox. Suppression is highest (0.87) because the constraint's persistence depends on active enforcement: Brahmin gatekeeping prevents lower castes from reading the text, military suppression of Buddhist and Jain communities is framed as righteous war, women are denied education and voice, and heterodox interpretations are labeled as spiritual corruption. Theater grows from 0.25 to 0.42 as the constraint's institutional machinery became more elaborate: increasingly, the 'authentic meaning' of the text required priestly mediation, ritual performance, and ceremonial validation rather than direct reading. The measurement grid on one shared timeline tracks the institutional hardening of the literal reading over centuries.
 *
 * PERSPECTIVAL GAP:
 *   From the Brahmin institutional seat, the constraint is coordination: assigning each caste its proper function and maintaining ritual order (a genuine problem to solve). From the Kshatriya warrior seat, it is vindication: your violence is not sin but sacred duty. From the Shudra seat, it is pure extraction: your labor and obedience are extracted in exchange for no choice and no escape. From the modern observer seat, it is a constructed reading: the text could be read differently, and has been. The engine computes each seat's type from the structural relationship; the Brahmin seat might compute toward rope (coordination with enforcement) while the Shudra seat computes toward snare (pure extraction with suppression). The analytical divergence is not error—it is the measurement the corpus takes.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahmins sit at d ≈ 0.1 (beneficiaries collecting institutional power and interpretive authority; high institutional power; mobile exit options because their status is internally reinforced). Kshatriyas sit at d ≈ 0.2 (beneficiaries receiving moral justification for violence; powerful institutionally; identity-locked because warrior status and ritual duty are fused). Shudras sit at d ≈ 0.95 (full targets; powerless, trapped, identity-locked—they cannot exit the caste or claim alternative readings). Women sit at d ≈ 0.92 (nearly full targets; subordinated through patriarchal duty; excluded from direct interpretation). Heterodox practitioners sit at d ≈ 0.88 (suppressed as adharmic; moderate power but institutionally excluded from authority). The engine computes these from the structural data (beneficiary/victim declarations + power/exit); the divergence across seats is the point—different agents experience the same constraint at radically different effective extraction levels.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Arjuna's dharmic dilemma on the battlefield) was real at composition. The constraint solves it by asserting caste duty as absolute and non-negotiable. But the founding problem is not eternally 'live'—it is historically specific to the moment of the text's composition and the institutional interests it served. The orthodox reading suppresses this history by reframing the problem as 'eternal' (every warrior faces duty-conflict, every caste needs assignment). This is classic mandatrophy: the constraint persists beyond the conditions that created it, and those conditions are now reframed as permanent cosmic features rather than contingent political contexts. The measurement series shows theater ratio growing (0.25 to 0.42) as the literal reading required increasingly elaborate commentary and ritual performance to maintain its authority—more and more of the institutional machinery became performative rather than functional.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_ambiguity_caste_mandate,
    'Does the Gita explicitly mandate caste hierarchy and birth-based duty, or is the caste system interpolated by later commentarial tradition reading its concerns into the text?',
    'Textual-critical analysis comparing early Gita manuscript layers with later brahminical commentaries (Shankara, Ramanuja, Madhva); philological examination of varna-related passages in original Sanskrit; comparison with other dharmaśāstra sources to locate the innovation.',
    'If the caste mandate is textually explicit, the orthodox reading''s authority claim is strengthened. If caste hierarchy is a commentarial elaboration, the reading is revealed as a constructed interpretation, not an inevitable extraction from the kernel. The constraint''s type could shift toward tangled_rope (coordination + enforcement of interpretation) or snare (pure institutional extraction masked as textual fidelity).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(textual_ambiguity_caste_mandate, empirical, 'Whether caste hierarchy is textually mandated or commentarially constructed.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of Shudra and women''s voices primarily structural (legal prohibition from education and textual access) or internalized (victims have adopted the reading''s own framing of themselves as ritually unfit)?',
    'Historical ethnographic evidence from contexts where structural barriers were removed: do Shudra and women interpreters immediately claim alternative readings, or does internalized suppression persist? Post-independence India provides natural experiment data.',
    'If primarily structural, enforcement withdrawal would create immediate contestation. If substantially internalized, the constraint''s effective suppression is higher than the institutional measure suggests—the victims carry the suppression internalized and may resist alternative readings even when barriers are removed. This affects long-term type stability after intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural vs. internalized suppression mechanism in hierarchical religious authority.').

omega_variable(
    karma_theory_contingency,
    'Is the constraint''s legitimation of Kshatriya violence dependent on a particular theory of karma (that action performed as duty creates no karmic debt), or could the constraint persist under different karma theories that penalize violence?',
    'Examine whether heterodox karma theories (Buddhist, Jain) that penalize all violence could coexist with the constraint''s duty-based killing justification. Test whether the constraint requires the specific brahminical karma reading or only requires some theodicy that absolves duty-bound violence.',
    'If karma-theory-dependent, a shift in spiritual metaphysics undermines the constraint''s justification. If the constraint could persist under alternative theodicies, its root structure is more robust—it is about duty hierarchy per se, not about specific metaphysical claims. This affects the constraint''s vulnerability to doctrinal challenge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(karma_theory_contingency, conceptual, 'Whether the constraint''s justification of violence is contingent on a particular karma theory or generalizable to other theodicies.').

omega_variable(
    reading_contingency_and_kernel_contestation,
    'Is this reading one legitimate interpretation among multiple equally valid readings of the kernel, or is the orthodoxy claim itself part of the constraint structure—i.e., the constraint''s power depends on suppressing the fact that it IS a reading?',
    'Historical analysis of whether Brahmin tradition acknowledged multiple readings as intellectually respectable, or whether it treated alternative readings as illegitimate/adharmic from the outset. Examine the grammar of exclusion: are sibling readings suppressed as ''false interpretation'' or as ''spiritual error''?',
    'If the orthodoxy claim is structural to the constraint (victims don''t know they''re in a reading), then revealing the constraint''s contingency would undermine its authority more completely than merely arguing the reading is suboptimal. This affects the classification of the constraint: if the suppression includes suppression of the fact that alternatives exist, it is higher than the raw institutional measure suggests.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contingency_and_kernel_contestation, conceptual, 'Whether the constraint''s authority depends on suppressing awareness that it IS a reading of a contested kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__orthodox_literal_reading, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_tr_t0, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(gita_tr_t500, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 500, 0.28).
narrative_ontology:measurement(gita_tr_t1000, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 1000, 0.32).
narrative_ontology:measurement(gita_tr_t1500, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 1500, 0.38).
narrative_ontology:measurement(gita_tr_t2000, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 2000, 0.42).

% Extraction over time
narrative_ontology:measurement(gita_be_t0, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(gita_be_t500, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 500, 0.76).
narrative_ontology:measurement(gita_be_t1000, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 1000, 0.79).
narrative_ontology:measurement(gita_be_t1500, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 1500, 0.8).
narrative_ontology:measurement(gita_be_t2000, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 2000, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(gita_su_t0, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 0, 0.78).
narrative_ontology:measurement(gita_su_t500, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 500, 0.81).
narrative_ontology:measurement(gita_su_t1000, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 1000, 0.83).
narrative_ontology:measurement(gita_su_t1500, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 1500, 0.85).
narrative_ontology:measurement(gita_su_t2000, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 2000, 0.87).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra_discourse__orthodox_literal_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gita_kurukshetra_discourse__orthodox_literal_reading, 0.12).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__orthodox_literal_reading, gita_kurukshetra_discourse__gandhian_allegorical_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__orthodox_literal_reading, gita_kurukshetra_discourse__universalist_devotional_reading).

% DUAL FORMULATION NOTE:
% The Bhagavad Gita kernel (gita_kurukshetra_discourse) decomposes into three constraint stories corresponding to the three major historical readings. Each reading instantiates a different constraint because each authorizes a different social order with different beneficiaries and victims. The orthodox_literal_reading (this constraint) treats the text as mandating caste hierarchy and justifying Kshatriya violence. The gandhian_allegorical_reading treats Kurukshetra as metaphor for spiritual struggle, dissolving caste hierarchy by removing the literal social mandate. The universalist_devotional_reading treats bhakti as universally accessible, again undermining caste hierarchy's claim to be divinely mandated. The ε-invariance principle applies: each reading has different empirical consequences (different social orders, different beneficiaries/victims, different extractiveness measures) and thus represents a different constraint, not different measurements of the same constraint. All three remain live readings in contemporary India; they compete for textual authority and institutional power. The orthodox reading forecloses the universalist reading (both cannot hold simultaneously—either caste is divinely mandated or it is not) but coexists with the Gandhian reading (held by different interpretive communities) through institutional suppression rather than logical incompatibility.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gita_kurukshetra_discourse__orthodox_literal_reading, powerless, 0.95).
constraint_indexing:directionality_override(gita_kurukshetra_discourse__orthodox_literal_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
