% ============================================================================
% CONSTRAINT STORY: sacrifice_commandment__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_commandment__performance_only, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sacrifice_commandment__performance_only
 *   human_readable: Sacrifice Commandment — Performance-Only Reading (Suspension Without Fulfillment)
 *   domain: religious/halakhic/commitment-system
 *
 * SUMMARY:
 *   Since the destruction of the Second Temple, the halakhic commitment
 *   system has carried a large body of sacrificial commandments classified as
 *   binding but suspended: requiring physical execution that is nowhere
 *   possible. This story authors the performance_only reading of that
 *   commitment — the claim that the commandments demand physical execution,
 *   that study neither substitutes for nor fulfills them, and that the
 *   obligation therefore sits in suspension rather than being discharged. The
 *   standing arrangement under contest — the epsilon referent — is the one
 *   this reading assesses: an academy system requiring every trained scholar
 *   to master the sacrificial corpus, a credentialing economy rewarding
 *   comprehensive coverage, and roughly nineteen centuries of scholarly
 *   attention directed at acts no one can perform. On this reading's own
 *   lights that attention yields no fulfillive return; its measurable product
 *   is credentials, continuity, and authority for the interpreting class, and
 *   its measurable cost is scholarly attention diverted from performable law.
 *   The claimed type and the metrics are authored independently: the metrics
 *   describe the arrangement's actual operation as this reading assesses it,
 *   and the engine computes per-seat classifications from the structural
 *   data. Epsilon's referent is the standing arrangement, never this
 *   reading's implicit preference for restored performance. KEY AGENTS (by
 *   structural relationship): - talmudic_academy_system: agenda setter
 *   (institutional/constrained) — sets curriculum, administers credentialing,
 *   maintains the closed canon - credentialed_rabbinic_class: primary
 *   beneficiary (organized/identity_locked) — collects authority and
 *   livelihood from comprehensive mastery; paid in its own formation years -
 *   yeshiva_students: primary target (powerless/constrained) — surrender
 *   formative years' attention to unperformable orders -
 *   communal_torah_scholars: secondary target (moderate/identity_locked) —
 *   advanced study hours pulled into sacrificial tractates -
 *   practical_halakha_questioners: excluded voice (powerless/constrained) —
 *   live practical questions compete for diverted expert attention -
 *   academic_jewish_studies: analytical observer (institutional/analytical) —
 *   sees the full allocation structure from outside the commitment system
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__performance_only, 0.75).
domain_priors:suppression_score(sacrifice_commandment__performance_only, 0.62).
domain_priors:theater_ratio(sacrifice_commandment__performance_only, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, extractiveness, 0.75).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__performance_only, tangled_rope).
narrative_ontology:human_readable(sacrifice_commandment__performance_only, "Sacrifice Commandment — Performance-Only Reading (Suspension Without Fulfillment)").
narrative_ontology:topic_domain(sacrifice_commandment__performance_only, "religious/halakhic/commitment-system").

domain_priors:requires_active_enforcement(sacrifice_commandment__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__performance_only, '915185a2-8620-4334-8496-e96236614435').
narrative_ontology:cs_kernel_codification('915185a2-8620-4334-8496-e96236614435', formalized).
narrative_ontology:cs_authority_grounding('915185a2-8620-4334-8496-e96236614435', lineage).
narrative_ontology:cs_interpretation_layer_present('915185a2-8620-4334-8496-e96236614435').
narrative_ontology:cs_reading_relation('915185a2-8620-4334-8496-e96236614435', sacrifice_commandment__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('915185a2-8620-4334-8496-e96236614435', sacrifice_commandment__archive_maintenance, influences).
narrative_ontology:cs_axiom('915185a2-8620-4334-8496-e96236614435', foundational, commandment_fulfillment_requires_physical_performance).
narrative_ontology:cs_axiom_status(commandment_fulfillment_requires_physical_performance, holdable).
narrative_ontology:cs_axiom_grounding('915185a2-8620-4334-8496-e96236614435', commandment_fulfillment_requires_physical_performance, deontological).
narrative_ontology:cs_axiom('915185a2-8620-4334-8496-e96236614435', secondary, suspension_discharges_nothing).
narrative_ontology:cs_axiom_status(suspension_discharges_nothing, holdable).
narrative_ontology:cs_axiom_grounding('915185a2-8620-4334-8496-e96236614435', suspension_discharges_nothing, conventional).
narrative_ontology:cs_reference_frame('915185a2-8620-4334-8496-e96236614435', performed_temple_service_norm).
narrative_ontology:cs_drift_state('915185a2-8620-4334-8496-e96236614435', contemporary_post_temple_exile, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('915185a2-8620-4334-8496-e96236614435', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__performance_only, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__performance_only, talmudic_academy_system).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__performance_only, credentialed_rabbinic_class).
narrative_ontology:constraint_victim(sacrifice_commandment__performance_only, yeshiva_students).
narrative_ontology:constraint_victim(sacrifice_commandment__performance_only, communal_torah_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(sacrifice_commandment__performance_only, credentialed_rabbinic_class).
narrative_ontology:constraint_vindicates(sacrifice_commandment__performance_only, canonical_completeness_doctrine).
narrative_ontology:constraint_vindicates(sacrifice_commandment__performance_only, performance_principle_of_commandment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates the yeshiva and kollel networks, sets the sequence of tractates every candidate must cover, and examines ordination candidates against the full corpus including the sacrificial orders. Its enrollment, funding, and continuity depend on donor and communal confidence that the traditional curriculum is intact. Changing the sequence unilaterally would put its graduates out of step with peer institutions and cost it recognition, so the syllabus moves only with a broad consensus that has not formed.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, talmudic_academy_system, agenda_setter,
    institutional, generational, constrained, global).

% Holds teaching posts, pulpits, and adjudication roles on the strength of credentials certifying comprehensive mastery, including the sacrificial orders. During its own training years it surrendered the same study hours it now allocates to others; in its career years it collects the status, authority, and livelihood that comprehensive coverage commands. Its professional self-understanding is bound to the ideal of the whole canon mastered, so stepping off that standard would mean forfeiting the distinction it lives by.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, credentialed_rabbinic_class, beneficiary,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_commandment__performance_only, credentialed_rabbinic_class, payer).

% Pass through a fixed multi-year sequence that includes extended engagement with sacrificial tractates they will never apply. Their daily schedule, financial support, and marriage prospects within the community all run through the academy system, so reallocating their own hours toward performable law is not a choice they can make individually; leaving the track altogether carries social and economic costs most do not accept.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, yeshiva_students, payer,
    powerless, immediate, constrained, global).

% Advanced learners and teachers whose discretionary study hours are drawn toward depth in the sacrificial orders through the global page-a-day cycle, specialized fellowships, and the prestige attached to difficult tractates. Many regard total engagement with the canon as constitutive of who they are; skipping inapplicable material feels like a diminishment of the learning itself, which keeps them at the table even where they privately question the allocation.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, communal_torah_scholars, payer,
    moderate, biographical, identity_locked, global).

% Households and businesses with live questions about food, observance, commerce, and family law that need expert attention now. They are represented in no body that sets the study agenda; their questions queue behind curriculum obligations, and the deepest specialists in the system are, by design, specialists in material that cannot be practiced.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, practical_halakha_questioners, excluded,
    powerless, immediate, constrained, regional).

% University-based historians and philologists of rabbinic literature who study the sacrificial corpus and its pedagogy from outside the commitment system. They document how the curriculum evolved, who it advantages, and what alternatives have looked like where communities relaxed it; they grant no credentials within the system and collect nothing from its operation.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, academic_jewish_studies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_commandment__performance_only, credentialed_rabbinic_class).
narrative_ontology:fixing_cost_class(sacrifice_commandment__performance_only, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains the oral Torah as a single transmissible canon: a fixed curriculum through which every trained scholar passes, creating a shared textual reference across dispersed communities and generations. Comprehensive coverage — including currently inapplicable orders — is the mechanism by which the canon remains one object rather than a menu of applicable fragments.
% TRANSFER_FUNCTION: Moves scholarly time and attention from young and mid-career learners toward the sacrificial orders of the canon — material that, on this reading, cannot be performed — and converts completed coverage into credentials, communal authority, and teaching positions held by the credentialed class.
% ABSENT_VOICES: Laypeople with live practical halakhic questions are not in the curriculum conversation, nor are the students themselves, who inherit a fixed sequence. Both would press for reallocating advanced study hours toward performable law; neither seat sits on any body that sets the syllabus.
% DISAPPEARANCE_RATIONALE: If compulsory coverage of the sacrificial orders and the credentialing built on it vanished overnight, the credentialing economy, the global page-cycle's shape, the commentary literature's scope, and the career structure of the scholarly class would all reorganize; freed attention would flow toward performable law and toward the practical questions now queued behind curriculum obligations. The arrangement is load-bearing for every institution that trains or certifies scholars — its disappearance would be felt immediately.
% FOUNDING_PROBLEM: After the Temple's destruction, the tradition faced the problem of what to do with a large body of binding commandments that could no longer be performed: whether to suspend them, transform them into something performable, or preserve them intact against restoration. The study apparatus was built to keep the sacrificial corpus alive within the canon despite universal non-performance.
% FOUNDING_PROBLEM_CORROBORATION: Yeshiva students bearing the allocation attest daily that the question of what is owed to unperformable commandments remains unresolved. Academic historians of rabbinic literature corroborate, from outside the benefiting parties, the continuity of the post-destruction preservation problem across nineteen centuries. Holders of the rival readings likewise attest the problem is live even while disputing the answer. No party attests the problem is dead.
narrative_ontology:disappearance_verdict(sacrifice_commandment__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_commandment__performance_only, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__performance_only, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_commandment__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_commandment__performance_only, 0.75, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_commandment__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_commandment__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_commandment__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.75 at interval end) because, on this reading's own lights, the diverted labor returns nothing toward the obligation it orbits: nearly two millennia of scholarly hours produce no performance and no fulfillment, only credential and continuity. Suppression (0.62) is authored as a raw structural property — examination gates, hiring standards, and funding conditioned on the standard sequence, with social sanction doing the rest; the engine scales only extractiveness by directionality and scope. Theater ratio (0.35) reflects real functions (canon transmission, credentialing) alongside a growing performative layer — public completion celebrations, demonstration dissections of sacrificial anatomy — that signals fidelity without altering practice. Accessibility collapse is low (0.30): once the suspension logic is understood, the alternative of reallocating hours to performable law remains fully visible; it is institutionally penalized, not unimaginable. Resistance (0.50) is persistent and internal: rival readings of the same kernel, curriculum reform experiments, and exit into academic Jewish studies. The three metric series share one time grid (70, 500, 1000, 1500, 2000, 2025). The suppression series traces enforcement decay through the modern exit-options era and partial re-hardening through mass learning infrastructure — a decay-and-revival arc, not oscillation-as-extraction. Coordination type is declared identity_coordination: the dominant function is maintenance of a shared comprehensive-mastery standard — the credential boundary — whose loss would fragment the canon into applicable fragments.
 *
 * PERSPECTIVAL GAP:
 *   From the academy seat the arrangement is the tradition's continuity itself: the canon kept whole, the chain of transmission unbroken. From the student seat the same structure is years surrendered to material that cannot be performed, under a schedule they did not set. The credentialed class straddles the line — it paid in formation and collects in career — which is why its computed position should sit between the pure beneficiary and pure target poles. Same-level differentiation appears between students and scholars, who hold adjacent nominal standing but differ sharply in exit options (constrained versus identity_locked) and horizon. The engine computes these per-seat classifications from the structural data; the divergence between seats is the finding, not something the authored claim adjudicates.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (academy system, credentialed class) derive low directionality — the arrangement subsidizes both; the credentialed class's identity-locked exit deepens its capture, since it cannot step off the very standard that privileges it. Victim declarations (students, scholars) derive high directionality; the scholars' identity lock places them nearer the full-target pole than their moderate power alone would suggest, while the students' constrained exit keeps them firmly in target territory. The excluded questioners sit outside the derivation — they pay in diverted expert attention without any seat in the arrangement. No directionality overrides were needed: the beneficiary/victim plus exit data produce the correct relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — what a binding tradition owes commandments it cannot perform — remains live, so no mandatrophy is declared: the arrangement has not outlived its problem; it has accreted extraction onto a live one. The tangled_rope classification prevents two mislabels: calling the whole apparatus pure extraction would erase the genuine coordination function (canon transmission predates and exceeds the extraction asymmetry); calling it pure coordination would erase the enforced asymmetry between those who surrender hours and those who collect credentials. If the Temple were restored or the obligation formally dissolved, the founding problem would die; the arrangement would then face the degraded-inertial trajectory — theatrical maintenance of a dead mandate — unless deliberately wound down, which the prohibitive fixing cost makes unlikely.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which reading of the sacrifice_commandment kernel correctly characterizes the obligation''s status in the Temple''s absence — suspension without fulfillment (this reading), fulfillment-through-study, or preservation-for-restoration?',
    'Meta-halakhic adjudication or changed circumstances (restored Temple service rendering the question moot); no in-framework data settles it while circumstances stand.',
    'Adoption of study_as_performance would collapse measured extraction toward coordination cost, since the same labor becomes the commandment''s exercise; adoption of archive_maintenance would convert the labor into instrumentally valued preparation and lower epsilon substantially. The victim set changes with the answer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'This constraint is one reading of the sacrifice_commandment kernel; the sibling readings instantiate different constraints with different epsilon and victims.').

omega_variable(
    suspension_horizon,
    'Is the suspension open-ended (indefinite exile) or bounded (awaiting a restoration the tradition expects)?',
    'An observable event — renewed Temple service — or an authoritative ruling declaring the suspension permanent.',
    'A bounded horizon reframes the arrangement as transitional support with a terminus, lowering persistence-weighted extraction; an open-ended horizon lets diverted attention accumulate without limit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suspension_horizon, empirical, 'Whether the suspension has a terminus.').

omega_variable(
    counterfactual_attention_use,
    'If curriculum hours devoted to the sacrificial orders were freed, would they flow to performable law (making diversion the harm this reading alleges) or dissipate into less demanding activity (shrinking the alleged harm)?',
    'Compare reallocation in settings that de-emphasized the sacrificial orders — academic programs, communities with shortened curricula — measuring where study hours actually went.',
    'Confirms or shrinks the victim-side harm and moves epsilon accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_attention_use, empirical, 'Marginal use of freed scholarly attention.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the difficulty of reallocating study time structural (examination gates, hiring standards, funding conditioned on the standard sequence) or internalized (learners experience comprehensive coverage as intrinsically owed, independent of external penalty)?',
    'Post-exit and post-reform trajectories: whether learners in settings that dropped the requirement redistribute hours freely or reproduce the allocation voluntarily.',
    'An internalized component means effective suppression exceeds the structural measure and persists after institutional change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Mechanism split of measured suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__performance_only, 70, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t70, sacrifice_commandment__performance_only, theater_ratio, 70, 0.1).
narrative_ontology:measurement(sacr_tr_t500, sacrifice_commandment__performance_only, theater_ratio, 500, 0.12).
narrative_ontology:measurement(sacr_tr_t1000, sacrifice_commandment__performance_only, theater_ratio, 1000, 0.15).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_commandment__performance_only, theater_ratio, 1500, 0.22).
narrative_ontology:measurement(sacr_tr_t2000, sacrifice_commandment__performance_only, theater_ratio, 2000, 0.32).
narrative_ontology:measurement(sacr_tr_t2025, sacrifice_commandment__performance_only, theater_ratio, 2025, 0.35).

% Extraction over time
narrative_ontology:measurement(sacr_be_t70, sacrifice_commandment__performance_only, base_extractiveness, 70, 0.5).
narrative_ontology:measurement(sacr_be_t500, sacrifice_commandment__performance_only, base_extractiveness, 500, 0.58).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_commandment__performance_only, base_extractiveness, 1000, 0.65).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_commandment__performance_only, base_extractiveness, 1500, 0.7).
narrative_ontology:measurement(sacr_be_t2000, sacrifice_commandment__performance_only, base_extractiveness, 2000, 0.74).
narrative_ontology:measurement(sacr_be_t2025, sacrifice_commandment__performance_only, base_extractiveness, 2025, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t70, sacrifice_commandment__performance_only, suppression_requirement, 70, 0.4).
narrative_ontology:measurement(sacr_su_t500, sacrifice_commandment__performance_only, suppression_requirement, 500, 0.5).
narrative_ontology:measurement(sacr_su_t1000, sacrifice_commandment__performance_only, suppression_requirement, 1000, 0.6).
narrative_ontology:measurement(sacr_su_t1500, sacrifice_commandment__performance_only, suppression_requirement, 1500, 0.65).
narrative_ontology:measurement(sacr_su_t2000, sacrifice_commandment__performance_only, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(sacr_su_t2025, sacrifice_commandment__performance_only, suppression_requirement, 2025, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__performance_only, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_commandment__performance_only, sacrifice_commandment__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_commandment__performance_only, sacrifice_commandment__archive_maintenance).

% DUAL FORMULATION NOTE:
% The colloquial label 'the sacrifice commandment without a Temple' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints sharing one kernel: performance_only (this file — study fulfills nothing; high extraction via attention diversion; victims are students and scholars), study_as_performance (the same study labor IS the commandment's exercise; extraction collapses toward coordination cost; different victim structure entirely), and archive_maintenance (study preserves technical knowledge for restoration; the labor gains instrumental value; intermediate epsilon). This reading is upstream in the family: its foundational premise — that non-performance leaves the obligation undischarged — creates the structural vacuum the sibling readings exist to fill, which is why its edge to archive_maintenance is typed influences and its edge to study_as_performance is typed forecloses. Each member links to the others via affects_constraints; no member is orphaned.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
