% ============================================================================
% CONSTRAINT STORY: kodashim_obligation__study_as_preparation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_obligation__study_as_preparation, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: kodashim_obligation__study_as_preparation
 *   human_readable: Kodashim Study as Preparatory Preservation for Messianic Temple Restoration
 *   domain: religious/legal/textual
 *
 * SUMMARY:
 *   This constraint models the study_as_preparation reading of the Kodashim
 *   obligation kernel: sacrificial law (the Mishnaic order Kodashim) remains
 *   formally binding in halakhic status even though the Temple's destruction
 *   has made its performance impossible. Under this reading, the obligation
 *   to study these laws exists instrumentally — to preserve exact technical
 *   knowledge (priestly procedure, sacrificial measurements, purity
 *   mechanics) so that a future messianic restoration can resume performance
 *   without having to reconstruct lost tradition from fragments. The current
 *   generation invests study labor whose payoff accrues to a hypothetical
 *   future community; extraction is low because no party is coercively
 *   exploiting this arrangement for material gain, but there is a genuine
 *   cost-bearing asymmetry between the present investors and the deferred
 *   beneficiary. This is deliberately ONE of three sibling readings of the
 *   same textual kernel (Kodashim's binding status) — study_as_performance
 *   holds that study itself is spiritually efficacious and
 *   restoration-independent, while study_as_archive holds the entire system
 *   is defunct and study is identity-maintenance rather than legal
 *   obligation. Each reading is its own constraint with its own ε; this file
 *   does not average across them.
 *
 * KEY AGENTS:
 *   - yeshiva_institutions: agenda-setter and incidental beneficiary — administers the curriculum and draws institutional continuity from it
 *   - current_generation_practitioners: bears the study-labor cost with a deferred, uncertain payoff
 *   - future_messianic_generation: non-agent structural beneficiary, projected rather than acting
 *   - reform_and_reconstructionist_communities: excluded from the halakhic conversation that sustains this reading
 *   - religious_studies_scholars: analytical observers comparing this reading to its siblings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__study_as_preparation, 0.18).
domain_priors:suppression_score(kodashim_obligation__study_as_preparation, 0.22).
domain_priors:theater_ratio(kodashim_obligation__study_as_preparation, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, extractiveness, 0.18).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__study_as_preparation, rope).
narrative_ontology:human_readable(kodashim_obligation__study_as_preparation, "Kodashim Study as Preparatory Preservation for Messianic Temple Restoration").
narrative_ontology:topic_domain(kodashim_obligation__study_as_preparation, "religious/legal/textual").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_preparation, 'e9909441-8344-4086-adff-cd1dce810250').
narrative_ontology:cs_kernel_codification('e9909441-8344-4086-adff-cd1dce810250', fixed_text).
narrative_ontology:cs_authority_grounding('e9909441-8344-4086-adff-cd1dce810250', lineage).
narrative_ontology:cs_interpretation_layer_present('e9909441-8344-4086-adff-cd1dce810250').
narrative_ontology:cs_reading_relation('e9909441-8344-4086-adff-cd1dce810250', kodashim_obligation__kodashim_obligation_study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('e9909441-8344-4086-adff-cd1dce810250', kodashim_obligation__kodashim_obligation_study_as_archive, influences).
narrative_ontology:cs_axiom('e9909441-8344-4086-adff-cd1dce810250', foundational, restoration_is_structurally_required_for_fulfillment).
narrative_ontology:cs_axiom_status(restoration_is_structurally_required_for_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('e9909441-8344-4086-adff-cd1dce810250', restoration_is_structurally_required_for_fulfillment, deontological).
narrative_ontology:cs_axiom('e9909441-8344-4086-adff-cd1dce810250', secondary, study_without_performance_is_incomplete_but_binding).
narrative_ontology:cs_axiom_status(study_without_performance_is_incomplete_but_binding, holdable).
narrative_ontology:cs_axiom_grounding('e9909441-8344-4086-adff-cd1dce810250', study_without_performance_is_incomplete_but_binding, conventional).
narrative_ontology:cs_reference_frame('e9909441-8344-4086-adff-cd1dce810250', temple_era_sacrificial_obligation).
narrative_ontology:cs_drift_state('e9909441-8344-4086-adff-cd1dce810250', post_destruction_diaspora_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('e9909441-8344-4086-adff-cd1dce810250', '').
narrative_ontology:cs_kernel_id(kodashim_obligation__study_as_preparation, kodashim_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_preparation, future_messianic_generation).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_preparation, yeshiva_institutions).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_preparation, halakhic_continuity_project).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_preparation, current_generation_practitioners).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_preparation, temple_restoration_necessity).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_preparation, binding_force_of_unperformable_commandments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the curriculum requiring intensive study of Kodashim tractates (Zevachim, Menachot, Chullin, etc.) despite the total absence of a functioning Temple. Administers ordination tracks, publishes commentary, and trains successive generations of scholars in sacrificial procedure. Draws institutional prestige, funding, and continuity of purpose from framing this study as urgent technical preparation rather than antiquarian exercise. Faces no material cost if restoration never occurs — the institution's function is self-sustaining regardless of outcome.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, yeshiva_institutions, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(kodashim_obligation__study_as_preparation, yeshiva_institutions, beneficiary).

% Individual students and observant Jews are obligated to study laws they will almost certainly never see performed in their lifetimes, investing years of intellectual labor toward a restoration whose timing is theologically indeterminate. They bear the opportunity cost of that labor and the psychological weight of maintaining fidelity to a system whose practical referent is absent. Exit is constrained by identity: abandoning the study track is experienced as abandoning covenantal obligation, not as a neutral career choice.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, current_generation_practitioners, payer,
    moderate, biographical, identity_locked, national).

% A hypothetical future community that would inherit fully preserved technical knowledge of sacrificial procedure at the moment of restoration, avoiding a multi-generational reconstruction problem. This beneficiary has no present agency and cannot corroborate or contest the arrangement — it is a projected recipient, not an acting party.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, future_messianic_generation, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(kodashim_obligation__study_as_preparation, future_messianic_generation).

% Communities that have formally reinterpreted or dropped the sacrificial system from liturgy and legal obligation are not party to the halakhic conversation that maintains this reading's authority. They would argue the preparatory framing is unfalsifiable and indefinitely deferrable, but their position is not adjudicated within the framework that governs this constraint.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, reform_and_reconstructionist_communities, excluded,
    organized, generational, mobile, global).

% Academic observers who analyze the study-as-preparation claim comparatively against sibling readings (study-as-performance, study-as-archive) and against the historical persistence of unperformable legal obligations in other traditions. They can document the structure without holding a stake in its resolution.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, religious_studies_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves highly technical, perishable procedural knowledge (exact sacrificial mechanics, priestly qualifications, Temple architecture and measurements) across centuries of non-use, so that if and when restoration becomes possible, the community is not starting from a reconstructed or lost tradition.
% TRANSFER_FUNCTION: Moves scholarly labor, curricular time, and formative attention from the current generation into a body of preserved technical knowledge whose beneficiary is a future generation that does not yet exist and cannot compensate the present investors.
% ABSENT_VOICES: Reform and Reconstructionist communities who have formally set aside the sacrificial system are not represented in the halakhic study apparatus that sustains this reading; a restorationist skeptic within Orthodoxy who views indefinite deferral as functionally identical to abandonment also has no institutional voice in curriculum-setting bodies.
% DISAPPEARANCE_RATIONALE: If Kodashim study ceased tomorrow, the yeshiva world's curriculum and self-conception would visibly rearrange (a core structural pillar of the study-hall system would vanish), but whether the 'world' that matters here — cosmic or covenantal order — would rearrange is exactly the question the three sibling readings dispute. The study-as-preparation reading holds that stopping would leave restoration technically unprepared should the messianic moment arrive; a skeptic holds nothing would change because restoration is not imminent regardless.
% FOUNDING_PROBLEM: Following the Temple's destruction, sacrificial law faced obsolescence through disuse: without continuous engagement, precise procedural knowledge (exact blood-application order, measurements, priestly rotations) would degrade or vanish within a few generations, leaving any future restoration to begin from fragments rather than a living technical tradition.
% FOUNDING_PROBLEM_CORROBORATION: Within the tradition, the Talmudic principle that studying sacrificial law is 'as if one had offered it' (Menachot 110a) is invoked by yeshiva authorities to attest the problem remains live and the study obligation binding. Outside the benefiting institutions, comparative religion scholars and Reform halakhic authorities attest that no restoration project is underway on any practical timeline, and that the 'preparation' framing has persisted unfalsified for nearly two millennia in a way indistinguishable from permanent deferral — a status this reading's own advocates do not independently corroborate from outside their institutional interest.
narrative_ontology:disappearance_verdict(kodashim_obligation__study_as_preparation, contested).
narrative_ontology:founding_problem_status(kodashim_obligation__study_as_preparation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_obligation__study_as_preparation, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kodashim_obligation__study_as_preparation, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_obligation__study_as_preparation, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_obligation__study_as_preparation_tests).
:- end_tests(kodashim_obligation__study_as_preparation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) and rises only marginally across the interval because study_as_preparation genuinely functions as knowledge preservation rather than as a mechanism for extracting rents from practitioners — no party materially profits from the deferral in the way a captured institution would. Suppression (0.22) is modest: practitioners are not coerced by external force, though identity-formation within observant communities makes departure costly. Theater ratio is low (0.15) and rises only slightly, reflecting that the study activity is substantively engaged with technical content (not merely ceremonial performance of study) even though its stated practical referent (an operating Temple) does not exist. Accessibility collapse is moderately high (0.6) because once a practitioner is formed within this framework, alternative readings (archive, performance) become difficult to adopt without a full reframing of covenantal identity — but it is not near-total, since sibling readings do coexist and conversion between them occurs.
 *
 * PERSPECTIVAL GAP:
 *   From the yeshiva institution's seat, this is a rope: coordinated, low-coercion preservation of technical knowledge with clear communal buy-in and no exit suppression. From the individual practitioner's seat under a lifetime horizon, the same arrangement can register as a tangled structure — a genuine coordination function (knowledge preservation) riding alongside an asymmetric cost (their labor, a future generation's payoff) that the identity-locked exit option makes hard to resist. The engine computing these seats separately, rather than reconciling them, is the intended behavior — the divergence itself is data about how deferred-benefit religious obligations distribute cost across generations.
 *
 * DIRECTIONALITY LOGIC:
 *   The future_messianic_generation is the structural full beneficiary (d near 0) but is a non-agent, projected party — it cannot corroborate, contest, or be measured, which is itself the omega this reading rests on. Current_generation_practitioners sit nearer the target end (d elevated) because they bear the real, present cost of study-labor without receiving the restoration payoff within their own lifetimes; their exit is identity-locked rather than merely constrained, since departure reads as covenantal abandonment, not neutral opt-out. Yeshiva_institutions occupy a genuine dual position: they administer the obligation (agenda_setter) and simultaneously draw real institutional benefit (prestige, continuity, funding) regardless of whether restoration ever occurs — this secondary beneficiary role is the seam most likely to generate seat divergence from the payer seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem/disappearance_verdict pairing is deliberately marked contested rather than resolved: the founding problem (technical knowledge loss absent continuous study) is either still live (if restoration remains a genuine future possibility) or effectively dead (if nearly two millennia of non-restoration functions as permanent deferral in practice). This reading does not resolve that tension — it structurally requires Temple restoration as the condition under which the preparatory function cashes out, and that requirement is exactly what keeps this from being classified alongside a piton: there is no theatrical maintenance masking an atrophied function here, because the stated function (technical preservation) is actively and substantively performed, not merely gestured at.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    restoration_imminence_indeterminacy,
    'Is messianic Temple restoration a live practical horizon that makes present study-labor genuinely preparatory, or has the indefinite deferral become structurally indistinguishable from permanent non-restoration?',
    'No empirical resolution mechanism exists within the tradition''s own framework (restoration timing is explicitly theologically indeterminate); the closest available proxy is comparative analysis of how other traditions'' indefinitely-deferred restorative obligations have or have not eventually resolved, and whether institutional behavior (investment horizons, curriculum design) treats the deferral as near-term or effectively permanent.',
    'If restoration is treated as genuinely imminent, the low extractiveness and rope-adjacent framing hold. If nearly two millennia of deferral is functionally equivalent to permanent non-restoration, the current generation''s study-labor investment increasingly resembles uncompensated extraction toward an institution that benefits regardless of outcome — pushing the classification toward tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(restoration_imminence_indeterminacy, conceptual, 'Whether indefinite deferral of the founding problem is structurally equivalent to its resolution or its abandonment.').

omega_variable(
    beneficiary_non_agency_problem,
    'Can a projected, non-existent future generation function as a structural beneficiary in the same sense as an acting present-day party, or does naming it as beneficiary launder an arrangement whose real, present beneficiary is the administering institution?',
    'Compare institutional behavior against the counterfactual: if yeshiva funding, prestige, and continuity mechanisms would be materially unaffected by a credible signal that restoration will never occur, the future generation''s beneficiary status is largely notional and the institution is the real accruing party.',
    'If the institution is the real beneficiary, the false_summit_mountain-adjacent question shifts to whether this reading (currently claimed as rope) understates institutional capture and should be read closer to a mild tangled_rope with the institution as the concentrated beneficiary rather than the diffuse future generation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_non_agency_problem, conceptual, 'Whether the future_messianic_generation beneficiary is structurally load-bearing or a legitimating fiction for present institutional benefit.').

omega_variable(
    sibling_reading_boundary_location,
    'Where exactly does the disagreement between study_as_preparation and study_as_performance live — is it a dispute about metaphysics (does study alone achieve cosmic efficacy) or a dispute about practical obligation (must physical restoration occur for the mitzvah to be fulfilled)?',
    'Close textual analysis of the specific Talmudic and later halakhic sources each reading cites (e.g., Menachot 110a is invoked by both sides with different emphases) to locate whether the disagreement is generative (different metaphysical premises) or terminological (same premise, different vocabulary).',
    'If the disagreement is genuinely metaphysical, the two readings coexist as live, mutually irreducible positions (coexists_with is correct). If it collapses to terminology, the readings may be closer to a single constraint artificially split — which would violate the ε-invariance decomposition this story relies on.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_boundary_location, conceptual, 'Whether the study_as_preparation / study_as_performance split represents genuinely distinct structural claims or a terminological variant of one claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_preparation, 0, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_obligation__study_as_preparation, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(koda_tr_t0, observed).
narrative_ontology:measurement(koda_tr_t300, kodashim_obligation__study_as_preparation, theater_ratio, 300, 0.11).
narrative_ontology:measurement_basis(koda_tr_t300, observed).
narrative_ontology:measurement(koda_tr_t700, kodashim_obligation__study_as_preparation, theater_ratio, 700, 0.12).
narrative_ontology:measurement_basis(koda_tr_t700, observed).
narrative_ontology:measurement(koda_tr_t1100, kodashim_obligation__study_as_preparation, theater_ratio, 1100, 0.13).
narrative_ontology:measurement_basis(koda_tr_t1100, observed).
narrative_ontology:measurement(koda_tr_t1500, kodashim_obligation__study_as_preparation, theater_ratio, 1500, 0.14).
narrative_ontology:measurement_basis(koda_tr_t1500, observed).
narrative_ontology:measurement(koda_tr_t1900, kodashim_obligation__study_as_preparation, theater_ratio, 1900, 0.15).
narrative_ontology:measurement_basis(koda_tr_t1900, observed).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_obligation__study_as_preparation, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(koda_be_t0, observed).
narrative_ontology:measurement(koda_be_t300, kodashim_obligation__study_as_preparation, base_extractiveness, 300, 0.14).
narrative_ontology:measurement_basis(koda_be_t300, observed).
narrative_ontology:measurement(koda_be_t700, kodashim_obligation__study_as_preparation, base_extractiveness, 700, 0.15).
narrative_ontology:measurement_basis(koda_be_t700, observed).
narrative_ontology:measurement(koda_be_t1100, kodashim_obligation__study_as_preparation, base_extractiveness, 1100, 0.16).
narrative_ontology:measurement_basis(koda_be_t1100, observed).
narrative_ontology:measurement(koda_be_t1500, kodashim_obligation__study_as_preparation, base_extractiveness, 1500, 0.17).
narrative_ontology:measurement_basis(koda_be_t1500, observed).
narrative_ontology:measurement(koda_be_t1900, kodashim_obligation__study_as_preparation, base_extractiveness, 1900, 0.18).
narrative_ontology:measurement_basis(koda_be_t1900, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kodashim_obligation__study_as_preparation, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_obligation__study_as_preparation, identity_coordination).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_preparation, kodashim_obligation_study_as_performance).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_preparation, kodashim_obligation_study_as_archive).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language claim 'the Kodashim obligation.' study_as_preparation (this file) claims low extractiveness, a deferred future beneficiary, and structurally requires eventual Temple restoration for the preparatory function to complete. study_as_performance claims restoration-independent spiritual efficacy of study itself, decoupling the cosmic function from physical Temple presence — a different ε profile because the 'unperformability' problem this file treats as a cost-bearing deferral is, in that reading, dissolved rather than deferred. study_as_archive claims the system is defunct and treats study as identity-maintenance without live legal obligation — near-zero extractiveness but also no claimed cosmic or restorative function, making it structurally closer to a rope with no deferred beneficiary at all. All three are linked here rather than merged; each carries its own beneficiary/victim structure and its own claimed_type.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
