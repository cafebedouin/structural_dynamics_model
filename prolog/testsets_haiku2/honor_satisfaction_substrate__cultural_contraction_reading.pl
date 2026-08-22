% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_substrate__cultural_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_substrate__cultural_contraction_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: honor_satisfaction_substrate__cultural_contraction_reading
 *   human_readable: Honor Code Substrate Collapse: From Honor to Dignity (Cultural Contraction Reading)
 *   domain: social/cultural/historical
 *
 * SUMMARY:
 *   This constraint describes the collapse of the honor-code substrate itself
 *   — the interpretive moral framework that made dueling thinkable as an
 *   honorable act. In this reading, dueling did not decline because it was
 *   suppressed; it became unthinkable because the cultural logic sustaining
 *   it eroded. As 'cultures of honor' (status through genealogy and
 *   willingness to fight) gave way to 'cultures of dignity' (status through
 *   rational argument, institutional position, and universal moral
 *   principle), the very idea of honor-satisfaction through combat lost
 *   coherence. The aristocratic classes did not exit the honor system through
 *   choice or coercion; they exited through the dissolution of the substrate
 *   that made honor-culture intelligible. The constraint this reading
 *   describes is the erosion of a mountain — a once-natural feature of the
 *   social landscape (honor-based status validation) that ceased to emerge
 *   from the underlying structure of what people understood as legitimate
 *   claims to worth.
 *
 * KEY AGENTS:
 *   - Gentry and aristocratic classes: The social group for which honor-combat was once a core identity practice. As the moral substrate shifted, this group experienced the loss of an entire interpretive framework without a clear suppressive agent — the framework simply ceased to cohere.
 *   - Dignity-centric intellectual classes: Carriers of the alternative moral framework. Their legitimacy claims rested on universal rational principles independent of genealogy. The substrate shift validated their worldview and marginalized honor-based authority.
 *   - Common people: Excluded from the honor system itself; the substrate shift affected them indirectly through reorganization of aristocratic-commoner relationships.
 *   - Legal and institutional apparatus: Recorded and formalized the decline. In this reading, law is secondary — it reinforces the cultural shift rather than causing it.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__cultural_contraction_reading, 0.12).
domain_priors:suppression_score(honor_satisfaction_substrate__cultural_contraction_reading, 0.15).
domain_priors:theater_ratio(honor_satisfaction_substrate__cultural_contraction_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__cultural_contraction_reading, mountain).
narrative_ontology:human_readable(honor_satisfaction_substrate__cultural_contraction_reading, "Honor Code Substrate Collapse: From Honor to Dignity (Cultural Contraction Reading)").
narrative_ontology:topic_domain(honor_satisfaction_substrate__cultural_contraction_reading, "social/cultural/historical").

domain_priors:emerges_naturally(honor_satisfaction_substrate__cultural_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__cultural_contraction_reading, '1a3a6ba0-6f1e-4e1f-a565-ffe3d9d570a0').
narrative_ontology:cs_kernel_codification('1a3a6ba0-6f1e-4e1f-a565-ffe3d9d570a0', implicit).
narrative_ontology:cs_authority_grounding('1a3a6ba0-6f1e-4e1f-a565-ffe3d9d570a0', practice).
narrative_ontology:cs_interpretation_layer_present('1a3a6ba0-6f1e-4e1f-a565-ffe3d9d570a0').
narrative_ontology:cs_reading_relation('1a3a6ba0-6f1e-4e1f-a565-ffe3d9d570a0', honor_satisfaction_substrate__practice_decline_reading, coexists_with).
narrative_ontology:cs_reading_relation('1a3a6ba0-6f1e-4e1f-a565-ffe3d9d570a0', honor_satisfaction_substrate__composite_overdetermined_reading, coexists_with).
narrative_ontology:cs_axiom('1a3a6ba0-6f1e-4e1f-a565-ffe3d9d570a0', foundational, honor_and_dignity_are_incommensurable_frameworks).
narrative_ontology:cs_axiom_status(honor_and_dignity_are_incommensurable_frameworks, holdable).
narrative_ontology:cs_axiom_grounding('1a3a6ba0-6f1e-4e1f-a565-ffe3d9d570a0', honor_and_dignity_are_incommensurable_frameworks, deontological).
narrative_ontology:cs_axiom('1a3a6ba0-6f1e-4e1f-a565-ffe3d9d570a0', foundational, substrate_collapse_makes_practices_incoherent).
narrative_ontology:cs_axiom_status(substrate_collapse_makes_practices_incoherent, holdable).
narrative_ontology:cs_axiom_grounding('1a3a6ba0-6f1e-4e1f-a565-ffe3d9d570a0', substrate_collapse_makes_practices_incoherent, empirically_contingent).
narrative_ontology:cs_reference_frame('1a3a6ba0-6f1e-4e1f-a565-ffe3d9d570a0', honor_code_as_natural_moral_substrate).
narrative_ontology:cs_drift_state('1a3a6ba0-6f1e-4e1f-a565-ffe3d9d570a0', contemporary_dignity_hegemony, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('1a3a6ba0-6f1e-4e1f-a565-ffe3d9d570a0', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__cultural_contraction_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__cultural_contraction_reading, dignity_centric_moral_framework).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__cultural_contraction_reading, dignity_moral_framework_carriers).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__cultural_contraction_reading, gentry_and_aristocratic_classes).
narrative_ontology:constraint_vindicates(honor_satisfaction_substrate__cultural_contraction_reading, moral_frameworks_are_mutable_cultural_artifacts).
narrative_ontology:constraint_vindicates(honor_satisfaction_substrate__cultural_contraction_reading, honor_and_dignity_are_incompatible_substrates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The dueling class itself. For centuries, honor-satisfaction through combat was inseparable from social identity and status maintenance within this group. As the honor-dignity boundary shifted, this class bore the loss of an entire legitimated action-set: dueling became unthinkable, not because it was suppressed, but because the interpretive substrate sustaining it as honorable disintegrated. They could not exit honor-culture without ceasing to be the kind of actors they understood themselves to be.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, gentry_and_aristocratic_classes, payer,
    powerful, generational, identity_locked, regional).

% Emerging intellectual classes, legal reformers, clerical authorities, and proto-professional bodies whose legitimacy rested on honor-agnostic, universalist moral claims. The dignity framework positioned them as rational arbiters of moral truth independent of genealogy or bloodline. The cultural substrate shift validated their framing and marginalized honor-based claims to authority.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, dignity_moral_framework_carriers, beneficiary,
    organized, generational, mobile, regional).

% Never held a seat in the honor system as a rule; dueling was not their practice to lose. The substrate shift affected them indirectly — as the gentry's behavioral option space contracted, the social relationship between classes reorganized. They were not part of the conversation about whether honor or dignity was the right framework.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, common_people_and_non_gentry, excluded,
    powerless, generational, trapped, regional).

% Recorded the decline in prosecutions, formalized bans, documented changing standards. Institutional recording may have been co-causal (legal suppression helped crystallize the cultural shift), but in this reading's frame, the legal apparatus is secondary to the substrate transformation — it records and reinforces rather than originates the shift.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, legal_institutional_apparatus, observer,
    institutional, generational, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_satisfaction_substrate__cultural_contraction_reading, diffuse).
narrative_ontology:fixing_cost_class(honor_satisfaction_substrate__cultural_contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The honor code coordinated intra-class conflict resolution and status maintenance: it provided a legitimated mechanism for settling disputes and repairing reputation within aristocratic society. The dignity framework replaced this with abstract, universal moral claims independent of genealogy or combat-proven worth.
% TRANSFER_FUNCTION: In the honor system, status and reputation flowed from demonstrated martial skill and willingness to defend one's honor through combat. The dignity framework redirected status claims to rational argument, institutional position, and universal moral principle — shifting who could claim authority and how.
% ABSENT_VOICES: The duelers themselves initially did not acknowledge the substrate shift; they experienced it as external suppression (legal prohibition) rather than the collapse of their own interpretive framework. The voices absent from the cultural conversation were those within the gentry who might have articulated why honor-culture was ceasing to make sense on its own terms — a reflexive self-critique that typically lags cultural transformation.
% DISAPPEARANCE_RATIONALE: If the honor-dignity substrate shift had not occurred, dueling would have persisted as a legitimate practice for centuries more, constrained but present, re-legitimated when legal suppression was removed. The actual disappearance of dueling as even a conceivable act shows that the substrate itself was what made the practice thinkable. Once the moral framework that validated honor-satisfaction through combat eroded, the practice became literally unthinkable to the class that once lived by it.
% FOUNDING_PROBLEM: Aristocratic societies required a mechanism for resolving disputes and validating status claims among equals who could not appeal to superior authority. Combat-based honor codes solved this by making the willingness to risk death a currency of status and a method of conflict resolution.
% FOUNDING_PROBLEM_CORROBORATION: Intellectual historians and moral philosophers working outside the honor-culture tradition (dignity-framework carriers) attest that the founding problem is solved by alternative mechanisms: legal adjudication, professional credentials, rational debate, institutional hierarchy. Historians of dueling (e.g., Freeman, Kiernan) document that the practice declined among the gentry not because they were forced to stop, but because it became culturally unthinkable — the substrate eroded before sustained legal suppression took hold in most jurisdictions.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__cultural_contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__cultural_contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__cultural_contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_satisfaction_substrate__cultural_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_substrate__cultural_contraction_reading, 0.12, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_substrate__cultural_contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(honor_satisfaction_substrate__cultural_contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(honor_satisfaction_substrate__cultural_contraction_reading),
    narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(honor_satisfaction_substrate__cultural_contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score declines sharply over the interval (0.52 → 0.12) because the constraint is a measure of how much the honor code continues to extract costs from those who must conform to it. At the start of the interval (pre-substrate shift), the code imposes significant costs on gentry — death-risk, social obligation, identity lock. As the substrate erodes, these costs no longer bind: dueling becomes unthinkable not because it is prohibited, but because the moral logic that made it honorable disintegrates. Suppression falls in tandem because there is no longer anything that needs suppression — the practice exits the action-set on its own. Theater_ratio stays low throughout because the honor-code never relied on performative maintenance; it was legitimated by the substrate, not by theater. Accessibility_collapse is high (0.92) because once the dignity framework takes hold, the honor-based alternative is structurally inaccessible — not blocked by law, but incoherent within the new moral framework. Resistance is low (0.08) because the shift is endogenous, not imposed — the gentry themselves eventually endorsed the dignity frame, even as they mourned the loss of honor-culture.
 *
 * PERSPECTIVAL GAP:
 *   The seat divergence arises from directionality: gentry experience high directionality toward target (they lose something fundamental to their identity), while dignity carriers experience low directionality (they gain validation without loss). Yet the underlying constraint is identical — it is the substrate itself that is collapsing. The engine computes per-seat types; the claim remains mountain erosion for all seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The gentry are identity_locked and experience the substrate shift as a loss they cannot exit — their self-understanding was fused with honor-culture. Directionality for this group runs toward the target end (~0.75) because the constraint's operation (substrate erosion) removes something fundamental to their identity. Dignity carriers are mobile and positioned as beneficiaries of the shift — the new substrate validates their authority claims. Directionality for this group runs toward beneficiary end (~0.15). The constraint itself is mountain-like (substrate erosion, not suppression), which should produce low extraction and low suppression across all seats if the substrate is genuinely eroding. The measured high accessibility_collapse (0.92) reflects that once the dignity frame takes hold, the honor alternative is not merely suppressed; it becomes rationally unthinkable, which is the signature of mountain erosion — the conditions that made the old practice coherent cease to exist.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is dead: honor-based status validation is no longer necessary in dignity-culture because alternative legitimation mechanisms (rational debate, institutional position, professional credential, law) solve the status-coordination problem differently. This reading avoids the mandatrophy trap by asserting that the constraint ceased to be extractive because the substrate itself dissolved — the practice is not zombified by institutional inertia, but rather rendered incoherent by cultural transformation. The founding_problem_status x disappearance_verdict mismatch (dead × world_rearranges) does not flag mandatrophy here because the disappearance is explained by substrate erosion, not by inertial persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substrate_shift_vs_suppression_causality,
    'Did the honor-code substrate erode endogenously (dignity framework replaced it in the gentry''s own moral reasoning), or did legal suppression trigger the reinterpretation?',
    'Counterfactual comparison: jurisdictions where dueling was legally permitted longer than others. If dueling persists longer in permissive jurisdictions, suppression was not the primary driver; if it declines at similar rates regardless of legality, the substrate shift was endogenous.',
    'If suppression was primary (practice_decline_reading), the constraint should be reclassified as snare (coercion extracting from the gentry). If substrate shift was primary (this reading), it remains mountain (the substrate dissolved, making dueling unthinkable). If mixed (composite_overdetermined_reading), the causal structure requires a different analysis entirely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substrate_shift_vs_suppression_causality, empirical, 'Whether the honor-code substrate eroded endogenously or was suppressed exogenously.').

omega_variable(
    dignity_framework_coherence,
    'Is the dignity framework genuinely incommensurable with honor-culture, or is it a repackaging of honor-based claims (e.g., ''dignified behavior'' as a new form of status currency)?',
    'Philosophical analysis of the axioms underlying each framework; ethnographic/historical comparison of status-validation mechanisms in honor vs. dignity cultures.',
    'If dignity is truly incommensurable, the frameworks cannot coexist, and substrate shift is the only coherent explanation for dueling''s disappearance. If dignity is a repackaging, the constraint is better understood as cultural reframing of persistent honor-logic — a piton (theatrical shift masking continuity) rather than mountain erosion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dignity_framework_coherence, conceptual, 'Whether honor and dignity are incommensurable moral frameworks or variations on a persistent status-logic.').

omega_variable(
    gentry_identity_lock_mechanism,
    'What was the specific identity-fusion mechanism binding the gentry to honor-culture? Was it professional identity (being a soldier/dueler), relational identity (honor in dyadic relationships), ideological identity (worldview), or institutional identity (belonging to an honor-based class)?',
    'Historical record of gentry self-descriptions, personal correspondence, and narratives of cultural loss or adaptation. Comparative ethnography of identity-lock in other cultural transitions.',
    'Different identity-lock mechanisms suggest different trajectories for cultural substrate collapse. If primarily professional, the transition could be facilitated by alternative status-earning paths (military promotion by merit). If primarily ideological or relational, the transition is more disruptive and slower. This affects whether the measured accessibility_collapse (0.92) is appropriate or should be modulated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gentry_identity_lock_mechanism, empirical, 'The specific mechanism binding gentry identity to honor-culture.').

omega_variable(
    false_summit_mountain_candidate,
    'Is the honor-code substrate collapse a genuinely natural process (mountain), or do identifiable beneficiaries (dignity-framework carriers, institutional authorities) accrue benefit from the shift in ways that make this a constructed constraint with false-summit characteristics?',
    'Check whether dignity-framework carriers actively promoted the shift or merely benefited from it. Did intellectual classes orchestrate the reframing (extraction-beneficiary structure), or did the shift emerge from broader social forces (mountain)?',
    'If beneficiaries actively promoted dignity-culture knowing it would displace honor-culture, the constraint is not a mountain but a tangled_rope or snare (coordination disguising extraction). If the shift was largely unintended from any seat''s perspective, it remains mountain erosion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(false_summit_mountain_candidate, empirical, 'Whether the substrate shift was endogenous/natural or engineered by beneficiary classes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__cultural_contraction_reading, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(hono_tr_t0, observed).
narrative_ontology:measurement(hono_tr_t50, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 50, 0.12).
narrative_ontology:measurement_basis(hono_tr_t50, observed).
narrative_ontology:measurement(hono_tr_t100, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 100, 0.08).
narrative_ontology:measurement_basis(hono_tr_t100, observed).
narrative_ontology:measurement(hono_tr_t150, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 150, 0.06).
narrative_ontology:measurement_basis(hono_tr_t150, observed).
narrative_ontology:measurement(hono_tr_t200, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 200, 0.05).
narrative_ontology:measurement_basis(hono_tr_t200, observed).
narrative_ontology:measurement(hono_tr_t300, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 300, 0.05).
narrative_ontology:measurement_basis(hono_tr_t300, observed).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(hono_be_t0, observed).
narrative_ontology:measurement(hono_be_t50, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 50, 0.42).
narrative_ontology:measurement_basis(hono_be_t50, observed).
narrative_ontology:measurement(hono_be_t100, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 100, 0.28).
narrative_ontology:measurement_basis(hono_be_t100, observed).
narrative_ontology:measurement(hono_be_t150, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 150, 0.18).
narrative_ontology:measurement_basis(hono_be_t150, observed).
narrative_ontology:measurement(hono_be_t200, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 200, 0.12).
narrative_ontology:measurement_basis(hono_be_t200, observed).
narrative_ontology:measurement(hono_be_t300, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 300, 0.12).
narrative_ontology:measurement_basis(hono_be_t300, observed).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(hono_su_t0, observed).
narrative_ontology:measurement(hono_su_t50, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 50, 0.28).
narrative_ontology:measurement_basis(hono_su_t50, observed).
narrative_ontology:measurement(hono_su_t100, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 100, 0.22).
narrative_ontology:measurement_basis(hono_su_t100, observed).
narrative_ontology:measurement(hono_su_t150, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 150, 0.18).
narrative_ontology:measurement_basis(hono_su_t150, observed).
narrative_ontology:measurement(hono_su_t200, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 200, 0.15).
narrative_ontology:measurement_basis(hono_su_t200, observed).
narrative_ontology:measurement(hono_su_t300, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 300, 0.15).
narrative_ontology:measurement_basis(hono_su_t300, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_substrate__cultural_contraction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_satisfaction_substrate__cultural_contraction_reading, 0.08).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__cultural_contraction_reading, honor_satisfaction_substrate__practice_decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__cultural_contraction_reading, honor_satisfaction_substrate__composite_overdetermined_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the honor_satisfaction_substrate kernel family. The family decomposes the natural-language concept 'decline of dueling' into three structurally distinct readings: practice_decline_reading (honor persists as substrate; dueling suppressed exogenously), cultural_contraction_reading (substrate erodes endogenously; dueling becomes unthinkable), and composite_overdetermined_reading (both mechanisms operate non-independently). Each reading has its own ε, beneficiary/victim structure, and classification. They are linked by network.affects_constraints because they share a referent (dueling's decline) but instantiate different constraints due to different causal structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_satisfaction_substrate__cultural_contraction_reading, powerful, 0.72).
constraint_indexing:directionality_override(honor_satisfaction_substrate__cultural_contraction_reading, organized, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
