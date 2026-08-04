% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_violence_legitimacy__contraction_reading, []).

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
 *   constraint_id: honor_violence_legitimacy__contraction_reading
 *   human_readable: Honor Legitimacy Contraction: Violence Exclusion from Honor Concept
 *   domain: social/legal/identity
 *
 * SUMMARY:
 *   Between 1650 and 1850, dueling—a practice through which honor was
 *   defended by personal combat—transitioned from structurally legitimate to
 *   structurally unthinkable. This constraint models the reading where the
 *   transition occurs via CONCEPTUAL REDEFINITION: the idea of honor itself
 *   contracted to exclude violence as a legitimate honor response.
 *   Enlightenment intellectual authority (philosophers, salons, literary
 *   figures), ecclesiastical authority (Church teaching on virtue), and state
 *   legal apparatus (codified criminalization) jointly advanced a new frame
 *   in which personal lethal violence is incompatible with honor. The
 *   aristocratic warrior class, for whom honor was constitutive of identity
 *   and dueling was the traditionally legitimate means of honor-defense,
 *   faces a constraint: they cannot defend their honor through the mechanism
 *   that previously defined it. The new honor frame benefits the intellectual
 *   class (as adjudicators of legitimacy), the state (monopoly on violence),
 *   and rising merchant/professional classes (included in the new honor
 *   frame). It extracts from the warrior class (exit is identity-locked) and
 *   excludes from voice those who never participated in the old frame. The
 *   constraint is claimed as 'rope' (genuine coordination function:
 *   stabilizes a new shared honor understanding) but the structural
 *   relationship between beneficiary seats and target seats is asymmetric and
 *   the transition was not voluntary for all parties—this is why metrics
 *   describe a substantially extractive operation despite the coordination
 *   function.
 *
 * KEY AGENTS:
 *   - Enlightenment intellectual class: agenda-setters, redefine honor through publishing and salon discourse
 *   - State legal apparatus: agenda-setters, embed the redefinition in criminal law and jurisprudence
 *   - Aristocratic warrior class: payer, identity-locked, cannot defend honor through dueling without losing honorability
 *   - Merchant/professional rising class: beneficiary, gain status inclusion in the new honor frame
 *   - Religious institutional authority: beneficiary and agenda-setter, reinforce contraction through moral teaching
 *   - Analytical observer: measures the structure of conceptual redefinition and its social consequences
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__contraction_reading, 0.31).
domain_priors:suppression_score(honor_violence_legitimacy__contraction_reading, 0.18).
domain_priors:theater_ratio(honor_violence_legitimacy__contraction_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, accessibility_collapse, 0.76).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__contraction_reading, rope).
narrative_ontology:human_readable(honor_violence_legitimacy__contraction_reading, "Honor Legitimacy Contraction: Violence Exclusion from Honor Concept").
narrative_ontology:topic_domain(honor_violence_legitimacy__contraction_reading, "social/legal/identity").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__contraction_reading, 'cc10f32c-2683-4330-a41b-12bb398fcd38').
narrative_ontology:cs_kernel_codification('cc10f32c-2683-4330-a41b-12bb398fcd38', distributed).
narrative_ontology:cs_authority_grounding('cc10f32c-2683-4330-a41b-12bb398fcd38', extraction).
narrative_ontology:cs_interpretation_layer_present('cc10f32c-2683-4330-a41b-12bb398fcd38').
narrative_ontology:cs_reading_relation('cc10f32c-2683-4330-a41b-12bb398fcd38', honor_violence_legitimacy__drop_reading, coexists_with).
narrative_ontology:cs_reading_relation('cc10f32c-2683-4330-a41b-12bb398fcd38', honor_violence_legitimacy__composite_reading, coexists_with).
narrative_ontology:cs_axiom('cc10f32c-2683-4330-a41b-12bb398fcd38', foundational, honor_incompatible_with_personal_violence).
narrative_ontology:cs_axiom_status(honor_incompatible_with_personal_violence, holdable).
narrative_ontology:cs_axiom_grounding('cc10f32c-2683-4330-a41b-12bb398fcd38', honor_incompatible_with_personal_violence, deontological).
narrative_ontology:cs_axiom('cc10f32c-2683-4330-a41b-12bb398fcd38', foundational, intellectual_discourse_constitutive_honor_frame).
narrative_ontology:cs_axiom_status(intellectual_discourse_constitutive_honor_frame, holdable).
narrative_ontology:cs_axiom_grounding('cc10f32c-2683-4330-a41b-12bb398fcd38', intellectual_discourse_constitutive_honor_frame, conventional).
narrative_ontology:cs_reference_frame('cc10f32c-2683-4330-a41b-12bb398fcd38', honor_through_martial_prowess).
narrative_ontology:cs_drift_state('cc10f32c-2683-4330-a41b-12bb398fcd38', enlightenment_intellectual_ascendance, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('cc10f32c-2683-4330-a41b-12bb398fcd38', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, enlightenment_intellectual_class).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, state_monopoly_on_violence).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, merchant_professional_class).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, religious_institutional_authority).
narrative_ontology:constraint_victim(honor_violence_legitimacy__contraction_reading, aristocratic_warrior_class).
narrative_ontology:constraint_victim(honor_violence_legitimacy__contraction_reading, lower_nobility_gentry).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__contraction_reading, honor_incompatible_with_personal_lethal_violence).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__contraction_reading, rational_discourse_as_honor_substitute).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Redefines honor through published philosophy, literary salons, and epistolary networks as incompatible with personal violence. Sets the conceptual frame that makes dueling intelligible as honor-destroying rather than honor-defending. Benefits from elevated social status as arbiters of legitimacy and from alignment with rising state authority. Controls the discourse machinery that adjudicates what counts as honorable.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, enlightenment_intellectual_class, agenda_setter,
    institutional, generational, arbitrage, continental).

% Consolidates claim to legitimate violence by reframing personal dueling as illegitimate and dishonorable. As dueling becomes conceptually unthinkable as an honor response, the state's monopoly on justice and legitimate force strengthens without requiring direct suppression of the practice—the practice exits the legitimacy frame entirely. Benefits from reduced vigilante violence and clearer territorial control.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, state_monopoly_on_violence, beneficiary,
    institutional, civilizational, arbitrage, continental).

% Historically defended honor through personal combat; dueling was a direct honor response to insult. As the concept of honor itself contracts to exclude violence, their traditional means of defending status becomes not just dangerous but conceptually dishonorable—they can no longer maintain honor through the mechanism that previously defined it. Exit is difficult because honor is constitutive of their identity and social role; they cannot simply adopt the new frame without dissolution of their status position.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, aristocratic_warrior_class, payer,
    powerful, biographical, identity_locked, continental).

% Rise in social status as honor redefinition privileges intellectual and professional accomplishment over martial prowess. Their honor can now be defended through reputation, economic success, and public standing rather than combat. No longer structurally disadvantaged by exclusion from aristocratic dueling; instead benefit from a new honor frame that includes them as legitimate participants. Can exit dueling without loss of status because they were never primarily locked into it.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, merchant_professional_class, beneficiary,
    organized, biographical, mobile, continental).

% Church authority reinforces honor redefinition by declaring violence-as-honor incompatible with Christian virtue. Provides moral legitimacy to the contraction frame. Benefits from stronger alignment with state authority and from reframing moral questions about honor as falling under ecclesiastical adjudication rather than aristocratic self-judgment.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, religious_institutional_authority, beneficiary,
    institutional, civilizational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(honor_violence_legitimacy__contraction_reading, religious_institutional_authority, agenda_setter).

% Codifies the honor-violence exclusion through law: statutes criminalize dueling, courts treat dueling as murder rather than legitimate dispute resolution. Embeds the conceptual contraction in enforceable doctrine. Benefits from centralization of conflict adjudication and from legitimacy derived from alignment with the new honor frame.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, legal_institutional_apparatus, agenda_setter,
    institutional, generational, arbitrage, continental).

% Were never included in the honorable-dueling frame; dueling was an exclusively male aristocratic practice. The contraction of honor to exclude violence does not directly affect them but maintains their structural exclusion from honor-defending mechanisms altogether. They would have alternative perspectives on honor redefinition but are not party to the discourse that defines it.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, women_and_excluded_classes, excluded,
    powerless, generational, trapped, continental).

% Less insulated than high aristocracy by wealth or power; dueling was economically and socially risky. The honor redefinition permits exit from dueling without status loss, but the transition period creates vulnerability: caught between the old frame (where refusing a challenge is cowardice) and the new frame (where accepting a challenge is dishonorable). Exit is constrained by the mismatch between their current position and the emerging legitimacy standard.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, lower_nobility_gentry, payer,
    moderate, biographical, constrained, continental).

% Historian or sociologist examining the constraint: how does a concept (honor) become redefined such that a previously legitimate response (dueling) becomes structurally unthinkable? What are the conditions for this kind of conceptual contraction? Does the contraction proceed via force, via intellectual persuasion, via institutional alignment, or via generational replacement?
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, observing_analytical_authority, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_violence_legitimacy__contraction_reading, enlightenment_intellectual_class).
narrative_ontology:fixing_cost_class(honor_violence_legitimacy__contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stabilizes a new shared understanding of what counts as honorable conduct, replacing physical combat with intellectual and professional achievement as the legitimate arena for honor. Coordinates the transition from a martial to a civilian honor frame. Enables new populations (merchants, professionals, intellectuals) to participate in honorable status maintenance.
% TRANSFER_FUNCTION: Transfers the adjudication of honor disputes from private (dueling) to institutional (legal/ecclesiastical/intellectual) channels. Moves the power to define and defend honor from individual aristocratic warriors to the collective institutional apparatus (state, church, intellectual class). Individuals lose direct control over their honor response and must submit to institutional judgment.
% ABSENT_VOICES: Women and lower classes who were never included in dueling but are now excluded from the new honor frame as well; they have no voice in redefining honor. Dueling practitioners themselves (actual aristocratic warriors) are not in the rooms where the redefinition happens—they discover it as fait accompli. Alternative honor frameworks (martial, combat-based, or non-state-aligned) are suppressed by the framing authority and do not get articulated as options.
% DISAPPEARANCE_RATIONALE: If the honor-violence exclusion vanished and dueling became again conceptually coherent as an honor response, the entire institutional apparatus built around non-violent honor adjudication would need reconceptualization. The state's monopoly on legitimate violence would be challenged. Professional and merchant status achievement would lose its honor legitimacy. The world does not revert—the cognitive frame is path-dependent—but the institutional and social organizing principle would shift substantially.
% FOUNDING_PROBLEM: Aristocratic personal combat (dueling) created social disruption, waste of martial talent, and challenged state monopoly on legitimate violence. Honor, as then understood, made dueling rationally coherent as a response to insult. The founding problem: how to eliminate dueling without denying honor itself?
% FOUNDING_PROBLEM_CORROBORATION: Historians document the problem as persistent into the 18th-19th centuries (Weinberg, McAleer, Freeman on dueling epidemiology). Legal reformers testify to the challenge of making dueling illegal without making honor-defense illegal. The state apparatus (through legal codification and enforcement records) attests the ongoing attempts to suppress the practice. Independent sources (travel narratives, diplomatic records, court documents) confirm dueling remained socially salient and practically harmful across the target interval. The framing as 'founding problem' comes from outside the benefiting parties—from the reform rhetoric itself and from historical reconstruction.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__contraction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-03',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(honor_violence_legitimacy__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_violence_legitimacy__contraction_reading, 0.31, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__contraction_reading_tests).
:- end_tests(honor_violence_legitimacy__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored as 0.31 (moderate, not high) because the constraint's primary function is genuinely coordinative—it stabilizes a new shared understanding of honor. However, the asymmetry in who benefits (institutional/merchant seats) versus who bears costs (warrior class with identity-lock) is real, and the transition was not negotiated as a voluntary exchange. Theater is low (0.22) because the intellectual and legal redefinition is substantive, not merely performative—the concept actually changes, and the practice actually ceases. But theater is not negligible because maintenance requires ongoing assertion that violence is dishonorable, institutional reinforcement through law and teaching, and periodic reassertion in a transition period when the old frame has not fully disappeared. Suppression is very low (0.18) because the constraint does not require active coercion once the conceptual frame is established; it works through redefinition rather than through force. Accessibility_collapse is high (0.76) because once the new honor frame is established, alternatives (dueling as an honor response) collapse almost completely—people cannot coherently maintain both that violence is dishonorable and that dueling is honorable. Resistance is moderate (0.42) because during the transition period, the warrior class mounts real resistance (they continue dueling, defend it through old-frame rhetoric, view the redefinition as dishonoring), but by the end of the interval the resistance has been largely absorbed into historical memory rather than active practice. The measurements show a rising trajectory across base_extractiveness, theater_ratio, and suppression_requirement—not because coercion is intensifying, but because institutional machinery is being built and the new frame is being codified. By 1850, the extraction has stabilized (leveling off), theater has stabilized (the initial legitimacy-building work is done, though periodic reinforcement continues), and suppression_requirement has leveled (the concept is now self-maintaining through cultural transmission).
 *
 * PERSPECTIVAL GAP:
 *   From the intellectual and state seats, this is a success story: the new honor frame is more rational, less wasteful, more inclusive, and aligns with state authority. From the warrior class seat, it is a loss: their traditional means of honor-defense is now dishonorable, and they cannot exit without identity dissolution. A low-nobility or merchant seat might experience it as a rise in status (they are now included as legitimate honor-defendants). The engine computes different directionalites from these asymmetric positions: the intellectual/state seats get d near 0.0-0.3 (beneficiaries), the warrior class gets d near 0.7-1.0 (target), the merchant/rising class gets d near 0.4-0.6 (symmetric or slightly beneficiary). These divergent classifications follow from the declared beneficiary/victim structure and exit options without need for explicit overrides—the structural asymmetry generates the perspectival gap automatically.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are: (1) enlightenment_intellectual_class (agenda-setter, benefits from authority to define legitimacy, directionality ~0.15), (2) state_monopoly_on_violence (beneficiary, consolidates monopoly claim, directionality ~0.25). Payers are: (1) aristocratic_warrior_class (identity-locked exit, loses traditional honor mechanism, directionality ~0.85), (2) lower_nobility_gentry (constrained exit, caught in frame mismatch, directionality ~0.65). Secondary beneficiaries are: merchant_professional_class (mobile exit, gains status inclusion, directionality ~0.35). The derived directionality for the warrior class is high because they are structurally targeted (the redefinition directly forecloses their traditional practice) and their exit is identity-locked (they cannot leave the constraint without losing the identity that the constraint defines). No overrides are necessary—the structure generates the correct directionality through the derivation chain.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (dueling creates social disruption and challenges state violence monopoly) remains live at the interval endpoint—dueling has not reappeared as a structurally normal practice, suggesting the constraint is not mandatrophic. However, a secondary question emerges: is the founding problem SOLVED, or merely MANAGED? If the real problem was 'aristocratic warrior class has too much independent power', then redefining honor to exclude violence solves it by making warrior-class power-defense illegitimate. If the problem was 'dueling is wasteful', it is similarly solved. But if the problem was 'how do we eliminate dueling without denying honor', then the solution is mandatrophic in a subtle way: it redefines honor such that dueling was never as legitimate as it seemed—the solution is retrospectively dissolving the problem by reframing it. This is not classic mandatrophy (the problem stays the same but we can't change the rule), but it is a constitutional-reframing variant: the problem exits the frame because we redefined the frame. The authored claim_type is 'rope' (genuine coordination function), which accepts this reframing as legitimate. A different reading might classify the constraint as a 'snare' (pure extraction using the language of honor-redefinition as cover for warrior-class suppression) or as a 'tangled_rope' (genuine coordination with asymmetric extraction). The claim/metric divergence is where the engine detects whether the reframing is coordinative or extractive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_vs_drop_mechanism,
    'Does dueling decline because the concept of honor itself contracted to exclude violence (contraction_reading), or because external costs (legal penalties, social stigma from institutional pressure) made the practice practically rare while the legitimacy frame remained unchanged (drop_reading)?',
    'Textual and behavioral evidence: (1) Do philosophical and legal sources REDEFINE honor to exclude violence, or do they merely PENALIZE dueling while leaving the legitimacy concept intact? (2) Do late-period duelists defend their action by appealing to honor, or do they apologize for acting dishonorable? (3) Do generational cohorts born after the redefinition treat dueling as literally unthinkable, or as forbidden-but-still-honorable?',
    'If contraction, the constraint is a coordinative redefinition of shared concepts with modest extraction (beneficiaries are the institutional framers and the state; costs are diffuse on the warrior class). If drop, dueling remains legitimacy-coherent but suppressed—the constraint would be more extractive (active coercion required) and the terminal state would be unstable (suppression must continue). This reading assumes contraction; the alternative reading (drop) is a sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_drop_mechanism, empirical, 'Whether dueling decline is caused by conceptual redefinition or by external coercion.').

omega_variable(
    identity_lock_dissolution,
    'For the aristocratic warrior class, is the exit from dueling a genuine choice (they could adopt the new honor frame without loss of identity), or is their identity so fused with martial honor-defense that exit requires identity dissolution?',
    'Biographical and legal records: Do late-period aristocrats renarrate their identity as compatible with the new honor frame (professional, intellectual, political achievement as substitute for martial prowess)? Or do they experience the transition as forced loss of identity, documented in letters, suicide rates, or withdrawal from public life during the transition period?',
    'If genuine choice, their exit_options should be rated higher (mobile or constrained, not identity_locked)—the constraint is less extractive from their seat because they can reorganize their self-concept. If identity dissolution, exit_options remain identity_locked, the constraint is highly extractive for them, and the measurement should show higher resistance from this seat during the transition. The authored identity_locked reflects the assumption that honor is constitutive of aristocratic identity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_dissolution, empirical, 'Whether the warrior class experiences identity continuity or identity dissolution during honor redefinition.').

omega_variable(
    composite_vs_pure_contraction,
    'Is the honor-violence exclusion a PURE conceptual redefinition (contraction_reading), or is it OVERDETERMINED by simultaneous legal suppression and intellectual redefinition (composite_reading)?',
    'Historical counterfactual: Could the intellectual redefinition have succeeded without legal backing? Did the concept contract first (making suppression unnecessary) or did legal suppression drive the conceptual shift? Timeline analysis: which moved first—intellectual discourse or legal codification?',
    'If pure contraction, the coordination frame alone sustains the constraint (low suppression_requirement, modest theater_ratio). If overdetermined, both intellectual and legal machinery operate redundantly—suppression would be higher, theater_ratio would reflect the performative aspect of maintaining both. The authored measurements assume modest suppression (0.18 at endpoint), consistent with contraction; a composite reading would show higher suppression and rising theater.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(composite_vs_pure_contraction, conceptual, 'Whether the honor redefinition is causally independent or overdetermined by simultaneous legal suppression.').

omega_variable(
    kernel_reading_ambiguity,
    'This constraint is one reading of the contested kernel ''honor_violence_legitimacy''. Sibling readings (drop_reading, composite_reading) decompose the same phenomenon through different explanatory frames. Is the kernel-level question resolvable, or is it constitutively ambiguous?',
    'Evidence hierarchy: (1) Internal consistency of each reading against primary sources. (2) Cross-reading coverage: can the drop_reading explain all observations this reading explains, and vice versa? (3) Observer independence: do historians from different schools converge or diverge in how they frame the causal mechanism?',
    'If resolvable, one reading terminates as the canonical constraint story and siblings become historical errors. If ambiguous, all three readings remain live, each capturing a different structural element of the historical transition. This authoring assumes contraction_reading is structurally coherent and empirically supportable; the presence of sibling readings reflects the kernel''s irreducible interpretive contest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the honor-violence-legitimacy kernel has a single correct reading or remains constitutively contested.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__contraction_reading, 1650, 1850).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1650, honor_violence_legitimacy__contraction_reading, theater_ratio, 1650, 0.05).
narrative_ontology:measurement_basis(hono_tr_t1650, projected).
narrative_ontology:measurement(hono_tr_t1700, honor_violence_legitimacy__contraction_reading, theater_ratio, 1700, 0.08).
narrative_ontology:measurement_basis(hono_tr_t1700, observed).
narrative_ontology:measurement(hono_tr_t1750, honor_violence_legitimacy__contraction_reading, theater_ratio, 1750, 0.12).
narrative_ontology:measurement_basis(hono_tr_t1750, observed).
narrative_ontology:measurement(hono_tr_t1800, honor_violence_legitimacy__contraction_reading, theater_ratio, 1800, 0.18).
narrative_ontology:measurement_basis(hono_tr_t1800, observed).
narrative_ontology:measurement(hono_tr_t1825, honor_violence_legitimacy__contraction_reading, theater_ratio, 1825, 0.2).
narrative_ontology:measurement_basis(hono_tr_t1825, observed).
narrative_ontology:measurement(hono_tr_t1850, honor_violence_legitimacy__contraction_reading, theater_ratio, 1850, 0.22).
narrative_ontology:measurement_basis(hono_tr_t1850, observed).

% Extraction over time
narrative_ontology:measurement(hono_be_t1650, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1650, 0.08).
narrative_ontology:measurement_basis(hono_be_t1650, projected).
narrative_ontology:measurement(hono_be_t1700, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1700, 0.14).
narrative_ontology:measurement_basis(hono_be_t1700, observed).
narrative_ontology:measurement(hono_be_t1750, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1750, 0.22).
narrative_ontology:measurement_basis(hono_be_t1750, observed).
narrative_ontology:measurement(hono_be_t1800, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1800, 0.28).
narrative_ontology:measurement_basis(hono_be_t1800, observed).
narrative_ontology:measurement(hono_be_t1825, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1825, 0.3).
narrative_ontology:measurement_basis(hono_be_t1825, observed).
narrative_ontology:measurement(hono_be_t1850, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1850, 0.31).
narrative_ontology:measurement_basis(hono_be_t1850, observed).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1650, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1650, 0.05).
narrative_ontology:measurement_basis(hono_su_t1650, projected).
narrative_ontology:measurement(hono_su_t1700, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1700, 0.08).
narrative_ontology:measurement_basis(hono_su_t1700, observed).
narrative_ontology:measurement(hono_su_t1750, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1750, 0.1).
narrative_ontology:measurement_basis(hono_su_t1750, observed).
narrative_ontology:measurement(hono_su_t1800, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1800, 0.14).
narrative_ontology:measurement_basis(hono_su_t1800, observed).
narrative_ontology:measurement(hono_su_t1825, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1825, 0.17).
narrative_ontology:measurement_basis(hono_su_t1825, observed).
narrative_ontology:measurement(hono_su_t1850, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1850, 0.18).
narrative_ontology:measurement_basis(hono_su_t1850, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__contraction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_violence_legitimacy__contraction_reading, 0.12).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy__drop_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy__composite_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, state_violence_monopoly_consolidation).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, warrior_class_status_dissolution).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel 'honor_violence_legitimacy'. Sibling readings (drop_reading, composite_reading) decompose the same historical phenomenon through different causal frames. Contraction_reading assumes conceptual redefinition is the primary mechanism; drop_reading assumes external costs; composite_reading assumes both. All three stories must be authored to capture the kernel's irreducible interpretive contest. Contraction_reading should show: low suppression_requirement, high accessibility_collapse, moderate theater_ratio. Drop_reading should show: high suppression_requirement, moderate accessibility_collapse, higher theater_ratio (legal machinery as performance). Composite_reading should show: both extractive mechanisms present, highest suppression and theater. Each reading has different beneficiary/victim structure, different stakeholder directionalities, and different terminal states.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
