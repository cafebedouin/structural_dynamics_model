% ============================================================================
% CONSTRAINT STORY: menu_curation_capture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_menu_curation_capture, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: menu_curation_capture
 *   human_readable: Self-Selected Kill-Condition Menu Curation Capture
 *   domain: epistemology/philosophy_of_technology/institutional_analysis
 *
 * SUMMARY:
 *   As LLM-assisted generation makes producing candidate falsifiable
 *   conditions nearly free (see upstream constraint
 *   omega_production_cost_asymmetry, a genuine mountain: the cost collapse
 *   itself is a structural fact of inference economics, not a choice by any
 *   party), a new discretionary step appears downstream: selecting WHICH
 *   generated candidate to publish as one's declared kill condition. This
 *   selection step is where capture happens. A declarant with slack can
 *   generate ten candidate conditions, silently discard the nine that would
 *   actually threaten their position, and publish the one most likely to look
 *   rigorous while never firing. The documented openness (a public,
 *   falsifiable-looking commitment) increases without any corresponding
 *   increase in actual exposure. This is the instrumentalist reading's
 *   structural delta from the kernel positional_disagreement_as_evidence: it
 *   exposes an extraction path invisible to the standpoint, pragmatist, and
 *   proceduralist readings, because those readings evaluate the
 *   disagreement's epistemic status, not the cost and curation structure of
 *   the tool used to generate candidate evidence.
 *
 * KEY AGENTS:
 *   - the_declarant_with_slack: beneficiary/agenda_setter (moderate/arbitrage) — selects the least-threatening menu item and collects reputational credit for apparent rigor
 *   - the_excluded_stakeholder_e_g_the_parent: payer (powerless/trapped) — bears the real risk the declaration was supposed to expose but cannot see the discarded menu
 *   - llm_menu_generator: non-agent observer (institutional/analytical) — produces the menu cheaply, applies no adversarial pressure
 *   - adversarial_designator: excluded (powerless/analytical) — the counterfactual party who would assign a real condition, structurally absent from the process
 *   - readers_of_the_declaration: excluded (powerless/constrained) — mistake documented openness for real exposure
 *   - methodological_auditor: observer (analytical/analytical) — the only seat capable of running the comparison that would make the capture visible
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(menu_curation_capture, 0.68).
domain_priors:suppression_score(menu_curation_capture, 0.52).
domain_priors:theater_ratio(menu_curation_capture, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(menu_curation_capture, extractiveness, 0.68).
narrative_ontology:constraint_metric(menu_curation_capture, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(menu_curation_capture, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(menu_curation_capture, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(menu_curation_capture, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(menu_curation_capture, snare).
narrative_ontology:human_readable(menu_curation_capture, "Self-Selected Kill-Condition Menu Curation Capture").
narrative_ontology:topic_domain(menu_curation_capture, "epistemology/philosophy_of_technology/institutional_analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(menu_curation_capture, '96481ef3-a2aa-4a10-b8d8-d8f09c1719e7').
narrative_ontology:cs_kernel_codification('96481ef3-a2aa-4a10-b8d8-d8f09c1719e7', distributed).
narrative_ontology:cs_authority_grounding('96481ef3-a2aa-4a10-b8d8-d8f09c1719e7', distributed).
narrative_ontology:cs_reading_relation('96481ef3-a2aa-4a10-b8d8-d8f09c1719e7', menu_curation_capture__standpoint_reading, coexists_with).
narrative_ontology:cs_reading_relation('96481ef3-a2aa-4a10-b8d8-d8f09c1719e7', menu_curation_capture__pragmatist_reading, coexists_with).
narrative_ontology:cs_reading_relation('96481ef3-a2aa-4a10-b8d8-d8f09c1719e7', menu_curation_capture__proceduralist_reading, influences).
narrative_ontology:cs_axiom('96481ef3-a2aa-4a10-b8d8-d8f09c1719e7', foundational, cheap_generation_can_still_ground_legitimate_evidence).
narrative_ontology:cs_axiom_status(cheap_generation_can_still_ground_legitimate_evidence, holdable).
narrative_ontology:cs_axiom_grounding('96481ef3-a2aa-4a10-b8d8-d8f09c1719e7', cheap_generation_can_still_ground_legitimate_evidence, instrumental).
narrative_ontology:cs_axiom('96481ef3-a2aa-4a10-b8d8-d8f09c1719e7', secondary, selection_discretion_is_a_distinct_extraction_surface_from_generation_cost).
narrative_ontology:cs_axiom_status(selection_discretion_is_a_distinct_extraction_surface_from_generation_cost, holdable).
narrative_ontology:cs_axiom_grounding('96481ef3-a2aa-4a10-b8d8-d8f09c1719e7', selection_discretion_is_a_distinct_extraction_surface_from_generation_cost, empirically_contingent).
narrative_ontology:cs_reference_frame('96481ef3-a2aa-4a10-b8d8-d8f09c1719e7', expensive_falsifier_generation_as_weak_filter).
narrative_ontology:cs_drift_state('96481ef3-a2aa-4a10-b8d8-d8f09c1719e7', cheap_llm_generation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('96481ef3-a2aa-4a10-b8d8-d8f09c1719e7', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(menu_curation_capture, the_declarant_with_slack).
narrative_ontology:constraint_victim(menu_curation_capture, the_excluded_stakeholder_e_g_the_parent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Has time, tooling, and survivable public error to run an LLM-generated menu of candidate kill conditions, then select the one that reads as maximally falsifiable while carrying the lowest actual probability of firing. Publishes the selection as an act of epistemic virtue — 'I precommitted to a condition that would prove me wrong' — while the underlying selection step, which determined which condition ever reached the page, is invisible to any reader of the final declaration. Bears essentially no downside: if the chosen condition never fires, credit accrues for rigor; if by chance it does fire, the declarant absorbs a survivable reputational cost because they have slack to spare.
narrative_ontology:constraint_stakeholder(menu_curation_capture, the_declarant_with_slack, beneficiary,
    moderate, biographical, arbitrage, local).
narrative_ontology:stakeholder_secondary_role(menu_curation_capture, the_declarant_with_slack, agenda_setter).

% Is the party whose interests the declared arrangement is actually about (e.g., the parent whose custody, care, or welfare claim depends on the declarant's honesty), but has no access to the generation step, no visibility into the discarded menu items, and no standing to propose an adversarial kill condition of their own. Experiences the arrangement as a closed loop: the declarant's public precommitment looks like accountability from outside, but the condition was chosen specifically because it was unlikely to expose the declarant's actual behavior. Cannot audit what they cannot see generated.
narrative_ontology:constraint_stakeholder(menu_curation_capture, the_excluded_stakeholder_e_g_the_parent, payer,
    powerless, biographical, trapped, local).

% Produces the candidate menu of falsifiable conditions cheaply and at scale on request. It has no stake in which item is selected and applies no adversarial pressure of its own — it is agreeable by design, generating plausible-sounding conditions across a range of stringency without ranking them by actual threat to the requester. It is the mechanism that makes curation possible, not an actor with an interest in the outcome.
narrative_ontology:constraint_stakeholder(menu_curation_capture, llm_menu_generator, observer,
    institutional, immediate, analytical, global).
narrative_ontology:stakeholder_non_agent(menu_curation_capture, llm_menu_generator).

% Represents whoever would assign a kill condition FROM OUTSIDE the declarant's control — an adversarial collaborator, a genuinely independent auditor, or the excluded stakeholder themselves, given standing. This party is not consulted; the whole point of self-selection from a self-generated menu is that no external party gets to pick the condition. If this role were filled, the empirical firing rate of the resulting condition would almost certainly differ from the self-selected one — that gap is the constraint's signature.
narrative_ontology:constraint_stakeholder(menu_curation_capture, adversarial_designator, excluded,
    powerless, immediate, analytical, local).

% See only the final, published kill condition and take its existence as evidence of epistemic honesty — the presence of ANY declared falsifier reads as rigor by convention, regardless of how it was selected. They have no way to compare the published condition against the discarded menu or against what an adversarial party would have assigned, so the documented openness is consumed as if it were real exposure.
narrative_ontology:constraint_stakeholder(menu_curation_capture, readers_of_the_declaration, excluded,
    powerless, immediate, constrained, national).

% Can, in principle, run the comparison the constraint's own observable calls for: self-selected menu conditions vs. unassisted self-generated conditions vs. adversarially-assigned conditions, then compare empirical firing rates across the three sets. This comparison is the only mechanism that would make the capture visible; absent it, the capture is invisible by construction because only the winning condition survives to publication.
narrative_ontology:constraint_stakeholder(menu_curation_capture, methodological_auditor, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(menu_curation_capture, the_declarant_with_slack).
narrative_ontology:fixing_cost_class(menu_curation_capture, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Declaring any falsifiable kill condition at all is a genuine improvement over declaring none — it creates public accountability infrastructure, commits the declarant to a checkable claim, and in principle lets outside parties monitor whether the declarant's confidence was warranted. The LLM menu genuinely lowers the cost of generating candidate conditions, which is a real service.
% TRANSFER_FUNCTION: Moves reputational credit (for rigor, honesty, epistemic virtue) to the declarant, funded by readers' and stakeholders' mistaken inference that a declared condition is representative of real exposure. The excluded stakeholder pays in the form of unmonitored risk that persists precisely because the visible accountability mechanism was tuned not to catch it.
% ABSENT_VOICES: The excluded stakeholder (e.g. the parent whose welfare depends on the declarant's honesty) never sees the discarded menu items and has no standing to propose or demand an adversarially-assigned condition. An independent auditor who would run the three-way comparison (self-selected vs. self-generated-unassisted vs. adversarial) is not part of the declaration process and is not compensated or empowered to be.
% DISAPPEARANCE_RATIONALE: If curated self-selection from an LLM menu disappeared and declarants had to either generate conditions unassisted (harder to make deceptively safe) or accept adversarially-assigned conditions, the empirical firing rate of published kill conditions would very likely rise, the reputational credit currently captured by low-risk declarations would evaporate, and excluded stakeholders would gain a real, not merely documented, accountability lever.
% FOUNDING_PROBLEM: Before cheap LLM generation, producing even one candidate falsifiable condition required real intellectual labor, which itself functioned as a weak filter against trivially safe conditions — generating a menu was expensive enough that most declarants settled for whatever condition they first thought of, which was not systematically curated for safety.
% FOUNDING_PROBLEM_CORROBORATION: The methodological auditor role, if actually exercised, would corroborate from outside the declarant's own account by running the three-set firing-rate comparison; no such comparison has yet been conducted in the cases this constraint describes, so at present the founding problem's obsolescence is asserted structurally (falling generation cost is independently documented, per the omega_production_cost_asymmetry upstream constraint) but not yet empirically corroborated by any party outside the declarant.
narrative_ontology:disappearance_verdict(menu_curation_capture, world_rearranges).
narrative_ontology:founding_problem_status(menu_curation_capture, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(menu_curation_capture, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-14',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(menu_curation_capture, 'none', 1).
narrative_ontology:epsilon_provenance(menu_curation_capture, 0.68, 'claude-sonnet-5', 'cheap_confession_2026_20260814_151329', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(menu_curation_capture_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(menu_curation_capture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(menu_curation_capture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction rises from 0.3 to 0.68 over the interval as menu-based declaration becomes normalized: early declarants had less menu depth to curate from (LLM generation quality and volume were lower), so early selections were closer to unassisted self-generation. As menu generation gets cheaper and richer (per the upstream mountain), the gap between the safest available candidate and the median candidate widens, and declarants increasingly select from the safe tail. Theater ratio tracks closely alongside extractiveness (0.35 to 0.71) because the entire visible apparatus — a published, falsifiable-looking condition — increasingly performs accountability while the underlying exposure it is supposed to represent does not rise correspondingly. Suppression (0.52) is moderate rather than high: nothing coercively prevents the excluded stakeholder from demanding an adversarial condition, but they lack the standing, tooling, and visibility to do so — this is exclusion by informational and procedural asymmetry, not by force. Accessibility collapse (0.4) is comparatively low because the alternative (unassisted self-generation, adversarial assignment) remains conceptually available and is not suppressed by rule — it is simply not chosen, which is exactly why resistance (0.55) sits at moderate rather than high: the capture is legible to anyone who looks at the comparison, but almost no one runs it.
 *
 * PERSPECTIVAL GAP:
 *   From the_declarant_with_slack's seat, publishing a falsifiable-looking condition IS the accountability act — they experience genuine effort (menu generation, drafting, publication) and genuine exposure of SOME kind, however curated. From the_excluded_stakeholder's seat, the same act is a closed loop that produces the appearance of exposure while systematically avoiding the conditions that would actually test the declarant's claims. The engine should compute these as structurally different experiences of the identical published artifact — the gap is not a matter of interpretation but of what each seat can see: only the declarant sees the discarded menu.
 *
 * DIRECTIONALITY LOGIC:
 *   the_declarant_with_slack derives low d (beneficiary end): they control both generation (via the LLM) and selection (via discretion), and they exit any given declaration cheaply if it goes wrong, since a survivable reputational cost is their worst case. the_excluded_stakeholder_e_g_the_parent derives high d (target end): trapped, powerless, and structurally incapable of generating or contesting the menu themselves — the disagreement structure of the upstream kernel (positional_disagreement_as_evidence) maps directly onto who bears the cost when a curated declaration substitutes for a real one. readers_of_the_declaration sit closer to victim-adjacent despite nominal neutrality, because their misplaced trust is what makes the extraction function at all — but they are not the PRIMARY victim, since their loss is diffuse (misplaced credit-granting) rather than concentrated (as the excluded stakeholder's is).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — no cheap way to generate candidate falsifiers, which forced accountability through effort-cost — is now dead: generation is nearly free (per the upstream mountain). But the arrangement (self-selection from a self-generated menu, presented as rigor) persists and has, if anything, intensified, because the selection discretion it introduced is a NEW extraction surface that did not exist when generation was expensive. Classifying this as snare rather than tangled_rope matters here: there is a genuine coordination function (declaring SOME condition is better than none) but the story's own metrics show the coordination story functioning almost entirely as cover — the theater_ratio trajectory (0.35 to 0.71) shows the visible accountability apparatus increasingly substituting for the exposure it claims to represent, and the extractiveness trajectory tracks it in lockstep rather than diverging from it, which is the signature of extraction using coordination as its cover story rather than genuine hybrid function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    curation_visibility_gap,
    'Would empirical firing rates actually differ across self-selected-menu conditions, unassisted self-generated conditions, and adversarially-assigned conditions — or is the suspected curation effect itself an artifact of selection bias in which cases get studied?',
    'Run the three-way comparison the constraint''s own primary observable calls for: collect a corpus of declared kill conditions produced under each regime, and measure actual firing rates against a matched set of ground-truth outcomes.',
    'If firing rates are statistically indistinguishable across the three sets, the curation-capture hypothesis fails and this constraint should be reclassified toward rope or tangled_rope (documented openness would then track real exposure). If firing rates diverge sharply and in the predicted direction (self-selected lowest, adversarial highest), the snare classification is strongly corroborated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(curation_visibility_gap, empirical, 'Whether the suspected curation effect is empirically real or an artifact of case selection.').

omega_variable(
    menu_generator_neutrality,
    'Does the LLM menu generator itself introduce a systematic bias toward agreeable, low-threat candidate conditions independent of the declarant''s selection discretion — i.e., is some of the capture happening at generation, not just at selection?',
    'Compare menu contents generated for the same underlying claim across multiple models and prompting strategies; measure the distribution of candidate-condition stringency before any selection occurs.',
    'If generation itself is skewed toward agreeable conditions, the capture is partly a property of the tool (upstream, structural) rather than solely the declarant''s discretionary choice — this would implicate the upstream mountain constraint''s neutrality and might require decomposing menu_curation_capture further.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(menu_generator_neutrality, empirical, 'Whether generation-stage bias compounds selection-stage curation.').

omega_variable(
    coordination_extraction_separability_menu,
    'Is the coordination function (declaring SOME falsifiable condition is better than declaring none) separable from the curation extraction (choosing the safest available condition from a menu), or does curation inevitably ride along with any menu-based generation process?',
    'Test whether mandatory disclosure of the full discarded menu (not just the selected item) restores the coordination function without the extraction — if transparency about rejected candidates eliminates the empirical firing-rate gap, the functions are separable.',
    'If separable via disclosure, this points toward a scaffold-type remedy (a sunset-clause requirement to publish full menus) rather than confirming the constraint is irreducibly a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability_menu, conceptual, 'Whether requiring full menu disclosure would separate legitimate coordination from curation extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(menu_curation_capture, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(menu_tr_t0, menu_curation_capture, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(menu_tr_t0, observed).
narrative_ontology:measurement(menu_tr_t4, menu_curation_capture, theater_ratio, 4, 0.45).
narrative_ontology:measurement_basis(menu_tr_t4, observed).
narrative_ontology:measurement(menu_tr_t8, menu_curation_capture, theater_ratio, 8, 0.55).
narrative_ontology:measurement_basis(menu_tr_t8, observed).
narrative_ontology:measurement(menu_tr_t12, menu_curation_capture, theater_ratio, 12, 0.62).
narrative_ontology:measurement_basis(menu_tr_t12, observed).
narrative_ontology:measurement(menu_tr_t16, menu_curation_capture, theater_ratio, 16, 0.67).
narrative_ontology:measurement_basis(menu_tr_t16, observed).
narrative_ontology:measurement(menu_tr_t20, menu_curation_capture, theater_ratio, 20, 0.7).
narrative_ontology:measurement_basis(menu_tr_t20, projected).
narrative_ontology:measurement(menu_tr_t24, menu_curation_capture, theater_ratio, 24, 0.71).
narrative_ontology:measurement_basis(menu_tr_t24, projected).

% Extraction over time
narrative_ontology:measurement(menu_be_t0, menu_curation_capture, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(menu_be_t0, observed).
narrative_ontology:measurement(menu_be_t4, menu_curation_capture, base_extractiveness, 4, 0.4).
narrative_ontology:measurement_basis(menu_be_t4, observed).
narrative_ontology:measurement(menu_be_t8, menu_curation_capture, base_extractiveness, 8, 0.5).
narrative_ontology:measurement_basis(menu_be_t8, observed).
narrative_ontology:measurement(menu_be_t12, menu_curation_capture, base_extractiveness, 12, 0.58).
narrative_ontology:measurement_basis(menu_be_t12, observed).
narrative_ontology:measurement(menu_be_t16, menu_curation_capture, base_extractiveness, 16, 0.63).
narrative_ontology:measurement_basis(menu_be_t16, observed).
narrative_ontology:measurement(menu_be_t20, menu_curation_capture, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(menu_be_t20, projected).
narrative_ontology:measurement(menu_be_t24, menu_curation_capture, base_extractiveness, 24, 0.68).
narrative_ontology:measurement_basis(menu_be_t24, projected).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(menu_curation_capture, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(menu_curation_capture, identity_coordination).
narrative_ontology:boltzmann_floor_override(menu_curation_capture, 0.08).
narrative_ontology:affects_constraint(menu_curation_capture, omega_production_cost_asymmetry).

% DUAL FORMULATION NOTE:
% menu_curation_capture is downstream of omega_production_cost_asymmetry (claimed mountain: the falling cost of generating candidate falsifiers is a structural fact of inference economics, not a discretionary choice by any party). The upstream constraint's ε should remain low/negligible because the cost collapse itself has no beneficiary/victim structure — it is a technological fact. menu_curation_capture inherits the opportunity the cost collapse creates but adds an entirely separate, discretionary selection layer with its own beneficiary (the_declarant_with_slack) and victim (the_excluded_stakeholder). These are two distinct constraints per the epsilon-invariance principle: one measuring the cost of generation (mountain-like, negligible extraction), one measuring the selection discretion built atop it (snare-like, substantial extraction). They are linked, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(menu_curation_capture, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
