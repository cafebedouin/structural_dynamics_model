% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_substrate__cultural_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Honor-Satisfaction Substrate: Cultural Contraction (Dignity Displaces Honor)
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   This story authors ONE reading of the honor_satisfaction_substrate
 *   kernel: the cultural_contraction_reading. On this reading, the decline of
 *   dueling in the 18th-20th century West is not the story of a persisting
 *   honor-code norm being suppressed by external legal and institutional
 *   force (that is the sibling practice_decline_reading), nor of two
 *   independent causal pathways operating together (the sibling
 *   composite_overdetermined_reading). It is the story of the interpretive
 *   substrate itself eroding: the shared cultural grammar in which an insult
 *   required combat-based 'satisfaction' to repair reputation stopped being
 *   intelligible as the middle classes' dignity-based status logic (status as
 *   inherent and equal, defensible through documentation and law rather than
 *   blood) displaced it. On this reading dueling did not become forbidden so
 *   much as it became unthinkable — closer to how one does not 'refrain from'
 *   believing the earth is flat, one simply no longer has that belief
 *   available. The extraction and suppression scores are kept low and the
 *   accessibility_collapse score kept high specifically because this reading
 *   treats the transition as substrate erosion (mountain-like, naturalizing)
 *   rather than active coercive suppression of a resistant population (which
 *   would show high suppression and active enforcement, as the
 *   practice_decline_reading would author).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__cultural_contraction_reading, 0.18).
domain_priors:suppression_score(honor_satisfaction_substrate__cultural_contraction_reading, 0.12).
domain_priors:theater_ratio(honor_satisfaction_substrate__cultural_contraction_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__cultural_contraction_reading, mountain).
narrative_ontology:human_readable(honor_satisfaction_substrate__cultural_contraction_reading, "Honor-Satisfaction Substrate: Cultural Contraction (Dignity Displaces Honor)").
narrative_ontology:topic_domain(honor_satisfaction_substrate__cultural_contraction_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:emerges_naturally(honor_satisfaction_substrate__cultural_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__cultural_contraction_reading, '0cafe7b4-3580-48f8-b938-138a8e18c2b3').
narrative_ontology:cs_kernel_codification('0cafe7b4-3580-48f8-b938-138a8e18c2b3', implicit).
narrative_ontology:cs_authority_grounding('0cafe7b4-3580-48f8-b938-138a8e18c2b3', practice).
narrative_ontology:cs_interpretation_layer_present('0cafe7b4-3580-48f8-b938-138a8e18c2b3').
narrative_ontology:cs_reading_relation('0cafe7b4-3580-48f8-b938-138a8e18c2b3', honor_satisfaction_substrate__practice_decline_reading, coexists_with).
narrative_ontology:cs_reading_relation('0cafe7b4-3580-48f8-b938-138a8e18c2b3', honor_satisfaction_substrate__composite_overdetermined_reading, influences).
narrative_ontology:cs_axiom('0cafe7b4-3580-48f8-b938-138a8e18c2b3', foundational, honor_code_is_interpretive_substrate_not_persisting_norm).
narrative_ontology:cs_axiom_status(honor_code_is_interpretive_substrate_not_persisting_norm, holdable).
narrative_ontology:cs_axiom_grounding('0cafe7b4-3580-48f8-b938-138a8e18c2b3', honor_code_is_interpretive_substrate_not_persisting_norm, conventional).
narrative_ontology:cs_axiom('0cafe7b4-3580-48f8-b938-138a8e18c2b3', foundational, dignity_status_logic_displaces_rather_than_suppresses_honor_logic).
narrative_ontology:cs_axiom_status(dignity_status_logic_displaces_rather_than_suppresses_honor_logic, holdable).
narrative_ontology:cs_axiom_grounding('0cafe7b4-3580-48f8-b938-138a8e18c2b3', dignity_status_logic_displaces_rather_than_suppresses_honor_logic, empirically_contingent).
narrative_ontology:cs_reference_frame('0cafe7b4-3580-48f8-b938-138a8e18c2b3', aristocratic_honor_satisfaction_norm).
narrative_ontology:cs_drift_state('0cafe7b4-3580-48f8-b938-138a8e18c2b3', post_dignity_culture_consolidation, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('0cafe7b4-3580-48f8-b938-138a8e18c2b3', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__cultural_contraction_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__cultural_contraction_reading, professional_middle_classes).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__cultural_contraction_reading, commercial_bourgeoisie).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__cultural_contraction_reading, state_monopoly_on_violence).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__cultural_contraction_reading, aristocratic_honor_class).
narrative_ontology:constraint_vindicates(honor_satisfaction_substrate__cultural_contraction_reading, dignity_egalitarian_personhood_doctrine).
narrative_ontology:constraint_vindicates(honor_satisfaction_substrate__cultural_contraction_reading, honor_code_historical_contingency_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rising commercial and professional classes whose status depended on contract, credit, and reputation for reliability rather than blood-honor. As the interpretive frame that made an insult 'satisfaction-worthy' eroded, they gained a social world in which standing could be built and defended through institutions (courts, credentialing, commerce) rather than personal combat. They did not lobby to end dueling; the ground simply stopped existing under it as their numbers and cultural weight grew.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, professional_middle_classes, beneficiary,
    organized, generational, mobile, national).

% Centralizing states benefited as private lethal violence for status-repair became culturally illegible rather than merely illegal. Statutes against dueling existed for centuries before the practice actually stopped in most places; on this reading the state's role is secondary to the deeper substrate shift — it collects the benefit of pacification without having authored the cause.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, state_monopoly_on_violence, beneficiary,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__cultural_contraction_reading, state_monopoly_on_violence, observer).

% The class whose entire self-concept and social standing were constituted through honor-satisfaction logic — reputation as a fragile, publicly defended possession redeemable only through ritual combat. On this reading they are not suppressed out of dueling; the semantic ground of 'satisfaction' collapses beneath them. What they lose is not a right but an intelligible world: by the time dueling is illegal almost everywhere, it has already become, for most of their own descendants, faintly absurd rather than forbidden.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, aristocratic_honor_class, payer,
    powerful, biographical, identity_locked, national).

% The specialists who administered code duello — arranging terms, certifying honor satisfied, adjudicating insults — had no voice in the transformation. Their expertise depended entirely on the honor-code substrate remaining legible; as dignity-culture displaced it, their function did not decline gradually through disuse but became a category error, a thing later generations struggled even to correctly describe.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, dueling_seconds_and_codifiers, excluded,
    moderate, biographical, trapped, regional).

% Historians and sociologists who read the same historical record as exogenous suppression of a persisting honor code, not its collapse. They observe the same duel-frequency data and disagree about what happened underneath it. Their disagreement is the kernel contest this story is one reading of.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, practice_decline_reading_proponents, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_satisfaction_substrate__cultural_contraction_reading, diffuse).
narrative_ontology:fixing_cost_class(honor_satisfaction_substrate__cultural_contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None, structurally — this is not a coordination mechanism at all on this reading. It is a description of a shared interpretive substrate (what counted as an insult, what counted as satisfaction, what a gentleman's reputation required) eroding as a rival substrate (equal, inherent, unearnable dignity) displaced it. Where the sibling readings find a persisting norm being suppressed, this reading finds the norm itself dissolving.
% TRANSFER_FUNCTION: Nothing is transferred between parties by this constraint as such — no rent, no coerced compliance. What changes is the availability of an entire action-category (ritual lethal combat for reputation) to any agent, regardless of their individual preference. Status-defense capacity shifts diffusely from combat-based to institution-based means (litigation, credentialing, credit rating), which incidentally advantages classes whose capital is portable and reputational systems are documentary rather than embodied.
% ABSENT_VOICES: The aristocratic honor class itself has no standing to contest this reading from within their own framework, because the reading's central claim is precisely that their framework stopped being available to think in — a party cannot object to a semantic ground disappearing using the vocabulary that ground alone supplied. Proponents of practice_decline_reading are present as observers but are, on this reading, mistaking effect (declining duel frequency) for cause (persisting norm plus suppression).
% DISAPPEARANCE_RATIONALE: If the honor-code substrate constraint 'disappeared overnight' the question is nearly incoherent on this reading's own terms, since the reading holds it already effectively dissolved by the late 19th/early 20th century in most Western contexts. Applied retrospectively to the transition period: had the substrate NOT eroded, dueling would very likely have persisted regardless of legal prohibition (as practice_decline_reading's own evidence of duels continuing after early statutes shows) — so on this reading's logic the world DOES rearrange around the constraint's presence or absence, which is exactly why the two sibling readings disagree with it.
% FOUNDING_PROBLEM: The honor code substrate was never 'built' to solve a problem in the engineering sense — it emerged as an interpretive system by which aristocratic and gentry status could be publicly defended and reputational injury publicly repaired without recourse to courts that early-modern elites often distrusted or considered beneath their dignity to use.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set by comparative historical sociologists (e.g. work in the Norbert Elias 'civilizing process' tradition and Pinker's synthesis of honor-to-dignity culture shift) who are not themselves members of the professional/commercial classes that benefited from the transition and who document the substrate's collapse using court records, dueling-frequency data, and etiquette-literature content analysis rather than the self-narrative of either the honor class or its successors.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__cultural_contraction_reading, contested).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__cultural_contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__cultural_contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_satisfaction_substrate__cultural_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_substrate__cultural_contraction_reading, 0.18, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness stays low throughout (0.10 to 0.18) because on this reading no party extracts rents from the constraint's operation — the professional classes and the state are incidental beneficiaries of a substrate shift, not its authors or rent-collectors. Suppression is kept low (0.12) deliberately, in sharp contrast to what practice_decline_reading would author, because this reading's whole point is that legal prohibition is not doing the causal work; duels continued in places with strict statutes right up until the cultural substrate for taking an insult as satisfaction-worthy actually dissolved among the relevant population. accessibility_collapse is authored very high (0.88) because the core claim is that the alternative (resorting to a duel) stopped being a live option in the deliberative sense, not merely a punished one — this is the signature of a mountain-type collapse of an action-category, not a snare-type suppression of a still-desired option. Resistance is low (0.15) because a substrate that has genuinely eroded meets little active resistance by definition; what resistance existed (some late holdouts, military officer corps duels into the early 20th century) is captured in the slowly rising theater_ratio, representing the increasing proportion of 'honor defenses' that had become performative gesture (formal apology rituals, symbolic duels, newspaper honor-affairs) rather than functioning combat.
 *
 * PERSPECTIVAL GAP:
 *   The aristocratic_honor_class seat and the professional_middle_classes seat would compute this constraint very differently if the engine could ask each what happened: the honor class (were they able to answer from within their own framework, which the reading holds they eventually cannot) would likely describe a snare — legal suppression of a legitimate practice by an ascendant class that benefited from its removal. The professional classes and the state see something closer to a mountain — an inevitable maturation of civilized personhood. This story deliberately authors the mountain-adjacent metrics because that is what the cultural_contraction_reading itself claims; the sibling practice_decline_reading is the story that would author the snare-adjacent metrics for the identical historical record.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (professional/commercial classes, the centralizing state) are coded with low derived d because the substrate shift subsidizes their preferred status-logic without them needing to coerce anyone into it — they simply inherit a world where their comparative advantage (documentary reputation, institutional credit) matters more. The aristocratic honor class is coded as payer with identity_locked exit rather than merely constrained, because on this reading their loss is not that an option was closed to them by force but that the very self-concept requiring the option dissolved out from under them across generations — there is no 'exit' available once the interpretive ground for the felt need is gone.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves a potential mandatrophy misclassification in the opposite direction from most stories: rather than a persisting mandate whose function died (classic mandatrophy — the founding problem is dead but the structure survives on inertia), this reading holds that BOTH the founding problem's saliency AND the interpretive apparatus needed to recognize it as a problem died together. There is no zombie institution here to declare mandatrophy-resolved against; the entire kernel evaporated. The founding_problem_status of 'dead' reflects this without positing any surviving administrative shell collecting theater-ratio maintenance costs beyond the modest late-stage performative honor-affairs captured in the measurement series.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substrate_erosion_vs_suppression_priority,
    'Did the honor code''s interpretive substrate genuinely dissolve as an independent cultural-cognitive event, or does the appearance of substrate dissolution retroactively rationalize what was actually successful legal and institutional suppression (courts, military discipline codes, insurance and employment sanctions against duelists)?',
    'Comparative analysis of regions/periods where legal suppression was weak or absent but dueling nonetheless declined sharply (would support substrate erosion) versus regions where legal suppression was strong but dueling persisted culturally underground until enforcement lapsed (would support practice_decline_reading). Content analysis of private correspondence and diaries for genuine attitude shift versus mere prudential avoidance.',
    'If suppression turns out to be doing most of the causal work even where this reading claims substrate erosion, this story''s low suppression score and mountain claim would be a misreading and the constraint would be better classified as tangled_rope or snare, matching practice_decline_reading''s structure instead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substrate_erosion_vs_suppression_priority, empirical, 'Whether the transition is genuine cultural-substrate dissolution or suppression misread as dissolution.').

omega_variable(
    mountain_vs_constructed_beneficiary_claim,
    'Is the honor-to-dignity transition a genuine mountain-like natural maturation of moral/social cognition (as the framing ''cultures of dignity'' suggests), or is it a constructed narrative that happens to flatter and legitimate the professional/commercial classes who benefited from it, making it a false summit?',
    'Cross-cultural comparison: does an equivalent honor-to-dignity substrate shift occur independently in societies without a rising commercial bourgeoisie, or does the shift correlate tightly and specifically with bourgeois ascendance in every documented case, suggesting constructed rather than natural transformation?',
    'If the shift correlates specifically and only with bourgeois-class ascendance rather than occurring as an independent cognitive/moral development, the false_summit_mountain signature is warranted and this constraint should be reclassified toward tangled_rope, with the professional middle classes as active beneficiaries of a constructed rather than natural transition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mountain_vs_constructed_beneficiary_claim, conceptual, 'Whether declaring beneficiaries alongside a mountain claim indicates a genuine natural transition or a false summit.').

omega_variable(
    kernel_disagreement_location,
    'Where exactly do the three kernel readings disagree — is it about the historical facts (duel frequency, statute timing, attitude-content of period sources), or about which causal story those shared facts license?',
    'This is the committer-structure question routed here per Rule 2: the disagreement is NOT about the raw historical record (duel frequency declined across the 18th-20th centuries in most Western societies, legal statutes against dueling long predate its practical disappearance in most jurisdictions) — all three readings accept these facts. The disagreement is about causal attribution: cultural_contraction_reading attributes decline primarily to substrate dissolution (mountain-like), practice_decline_reading attributes it primarily to enforcement plus opportunity cost (snare/tangled_rope-like, substrate persists), composite_overdetermined_reading holds both mechanisms operated with non-independent (mutually reinforcing) pathways.',
    'Because the disagreement is causal-interpretive rather than factual, no single additional historical dataset resolves it outright; the readings would need to specify falsifiable predictions (e.g. differential timing across regions with different suppression regimes) to distinguish them empirically, which is itself a research program rather than a settled fact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_disagreement_location, conceptual, 'Where the three kernel readings'' disagreement is structurally located: causal attribution over shared facts, not the facts themselves.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__cultural_contraction_reading, 1750, 1930).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1750, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1750, 0.05).
narrative_ontology:measurement(hono_tr_t1780, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1780, 0.06).
narrative_ontology:measurement(hono_tr_t1810, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1810, 0.09).
narrative_ontology:measurement(hono_tr_t1850, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1850, 0.14).
narrative_ontology:measurement(hono_tr_t1880, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1880, 0.19).
narrative_ontology:measurement(hono_tr_t1910, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1910, 0.22).
narrative_ontology:measurement(hono_tr_t1930, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1930, 0.22).

% Extraction over time
narrative_ontology:measurement(hono_be_t1750, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1750, 0.1).
narrative_ontology:measurement(hono_be_t1780, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1780, 0.11).
narrative_ontology:measurement(hono_be_t1810, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1810, 0.13).
narrative_ontology:measurement(hono_be_t1850, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1850, 0.15).
narrative_ontology:measurement(hono_be_t1880, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1880, 0.17).
narrative_ontology:measurement(hono_be_t1910, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1910, 0.18).
narrative_ontology:measurement(hono_be_t1930, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1930, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(honor_satisfaction_substrate__cultural_contraction_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_substrate__cultural_contraction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_satisfaction_substrate__cultural_contraction_reading, 0.08).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__cultural_contraction_reading, honor_satisfaction_substrate__practice_decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__cultural_contraction_reading, honor_satisfaction_substrate__composite_overdetermined_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the honor_satisfaction_substrate kernel, each authored as a separate ε-invariant constraint per the decomposition principle. cultural_contraction_reading (this story) authors low, stable extraction and suppression, high accessibility_collapse, and a mountain claim, treating the honor-to-dignity transition as substrate erosion. practice_decline_reading authors high suppression and requires_active_enforcement, lower accessibility_collapse, and likely a snare or tangled_rope claim, treating the honor code as a persisting norm suppressed by external legal/institutional force. composite_overdetermined_reading authors intermediate values reflecting non-independent, mutually reinforcing suppression-plus-delegitimation pathways and likely claims tangled_rope. The three do not average into one constraint; each is a distinct structural claim about the same historical record, linked here for contamination-propagation and family-tracing purposes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
