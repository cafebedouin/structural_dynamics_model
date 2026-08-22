% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_mechanism__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_mechanism__contraction_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: honor_satisfaction_mechanism__contraction_reading
 *   human_readable: The Honor-Satisfaction Duel as a Category That No Longer Exists (Contraction Reading)
 *   domain: historical_sociology/legal_history/normative_systems
 *
 * SUMMARY:
 *   This story instantiates the CONTRACTION reading of the
 *   honor-satisfaction-mechanism kernel: dueling did not merely become rare
 *   (decline_reading) or get dismantled by several independent causal
 *   channels operating in parallel (composite_reading) — under this reading
 *   the category of 'a duel as a live option for resolving honor injury'
 *   became cognitively unavailable to the relevant population, a
 *   category-level impossibility rather than a diminishing-frequency
 *   phenomenon. The extractiveness score here tracks a distinct claim from
 *   the sibling readings: the residual social cost borne by the vestigial
 *   honor-culture population whose entire status-repair apparatus was
 *   rendered unintelligible, not the risk/cost borne by active duelists
 *   (which the decline_reading would track) and not a composite of
 *   overlapping mechanisms (which the composite_reading would track). The
 *   rising extractiveness series reflects the accelerating unavailability of
 *   the honor-repair function to a shrinking population still oriented around
 *   aristocratic status codes, culminating near 1900-1930 when the category
 *   had fully evacuated even from military holdout subcultures in most
 *   jurisdictions.
 *
 * KEY AGENTS:
 *   - historical_aristocratic_honor_culture_bearers: Primary bearer of the loss (powerless/trapped) — the population for whom the option vanished from thinkability
 *   - professional_gentleman_class_successors: Primary beneficiary (institutional/mobile) — inherited the reputational-stakes terrain without the combat obligation
 *   - state_monopoly_on_violence_administrators: Secondary institutional beneficiary/observer — enforcement burden dissolved as the category evacuated
 *   - military_dueling_holdouts: Excluded anomalous persisters whose continued practice complicates the contraction claim
 *   - social_historians: Analytical observers adjudicating between the three kernel readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__contraction_reading, 0.62).
domain_priors:suppression_score(honor_satisfaction_mechanism__contraction_reading, 0.55).
domain_priors:theater_ratio(honor_satisfaction_mechanism__contraction_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, accessibility_collapse, 0.93).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__contraction_reading, piton).
narrative_ontology:human_readable(honor_satisfaction_mechanism__contraction_reading, "The Honor-Satisfaction Duel as a Category That No Longer Exists (Contraction Reading)").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__contraction_reading, "historical_sociology/legal_history/normative_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__contraction_reading, 'c062ead2-c24e-43fc-8870-2ed2cffbc3ce').
narrative_ontology:cs_kernel_codification('c062ead2-c24e-43fc-8870-2ed2cffbc3ce', distributed).
narrative_ontology:cs_authority_grounding('c062ead2-c24e-43fc-8870-2ed2cffbc3ce', distributed).
narrative_ontology:cs_reading_relation('c062ead2-c24e-43fc-8870-2ed2cffbc3ce', honor_satisfaction_mechanism__decline_reading, coexists_with).
narrative_ontology:cs_reading_relation('c062ead2-c24e-43fc-8870-2ed2cffbc3ce', honor_satisfaction_mechanism__composite_reading, influences).
narrative_ontology:cs_axiom('c062ead2-c24e-43fc-8870-2ed2cffbc3ce', foundational, category_disappearance_is_discontinuous_not_gradual).
narrative_ontology:cs_axiom_status(category_disappearance_is_discontinuous_not_gradual, holdable).
narrative_ontology:cs_axiom_grounding('c062ead2-c24e-43fc-8870-2ed2cffbc3ce', category_disappearance_is_discontinuous_not_gradual, empirically_contingent).
narrative_ontology:cs_axiom('c062ead2-c24e-43fc-8870-2ed2cffbc3ce', foundational, cognitive_unthinkability_is_a_distinct_causal_kind_from_suppression).
narrative_ontology:cs_axiom_status(cognitive_unthinkability_is_a_distinct_causal_kind_from_suppression, holdable).
narrative_ontology:cs_axiom_grounding('c062ead2-c24e-43fc-8870-2ed2cffbc3ce', cognitive_unthinkability_is_a_distinct_causal_kind_from_suppression, conventional).
narrative_ontology:cs_reference_frame('c062ead2-c24e-43fc-8870-2ed2cffbc3ce', aristocratic_honor_code_as_live_option_space).
narrative_ontology:cs_drift_state('c062ead2-c24e-43fc-8870-2ed2cffbc3ce', post_ww1_western_elite_culture, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('c062ead2-c24e-43fc-8870-2ed2cffbc3ce', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__contraction_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__contraction_reading, professional_gentleman_class_successors).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__contraction_reading, state_monopoly_on_violence_administrators).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__contraction_reading, historical_aristocratic_honor_culture_bearers).
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__contraction_reading, category_evacuation_over_gradual_decline).
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__contraction_reading, modern_dignitary_law_supersedes_combat_satisfaction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The descendant social stratum whose entire honor-satisfaction apparatus — codes of conduct, seconds, the ritualized economy of insult and reply — no longer has a cognitive slot to occupy. They cannot revive the practice even if they wished to; the option is not suppressed so much as unthinkable to contemporaries, which forecloses any claim they might once have pressed for redress through combat. This is a loss with no living claimant able to name it as a loss in the terms the practice once used.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, historical_aristocratic_honor_culture_bearers, payer,
    powerless, civilizational, trapped, national).

% The occupational and professional strata that absorbed the social function honor once regulated (reputation, credit-worthiness, standing) now operate through law, credentialing, and civil remedy instead of combat. They inherit the reputational stakes without inheriting any duty to risk death over them; the contraction of the category is a pure gain to their operating environment, requiring no maintenance on their part because there is nothing left to maintain.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, professional_gentleman_class_successors, beneficiary,
    institutional, generational, mobile, national).

% The apparatus of courts, police, and criminal codification did not need to keep suppressing dueling by the twentieth century because the practice had exited the space of intelligible options for the relevant population; the administrators observe the category's disappearance and benefit incidentally from not needing continuous enforcement resources against a threat that no longer registers as available.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, state_monopoly_on_violence_administrators, observer,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_mechanism__contraction_reading, state_monopoly_on_violence_administrators, beneficiary).

% Isolated military-officer subcultures (parts of the German Student Corps, some officer corps into the early twentieth century) continued treating dueling as a live option well past its general disappearance elsewhere. Under the contraction reading their persistence is treated as a residual anomaly rather than evidence against the category-collapse claim, and their voice — that the practice remained thinkable to them — is not admitted as counting against the general population's cognitive foreclosure.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, military_dueling_holdouts, excluded,
    moderate, biographical, identity_locked, regional).

% Scholars debate whether dueling's end is best modeled as gradual frequency decline (decline_reading), multi-causal overdetermination (composite_reading), or genuine conceptual evacuation (this reading). They assemble records of dueling frequency, legal prosecutions, newspaper commentary, and etiquette manuals to adjudicate which structural story the evidence supports, without themselves bearing any stake in the outcome.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, social_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In its operative period, the honor-satisfaction duel coordinated a socially legible mechanism for resolving reputational injury among status-equals without recourse to courts perceived as beneath the class's dignity — a genuine (if narrow) coordination function for a specific stratum.
% TRANSFER_FUNCTION: Under the contraction reading, nothing is currently being transferred by this constraint because there is no constraint left to operate — the 'transfer' is historical: risk of death and reputational capital moved between duelists and their social circle; today the vacated category transfers nothing, having been replaced entirely by other reputational and legal mechanisms.
% ABSENT_VOICES: The historical honor-culture bearers who might insist the practice still matters cannot be consulted — the population that once treated dueling as live has no living representative whose cognitive frame includes it as an option; the military holdout subcultures who persisted past general disappearance are excluded from this reading's evidentiary weight, since their persistence is treated as anomalous rather than falsifying.
% DISAPPEARANCE_RATIONALE: Because this reading claims the constraint has ALREADY disappeared — not merely weakened but evacuated from the space of intelligible options — its further disappearance changes nothing: the world has already rearranged itself around the duel's absence, and no institution today depends on dueling remaining a live category. The rearrangement occurred at the moment of category evacuation, not as an ongoing effect.
% FOUNDING_PROBLEM: The duel was built to solve a jurisdictional and status problem: aristocratic honor injuries were not cognizable in ordinary courts (beneath the dignity of the parties, or the courts lacked appropriate remedy for insult-to-standing), so an extrajudicial, ritualized, peer-adjudicated combat mechanism filled the gap.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians outside any dueling-descended interest group (e.g., research on the codification of dignitary torts and the professionalization of honor into defamation law) attest that modern courts, credentialing bodies, and civil remedies fully absorbed the reputational-injury function decades before the last duels occurred, and that no contemporary institution or claimant treats the founding problem as unaddressed by non-lethal means.
narrative_ontology:disappearance_verdict(honor_satisfaction_mechanism__contraction_reading, world_unchanged).
narrative_ontology:founding_problem_status(honor_satisfaction_mechanism__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_mechanism__contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_satisfaction_mechanism__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_mechanism__contraction_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_mechanism__contraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_satisfaction_mechanism__contraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_satisfaction_mechanism__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate-high (0.62 at endpoint) not because an active mechanism extracts rents from a population today, but because the CONTRACTION reading's own claim implies a one-way, non-negotiated cost: an entire status-repair grammar was removed from a population's option set without substitute negotiation, compensation, or transition period — a structural cost that the reading itself treats as historically real even though no active extractive apparatus persists today. Suppression is authored at 0.55, reflecting genuine partial suppression (dueling statutes, criminalization, social sanction against duelists) that operated ALONGSIDE the cognitive evacuation this reading emphasizes — the contraction reading does not claim suppression was absent, only that suppression is not the PRIMARY mechanism; sole reliance on suppression-as-explanation is closer to a naive decline story. Theater ratio is low (0.15) because there is essentially nothing performative left to measure — a genuinely evacuated category has no ritual maintenance, no theatrical residue, unlike a piton that persists through empty performance. Accessibility collapse is authored very high (0.93) precisely because the contraction reading's defining claim IS complete collapse of the option's availability as a live choice, not partial or contested unavailability. Resistance is authored very low (0.1) because a category that has become cognitively unthinkable, almost by definition, meets no active resistance from those it would have applied to — there is no one contesting the RIGHT to duel because dueling is not held in mind as an option to contest.
 *
 * PERSPECTIVAL GAP:
 *   The historical honor-bearer seat and the professional-successor seat diverge sharply: from the (counterfactually reconstructed) honor-bearer's position, an entire coordination mechanism for status-repair was foreclosed, a genuine and uncompensated structural loss; from the professional-successor seat, nothing was lost because the reputational function was fully and better served by alternative institutions (law, credentialing) that required no risk of death. The engine should compute these divergently: the payer seat trends toward tangled-rope/snare-like extraction-of-option because the loss is total and non-consensual from that vantage, while the beneficiary and observer seats trend toward mountain-like inevitability or rope-like improvement, because from their vantage nothing coercive is currently operating — the category simply isn't there to coerce with.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (professional successors, state administrators) are declared with mobile/analytical exit options and institutional power — the contraction cost the category's evacuation, if a cost at all, does not fall on them; it falls entirely on the historical population that had organized status-repair around dueling and for whom no substitute negotiation occurred. That population is declared trapped/powerless because by construction of the contraction reading, they cannot even conceive of the option to contest its removal — trapped-ness here is cognitive rather than legal or economic, which is the reading's defining structural claim.
 *
 * MANDATROPHY ANALYSIS:
 *   The contraction reading resists mandatrophy mislabeling in a specific direction: because the mechanism has genuinely disappeared (not merely gone underground or become rare), there is no live mandate to evaluate for obsolescence — the founding problem (extrajudicial honor-injury remedy) is dead, and unlike a piton or a scaffold, there is no residual institution collecting maintenance rents on a vestigial claim. The classification as piton reflects an analytical stance from the OBSERVER seat looking backward at what remains structurally (near-zero live function, near-zero theater, near-total category collapse) rather than a claim that any present-day actor is extracting anything. The alternative claimed types (rope for the historical coordination function, snare from the aristocratic bearer's uncompensated-loss vantage) are both defensible from other seats; the engine's per-seat computation should surface that divergence rather than collapse it into one verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contraction_vs_decline_evidentiary_test,
    'Is the historical record better explained by a genuine category-level cognitive evacuation (this reading) or by a gradual frequency decline that never crossed a threshold of unthinkability (decline_reading)?',
    'Examine contemporaneous etiquette manuals, legal commentary, and personal correspondence from 1850-1930 for evidence of whether elites who declined to duel framed the option as morally/practically unavailable (contraction) versus merely unfashionable or risky (decline). Persistence of military holdout dueling into the 1900s-1930s is a key falsifying data point for a strong contraction claim and must be weighed against general-population evidence.',
    'If evidence favors decline over contraction, this story''s claimed_type and extractiveness framing (uncompensated categorical loss) would need substantial revision toward a frequency-decline model with different beneficiary/victim structure; the category-collapse claim would be overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contraction_vs_decline_evidentiary_test, empirical, 'Whether the historical record supports category-level evacuation versus mere frequency decline.').

omega_variable(
    reading_boundary_location,
    'Where exactly does the disagreement between the three kernel readings live — is it about WHICH causal mechanism dominated (composite vs. single-mechanism), or about the PHENOMENOLOGY of the outcome (unthinkable vs. merely rare)?',
    'Structural analysis of how each reading''s claim would be falsified: contraction_reading is falsified by evidence that dueling remained a live, considered (even if rejected) option for the general elite population late into the period; decline_reading is falsified by evidence of a sharp discontinuity rather than gradual attrition; composite_reading is falsified by evidence that one mechanism alone fully accounts for the outcome.',
    'Clarifies that this is a conceptual disagreement about the SHAPE of historical change (discontinuous evacuation vs. continuous decline vs. multi-causal overdetermination), not merely a factual dispute resolvable by more data on dueling frequency alone — some of the disagreement is genuinely about what counts as evidence of ''unthinkability.''',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_boundary_location, conceptual, 'Whether the kernel''s three readings disagree about mechanism, phenomenology, or evidentiary standard.').

omega_variable(
    military_holdout_weight,
    'How much evidentiary weight should isolated military-officer dueling subcultures persisting into the early twentieth century carry against a general-population contraction claim?',
    'Compare the size, social influence, and duration of holdout subcultures (German Student Corps Mensur culture, some Latin American and French military circles) against general elite population dueling rates; assess whether holdouts represent a distinct bounded subculture (compatible with general contraction) or evidence the category remained broadly thinkable.',
    'If holdouts are substantial and representative rather than anomalous, the contraction reading''s claim of general cognitive evacuation weakens significantly and shifts weight toward the decline_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(military_holdout_weight, empirical, 'Whether military dueling holdouts falsify or merely qualify the general contraction claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__contraction_reading, 1780, 1930).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1780, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1780, 0.05).
narrative_ontology:measurement(hono_tr_t1810, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1810, 0.07).
narrative_ontology:measurement(hono_tr_t1840, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1840, 0.1).
narrative_ontology:measurement(hono_tr_t1870, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1870, 0.12).
narrative_ontology:measurement(hono_tr_t1900, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1900, 0.14).
narrative_ontology:measurement(hono_tr_t1930, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1930, 0.15).

% Extraction over time
narrative_ontology:measurement(hono_be_t1780, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1780, 0.35).
narrative_ontology:measurement(hono_be_t1810, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1810, 0.4).
narrative_ontology:measurement(hono_be_t1840, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1840, 0.48).
narrative_ontology:measurement(hono_be_t1870, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1870, 0.55).
narrative_ontology:measurement(hono_be_t1900, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1900, 0.6).
narrative_ontology:measurement(hono_be_t1930, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1930, 0.62).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(honor_satisfaction_mechanism__contraction_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(honor_satisfaction_mechanism__contraction_reading, honor_satisfaction_mechanism__decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__contraction_reading, honor_satisfaction_mechanism__composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the honor_satisfaction_mechanism kernel, decomposed per the ε-invariance principle because the natural-language label 'why dueling ended' conflates structurally distinct claims: (1) this file, the contraction reading, claims category-level cognitive evacuation with ε=0.62 authored against the historical honor-bearer population's uncompensated loss of an option-space; (2) the decline_reading sibling claims persistence-at-declining-frequency, which would author a different, likely lower and more gradually-rising ε profile tracking actual duelist risk/frequency rather than categorical unavailability; (3) the composite_reading sibling claims multi-causal overdetermination (state monopoly, bourgeois norms, insurance actuarial pressure, category-shift jointly), which would author a blended ε reflecting the aggregate of several partial mechanisms rather than any single dominant cause. Each reading has its own beneficiary/victim structure and its own claimed_type; they are linked here rather than merged because averaging or parameterizing a single ε across three structurally distinct mechanism-claims would violate ε-invariance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
