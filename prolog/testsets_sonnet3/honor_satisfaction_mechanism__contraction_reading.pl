% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_mechanism__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: honor_satisfaction_mechanism__contraction_reading
 *   human_readable: The Duel as Honor-Satisfaction Mechanism (Categorical Contraction Reading)
 *   domain: historical_sociology/legal_history/normative_systems
 *
 * SUMMARY:
 *   This story instantiates the CONTRACTION reading of the honor-satisfaction
 *   mechanism kernel: the claim that dueling did not merely decline in
 *   frequency (decline_reading) or get replaced by a bundle of distinct
 *   suppressing mechanisms operating in parallel (composite_reading), but
 *   that the entire cognitive category of 'lethal combat as a legitimate
 *   answer to an insult' was evacuated from the space of thinkable options
 *   for the relevant social classes. Under this reading, by some point in the
 *   late 19th/early 20th century, proposing a duel was not a
 *   forbidden-but-comprehensible act (like proposing an illegal drag race)
 *   but a categorically unavailable one, closer to proposing trial by combat
 *   to settle a modern lawsuit. ε is authored for the standing arrangement
 *   under contest — the residual honor-economy as this reading characterizes
 *   it, not for the modern arrangement that replaced it — per the fixed
 *   ε-referent rule for kernel readings.
 *
 * KEY AGENTS:
 *   - post_honor_professional_class: Primary beneficiary (institutional/analytical) — inherits a world without the category, at no felt cost
 *   - modern_state_monopoly_on_violence: Structural beneficiary (institutional/analytical) — consistent with and reinforced by the category's absence
 *   - historical_dueling_participants_class: Historical payer (powerless/trapped) — bore the mechanism's stakes before the category contracted out of existence
 *   - social_historians: Analytical observer — adjudicates between the three kernel readings using the historical record
 *   - residual_dueling_subcultures: Excluded — their continued practice is the reading's own awkward remainder
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__contraction_reading, 0.28).
domain_priors:suppression_score(honor_satisfaction_mechanism__contraction_reading, 0.35).
domain_priors:theater_ratio(honor_satisfaction_mechanism__contraction_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, accessibility_collapse, 0.93).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__contraction_reading, piton).
narrative_ontology:human_readable(honor_satisfaction_mechanism__contraction_reading, "The Duel as Honor-Satisfaction Mechanism (Categorical Contraction Reading)").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__contraction_reading, "historical_sociology/legal_history/normative_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__contraction_reading, '7e954000-79ea-4e3e-ab17-aec46f4c2a72').
narrative_ontology:cs_kernel_codification('7e954000-79ea-4e3e-ab17-aec46f4c2a72', distributed).
narrative_ontology:cs_authority_grounding('7e954000-79ea-4e3e-ab17-aec46f4c2a72', practice).
narrative_ontology:cs_interpretation_layer_present('7e954000-79ea-4e3e-ab17-aec46f4c2a72').
narrative_ontology:cs_reading_relation('7e954000-79ea-4e3e-ab17-aec46f4c2a72', honor_satisfaction_mechanism__decline_reading, coexists_with).
narrative_ontology:cs_reading_relation('7e954000-79ea-4e3e-ab17-aec46f4c2a72', honor_satisfaction_mechanism__composite_reading, influences).
narrative_ontology:cs_axiom('7e954000-79ea-4e3e-ab17-aec46f4c2a72', foundational, categorical_evacuation_not_gradual_decline).
narrative_ontology:cs_axiom_status(categorical_evacuation_not_gradual_decline, holdable).
narrative_ontology:cs_axiom_grounding('7e954000-79ea-4e3e-ab17-aec46f4c2a72', categorical_evacuation_not_gradual_decline, empirically_contingent).
narrative_ontology:cs_axiom('7e954000-79ea-4e3e-ab17-aec46f4c2a72', secondary, single_mechanism_sufficiency_of_conceptual_change).
narrative_ontology:cs_axiom_status(single_mechanism_sufficiency_of_conceptual_change, holdable).
narrative_ontology:cs_axiom_grounding('7e954000-79ea-4e3e-ab17-aec46f4c2a72', single_mechanism_sufficiency_of_conceptual_change, conventional).
narrative_ontology:cs_reference_frame('7e954000-79ea-4e3e-ab17-aec46f4c2a72', elite_honor_code_as_operative_norm).
narrative_ontology:cs_drift_state('7e954000-79ea-4e3e-ab17-aec46f4c2a72', post_professionalization_era, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('7e954000-79ea-4e3e-ab17-aec46f4c2a72', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__contraction_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__contraction_reading, post_honor_professional_class).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__contraction_reading, modern_state_monopoly_on_violence).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__contraction_reading, historical_dueling_participants_class).
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__contraction_reading, conceptual_category_replacement_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Occupies the social position formerly governed by honor-satisfaction logic (military officers, lawyers, gentlemen of standing) but under a successor conceptual scheme where insult is resolved through litigation, reputation management, or professional discipline. They do not experience dueling as suppressed or forbidden — the category of 'affair of honor requiring mortal combat' simply does not arise as an available description of any dispute they have. They benefit from an expanded, safer repertoire of dispute resolution without having to weigh a duel as a live option at all.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, post_honor_professional_class, beneficiary,
    institutional, generational, analytical, national).

% The nation-state's claim to sole legitimate authority over lethal violence is structurally consistent with a world in which private lethal combat over honor is not a live category. It did not need to wage an active suppression campaign against dueling as a going concern; the category's evacuation from the possibility space is a structural precondition the state benefits from without having to enforce continuously against a real countervailing demand.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, modern_state_monopoly_on_violence, beneficiary,
    institutional, civilizational, analytical, national).

% Historical gentlemen, officers, and professionals who lived inside the honor-satisfaction schema and for whom refusing a challenge, or issuing one, carried real stakes of social death or physical death. They are payers under the OLD arrangement (the kernel's referent, per the ε-invariance rule) — the contraction reading describes their world as having vanished as a category, which means their entire structure of felt necessity is retrospectively rendered unintelligible rather than merely regulated away. They cannot be consulted; the category that structured their choices has no living analogue to report back from.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, historical_dueling_participants_class, payer,
    powerless, biographical, trapped, national).

% Study the transition and adjudicate between the decline, composite, and contraction readings by examining court records, dueling manuals, satirical press, and legislative debate. Their own scholarly incentives sometimes favor the more dramatic contraction narrative because it makes a stronger causal/conceptual claim, which is itself a fact this story's omegas must track.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, social_historians, observer,
    analytical, civilizational, analytical, global).

% Isolated, geographically or subculturally marginal groups (e.g., some student fraternities, isolated regional aristocracies into the early 20th century) who continued dueling in some form after it had become mainstream-unthinkable elsewhere. Under the strict contraction reading their continued practice is either denied full reality (dismissed as atavistic theater, not a live instance of the same category) or treated as evidence against the reading's universality — they are structurally excluded from the contraction reading's own account of itself, which is exactly what the reading requires to hold cleanly.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, residual_dueling_subcultures, excluded,
    powerless, biographical, trapped, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None, properly speaking, under this reading — there is no coordination problem being solved because there is no longer an activity to coordinate around. The prior honor-satisfaction mechanism DID coordinate elite conflict resolution and status-repair; the contraction reading's claim is precisely that this coordination function was not replaced, regulated, or out-competed, but rendered a category error, so asking 'what problem does the current arrangement solve' has no honest answer beyond 'the absence of an intelligible alternative.'
% TRANSFER_FUNCTION: Nothing is transferred under the contraction reading in the present tense — there is no ongoing flow of costs or benefits between live parties, because the underlying activity is not merely rare but conceptually impossible. The only transfer this reading identifies is historical and one-directional: the entire stock of honor-satisfaction stakes borne by historical participants was extinguished without residue, converted into nothing (no successor institution inherited their specific stakes as such).
% ABSENT_VOICES: Historical dueling participants cannot be interviewed about whether their world was 'suppressed,' 'declined,' or 'evacuated' — they experienced live stakes inside a category the contraction reading says no longer exists as a describable option for anyone. Residual subculture participants who continued dueling after the supposed contraction are structurally awkward for this reading and are treated as exceptions-that-prove-the-rule rather than counterevidence, which the reading's own proponents (mostly conceptual historians and philosophers of normative change) would need to explain rather than explain away.
% DISAPPEARANCE_RATIONALE: The contraction reading's entire claim is that the constraint already disappeared — not partially, not into a fringe practice, but categorically, so there is no 'if it vanished' counterfactual left to run: it is not a live constraint on any present arrangement, its 'disappearance' already occurred and the modern world already reflects that. This is the reading's most distinctive and most falsifiable feature relative to its siblings.
% FOUNDING_PROBLEM: The honor-satisfaction mechanism was built to resolve disputes over reputation and status among social equals in the absence of a trusted, impartial arbiter with jurisdiction over 'honor' as a legal category — courts could adjudicate property and injury but historically could not restore lost honor, so a private, ritualized, mutually-binding combat procedure filled that gap.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians outside the dueling-culture tradition (e.g. studies of the rise of libel law, insult ordinances, and professional codes of conduct in 19th-century Europe) attest that formal legal and professional mechanisms for reputational injury emerged and absorbed the functional space the duel once occupied, corroborating that the founding problem itself was resolved by institutional substitution rather than merely abandoned. No party that benefited from the older honor economy (aristocratic dueling societies, military codes of honor) survives to attest independently; the corroboration is necessarily retrospective and institutional rather than contemporaneous.
narrative_ontology:disappearance_verdict(honor_satisfaction_mechanism__contraction_reading, world_unchanged).
narrative_ontology:founding_problem_status(honor_satisfaction_mechanism__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_mechanism__contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_satisfaction_mechanism__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_mechanism__contraction_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_mechanism__contraction_reading_tests).
:- end_tests(honor_satisfaction_mechanism__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate-declining (0.55 -> 0.28) because under the contraction reading the honor-satisfaction mechanism's costs to participants (forced combat, social death for refusal) were real historically but the reading claims these costs simply cease to be extracted from anyone once the category contracts — there is no successor institution charging rent on the same axis. Suppression is authored moderate (0.35), notably LOWER than what a decline or composite reading would author for the same period, because the contraction reading's distinctive claim is that active, continuous suppression was NOT the primary mechanism — a merely-suppressed practice would show high, sustained suppression values as authorities kept fighting a live demand; a categorically-contracted practice shows suppression tapering because there is decreasingly anything left needing to be suppressed. Accessibility_collapse is authored very high (0.93) because the reading's whole thesis is near-total collapse of the option from the space of intelligible choices, not merely from the space of legal choices. Resistance is authored very low (0.08) because a genuinely contracted category, almost by definition, meets little active resistance in its terminal state — nobody argues for reviving trial by combat either.
 *
 * PERSPECTIVAL GAP:
 *   The historical participant class and the modern professional class would compute this constraint completely differently if either could report on it, but that asymmetry is not a live seat divergence in the way most constraints exhibit it — the historical participants are not merely disadvantaged relative to the modern beneficiaries, they occupy a WORLD in which the category existed at all, which the modern beneficiaries do not. This is the contraction reading's most unusual structural feature: the 'seat divergence' is temporal-categorical rather than a live simultaneous asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Historical dueling participants are declared victims/payers with trapped exit because, within the honor-satisfaction schema as it operated on them, refusing a challenge or failing to issue one carried catastrophic social (and sometimes literal) costs — there was no meaningful exit from the logic once one's honor was implicated. The post-honor professional class and the modern state are declared beneficiaries with analytical exit because they simply do not face the choice architecture the mechanism imposed; their 'exit' is not a chosen escape but structural non-exposure. Residual subcultures are marked excluded rather than payer or beneficiary because the contraction reading, by its own logic, has difficulty classifying their continued practice at all.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy question here is unusually sharp: this reading's own founding_problem_status is 'dead' and disappearance_verdict is 'world_unchanged' — which is not a mismatch flag but the reading's central and most falsifiable claim. A mismatch WOULD arise if evidence showed the category persisted informally (in the residual subcultures) while the reading insists otherwise; that tension is deliberately preserved via the excluded stakeholder and the omega below rather than resolved by fiat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contraction_vs_decline_vs_composite,
    'Is the disappearance of dueling better modeled as a genuine category-level cognitive contraction (this reading), a mere frequency decline to fringe status (decline_reading), or a composite of several independently sufficient mechanisms — state monopoly, bourgeois respectability norms, insurance/liability regimes, and category-shift — operating in parallel (composite_reading)?',
    'Close reading of primary sources (memoirs, court records, satirical press, dueling manuals) for evidence of whether contemporaries treated late-period challenges as comprehensible-but-forbidden acts (supports decline/composite) versus category errors or jokes (supports contraction); cross-national comparison of timing against state monopoly consolidation, professional-class growth, and insurance markets would help apportion causal weight if composite.',
    'If decline_reading or composite_reading is correct, this story''s suppression and resistance values are authored too low — a merely-declining or actively-multiply-suppressed practice would show sustained higher suppression and resistance late into the interval, and residual subcultures would need to be treated as central counter-evidence rather than a marginal excluded case.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contraction_vs_decline_vs_composite, conceptual, 'Which of the three kernel readings (contraction, decline, composite) best fits the historical record.').

omega_variable(
    residual_subculture_falsification_risk,
    'Does the persistence of dueling in fringe subcultures (student corps, isolated regional aristocracies) into the 20th century falsify the strong contraction claim, or is it compatible with contraction as a claim about the MAINSTREAM cognitive category while allowing bounded subcultural exceptions?',
    'Examine whether residual practitioners themselves experienced their dueling as continuous with the older honor-satisfaction logic (supporting a bounded-exception reading) or as a self-consciously anachronistic, theatrical revival (supporting the theater_ratio uptick already authored and a weaker contraction claim).',
    'If residual practice was experientially continuous rather than theatrical, the accessibility_collapse value (0.93) is authored too high and the contraction reading''s universality claim is overstated relative to the composite reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_subculture_falsification_risk, empirical, 'Whether fringe survival of dueling is compatible with or falsifies the strong contraction thesis.').

omega_variable(
    scholarly_incentive_toward_dramatic_reading,
    'Do historians and theorists of normative change have a professional incentive to favor the more dramatic ''categorical impossibility'' narrative over the more mundane decline narrative, independent of the evidence?',
    'Survey citation patterns and methodological commitments across the relevant historiography; check whether contraction-reading claims cluster in theoretically ambitious works versus archival social history.',
    'If such an incentive exists and is strong, the contraction reading''s prevalence in the literature is itself partly an artifact of scholarly selection rather than pure evidentiary support, which should lower confidence in this story''s claimed_type and metrics without changing them retroactively.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scholarly_incentive_toward_dramatic_reading, conceptual, 'Possible scholarly selection bias favoring the dramatic contraction narrative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__contraction_reading, 1750, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1750, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1750, 0.05).
narrative_ontology:measurement(hono_tr_t1790, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1790, 0.07).
narrative_ontology:measurement(hono_tr_t1830, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1830, 0.1).
narrative_ontology:measurement(hono_tr_t1870, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1870, 0.13).
narrative_ontology:measurement(hono_tr_t1910, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1910, 0.15).
narrative_ontology:measurement(hono_tr_t1950, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1950, 0.15).

% Extraction over time
narrative_ontology:measurement(hono_be_t1750, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1750, 0.55).
narrative_ontology:measurement(hono_be_t1790, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1790, 0.5).
narrative_ontology:measurement(hono_be_t1830, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1830, 0.4).
narrative_ontology:measurement(hono_be_t1870, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1870, 0.32).
narrative_ontology:measurement(hono_be_t1910, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1910, 0.28).
narrative_ontology:measurement(hono_be_t1950, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1950, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(honor_satisfaction_mechanism__contraction_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_mechanism__contraction_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__contraction_reading, honor_satisfaction_mechanism__decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__contraction_reading, honor_satisfaction_mechanism__composite_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the honor_satisfaction_mechanism kernel. decline_reading models dueling as a practice persisting at declining frequency into fringe status (higher, more sustained suppression and resistance values expected, since something is still being actively pushed to the margins). composite_reading models multiple independently sufficient mechanisms (state monopoly, bourgeois norms, insurance, category-shift) operating together, and would author a different beneficiary/victim structure per mechanism rather than one unified category-contraction. All three readings share the same underlying historical kernel but assign structurally different ε, suppression, and accessibility_collapse profiles per the ε-invariance principle — each is linked to the others via affects_constraints rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
