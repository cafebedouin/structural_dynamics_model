% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: honor_violence_legitimacy__contraction_reading
 *   human_readable: Honor Code as Redefined to Exclude Violence (Contraction Reading)
 *   domain: historical_sociology/legal_anthropology
 *
 * SUMMARY:
 *   This story instantiates the contraction reading of the
 *   honor-violence-legitimacy kernel: dueling did not simply become rare
 *   while remaining conceptually available as an honorable act (the drop
 *   reading) — the very content of 'honor' was actively narrated by courts,
 *   clergy, and print discourse to exclude violent vindication from its
 *   legitimate scope. Under this reading, a would-be duelist by 1880 faces
 *   not merely elevated practical cost but a conceptual vacancy: there is no
 *   longer a socially legible category of 'honorable violence' to invoke. The
 *   residual practitioners who still orient by the older code experience
 *   genuine extraction — loss of a legible remedy — but the overall
 *   constraint is low-extraction and low-suppression by 1900 because so few
 *   people any longer hold the older code as operative; what enforcement
 *   exists is mostly rhetorical (ridicule, professional censure) rather than
 *   coercive, which is why the type moves toward piton (a residual, largely
 *   performative code-holdout status) rather than snare.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__contraction_reading, 0.28).
domain_priors:suppression_score(honor_violence_legitimacy__contraction_reading, 0.35).
domain_priors:theater_ratio(honor_violence_legitimacy__contraction_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__contraction_reading, piton).
narrative_ontology:human_readable(honor_violence_legitimacy__contraction_reading, "Honor Code as Redefined to Exclude Violence (Contraction Reading)").
narrative_ontology:topic_domain(honor_violence_legitimacy__contraction_reading, "historical_sociology/legal_anthropology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__contraction_reading, 'bb5c61f3-27db-40ce-8fce-f76d2f918d17').
narrative_ontology:cs_kernel_codification('bb5c61f3-27db-40ce-8fce-f76d2f918d17', distributed).
narrative_ontology:cs_authority_grounding('bb5c61f3-27db-40ce-8fce-f76d2f918d17', practice).
narrative_ontology:cs_interpretation_layer_present('bb5c61f3-27db-40ce-8fce-f76d2f918d17').
narrative_ontology:cs_reading_relation('bb5c61f3-27db-40ce-8fce-f76d2f918d17', honor_violence_legitimacy__drop_reading, coexists_with).
narrative_ontology:cs_reading_relation('bb5c61f3-27db-40ce-8fce-f76d2f918d17', honor_violence_legitimacy__composite_reading, influences).
narrative_ontology:cs_axiom('bb5c61f3-27db-40ce-8fce-f76d2f918d17', foundational, honor_conceptually_severable_from_violent_defense).
narrative_ontology:cs_axiom_status(honor_conceptually_severable_from_violent_defense, holdable).
narrative_ontology:cs_axiom_grounding('bb5c61f3-27db-40ce-8fce-f76d2f918d17', honor_conceptually_severable_from_violent_defense, conventional).
narrative_ontology:cs_axiom('bb5c61f3-27db-40ce-8fce-f76d2f918d17', secondary, conceptual_exclusion_sufficient_absent_coercion).
narrative_ontology:cs_axiom_status(conceptual_exclusion_sufficient_absent_coercion, holdable).
narrative_ontology:cs_axiom_grounding('bb5c61f3-27db-40ce-8fce-f76d2f918d17', conceptual_exclusion_sufficient_absent_coercion, empirically_contingent).
narrative_ontology:cs_reference_frame('bb5c61f3-27db-40ce-8fce-f76d2f918d17', code_duello_autonomous_class_adjudication).
narrative_ontology:cs_drift_state('bb5c61f3-27db-40ce-8fce-f76d2f918d17', post_reform_discourse_consolidation, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('bb5c61f3-27db-40ce-8fce-f76d2f918d17', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, bourgeois_professional_class).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, state_judicial_authorities).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, aristocratic_families_avoiding_ruin).
narrative_ontology:constraint_victim(honor_violence_legitimacy__contraction_reading, residual_dueling_practitioners).
narrative_ontology:constraint_victim(honor_violence_legitimacy__contraction_reading, military_officer_corps_holdouts).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, clergy_and_moral_reformers).
narrative_ontology:constraint_victim(honor_violence_legitimacy__contraction_reading, aristocratic_families_avoiding_ruin).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__contraction_reading, honor_is_conceptually_separable_from_violence).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__contraction_reading, civility_discourse_supersedes_martial_code).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rising professionals (lawyers, physicians, merchants) whose social standing depends on reputational codes that do not require risking life or liberty in combat. Benefit directly from a redefinition of honor that lets them claim full social respectability through litigation, print retraction, and institutional standing rather than the duel, which their class was historically barred from or disadvantaged in.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, bourgeois_professional_class, beneficiary,
    organized, generational, mobile, national).

% Courts, legislatures, and monarchic authority progressively redefine 'true honor' in statute, sermon, and jurisprudence as compatible with submission to law rather than private violence. They administer the redefinition through libel law, military discipline codes, and public moral discourse, and could in principle have left the old conceptual space intact, but instead actively narrated dueling out of the category of honorable action.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, state_judicial_authorities, agenda_setter,
    institutional, civilizational, analytical, national).

% Old noble houses that had been bleeding heirs and fortunes to duels for generations. Benefit from a conceptual shift that lets a refusal to duel be read as honorable restraint rather than cowardice — but pay a cost in loss of an older, autonomous, class-specific code of self-adjudicated honor that had insulated them from state authority.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, aristocratic_families_avoiding_ruin, beneficiary,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(honor_violence_legitimacy__contraction_reading, aristocratic_families_avoiding_ruin, payer).

% Individuals who still feel bound by the older honor code, for whom the redefinition removes their available vocabulary for legitimate response to insult. They cannot simply choose to duel and be understood as honorable; the conceptual space that would have validated the act no longer exists in the surrounding discourse, leaving them without a legible route to restore standing.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, residual_dueling_practitioners, payer,
    moderate, biographical, trapped, regional).

% Officer subcultures that retained code-duello norms longest, now increasingly out of step with civilian and even institutional military definitions of honorable conduct. They face court-martial, ridicule, or professional exile for acts their own subcultural code still endorses, caught between an internal residual code and an external redefinition that has already occurred.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, military_officer_corps_holdouts, payer,
    organized, biographical, constrained, national).

% Religious and reform-minded writers who campaigned explicitly to strip dueling of its honor content, framing it as sin and vanity rather than virtue. They administer the redefinition through sermon, pamphlet, and moral education, and gain cultural authority as the conceptual arbiters of what honor properly means.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, clergy_and_moral_reformers, agenda_setter,
    organized, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(honor_violence_legitimacy__contraction_reading, clergy_and_moral_reformers, beneficiary).

% Retrospective analysts weighing whether the honor code's redefinition was the operative cause of dueling's disappearance, as against external cost mechanisms (legal penalty, social ostracism) that left the code nominally intact but made acting on it too costly. This story adopts the redefinition-as-cause reading.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, social_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_violence_legitimacy__contraction_reading, diffuse).
narrative_ontology:fixing_cost_class(honor_violence_legitimacy__contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides society with a shared, non-lethal vocabulary for adjudicating and restoring reputational standing after insult, replacing a system that required physical risk with one resolvable through legal, professional, or rhetorical channels.
% TRANSFER_FUNCTION: Moves the authority to adjudicate honor claims from autonomous class-based codes (aristocratic self-governance, officer subculture) to centralized institutions (courts, print discourse, clergy) — and moves the cost of that authority transfer onto those still oriented by the older code, who lose their legible path to restored standing.
% ABSENT_VOICES: Practitioners still committed to the code-duello tradition are rhetorically present only as figures of ridicule or backwardness in the reform discourse that redefines honor; their own account of what constitutes legitimate defense of reputation is not solicited by the authorities doing the redefining, only judged by it.
% DISAPPEARANCE_RATIONALE: If the redefinition were somehow reversed and honor reverted to include violent vindication as a legitimate category, courts, professional bodies, and military codes would have to reopen questions they treat as long settled — but whether this would actually cause a resurgence in dueling (versus merely restoring a dormant conceptual option) is exactly the empirical question this reading and its siblings (drop_reading, composite_reading) dispute.
% FOUNDING_PROBLEM: European aristocratic and officer society needed some mechanism to adjudicate insult and restore public standing without indefinite feuding or judicial dependency; dueling filled that role for centuries as a self-administered, honor-preserving remedy outside state courts.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and comparative anthropologists outside the reforming clergy and the bourgeois beneficiary class corroborate that the underlying reputational-adjudication problem persisted (people still needed ways to restore standing after insult) but was structurally re-routed into litigation, press retraction, and professional censure — the historical record of libel-law expansion tracking the decline of dueling is independent evidence for the redefinition claim, though drop_reading's proponents dispute that this evidence establishes conceptual contraction rather than mere cost substitution.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__contraction_reading, contested).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-04',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_violence_legitimacy__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_violence_legitimacy__contraction_reading, 0.28, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is modest and rising slowly (0.15→0.28) because the redefinition itself extracts little in resource terms — its cost falls narrowly on a shrinking population of holdouts who lose a specific symbolic remedy, not on society broadly. Theater ratio rises correspondingly (0.08→0.22) as the residual code-duello subculture becomes increasingly performative — surviving officer-corps dueling by 1900 is largely ritual assertion of an identity that the surrounding discourse no longer recognizes as honor-conferring, rather than a functioning adjudicative mechanism. Suppression (0.35) is moderate: no one is coercively prevented from dueling by 1900 in most jurisdictions where it has vanished from the honor vocabulary — the constraint operates by conceptual exclusion, not physical barrier, which is a structurally different suppression mechanism than a legal ban.
 *
 * DIRECTIONALITY LOGIC:
 *   State authorities and clergy are agenda-setters administering the redefinition; they bear little cost and gain conceptual authority. The bourgeois professional class and honor-preserving aristocratic families are beneficiaries — the redefinition offers them dignity without risk. Residual practitioners and officer-corps holdouts are the targets: the same discursive shift that liberates others from violent risk strips them of their only legible vocabulary for restoring standing, which is why their d sits toward the target end despite no coercive apparatus being deployed against them specifically.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (adjudicating insult without endless feud) is dead in its original form — courts and press now perform that function — yet a residual subculture (military holdouts) still treats the old code as live, producing exactly the mismatch (status=dead, but a subset experiences world_rearranges-adjacent disruption) that flags a zombie-code pattern: not a captured extraction structure but an inertial piton where the surviving 'enforcement' is almost entirely reputational theater rather than functional coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contraction_vs_drop_discriminator,
    'Is the observed decline in dueling better explained by conceptual redefinition of honor (this reading) or by rising external costs (legal penalty, social ostracism) leaving the old honor code conceptually intact but practically unusable (drop_reading)?',
    'Textual analysis of contemporaneous honor discourse (sermons, dueling codes, court records, private correspondence) for direct evidence that duelists themselves, at the moment of decline, described dueling as no longer honorable (contraction) versus still honorable but too risky (drop). Convergent decline timing across jurisdictions with differing legal penalties would favor contraction; correlation with penalty severity alone would favor drop.',
    'If drop_reading is empirically favored, this story''s core mechanism (conceptual exclusion) collapses and the constraint should be re-typed toward a pure cost/suppression mechanism rather than a redefinition mechanism — changing beneficiary structure substantially, since the beneficiaries of a cost-based deterrent (state enforcement apparatus) differ from the beneficiaries of a conceptual redefinition (discourse-shaping institutions, rising professional classes).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contraction_vs_drop_discriminator, empirical, 'Whether decline is caused by conceptual exclusion or cost deterrence — the central kernel dispute.').

omega_variable(
    residual_holdout_authenticity,
    'Are the military officer-corps holdouts genuinely operating under an intact older honor code (supporting contraction as a real, contested conceptual boundary) or are they performing a nostalgic identity that both they and observers already recognize as anachronistic (supporting the piton/theater reading)?',
    'Court-martial records and officer memoirs from the late holdout period: do defendants argue their conduct WAS honorable by prevailing standards (contesting the redefinition) or do they concede the redefinition and argue only for leniency or tradition-based exception?',
    'If holdouts contest the redefinition on its own terms, the conceptual space has not fully contracted and the constraint is less settled than claimed_type=piton suggests. If holdouts concede the redefinition, the piton/theater characterization is strongly supported.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(residual_holdout_authenticity, empirical, 'Whether officer-corps holdouts contest or concede the redefinition.').

omega_variable(
    redefinition_naturalness_ambiguity,
    'Is the redefinition of honor to exclude violence a genuine moral/conceptual discovery (progress in ethical understanding) or a constructed narrative serving the interests of classes disadvantaged by the older violent code?',
    'Comparative cross-cultural analysis: if honor codes independently converge on excluding lethal violence across unconnected societies experiencing similar professionalization/state-centralization pressures, that would support a structural (not merely interest-driven) account; if the redefinition tracks class interest closely and reverses when class interests reverse, that supports constructed narrative.',
    'Bears on whether the beneficiary declarations (bourgeois_professional_class, state_judicial_authorities) indicate a constructed extraction dynamic riding on moral progress rhetoric, or whether the moral shift is substantially autonomous of those interests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(redefinition_naturalness_ambiguity, conceptual, 'Whether the honor redefinition is autonomous moral development or interest-driven narrative construction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__contraction_reading, 1770, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1770, honor_violence_legitimacy__contraction_reading, theater_ratio, 1770, 0.08).
narrative_ontology:measurement(hono_tr_t1800, honor_violence_legitimacy__contraction_reading, theater_ratio, 1800, 0.1).
narrative_ontology:measurement(hono_tr_t1820, honor_violence_legitimacy__contraction_reading, theater_ratio, 1820, 0.13).
narrative_ontology:measurement(hono_tr_t1840, honor_violence_legitimacy__contraction_reading, theater_ratio, 1840, 0.16).
narrative_ontology:measurement(hono_tr_t1860, honor_violence_legitimacy__contraction_reading, theater_ratio, 1860, 0.19).
narrative_ontology:measurement(hono_tr_t1880, honor_violence_legitimacy__contraction_reading, theater_ratio, 1880, 0.21).
narrative_ontology:measurement(hono_tr_t1900, honor_violence_legitimacy__contraction_reading, theater_ratio, 1900, 0.22).

% Extraction over time
narrative_ontology:measurement(hono_be_t1770, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1770, 0.15).
narrative_ontology:measurement(hono_be_t1800, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1800, 0.18).
narrative_ontology:measurement(hono_be_t1820, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1820, 0.21).
narrative_ontology:measurement(hono_be_t1840, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1840, 0.24).
narrative_ontology:measurement(hono_be_t1860, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1860, 0.26).
narrative_ontology:measurement(hono_be_t1880, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1880, 0.27).
narrative_ontology:measurement(hono_be_t1900, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1900, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(honor_violence_legitimacy__contraction_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__contraction_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy__drop_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the honor_violence_legitimacy kernel. contraction_reading (this file) holds the conceptual category of honorable violence itself contracted. drop_reading holds the category remained intact but external costs made acting on it rare. composite_reading holds both mechanisms operated together and are not separable in the historical record. Each carries its own ε, beneficiary/victim structure, and classification per the ε-invariance principle; they are linked via affects_constraints rather than merged into one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
