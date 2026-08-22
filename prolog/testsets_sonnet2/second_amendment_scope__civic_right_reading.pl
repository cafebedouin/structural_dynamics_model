% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__civic_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_scope__civic_right_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: second_amendment_scope__civic_right_reading
 *   human_readable: Second Amendment as Civic-Republican Militia-Conditioned Right
 *   domain: constitutional_law/political_theory/rights_jurisprudence
 *
 * SUMMARY:
 *   This constraint instantiates the civic-right reading of the Second
 *   Amendment kernel: the right to keep and bear arms is constitutionally
 *   protected but its scope and the permissible extent of regulation are
 *   keyed to a civic-militia function rather than to unconditioned individual
 *   ownership (the individual_right_reading) or to state authority alone with
 *   no individual claim (the collective_right_reading). Under this reading,
 *   militia-eligible individuals and the regulatory authorities that
 *   administer the militia-connected boundary both gain something
 *   structurally real — a workable line for regulation that need not either
 *   eliminate or unconditionally protect the right — while individuals whose
 *   firearm ownership has no civic-defense connection, and those historically
 *   excluded from militia-eligibility categories, bear the interpretive cost
 *   of a right conditioned on a status they do not hold. The extraction here
 *   is moderate, not severe: this reading does not abolish the individual
 *   right (unlike a strict collective-right reading) but it does gate access
 *   to its fullest protection behind a service-connected predicate, which
 *   functions as an implicit means-test on constitutional protection.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__civic_right_reading, 0.42).
domain_priors:suppression_score(second_amendment_scope__civic_right_reading, 0.38).
domain_priors:theater_ratio(second_amendment_scope__civic_right_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__civic_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_scope__civic_right_reading, "Second Amendment as Civic-Republican Militia-Conditioned Right").
narrative_ontology:topic_domain(second_amendment_scope__civic_right_reading, "constitutional_law/political_theory/rights_jurisprudence").

domain_priors:requires_active_enforcement(second_amendment_scope__civic_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__civic_right_reading, 'fea21d28-da90-44dc-81af-15f61629174b').
narrative_ontology:cs_kernel_codification('fea21d28-da90-44dc-81af-15f61629174b', fixed_text).
narrative_ontology:cs_authority_grounding('fea21d28-da90-44dc-81af-15f61629174b', lineage).
narrative_ontology:cs_interpretation_layer_present('fea21d28-da90-44dc-81af-15f61629174b').
narrative_ontology:cs_reading_relation('fea21d28-da90-44dc-81af-15f61629174b', second_amendment_scope__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('fea21d28-da90-44dc-81af-15f61629174b', second_amendment_scope__collective_right_reading, coexists_with).
narrative_ontology:cs_axiom('fea21d28-da90-44dc-81af-15f61629174b', foundational, right_conditioned_on_civic_militia_connection).
narrative_ontology:cs_axiom_status(right_conditioned_on_civic_militia_connection, holdable).
narrative_ontology:cs_axiom_grounding('fea21d28-da90-44dc-81af-15f61629174b', right_conditioned_on_civic_militia_connection, conventional).
narrative_ontology:cs_axiom('fea21d28-da90-44dc-81af-15f61629174b', secondary, regulation_permissible_when_disconnected_from_militia_purpose).
narrative_ontology:cs_axiom_status(regulation_permissible_when_disconnected_from_militia_purpose, holdable).
narrative_ontology:cs_axiom_grounding('fea21d28-da90-44dc-81af-15f61629174b', regulation_permissible_when_disconnected_from_militia_purpose, instrumental).
narrative_ontology:cs_reference_frame('fea21d28-da90-44dc-81af-15f61629174b', founding_era_organized_militia_service).
narrative_ontology:cs_drift_state('fea21d28-da90-44dc-81af-15f61629174b', post_heller_contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fea21d28-da90-44dc-81af-15f61629174b', '').
narrative_ontology:cs_kernel_id(second_amendment_scope__civic_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__civic_right_reading, militia_eligible_individuals).
narrative_ontology:constraint_beneficiary(second_amendment_scope__civic_right_reading, state_regulatory_authorities).
narrative_ontology:constraint_victim(second_amendment_scope__civic_right_reading, unaffiliated_gun_owners).
narrative_ontology:constraint_victim(second_amendment_scope__civic_right_reading, individuals_excluded_from_militia_eligibility).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(second_amendment_scope__civic_right_reading, gun_control_advocacy_organizations).
narrative_ontology:constraint_vindicates(second_amendment_scope__civic_right_reading, civic_republican_constitutionalism).
narrative_ontology:constraint_vindicates(second_amendment_scope__civic_right_reading, well_regulated_militia_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adult citizens who would qualify for organized militia service (historically the 'able-bodied' citizenry, today mapped loosely onto National Guard eligibility or analogous civic-defense structures) retain a constitutionally protected right to keep and bear arms BECAUSE that service obligation and its correlative right run together. Their right is real but framed as flowing from civic role rather than from personhood alone.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, militia_eligible_individuals, beneficiary,
    moderate, generational, constrained, national).

% Legislatures and courts adopting this reading gain wide latitude to condition, license, and structure firearms possession around militia-relevant criteria (registration, training, organizational affiliation) without being seen as abrogating the right altogether. They administer the boundary between protected civic-armament and unprotected private accumulation, and collect the discretionary regulatory authority this reading confers.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, state_regulatory_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Individuals who own firearms for self-defense, sport, or personal reasons entirely disconnected from any militia framework find their claim to constitutional protection weakened or contingent under this reading. They bear the interpretive cost: their ownership must be justified by reference to a civic function they neither perform nor necessarily endorse, or it falls outside the amendment's core protection and becomes subject to ordinary regulatory burden.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, unaffiliated_gun_owners, payer,
    powerless, biographical, constrained, national).

% Historically and functionally, militia-eligibility criteria have tracked exclusionary categories (age, citizenship status, disability, and historically race and sex). Those outside the eligible class under a strict civic-right framing risk a doubly weakened claim to the right — excluded from the militia predicate that grounds it, and thus most exposed to restriction with least room to contest it in militia-service terms.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, individuals_excluded_from_militia_eligibility, payer,
    powerless, biographical, trapped, national).

% Organizations built around an unconditional personhood-based reading are structurally sidelined by this framing: their preferred textual and historical arguments (individual right regardless of militia service) are treated as the losing account rather than as one live possibility. They contest the reading in litigation and legislative testimony but are not the interpretive authority under this reading.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, gun_rights_advocacy_organizations, excluded,
    organized, biographical, mobile, national).

% Organizations favoring stronger regulation benefit from this reading's latitude for civic-function-based conditioning, but are also excluded from the more restrictive collective-right reading they might prefer (state authority only, no individual right at all) — this reading is a partial, not full, win for their preferred outcome.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, gun_control_advocacy_organizations, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_scope__civic_right_reading, gun_control_advocacy_organizations, excluded).

% Scholars of founding-era militia statutes, English common-law antecedents, and ratification debates assess whether the civic-right reading accurately reconstructs original public meaning or is itself a compromise construction assembled to split the difference between the individual-right and collective-right camps.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ties the constitutionally protected right to keep and bear arms to a recognizable civic obligation — organized, trained, and regulable militia service — giving legislatures and courts a principled anchor for permissible regulation that neither eliminates the individual right nor leaves it wholly unconditioned.
% TRANSFER_FUNCTION: Moves interpretive authority and regulatory latitude toward state and federal authorities administering militia-adjacent criteria, and moves constitutional protection preferentially toward those who can plausibly claim militia-connected purposes, away from those whose firearm ownership is purely private and unconnected to civic defense.
% ABSENT_VOICES: Firearms owners for whom the civic-militia frame is simply irrelevant to their actual reason for ownership (personal self-defense, hunting, collecting) rarely get to reframe the debate in those terms within this reading's own logic — the frame itself structures what counts as a cognizable claim, so their objection is heard as a request for exception rather than as a challenge to the premise.
% DISAPPEARANCE_RATIONALE: If the civic-right reading were abandoned, courts adopting the individual-right reading would extend broader unconditional protection (world rearranges toward less regulability), while courts adopting the collective-right reading would eliminate individual protection nearly entirely (world rearranges toward more regulability) — which direction the world moves depends entirely on which sibling reading fills the vacuum, which is exactly why the three readings are separate constraints rather than one.
% FOUNDING_PROBLEM: The founding-era problem was distrust of standing armies and reliance on an armed, organized citizenry (the militia) for common defense and as a check on federal military power — the right was historically bundled with an expectation of organized, disciplined service.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians and founding-era militia-statute scholars attest that organized militia service was the operative historical context for the amendment's drafting; this attestation sits outside both the militia-eligible beneficiary class and the state-regulatory-authority beneficiary class, though it is itself contested by historians favoring the individual-right account of the same ratification record.
narrative_ontology:disappearance_verdict(second_amendment_scope__civic_right_reading, contested).
narrative_ontology:founding_problem_status(second_amendment_scope__civic_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__civic_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_scope__civic_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_scope__civic_right_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_scope__civic_right_reading_tests).
:- end_tests(second_amendment_scope__civic_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects genuine but bounded cost: unaffiliated owners and excluded classes face regulatory exposure keyed to a status test, but the reading preserves an individual right in some form, unlike the collective-right sibling. Suppression (0.38) is moderate — enforcement runs through ordinary licensing and regulatory apparatus, not through wholesale prohibition. Resistance is comparatively high (0.62) because this reading sits contested between two more absolute sibling positions, each with organized advocacy constituencies actively litigating against the middle ground. Accessibility collapse is moderate-low (0.35): alternative readings remain fully live in courts and legislatures, this is not a settled doctrinal floor.
 *
 * DIRECTIONALITY LOGIC:
 *   Militia-eligible individuals and state regulatory authorities sit toward the beneficiary end: the former retain a right whose scope tracks a status many of them satisfy or can satisfy, the latter gain principled regulatory latitude. Unaffiliated gun owners and militia-ineligible individuals sit toward the target end: their claim to protection is conditioned on a predicate irrelevant or unavailable to their actual situation, and their exit options (litigating for individual-right reclassification, or accepting regulatory exposure) are constrained or trapped respectively.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — organized citizen militias as a check on standing armies — is largely dead in practical military terms (professional militaries and National Guard structures have superseded citizen-militia defense), yet the interpretive framework persists because it offers a workable doctrinal compromise, not because the original civic-defense function is live. This is the classic tangled-rope signature: real coordination benefit (a principled regulatory anchor) layered with real asymmetric cost (conditioning protection on an increasingly notional civic-service predicate) sustained by active judicial and legislative enforcement rather than by the founding problem's continued vitality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    civic_right_as_genuine_compromise_or_constructed_middle,
    'Is the civic-right reading a historically accurate reconstruction of founding-era original public meaning, or is it a jurisprudentially constructed middle position assembled to mediate between two more textually direct extremes?',
    'Comparative analysis of founding-era militia statutes, state constitutional analogues, and ratification-era commentary against the specific claim that the right was understood as service-conditioned rather than either purely individual or purely collective.',
    'If the reading is a constructed compromise rather than an original-meaning reconstruction, its claim to constitutional authority is weaker than its practical utility as a judicial management tool, which would reframe its persistence as institutional convenience rather than interpretive fidelity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civic_right_as_genuine_compromise_or_constructed_middle, conceptual, 'Whether the civic-right reading is genuine original meaning or a constructed doctrinal compromise.').

omega_variable(
    militia_eligibility_criteria_exclusion_history,
    'To what extent do historical militia-eligibility criteria (age, sex, race, disability, citizenship) still shadow this reading''s implicit boundary of who counts as a rights-bearing civic participant, even when formally reformed?',
    'Doctrinal and historical audit of which groups courts applying a civic-right framework have treated as presumptively within or outside the militia-connected class, tracked over time.',
    'If exclusionary historical criteria persist functionally even after formal reform, the victim group ''individuals_excluded_from_militia_eligibility'' carries a durable structural disadvantage baked into the reading''s logic rather than a transitional one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_eligibility_criteria_exclusion_history, empirical, 'Whether historical exclusion from militia-eligibility persists functionally within this reading''s boundary logic.').

omega_variable(
    sibling_reading_selection_pressure,
    'Given that all three kernel readings remain live in contemporary courts and legislatures, what determines which reading a given jurisdiction adopts — is it doctrinal reasoning, judicial composition, or exogenous political alignment?',
    'Cross-jurisdictional comparison of adopted reading against judicial appointment history and legislative composition at time of adoption.',
    'If reading-adoption tracks political alignment more than doctrinal reasoning, the civic-right reading''s claimed moderate/compromise character is itself a contested political artifact rather than a neutral interpretive position.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_selection_pressure, empirical, 'Whether adoption of this reading over its siblings tracks doctrine or political composition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__civic_right_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_scope__civic_right_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(seco_tr_t8, second_amendment_scope__civic_right_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(seco_tr_t16, second_amendment_scope__civic_right_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement(seco_tr_t24, second_amendment_scope__civic_right_reading, theater_ratio, 24, 0.25).
narrative_ontology:measurement(seco_tr_t32, second_amendment_scope__civic_right_reading, theater_ratio, 32, 0.27).
narrative_ontology:measurement(seco_tr_t40, second_amendment_scope__civic_right_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_scope__civic_right_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(seco_be_t8, second_amendment_scope__civic_right_reading, base_extractiveness, 8, 0.33).
narrative_ontology:measurement(seco_be_t16, second_amendment_scope__civic_right_reading, base_extractiveness, 16, 0.36).
narrative_ontology:measurement(seco_be_t24, second_amendment_scope__civic_right_reading, base_extractiveness, 24, 0.39).
narrative_ontology:measurement(seco_be_t32, second_amendment_scope__civic_right_reading, base_extractiveness, 32, 0.41).
narrative_ontology:measurement(seco_be_t40, second_amendment_scope__civic_right_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_scope__civic_right_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement(seco_su_t8, second_amendment_scope__civic_right_reading, suppression_requirement, 8, 0.34).
narrative_ontology:measurement(seco_su_t16, second_amendment_scope__civic_right_reading, suppression_requirement, 16, 0.35).
narrative_ontology:measurement(seco_su_t24, second_amendment_scope__civic_right_reading, suppression_requirement, 24, 0.36).
narrative_ontology:measurement(seco_su_t32, second_amendment_scope__civic_right_reading, suppression_requirement, 32, 0.37).
narrative_ontology:measurement(seco_su_t40, second_amendment_scope__civic_right_reading, suppression_requirement, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__civic_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_scope__civic_right_reading, 0.12).
narrative_ontology:affects_constraint(second_amendment_scope__civic_right_reading, individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__civic_right_reading, collective_right_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language 'Second Amendment scope' claim per the epsilon-invariance principle: civic_right_reading (this story, moderate epsilon ~0.42, tangled_rope), individual_right_reading (expected lower epsilon, rope-leaning), and collective_right_reading (expected different beneficiary/victim structure, likely tangled_rope or snare depending on how completely it eliminates individual claims). Each carries its own epsilon and classification; they are linked here, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
