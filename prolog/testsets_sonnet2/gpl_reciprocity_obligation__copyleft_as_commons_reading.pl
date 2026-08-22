% ============================================================================
% CONSTRAINT STORY: gpl_reciprocity_obligation__copyleft_as_commons_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_reciprocity_obligation__copyleft_as_commons_reading, []).

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
 *   constraint_id: gpl_reciprocity_obligation__copyleft_as_commons_reading
 *   human_readable: GPL Reciprocity Obligation as Commons-Preservation Institution
 *   domain: software licensing / intellectual property / open source governance
 *
 * SUMMARY:
 *   This story authors the commons-preservation reading of the GPL's
 *   reciprocity obligation: the copyleft term as institutional technology
 *   whose function is preventing the enclosure of a shared software resource.
 *   Under this reading, the commons (and the developers who continue
 *   depositing into it) is the beneficiary class, and the victim class is
 *   individual actors who would otherwise extract private value from the
 *   commons without reciprocating — exit-maximizing developers and
 *   proprietary integrators whose business models depend on closed
 *   derivatives. This is a tangled rope: a genuine coordination function
 *   (sustaining a non-depleting shared resource) coexists with real, actively
 *   enforced extraction from a specific class (those who wanted to capture
 *   value without returning it). The obligation requires active enforcement —
 *   license compliance litigation, contributor agreements, and community
 *   policing — which is why requires_active_enforcement is true and both a
 *   beneficiary and a victim class are named.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.42).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.55).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_commons_reading, tangled_rope).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_commons_reading, "GPL Reciprocity Obligation as Commons-Preservation Institution").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_commons_reading, "software licensing / intellectual property / open source governance").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_commons_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_commons_reading, '740b58aa-b409-49bd-bca5-000bc85bac8a').
narrative_ontology:cs_kernel_codification('740b58aa-b409-49bd-bca5-000bc85bac8a', fixed_text).
narrative_ontology:cs_authority_grounding('740b58aa-b409-49bd-bca5-000bc85bac8a', practice).
narrative_ontology:cs_interpretation_layer_present('740b58aa-b409-49bd-bca5-000bc85bac8a').
narrative_ontology:cs_reading_relation('740b58aa-b409-49bd-bca5-000bc85bac8a', gpl_reciprocity_obligation__copyleft_as_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('740b58aa-b409-49bd-bca5-000bc85bac8a', gpl_reciprocity_obligation__copyleft_as_restriction_reading, influences).
narrative_ontology:cs_axiom('740b58aa-b409-49bd-bca5-000bc85bac8a', foundational, commons_non_depletion_is_the_governing_purpose).
narrative_ontology:cs_axiom_status(commons_non_depletion_is_the_governing_purpose, holdable).
narrative_ontology:cs_axiom_grounding('740b58aa-b409-49bd-bca5-000bc85bac8a', commons_non_depletion_is_the_governing_purpose, instrumental).
narrative_ontology:cs_axiom('740b58aa-b409-49bd-bca5-000bc85bac8a', secondary, reciprocity_obligation_is_proportionate_to_enclosure_risk).
narrative_ontology:cs_axiom_status(reciprocity_obligation_is_proportionate_to_enclosure_risk, holdable).
narrative_ontology:cs_axiom_grounding('740b58aa-b409-49bd-bca5-000bc85bac8a', reciprocity_obligation_is_proportionate_to_enclosure_risk, empirically_contingent).
narrative_ontology:cs_reference_frame('740b58aa-b409-49bd-bca5-000bc85bac8a', commons_preservation_founding_intent).
narrative_ontology:cs_drift_state('740b58aa-b409-49bd-bca5-000bc85bac8a', contemporary_commercial_open_source_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('740b58aa-b409-49bd-bca5-000bc85bac8a', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_commons_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_commons_reading, software_commons).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_commons_reading, downstream_reciprocal_developers).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_commons_reading, long_term_ecosystem_participants).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_commons_reading, individual_exit_maximizers).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_commons_reading, proprietary_integrators).
narrative_ontology:constraint_vindicates(gpl_reciprocity_obligation__copyleft_as_commons_reading, commons_non_enclosure_principle).
narrative_ontology:constraint_vindicates(gpl_reciprocity_obligation__copyleft_as_commons_reading, reciprocal_contribution_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The accumulated body of GPL-licensed code that persists as a shared, non-enclosable resource because every downstream modification must be released under the same terms. It collects no rents itself but is the entity whose non-depletion the reciprocity obligation exists to guarantee. Not a real actor; named for completeness.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, software_commons, beneficiary,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(gpl_reciprocity_obligation__copyleft_as_commons_reading, software_commons).

% Developers who build on GPL code and release their derivatives under the same license. They receive a growing, permanently open pool of reusable software in exchange for foregoing exclusive capture of their own improvements. Their exit option — relicensing privately — is blocked by the copyleft term, but they experience this as the price of continued access to a commons that keeps expanding because everyone else is bound the same way.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, downstream_reciprocal_developers, beneficiary,
    moderate, generational, constrained, global).

% Foundations, maintainer communities, and copyleft advocacy organizations (e.g. the FSF and analogous stewards) that both benefit from the durability of the commons and actively defend the reciprocity term through license enforcement, litigation, and community norm-setting. They treat any weakening of the obligation as an existential threat to the resource they steward.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, long_term_ecosystem_participants, beneficiary,
    organized, civilizational, mobile, global).
narrative_ontology:stakeholder_secondary_role(gpl_reciprocity_obligation__copyleft_as_commons_reading, long_term_ecosystem_participants, agenda_setter).

% Individual developers or small teams who want to take GPL code, extend it, and capture the extended value privately — selling a closed derivative or a proprietary SaaS wrapper without releasing their modifications. The reciprocity obligation forecloses this path entirely: any distribution of a derivative work triggers the copyleft requirement, so their only options are compliance (releasing their gains back to the commons), avoidance (not using GPL code at all), or violation (accepting legal exposure).
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, individual_exit_maximizers, payer,
    moderate, biographical, trapped, global).

% Commercial firms whose business models depend on proprietary differentiation. They can use permissively licensed alternatives, pay for dual-licensed commercial terms, or engineer around GPL components at cost, but cannot integrate GPL code into a closed product without triggering the reciprocity obligation. From the commons-reading perspective, they are the class the mandatory-reciprocity term specifically exists to prevent from enclosing shared contributions.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, proprietary_integrators, payer,
    powerful, biographical, constrained, global).

% Large firms that contribute to GPL projects under contributor license agreements would prefer weaker reciprocity terms (e.g. permissive relicensing rights) to preserve future proprietary optionality. Their preference is structurally present in license-negotiation contexts but is not part of the commons-reading's own justification — the commons reading treats their objection as exactly the enclosure risk the obligation guards against, not as a voice it needs to accommodate.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, corporate_contributors_under_cla, excluded,
    powerful, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine collective-action problem of commons depletion: without a binding reciprocity term, any actor can extract value from shared code, improve it privately, and never return the improvement, which over time converts a shared resource into a series of private forks and starves the commons of the contributions needed to sustain it.
% TRANSFER_FUNCTION: Moves the right to capture exclusive value from a derivative work away from the individual improver and back into the shared pool — the individual's labor-added value is transferred from potential private rent to permanent commons deposit, in exchange for perpetual access to everyone else's deposits.
% ABSENT_VOICES: Corporate contributors who would prefer weaker terms (dual-licensing flexibility, permissive relicensing rights) are present in license-negotiation rooms but structurally outside the commons reading's own justificatory frame, which treats their preference as the enclosure risk to be defended against rather than a stakeholder position to be balanced.
% DISAPPEARANCE_RATIONALE: If the reciprocity obligation vanished overnight, downstream forks could be closed and sold without returning improvements, and rational actors capturing private advantage would rapidly outcompete continued open contribution — within a few release cycles the shared commons would fragment into proprietary derivatives and the pool of freely reusable code would stop growing.
% FOUNDING_PROBLEM: Early free-software projects were losing ground to proprietary forks: a contributor's improvements could be taken, closed off, and sold back to the community that produced them, which discouraged contribution and threatened to hollow out the shared codebase entirely.
% FOUNDING_PROBLEM_CORROBORATION: Corporate legal departments that treat GPL compliance as an ongoing operational risk (independent of any FSF or foundation framing) attest that the enclosure risk is real and current, not historical — their compliance tooling and license-scanning industry exists specifically because firms still routinely attempt exactly the extraction the obligation targets.
narrative_ontology:disappearance_verdict(gpl_reciprocity_obligation__copyleft_as_commons_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_commons_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_commons_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gpl_reciprocity_obligation__copyleft_as_commons_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_reciprocity_obligation__copyleft_as_commons_reading_tests).
:- end_tests(gpl_reciprocity_obligation__copyleft_as_commons_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is set at a medium level (0.42 by interval end) because the cost imposed on exit-maximizers is real (foreclosed private capture) but bounded — they retain the option of avoiding GPL code entirely or paying for permissively-licensed or dual-licensed alternatives, so the obligation is not a full trap. Suppression is moderate (0.55): the reciprocity term is triggered only on distribution of a derivative work, and enforcement relies on legal mechanisms rather than physical coercion, but the term does foreclose a real business strategy (private enclosure) once GPL code is used. Theater ratio stays low throughout (0.15 at end) because the enforcement machinery (license compliance audits, litigation such as historical BusyBox/SFC cases) performs a genuine, non-decorative function; it has grown slightly over time as the commercial software ecosystem's reliance on open components has grown and compliance tooling has professionalized.
 *
 * DIRECTIONALITY LOGIC:
 *   The commons itself and reciprocal downstream developers sit near the beneficiary end of directionality: they receive perpetual access to an expanding, non-depletable resource in exchange for a reciprocity obligation they already intend to honor. Long-term ecosystem stewards (foundations, maintainer communities) are both beneficiaries and agenda-setters — they actively enforce the term because their institutional survival depends on the commons remaining non-enclosable. Individual exit-maximizers and proprietary integrators sit near the target end: the obligation specifically forecloses the strategy they would otherwise pursue (private capture of derivative value), and their exit options are constrained or trapped once they have adopted GPL code, though they retain upstream choice not to adopt it in the first place.
 *
 * MANDATROPHY ANALYSIS:
 *   The commons reading resists collapsing this into pure extraction (the copyleft-as-restriction sibling's frame) by keeping the coordination function structurally load-bearing: the reciprocity term is not merely a tax on integrators, it is the mechanism by which the shared resource avoids the tragedy-of-the-commons failure mode that would make the whole ecosystem worse off, including for future integrators. Equally, it resists collapsing into pure rope (the copyleft-as-freedom sibling's frame) by naming a real victim class whose foreclosed strategy is a genuine cost, not merely a preference frustrated by good coordination. The founding problem — proprietary fork extraction depleting community contribution — remains live per corporate compliance-industry evidence, which is corroboration from outside the beneficiary class (FSF, foundations) that would otherwise be the only voice attesting the problem persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commons_vs_restriction_framing_choice,
    'Is the primary beneficiary of the reciprocity obligation the software commons as an institution (this reading), or is ''the commons'' itself a legitimizing narrative for what is structurally a restriction on commercial integrators'' business models (the copyleft_as_restriction_reading)?',
    'Track which framing better predicts real-world outcomes: if projects with strong copyleft enforcement show measurably higher long-term contribution rates and lower fork-abandonment than permissively-licensed comparators, the commons framing gains support; if enforcement primarily correlates with litigation revenue or foundation fundraising rather than measurable commons health, the restriction framing gains support.',
    'If the restriction framing is correct, extractiveness should be authored substantially higher and the beneficiary set narrowed to license-enforcement institutions rather than ''the commons'' broadly, which would likely shift this story''s own classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commons_vs_restriction_framing_choice, conceptual, 'Whether commons-preservation or business-restriction is the structurally accurate primary function of the same reciprocity clause.').

omega_variable(
    corporate_participation_disagreement,
    'Do corporate contributors who chafe under copyleft terms while still participating represent a legitimate dissenting stakeholder position that the commons reading under-weighs, or are they simply the enclosure-seeking class the obligation is designed to constrain?',
    'Examine whether corporate contributors who successfully negotiate weaker terms (dual licensing, CLAs with relicensing rights) produce measurably different downstream commons health than projects that hold a strict copyleft line.',
    'If corporate-negotiated weaker terms do not measurably harm commons health, the commons reading''s treatment of corporate preference as pure enclosure risk is overstated and the excluded-voice framing understates a legitimate interest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(corporate_participation_disagreement, preference, 'Whether corporate dissent from strict copyleft terms is legitimate stakeholder interest or enclosure-seeking to be resisted.').

omega_variable(
    extractiveness_magnitude_across_readings,
    'Given that all three sibling readings share the identical mechanical license term, why should ε differ across them at all, rather than reflecting one true underlying extraction level that each reading merely describes differently?',
    'This is resolved by the ε-invariance principle itself: each reading authors a different beneficiary/victim structure over the same text, and ε is a property of the reading''s own account of who bears cost and who benefits, not a single observer-independent fact about the clause. The resolution is conceptual, not empirical — confirm the three sibling stories remain internally consistent in their own beneficiary/victim declarations rather than seeking a single ''true'' ε.',
    'Confirms these are legitimately three separate constraints rather than one constraint inconsistently measured; if instead a single ε were forced to fit all three narratives, that would itself indicate mis-decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extractiveness_magnitude_across_readings, conceptual, 'Why the same license clause legitimately carries different ε across sibling readings rather than indicating measurement error.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(gpl__tr_t6, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 6, 0.09).
narrative_ontology:measurement(gpl__tr_t12, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 12, 0.1).
narrative_ontology:measurement(gpl__tr_t18, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 18, 0.12).
narrative_ontology:measurement(gpl__tr_t24, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 24, 0.14).
narrative_ontology:measurement(gpl__tr_t30, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 30, 0.15).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(gpl__be_t6, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 6, 0.33).
narrative_ontology:measurement(gpl__be_t12, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 12, 0.36).
narrative_ontology:measurement(gpl__be_t18, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 18, 0.38).
narrative_ontology:measurement(gpl__be_t24, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 24, 0.4).
narrative_ontology:measurement(gpl__be_t30, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 30, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(gpl__su_t6, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 6, 0.44).
narrative_ontology:measurement(gpl__su_t12, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 12, 0.47).
narrative_ontology:measurement(gpl__su_t18, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 18, 0.5).
narrative_ontology:measurement(gpl__su_t24, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 24, 0.53).
narrative_ontology:measurement(gpl__su_t30, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_commons_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.12).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_commons_reading, gpl_reciprocity_obligation__copyleft_as_freedom_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_commons_reading, gpl_reciprocity_obligation__copyleft_as_restriction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the gpl_reciprocity_obligation kernel, decomposed per the ε-invariance principle rather than authored as a single observer-relative constraint. copyleft_as_commons_reading (this story) authors the commons-as-institution as beneficiary and individual exit-maximizers as victim, at medium extractiveness (0.42), classified tangled_rope. copyleft_as_freedom_reading authors end-user software freedom as the beneficiary interest, typically rope-leaning with lower extraction. copyleft_as_restriction_reading authors commercial integrators broadly as victims of a business-model constraint, typically snare-leaning with higher extraction. All three share the identical GPL derivative-work clause as their textual kernel but diverge in beneficiary/victim structure and resulting classification; they are linked bidirectionally via affects_constraints so contamination and drift analysis can propagate across the reading family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
