% ============================================================================
% CONSTRAINT STORY: all_men_created_equal__universalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_all_men_created_equal__universalist_reading, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: all_men_created_equal__universalist_reading
 *   human_readable: The Declaration's Equality Clause Read as a Universal, Self-Expanding Principle
 *   domain: constitutional_law/political_philosophy/american_studies
 *
 * SUMMARY:
 *   This story instantiates the universalist reading of the 'all men are
 *   created equal' kernel: the clause is treated as a standing,
 *   self-expanding moral-textual commitment whose scope was under-realized at
 *   the founding and is progressively completed through subsequent legal and
 *   political struggle, independent of what the framers actually intended to
 *   include. This is a distinct constraint from the originalist reading
 *   (which bounds the clause's scope by 18th-century social taxonomy and
 *   treats founder intent as dispositive) and from the textualist paradox
 *   reading (which reads the same historical gap as an irreconcilable
 *   performative contradiction rather than a program of completion). All
 *   three readings share the same kernel text but diverge on what kind of
 *   claim the text makes and what follows from its unrealized scope at any
 *   given moment; this file authors only the universalist reading's own ε,
 *   beneficiaries, victims, and classification.
 *
 * KEY AGENTS:
 *   - civil_rights_movements: agenda_setter/beneficiary (organized/constrained) — mobilizes and administers the expansion
 *   - previously_excluded_groups_claiming_inclusion: beneficiary (powerless/trapped) — the class currently pressing a claim
 *   - groups_still_denied_equal_status_under_incomplete_expansion: payer (powerless/trapped) — bears the cost of the gap between rhetoric and realization
 *   - incumbent_beneficiaries_of_narrower_readings: payer (powerful/constrained) — loses relative status with each expansion
 *   - constitutional_courts: observer/agenda_setter (institutional/analytical) — adjudicates and thereby constructs current scope
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(all_men_created_equal__universalist_reading, 0.42).
domain_priors:suppression_score(all_men_created_equal__universalist_reading, 0.38).
domain_priors:theater_ratio(all_men_created_equal__universalist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(all_men_created_equal__universalist_reading, tangled_rope).
narrative_ontology:human_readable(all_men_created_equal__universalist_reading, "The Declaration's Equality Clause Read as a Universal, Self-Expanding Principle").
narrative_ontology:topic_domain(all_men_created_equal__universalist_reading, "constitutional_law/political_philosophy/american_studies").

domain_priors:requires_active_enforcement(all_men_created_equal__universalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(all_men_created_equal__universalist_reading, 'ff50eaef-82d6-453e-872c-39fb77e69eb6').
narrative_ontology:cs_kernel_codification('ff50eaef-82d6-453e-872c-39fb77e69eb6', fixed_text).
narrative_ontology:cs_authority_grounding('ff50eaef-82d6-453e-872c-39fb77e69eb6', practice).
narrative_ontology:cs_interpretation_layer_present('ff50eaef-82d6-453e-872c-39fb77e69eb6').
narrative_ontology:cs_reading_relation('ff50eaef-82d6-453e-872c-39fb77e69eb6', all_men_created_equal__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('ff50eaef-82d6-453e-872c-39fb77e69eb6', all_men_created_equal__textualist_paradox_reading, influences).
narrative_ontology:cs_axiom('ff50eaef-82d6-453e-872c-39fb77e69eb6', foundational, equality_principle_scope_exceeds_founding_application).
narrative_ontology:cs_axiom_status(equality_principle_scope_exceeds_founding_application, holdable).
narrative_ontology:cs_axiom_grounding('ff50eaef-82d6-453e-872c-39fb77e69eb6', equality_principle_scope_exceeds_founding_application, deontological).
narrative_ontology:cs_axiom('ff50eaef-82d6-453e-872c-39fb77e69eb6', secondary, founder_intent_nondispositive_of_proper_scope).
narrative_ontology:cs_axiom_status(founder_intent_nondispositive_of_proper_scope, holdable).
narrative_ontology:cs_axiom_grounding('ff50eaef-82d6-453e-872c-39fb77e69eb6', founder_intent_nondispositive_of_proper_scope, conventional).
narrative_ontology:cs_reference_frame('ff50eaef-82d6-453e-872c-39fb77e69eb6', declaration_as_incomplete_universal_commitment).
narrative_ontology:cs_drift_state('ff50eaef-82d6-453e-872c-39fb77e69eb6', post_civil_rights_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('ff50eaef-82d6-453e-872c-39fb77e69eb6', '').
narrative_ontology:cs_kernel_id(all_men_created_equal__universalist_reading, all_men_created_equal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(all_men_created_equal__universalist_reading, previously_excluded_groups_claiming_inclusion).
narrative_ontology:constraint_beneficiary(all_men_created_equal__universalist_reading, civil_rights_movements).
narrative_ontology:constraint_beneficiary(all_men_created_equal__universalist_reading, constitutional_reform_coalitions).
narrative_ontology:constraint_victim(all_men_created_equal__universalist_reading, groups_still_denied_equal_status_under_incomplete_expansion).
narrative_ontology:constraint_victim(all_men_created_equal__universalist_reading, incumbent_beneficiaries_of_narrower_readings).
narrative_ontology:constraint_vindicates(all_men_created_equal__universalist_reading, moral_progress_thesis).
narrative_ontology:constraint_vindicates(all_men_created_equal__universalist_reading, constitution_as_living_document_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mobilize the universalist reading as a lever: they invoke 'all men are created equal' as a standing promissory note and press courts, legislatures, and public opinion to extend its scope to groups the founders did not contemplate. They administer the expansion in practice — bringing the cases, drafting the amendments, building the coalitions — but bear the full cost of each fight in resources, risk, and time before any legal payoff lands.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, civil_rights_movements, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(all_men_created_equal__universalist_reading, civil_rights_movements, beneficiary).

% Formerly enslaved people, women, non-property-holders, and later other excluded classes whose claim to equal standing was denied at the founding. The universalist reading gives them a textual hook to demand inclusion without needing a new constitutional text; they cannot exit the polity to escape their exclusion, so the reading's expansion is often their only available lever.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, previously_excluded_groups_claiming_inclusion, beneficiary,
    powerless, biographical, trapped, national).

% At any given moment the universalist reading is only partially realized — some class of persons remains outside its practical scope even as the principle claims to already cover them. This group bears the cost of the gap between the principle's rhetorical universality and its incomplete legal instantiation, and the doctrine's own success stories are cited against their claim ('the principle already includes everyone; be patient').
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, groups_still_denied_equal_status_under_incomplete_expansion, payer,
    powerless, biographical, trapped, national).

% Those who held privileged legal, economic, or social status precisely because the equality clause was read narrowly at a given moment — slaveholders, then those benefiting from coverture, then those benefiting from Jim Crow's separate-but-equal doctrine. Each expansion of the universalist reading directly redistributes standing, franchise, or property away from this group; their exit option is political resistance or relitigation, not departure.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, incumbent_beneficiaries_of_narrower_readings, payer,
    powerful, biographical, constrained, national).

% Judges and scholars committed to founder-intent readings object that the universalist reading substitutes present-day moral commitment for the document's actual historical meaning, but within the universalist framework their objection is treated as a symptom of the problem (residual restriction) rather than a competing account of what the text means. Their reading survives as a live sibling elsewhere, but is not part of this constraint's internal adjudication.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, originalist_jurists, excluded,
    institutional, generational, constrained, national).

% Amendment drafters and ratification campaigners who convert the universalist reading's moral pressure into formal textual change (13th/14th/15th/19th Amendments and beyond). They benefit from the reading's legitimating force — it makes their proposed changes look like completion rather than innovation — while carrying the organizing cost of each ratification campaign.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, constitutional_reform_coalitions, beneficiary,
    organized, civilizational, constrained, national).
narrative_ontology:stakeholder_secondary_role(all_men_created_equal__universalist_reading, constitutional_reform_coalitions, agenda_setter).

% Adjudicate which groups' claims to inclusion the equality principle currently reaches, weighing precedent, text, and social consensus. Their rulings both apply and construct the universalist reading's practical scope at any given moment, making them simultaneously analysts of the doctrine and agents who expand or restrict its reach.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(all_men_created_equal__universalist_reading, constitutional_courts, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(all_men_created_equal__universalist_reading, diffuse).
narrative_ontology:fixing_cost_class(all_men_created_equal__universalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, stable moral-textual anchor that successive generations and movements can invoke to coordinate claims for inclusion without having to draft an entirely new founding document each time — the existing text is treated as already containing the warrant for expansion, which lowers the argumentative burden of each subsequent claim.
% TRANSFER_FUNCTION: Moves legal standing, franchise, and formal equal treatment from groups currently holding a status advantage under the narrower historical application toward groups pressing a claim to inclusion under the same textual principle; also moves interpretive authority from founder-intent toward present and future claimants and adjudicators.
% ABSENT_VOICES: Groups whose claims to inclusion have not yet been organized into a legible political or legal movement are structurally invisible to this reading's mechanism — the principle expands where mobilization exists, not automatically; unmobilized excluded groups have no seat at all, and originalist jurists who dispute the reading's legitimacy are present as objectors but their account of the text's actual meaning is not incorporated into how the universalist reading operates.
% DISAPPEARANCE_RATIONALE: If the universalist reading were abandoned overnight in favor of strict founder-intent construction, the textual basis for extending 'created equal' beyond the 18th-century propertied white male citizenry would collapse — civil rights jurisprudence resting on this reading (much of Reconstruction and post-Reconstruction equal-protection doctrine) would lose its interpretive anchor, forcing reliance on separate positive enactments alone; movements currently invoking the clause would lose a primary rhetorical and legal lever.
% FOUNDING_PROBLEM: The Declaration's equality clause was invoked to justify separation from a monarchy on grounds of inherent human equality, while the polity that adopted it simultaneously excluded large classes of people from its practical scope — the universalist reading was constructed to resolve that gap by treating the principle as always having been broader than its initial application, requiring only recognition and enforcement rather than new invention.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians outside the civil rights movements themselves (constitutional scholars documenting the gap between 1776 rhetoric and 1776 practice) corroborate that the founding-era application was narrow and that subsequent expansion required real political and legal work rather than mere textual discovery. Originalist jurists, from outside the universalist reading's own tradition, dispute that the founding problem the universalist reading claims to solve was ever the text's actual problem — they hold the founders' intent was itself the boundary, not a temporary restriction awaiting correction. No source fully outside all contesting parties settles which account of the founding problem is correct.
narrative_ontology:disappearance_verdict(all_men_created_equal__universalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(all_men_created_equal__universalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(all_men_created_equal__universalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(all_men_created_equal__universalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(all_men_created_equal__universalist_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(all_men_created_equal__universalist_reading_tests).
:- end_tests(all_men_created_equal__universalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) rather than low or high because the universalist reading's own operation redistributes real standing and resources at each expansion — the coordination benefit (a shared textual anchor lowering the argumentative cost of each inclusion claim) is real, but so is the transfer away from incumbent beneficiaries of the narrower scope. Suppression sits at 0.38 because the reading's persistence depends partly on active enforcement (litigation, amendment, judicial doctrine) and partly on genuine, non-coerced moral consensus that has grown over time — it is not purely coercive but not purely voluntary either. Accessibility collapse is moderate-low (0.3): the originalist and textualist-paradox alternatives remain live, argued positions rather than fully foreclosed, which is why this reading requires active enforcement to prevail in practice rather than resting on settled consensus. Resistance is comparatively high (0.62) because incumbent beneficiaries and originalist jurists mount sustained, organized opposition to each expansion.
 *
 * DIRECTIONALITY LOGIC:
 *   Civil rights movements and constitutional reform coalitions sit near the beneficiary end structurally (d low) because the universalist reading legitimates and accelerates their claims, even though they personally bear organizing costs. Previously excluded groups claiming inclusion are the clearest beneficiaries once inclusion is won, but their trapped exit options (they cannot leave the polity to escape exclusion) mean the reading is often their only lever, which the derivation should register as high dependency rather than low stakes. Groups still denied equal status under incomplete expansion and incumbent beneficiaries of narrower readings are the two payer classes, but for structurally opposite reasons: the former pay because the principle's promise outruns its current legal reach; the latter pay because each completed expansion strips them of a relative advantage they held under the narrower reading. Both are declared victims but the directionality derivation should not conflate them.
 *
 * MANDATROPHY ANALYSIS:
 *   The universalist reading resists mandatrophy exactly where its founding problem remains contested rather than dead: as long as identifiable classes remain outside full equal standing (the payer group 'groups_still_denied_equal_status'), the reading's coordination function — providing a textual lever for further claims — stays live. If a future state were reached where no class could plausibly claim exclusion, continued invocation of the clause to justify further redistribution would risk becoming pure extraction dressed as completion; the R5 fields here mark the founding problem as contested rather than resolved specifically to keep that boundary visible rather than assumed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founder_intent_relevance,
    'Is the universalist reading''s claim that founder intent is irrelevant to the clause''s proper scope itself defensible, or does it substitute present moral commitment for the text''s actual original meaning?',
    'Historical-textual analysis of drafting debates and contemporaneous usage of ''all men'' against later invocations by expansion movements; comparison of how originalist and universalist traditions each treat the same primary sources.',
    'If founder intent is judged dispositive of proper scope, the universalist reading''s expansions are better classified as new positive commitments layered onto the text rather than completions of an existing one, which would shift some of the measured coordination function toward pure innovation rather than fidelity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founder_intent_relevance, conceptual, 'Whether founder intent is a legitimate boundary on the clause''s scope or an irrelevant historical accident.').

omega_variable(
    expansion_endpoint_indeterminacy,
    'Does the universalist reading have any principled stopping point for what counts as a legitimate claim to inclusion, or does ''iterative expansion'' license indefinite redefinition of who the principle covers?',
    'Track whether courts and movements applying this reading have ever articulated a limiting principle distinguishing legitimate expansion claims from illegitimate ones, versus relying purely on political mobilization success as the filter.',
    'If no principled endpoint exists, the reading''s coordination function is harder to distinguish from an open-ended license for whichever coalition can mobilize enough power, which would push the classification toward tangled_rope with a larger extractive component over time; if a principled endpoint exists, the reading is more clearly a genuine completion doctrine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expansion_endpoint_indeterminacy, conceptual, 'Whether the universalist reading has a limiting principle or is open-ended.').

omega_variable(
    kernel_committer_structure,
    'This constraint is one of three readings of the all_men_created_equal kernel (universalist, originalist, textualist_paradox). Where exactly does the disagreement between this reading and the sibling readings live?',
    'The disagreement is located at whether the clause''s original restricted application was (a) a temporary gap awaiting completion (this reading), (b) the clause''s actual bounded meaning (originalist_reading), or (c) evidence the text is self-contradictory and cannot be resolved by either completion or restriction (textualist_paradox_reading). A sibling reading adopting (b) would foreclose this reading''s premise that expansion is fidelity rather than innovation; a sibling adopting (c) would not foreclose this reading but would deny that ''iterative expansion'' resolves anything at the level of the text itself.',
    'If the originalist reading''s premise is adopted as authoritative by a controlling legal institution, this reading''s coordination claim (that expansion completes rather than amends the founding commitment) loses its textual warrant and each expansion becomes visibly a new enactment rather than a discovery — this would not change this reading''s authored ε directly, since ε is authored per-reading, but it documents why the readings cannot be merged into one constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_committer_structure, conceptual, 'Locating the structural disagreement among the three sibling readings of the shared kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__universalist_reading, 1776, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(all__tr_t1776, all_men_created_equal__universalist_reading, theater_ratio, 1776, 0.5).
narrative_ontology:measurement(all__tr_t1820, all_men_created_equal__universalist_reading, theater_ratio, 1820, 0.55).
narrative_ontology:measurement(all__tr_t1865, all_men_created_equal__universalist_reading, theater_ratio, 1865, 0.35).
narrative_ontology:measurement(all__tr_t1920, all_men_created_equal__universalist_reading, theater_ratio, 1920, 0.4).
narrative_ontology:measurement(all__tr_t1965, all_men_created_equal__universalist_reading, theater_ratio, 1965, 0.25).
narrative_ontology:measurement(all__tr_t2020, all_men_created_equal__universalist_reading, theater_ratio, 2020, 0.28).

% Extraction over time
narrative_ontology:measurement(all__be_t1776, all_men_created_equal__universalist_reading, base_extractiveness, 1776, 0.15).
narrative_ontology:measurement(all__be_t1820, all_men_created_equal__universalist_reading, base_extractiveness, 1820, 0.22).
narrative_ontology:measurement(all__be_t1865, all_men_created_equal__universalist_reading, base_extractiveness, 1865, 0.48).
narrative_ontology:measurement(all__be_t1920, all_men_created_equal__universalist_reading, base_extractiveness, 1920, 0.4).
narrative_ontology:measurement(all__be_t1965, all_men_created_equal__universalist_reading, base_extractiveness, 1965, 0.5).
narrative_ontology:measurement(all__be_t2020, all_men_created_equal__universalist_reading, base_extractiveness, 2020, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(all__su_t1776, all_men_created_equal__universalist_reading, suppression_requirement, 1776, 0.6).
narrative_ontology:measurement(all__su_t1820, all_men_created_equal__universalist_reading, suppression_requirement, 1820, 0.62).
narrative_ontology:measurement(all__su_t1865, all_men_created_equal__universalist_reading, suppression_requirement, 1865, 0.75).
narrative_ontology:measurement(all__su_t1920, all_men_created_equal__universalist_reading, suppression_requirement, 1920, 0.5).
narrative_ontology:measurement(all__su_t1965, all_men_created_equal__universalist_reading, suppression_requirement, 1965, 0.55).
narrative_ontology:measurement(all__su_t2020, all_men_created_equal__universalist_reading, suppression_requirement, 2020, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(all_men_created_equal__universalist_reading, identity_coordination).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, originalist_reading).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, textualist_paradox_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the all_men_created_equal kernel, each authored as a separate file per the epsilon-invariance principle: universalist_reading (this file, moderate extraction, tangled_rope), originalist_reading (bounded scope, founder-intent governs), and textualist_paradox_reading (irreconcilable performative contradiction). Each carries its own epsilon, beneficiary/victim structure, and classification; they are linked here rather than merged because measuring the same kernel text by the lens of 'what the text was originally understood to cover' versus 'what the text universally commits to regardless of original understanding' versus 'whether the text can be coherently applied at all' yields structurally different constraints, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(all_men_created_equal__universalist_reading, powerless, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
