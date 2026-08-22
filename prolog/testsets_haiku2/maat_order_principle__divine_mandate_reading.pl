% ============================================================================
% CONSTRAINT STORY: maat_order_principle__divine_mandate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maat_order_principle__divine_mandate_reading, []).

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
 *   constraint_id: maat_order_principle__divine_mandate_reading
 *   human_readable: Ma'at Divine Mandate — Pharaoh as Cosmic Order Source
 *   domain: religious/political/philosophical
 *
 * SUMMARY:
 *   In this reading of the Ma'at kernel, the cosmic principle of divine order
 *   flows from a transcendent realm through the Pharaoh to human society. The
 *   Pharaoh is not subject to Ma'at; rather, the Pharaoh IS Ma'at—the living
 *   embodiment of cosmic order. By definition, the Pharaoh cannot violate
 *   Ma'at because any act the Pharaoh takes expresses Ma'atic order. This
 *   reading justifies centralized authority and extraction as cosmic
 *   necessity. It suppresses alternative readings (reciprocity, distributed
 *   maintenance) that would constrain royal action or impose mutual
 *   obligations. The Pharaonic court and priestly apparatus benefit from the
 *   reading's insulation of royal authority from constraint. The key
 *   structural feature: what is claimed as a mountain (natural cosmic order)
 *   actually functions as a constructed exemption from constraint, with
 *   identifiable beneficiaries. This makes it a false-summit candidate—a
 *   constraint with declared beneficiaries requiring an omega documenting the
 *   natural-law vs. constructed ambiguity.
 *
 * KEY AGENTS:
 *   - Pharaonic authority: claims cosmic status as source of Ma'at; exempt from constraint by definition
 *   - Priestly apparatus: interprets and enforces the reading; derives institutional power from Pharaonic exemption
 *   - Administrative officials: execute Pharaonic commands while bearing responsibility for outcomes; caught between the reading's logic
 *   - Commoner population: powerless; trapped; cannot appeal to Ma'at to constrain royal action
 *   - Alternative theological traditions: excluded; suppressed as cosmically dangerous
 *   - Conquered territories: subject to extraction justified as bringing Ma'atic order
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maat_order_principle__divine_mandate_reading, 0.81).
domain_priors:suppression_score(maat_order_principle__divine_mandate_reading, 0.87).
domain_priors:theater_ratio(maat_order_principle__divine_mandate_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, suppression_requirement, 0.87).
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, resistance, 0.41).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_order_principle__divine_mandate_reading, mountain).
narrative_ontology:human_readable(maat_order_principle__divine_mandate_reading, "Ma'at Divine Mandate — Pharaoh as Cosmic Order Source").
narrative_ontology:topic_domain(maat_order_principle__divine_mandate_reading, "religious/political/philosophical").

domain_priors:emerges_naturally(maat_order_principle__divine_mandate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__divine_mandate_reading, '6ffd06ec-f144-4314-ae48-c0929639c8c5').
narrative_ontology:cs_kernel_codification('6ffd06ec-f144-4314-ae48-c0929639c8c5', fixed_text).
narrative_ontology:cs_authority_grounding('6ffd06ec-f144-4314-ae48-c0929639c8c5', extraction).
narrative_ontology:cs_interpretation_layer_present('6ffd06ec-f144-4314-ae48-c0929639c8c5').
narrative_ontology:cs_reading_relation('6ffd06ec-f144-4314-ae48-c0929639c8c5', maat_order_principle__reciprocity_reading, forecloses).
narrative_ontology:cs_reading_relation('6ffd06ec-f144-4314-ae48-c0929639c8c5', maat_order_principle__distributed_maintenance_reading, forecloses).
narrative_ontology:cs_axiom('6ffd06ec-f144-4314-ae48-c0929639c8c5', foundational, pharaoh_embodies_maat_cosmic_identity).
narrative_ontology:cs_axiom_status(pharaoh_embodies_maat_cosmic_identity, holdable).
narrative_ontology:cs_axiom_grounding('6ffd06ec-f144-4314-ae48-c0929639c8c5', pharaoh_embodies_maat_cosmic_identity, theological).
narrative_ontology:cs_axiom('6ffd06ec-f144-4314-ae48-c0929639c8c5', foundational, pharaonic_action_cosmically_necessary_and_inviolable).
narrative_ontology:cs_axiom_status(pharaonic_action_cosmically_necessary_and_inviolable, holdable).
narrative_ontology:cs_axiom_grounding('6ffd06ec-f144-4314-ae48-c0929639c8c5', pharaonic_action_cosmically_necessary_and_inviolable, deontological).
narrative_ontology:cs_reference_frame('6ffd06ec-f144-4314-ae48-c0929639c8c5', pharaonic_cosmic_mandate_framework).
narrative_ontology:cs_drift_state('6ffd06ec-f144-4314-ae48-c0929639c8c5', late_dynastic_imperial_consolidation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6ffd06ec-f144-4314-ae48-c0929639c8c5', '').
narrative_ontology:cs_kernel_id(maat_order_principle__divine_mandate_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_order_principle__divine_mandate_reading, pharaonic_authority).
narrative_ontology:constraint_beneficiary(maat_order_principle__divine_mandate_reading, priestly_apparatus).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(maat_order_principle__divine_mandate_reading, administrative_officials).
narrative_ontology:constraint_victim(maat_order_principle__divine_mandate_reading, commoner_population).
narrative_ontology:constraint_victim(maat_order_principle__divine_mandate_reading, conquered_territories).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Embodies Ma'at as the living conduit of cosmic order. Acts as the bridge between divine realm and human society. Sets policy, dispenses justice, and controls resource distribution. Under this reading, cannot violate Ma'at by definition—any royal action IS Ma'atic order expressing through the Pharaoh's person. Bears the burden of maintaining cosmic balance but is exempt from constraint under the same framework that claims cosmic necessity justifies the exemption.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, pharaonic_authority, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Interprets Ma'at doctrine, performs rituals sustaining cosmic order, adjudicates disputes through religious authority. Derives institutional legitimacy and resource allocation from the reading that Pharaoh cannot violate Ma'at—the Pharaoh's cosmic status justifies priestly monopoly on adjudication and interpretation. Enforces the reading against competing interpretations through temple authority.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, priestly_apparatus, beneficiary,
    organized, generational, constrained, national).

% Execute Pharaonic commands framed as divine order. Disciplined if outcomes fail, credited if Ma'atic outcomes are achieved. Bound by the reading's logic: they cannot argue the Pharaoh's orders violate Ma'at because the Pharaoh IS Ma'at. Must implement directives while carrying personal responsibility if policy proves disastrous—the divine mandate protects the source but not the conduit.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, administrative_officials, payer,
    powerful, biographical, constrained, national).

% Subjected to Pharaonic judgment and resource extraction justified as cosmic necessity. Cannot appeal to Ma'at principles to constrain royal action because the reading defines royal action as Ma'atic by definition. Bound to obey. Resistance is framed as cosmic violation. Subject to conscription, taxation, and corvée labor justified as sustaining Ma'atic order.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, commoner_population, payer,
    powerless, biographical, trapped, national).

% Offer competing readings of Ma'at (reciprocity, distributed maintenance) that would constrain Pharaonic action or impose mutual obligations. Are suppressed through religious orthodoxy enforcement, temple control of interpretive authority, and framing as cosmically dangerous heresy. Their voices are structurally kept out of legitimacy discourse.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, alternative_theological_traditions, excluded,
    moderate, generational, trapped, national).

% Subject to Pharaonic expansion and tribute extraction justified as bringing Ma'atic order to chaos-lands. Cannot negotiate or appeal to Ma'at principles; the reading defines the Pharaoh's military and extractive acts as expressions of cosmic order itself.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, conquered_territories, payer,
    powerless, biographical, trapped, regional).

% Examines the reading's structural logic: how the claim that Pharaoh embodies Ma'at and cannot violate it functions as a theological exemption from constraint rather than a constraint itself.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, observer_seat, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(maat_order_principle__divine_mandate_reading, pharaonic_authority).
narrative_ontology:fixing_cost_class(maat_order_principle__divine_mandate_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stabilizes social order by anchoring all law, justice, and resource distribution to a single cosmic principle (Ma'at) expressed through the Pharaoh's person. Eliminates the possibility of competing legitimacy claims—all authority flows from one source, reducing intra-elite contestation.
% TRANSFER_FUNCTION: Moves labor, tribute, and obedience from commoner and administrative populations to Pharaonic authority and priestly interpreters, justified as the price of maintaining cosmic order. The Pharaoh collects surplus production; priests collect interpretive authority and resource control; administrators execute extraction while bearing responsibility; commoners bear all costs.
% ABSENT_VOICES: Theological traditions that read Ma'at as imposing mutual obligations on Pharaoh, or as distributed maintenance responsibility across all social levels, are structurally excluded. They would argue for constraining royal action or sharing Ma'atic responsibility—but their voices are kept out by the same orthodoxy that defines them as cosmically dangerous.
% DISAPPEARANCE_RATIONALE: If this reading collapsed and was replaced by the reciprocity or distributed-maintenance readings, Pharaonic authority would become constrained by Ma'at principles rather than identical to them. Resource extraction would require justification beyond cosmic necessity. Priestly interpretation authority would face competition from alternative voices. The power structure would reorganize around contestable rather than absolute legitimacy.
% FOUNDING_PROBLEM: The transition from tribal kinship authority to territorial empire required a legitimacy principle that could justify centralized power over large, diverse populations without requiring consent or ongoing negotiation. Ma'at provided that principle—a transcendent cosmic order that made resistance illegitimate by definition.
% FOUNDING_PROBLEM_CORROBORATION: Pharaonic court inscriptions and priestly temple texts attest the founding problem is ongoing—the empire requires cosmic order maintained through Pharaonic action. Modern scholars of Egyptian political theology (outside the benefiting parties) document that this reading served to suppress alternative Ma'at interpretations that would have constrained royal power. The reading persists because the institutional structure it justifies remains in place.
narrative_ontology:disappearance_verdict(maat_order_principle__divine_mandate_reading, world_rearranges).
narrative_ontology:founding_problem_status(maat_order_principle__divine_mandate_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(maat_order_principle__divine_mandate_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(maat_order_principle__divine_mandate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(maat_order_principle__divine_mandate_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maat_order_principle__divine_mandate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(maat_order_principle__divine_mandate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, ExtMetricName, E),
    domain_priors:suppression_score(maat_order_principle__divine_mandate_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(maat_order_principle__divine_mandate_reading),
    narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(maat_order_principle__divine_mandate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.81) and rising slightly over the interval because the reading systematically exempts the Pharaoh from constraint while justifying extraction of labor, tribute, and obedience. Suppression is highest (0.87) because the reading actively suppresses alternative Ma'at interpretations through temple orthodoxy and characterizes competing readings as cosmically dangerous. Theater ratio (0.62) is elevated because much of the enforcement activity defends the reading's theological monopoly rather than sustaining its purported coordination function. Accessibility collapse (0.78) is high: once the reading is accepted, alternatives appear logically impossible—Ma'at cannot simultaneously permit and constrain the Pharaoh. Resistance (0.41) is moderate because the reading's circularity makes it difficult to challenge without rejecting the theological framework itself. The measurement series track extraction accumulation and theater ratio rising over time as the reading becomes institutionalized—Pharaonic power expands, alternative voices are suppressed more thoroughly, and more of the enforcement machinery focuses on orthodoxy enforcement rather than justice administration.
 *
 * PERSPECTIVAL GAP:
 *   From the Pharaonic seat: Ma'at is a natural cosmic order requiring the Pharaoh's maintenance; extraction and commandment are expressions of Ma'atic duty; the reading is non-negotiable. From the commoner seat: the same structure appears as unlimited extraction without recourse; the reading makes resistance cosmically illegitimate; they cannot argue for constraint. From the priestly seat: the reading is a truth requiring theological enforcement; suppression of alternatives is cosmic protection. From the observer seat: the reading functions as a constructed exemption justified by appeal to natural law; high suppression and rising extraction indicate institutional capture dressed as cosmic necessity. The engine computes per-seat types from the structural data; the divergence is the measurement itself.
 *
 * DIRECTIONALITY LOGIC:
 *   The Pharaonic authority and priestly apparatus sit at the beneficiary end of directionality (d near 0.0): they collect authority and resource flows from the reading; they have high institutional power and arbitrage options (can shift theological emphasis if challenged); the reading is designed to exempt them from constraint. Commoner population sits at the target end (d near 1.0): powerless, trapped, unable to appeal to Ma'at, subject to extraction justified as cosmic necessity. Administrative officials sit asymmetrically: powerful enough to understand the reading's paradox, but constrained (identity-locked to their administrative role, which depends on the reading's legitimacy). Alternative theological traditions are excluded rather than positioned on the directionality spectrum—they are kept out of the constraint system entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legitimizing centralized authority without ongoing consent) was live during the reading's establishment but becomes increasingly dead over the interval. By the end of the interval, the reading persists not because the Pharaoh needs Ma'atic legitimacy to govern (institutional power is established), but because the Pharaonic apparatus has invested so much in the reading's enforcement that abandoning it would undermine the entire authority structure. The rising theater ratio (0.45 to 0.62) tracks this shift: early in the interval, the reading performs a genuine coordination and legitimacy function; late in the interval, a growing share of effort goes to defending the reading against challenges and suppressing alternatives. The constraint has become partly mandatropic—it persists not because it solves its founding problem, but because institutional interests depend on its continuation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_legitimacy,
    'Is this constraint a description of actual cosmic order (a mountain), or is it a constructed theological reading that benefits identifiable institutional parties?',
    'Examine whether the reading serves to exempt Pharaonic action from constraint while claiming the Pharaoh cannot violate Ma''at. If the logic is circular (Pharaoh IS Ma''at, therefore cannot violate it), the constraint is a constructed reading, not a natural law. Compare to sibling readings to see if they offer competing interpretations of the same kernel.',
    'If natural law: the high extraction and suppression scores reflect cosmic necessity, not institutional capture. If constructed: extraction is justified by appeal to natural law as a cover story; high suppression indicates the reading''s own fragility. FSM candidate — beneficiaries declared on a mountain reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_legitimacy, conceptual, 'Whether the divine mandate is a discovered natural order or a constructed theological exemption.').

omega_variable(
    kernel_interpretation_contest,
    'What is the relationship between this reading and the sibling readings (reciprocity and distributed-maintenance) of the same Ma''at kernel? Do they coexist as live theological positions, or does this reading foreclose the others?',
    'Historical and textual analysis: examine whether all three readings appear in ancient Egyptian sources (coexistence), or whether this reading systematically suppresses the others (foreclosure). Assess whether a unified theological framework could hold all three simultaneously or whether they are genuinely incompatible.',
    'If coexists_with: all three readings are live interpretive traditions in tension. If forecloses: this reading''s establishment suppressed competing interpretations of Ma''at, and the suppression is structural to the reading itself. Affects how to classify the sibling constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_interpretation_contest, empirical, 'The structural relationship between this reading and alternative Ma''at interpretations.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the high suppression score (0.87) structural coercion (legal prohibition, temple control, enforcement apparatus) or internalized (commoners believe the reading is true and violating it would be cosmically dangerous)?',
    'Examine evidence of resistance: if alternative readings persist covertly or appear in private contexts despite prohibition, suppression is structural. If the reading is unchallenged even in unmonitored contexts, suppression may be internalized. Post-collapse observations: if suppression persists after the constraint''s institutional apparatus collapses, it is internalized.',
    'If structural: the constraint''s persistence depends on active enforcement; removing the apparatus weakens it. If internalized: the reading persists even after institutional support ends; it has fused with commoner identity and belief. Affects estimates of resistance durability and the cost of constraint removal.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of alternative readings operates through external enforcement or internalized belief.').

omega_variable(
    pharaonic_action_constraint_paradox,
    'If the Pharaoh embodies Ma''at and cannot violate it by definition, in what sense does Ma''at function as a constraint on royal action rather than as a justification for any royal action?',
    'Examine how resistance to Pharaonic policy was framed in ancient texts. If challengers appeal to Ma''at principles to demand the Pharaoh conform to them, the constraint does operate (the Pharaoh''s actions can be judged against Ma''at). If challengers have no language available except rebellion, the constraint functions only as a justification apparatus, not a constraint.',
    'If constraint functions: the reading is closer to rope or tangled_rope (coordination with enforcement). If justification only: the reading is pure extraction with theological cover (snare). The claimed type is mountain; the metrics and this omega probe whether that claim reflects actual structure or false summit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pharaonic_action_constraint_paradox, conceptual, 'The paradox of a constraint that exempts its primary subject by definition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__divine_mandate_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_tr_t0, maat_order_principle__divine_mandate_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(maat_tr_t5, maat_order_principle__divine_mandate_reading, theater_ratio, 5, 0.48).
narrative_ontology:measurement(maat_tr_t10, maat_order_principle__divine_mandate_reading, theater_ratio, 10, 0.51).
narrative_ontology:measurement(maat_tr_t15, maat_order_principle__divine_mandate_reading, theater_ratio, 15, 0.54).
narrative_ontology:measurement(maat_tr_t20, maat_order_principle__divine_mandate_reading, theater_ratio, 20, 0.57).
narrative_ontology:measurement(maat_tr_t25, maat_order_principle__divine_mandate_reading, theater_ratio, 25, 0.59).
narrative_ontology:measurement(maat_tr_t30, maat_order_principle__divine_mandate_reading, theater_ratio, 30, 0.6).
narrative_ontology:measurement(maat_tr_t35, maat_order_principle__divine_mandate_reading, theater_ratio, 35, 0.61).
narrative_ontology:measurement(maat_tr_t40, maat_order_principle__divine_mandate_reading, theater_ratio, 40, 0.62).

% Extraction over time
narrative_ontology:measurement(maat_be_t0, maat_order_principle__divine_mandate_reading, base_extractiveness, 0, 0.64).
narrative_ontology:measurement(maat_be_t5, maat_order_principle__divine_mandate_reading, base_extractiveness, 5, 0.68).
narrative_ontology:measurement(maat_be_t10, maat_order_principle__divine_mandate_reading, base_extractiveness, 10, 0.72).
narrative_ontology:measurement(maat_be_t15, maat_order_principle__divine_mandate_reading, base_extractiveness, 15, 0.75).
narrative_ontology:measurement(maat_be_t20, maat_order_principle__divine_mandate_reading, base_extractiveness, 20, 0.78).
narrative_ontology:measurement(maat_be_t25, maat_order_principle__divine_mandate_reading, base_extractiveness, 25, 0.79).
narrative_ontology:measurement(maat_be_t30, maat_order_principle__divine_mandate_reading, base_extractiveness, 30, 0.8).
narrative_ontology:measurement(maat_be_t35, maat_order_principle__divine_mandate_reading, base_extractiveness, 35, 0.81).
narrative_ontology:measurement(maat_be_t40, maat_order_principle__divine_mandate_reading, base_extractiveness, 40, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(maat_su_t0, maat_order_principle__divine_mandate_reading, suppression_requirement, 0, 0.71).
narrative_ontology:measurement(maat_su_t5, maat_order_principle__divine_mandate_reading, suppression_requirement, 5, 0.74).
narrative_ontology:measurement(maat_su_t10, maat_order_principle__divine_mandate_reading, suppression_requirement, 10, 0.77).
narrative_ontology:measurement(maat_su_t15, maat_order_principle__divine_mandate_reading, suppression_requirement, 15, 0.79).
narrative_ontology:measurement(maat_su_t20, maat_order_principle__divine_mandate_reading, suppression_requirement, 20, 0.81).
narrative_ontology:measurement(maat_su_t25, maat_order_principle__divine_mandate_reading, suppression_requirement, 25, 0.83).
narrative_ontology:measurement(maat_su_t30, maat_order_principle__divine_mandate_reading, suppression_requirement, 30, 0.84).
narrative_ontology:measurement(maat_su_t35, maat_order_principle__divine_mandate_reading, suppression_requirement, 35, 0.86).
narrative_ontology:measurement(maat_su_t40, maat_order_principle__divine_mandate_reading, suppression_requirement, 40, 0.87).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maat_order_principle__divine_mandate_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(maat_order_principle__divine_mandate_reading, 0.25).
narrative_ontology:affects_constraint(maat_order_principle__divine_mandate_reading, maat_order_principle__reciprocity_reading).
narrative_ontology:affects_constraint(maat_order_principle__divine_mandate_reading, maat_order_principle__distributed_maintenance_reading).

% DUAL FORMULATION NOTE:
% The Ma'at kernel admits three structurally distinct constraint instantiations corresponding to three competing theological readings. This constraint (divine_mandate_reading) differs from the reciprocity_reading and distributed_maintenance_reading in its core claim about Pharaonic exemption and its suppression of alternative readings. All three are linked as sibling constraints within the same kernel decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(maat_order_principle__divine_mandate_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
