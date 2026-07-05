% ============================================================================
% CONSTRAINT STORY: all_men_created_equal__universalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: all_men_created_equal__universalist_reading
 *   human_readable: The Declaration's Equality Clause as Iteratively Expanding Universal Principle
 *   domain: Constitutional Law / Political Philosophy / American Studies
 *
 * SUMMARY:
 *   This story instantiates the UNIVERSALIST reading of the contested 'all
 *   men are created equal' kernel: the claim that the equality principle
 *   stated in the Declaration is a universal moral commitment whose scope was
 *   underdetermined and incompletely applied at founding, and which properly
 *   expands over time as excluded groups successfully claim inclusion —
 *   regardless of whether the founders intended that expansion. This is a
 *   distinct constraint from the ORIGINALIST reading (which bounds equality
 *   to 18th-century social taxonomy and treats founder intent as dispositive)
 *   and from the TEXTUALIST PARADOX reading (which treats the universal
 *   language and restricted application as an irreconcilable performative
 *   contradiction rather than a program of legitimate expansion). Each
 *   reading has its own epsilon, its own beneficiary/victim structure, and
 *   its own classification; they are linked here only through network edges,
 *   not folded into one constraint. Under the universalist reading,
 *   extractiveness is moderate and arises from the genuine coordination costs
 *   of expansion (litigation, amendment, political mobilization) combined
 *   with the real cost borne by incumbents who lose relative privilege and by
 *   not-yet-included groups who remain outside scope during any given
 *   interval.
 *
 * KEY AGENTS:
 *   - previously_excluded_groups_claiming_inclusion: primary beneficiary of successful expansion claims (organized/constrained) — gains formal inclusion but bears the cost of the gap while claim is pending
 *   - civil_rights_movements: primary agenda-setter driving iteration (organized/constrained) — mobilizes political and legal pressure without holding formal constitutional authority
 *   - groups_denied_equal_status_pending_expansion: primary payer (powerless/trapped) — bears the cost of the temporal gap between principle and application
 *   - incumbent_beneficiaries_of_narrower_readings: secondary payer (powerful/constrained) — loses relative advantage as scope widens
 *   - federal_judiciary: institutional agenda-setter (institutional/arbitrage) — converts political pressure into binding doctrine at its own pace
 *   - originalist_jurists: excluded voice — contests the reading's legitimacy from outside its operation, not from within it
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
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(all_men_created_equal__universalist_reading, tangled_rope).
narrative_ontology:human_readable(all_men_created_equal__universalist_reading, "The Declaration's Equality Clause as Iteratively Expanding Universal Principle").
narrative_ontology:topic_domain(all_men_created_equal__universalist_reading, "Constitutional Law / Political Philosophy / American Studies").

domain_priors:requires_active_enforcement(all_men_created_equal__universalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(all_men_created_equal__universalist_reading, 'ce377bc6-e981-4ab0-bc1b-021cd52e4314').
narrative_ontology:cs_kernel_codification('ce377bc6-e981-4ab0-bc1b-021cd52e4314', fixed_text).
narrative_ontology:cs_authority_grounding('ce377bc6-e981-4ab0-bc1b-021cd52e4314', practice).
narrative_ontology:cs_interpretation_layer_present('ce377bc6-e981-4ab0-bc1b-021cd52e4314').
narrative_ontology:cs_reading_relation('ce377bc6-e981-4ab0-bc1b-021cd52e4314', all_men_created_equal__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('ce377bc6-e981-4ab0-bc1b-021cd52e4314', all_men_created_equal__textualist_paradox_reading, influences).
narrative_ontology:cs_axiom('ce377bc6-e981-4ab0-bc1b-021cd52e4314', foundational, textual_universality_binds_regardless_of_original_scope).
narrative_ontology:cs_axiom_status(textual_universality_binds_regardless_of_original_scope, holdable).
narrative_ontology:cs_axiom_grounding('ce377bc6-e981-4ab0-bc1b-021cd52e4314', textual_universality_binds_regardless_of_original_scope, deontological).
narrative_ontology:cs_axiom('ce377bc6-e981-4ab0-bc1b-021cd52e4314', secondary, moral_progress_licenses_reinterpretation_over_original_intent).
narrative_ontology:cs_axiom_status(moral_progress_licenses_reinterpretation_over_original_intent, holdable).
narrative_ontology:cs_axiom_grounding('ce377bc6-e981-4ab0-bc1b-021cd52e4314', moral_progress_licenses_reinterpretation_over_original_intent, conventional).
narrative_ontology:cs_reference_frame('ce377bc6-e981-4ab0-bc1b-021cd52e4314', founding_era_declared_universal_principle).
narrative_ontology:cs_drift_state('ce377bc6-e981-4ab0-bc1b-021cd52e4314', contemporary_civil_rights_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ce377bc6-e981-4ab0-bc1b-021cd52e4314', '').
narrative_ontology:cs_kernel_id(all_men_created_equal__universalist_reading, all_men_created_equal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(all_men_created_equal__universalist_reading, previously_excluded_groups_claiming_inclusion).
narrative_ontology:constraint_beneficiary(all_men_created_equal__universalist_reading, civil_rights_movements).
narrative_ontology:constraint_beneficiary(all_men_created_equal__universalist_reading, constitutional_reform_coalitions).
narrative_ontology:constraint_victim(all_men_created_equal__universalist_reading, groups_denied_equal_status_pending_expansion).
narrative_ontology:constraint_victim(all_men_created_equal__universalist_reading, incumbent_beneficiaries_of_narrower_readings).
narrative_ontology:constraint_vindicates(all_men_created_equal__universalist_reading, moral_progress_thesis).
narrative_ontology:constraint_vindicates(all_men_created_equal__universalist_reading, constitution_as_living_document_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enslaved people, women, non-property-holders, and later racial and sexual minorities invoke the universal language of the equality clause to demand legal and political inclusion the founding generation did not extend to them. They cannot exit the constitutional order itself — their strategy is to win a wider reading of it from within, through litigation, amendment, and social movement.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, previously_excluded_groups_claiming_inclusion, beneficiary,
    organized, generational, constrained, national).

% Abolitionists, suffragists, and later civil rights organizations actively drive the expansion — mounting legal challenges, organizing political pressure, and reframing public understanding of what 'equal' must mean. They administer the iterative expansion process even though they hold no formal constitutional authority; their leverage is moral and political mobilization rather than office.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, civil_rights_movements, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(all_men_created_equal__universalist_reading, civil_rights_movements, beneficiary).

% At any given moment in the iterative process, some group remains outside the currently recognized scope of equality — held there by the still-unexpanded reading. They bear the cost of the gap between the principle's stated universality and its present application, with no exit from the jurisdiction and no guarantee expansion will reach them in their lifetime.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, groups_denied_equal_status_pending_expansion, payer,
    powerless, biographical, trapped, national).

% Those who held privileged status under the founders' original, narrower application (property-holding white men, in the paradigm case) lose relative advantage as the circle of inclusion widens. They cannot exit the polity to escape the expansion; their recourse is political resistance, litigation to slow the reading, or accommodation.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, incumbent_beneficiaries_of_narrower_readings, payer,
    powerful, biographical, constrained, national).

% Courts, especially the Supreme Court, adjudicate how far the equality principle extends at any given moment, converting social and political pressure into binding doctrine (or refusing to). They control the pace and shape of iteration and can accelerate, ratify, or stall expansion, insulated from direct electoral consequence.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, federal_judiciary, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Amendment-era coalitions (Reconstruction Congress, Nineteenth Amendment campaigners) formalize expansions into text, converting contested readings into settled constitutional law. They gain durable legitimacy for their preferred scope once codified, at the cost of prolonged and uncertain political struggle to get there.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, constitutional_reform_coalitions, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(all_men_created_equal__universalist_reading, constitutional_reform_coalitions, agenda_setter).

% Jurists and scholars committed to founder-intent-bounded readings object that the universalist reading substitutes present moral judgment for original textual meaning, but their objection operates as a rival reading rather than a voice inside this constraint's own operation — they are structurally external to the iterative-expansion project even as they contest its legitimacy from outside.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, originalist_jurists, excluded,
    institutional, civilizational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, evolving standard by which previously excluded groups can claim legal and political inclusion without requiring the polity to draft an entirely new founding document each time the boundary of 'equal' needs to move — the existing text is reinterpreted rather than replaced.
% TRANSFER_FUNCTION: Moves formal legal recognition, political standing, and downstream material protections from incumbents who benefited from a narrower application toward groups newly brought inside the principle's scope, mediated through litigation, legislation, and constitutional amendment.
% ABSENT_VOICES: Groups not yet organized enough to mount a claim, or whose exclusion has not yet been named as an equality violation, have no seat in the current iteration — the expansion process only responds to claims that have been articulated and mobilized, so the 'next' excluded group is by definition not yet part of the conversation.
% DISAPPEARANCE_RATIONALE: If the universalist reading were abandoned in favor of a founder-intent-locked scope, the entire apparatus of civil rights litigation, constitutional amendment as inclusion-mechanism, and doctrinal expansion (Reconstruction Amendments, Nineteenth Amendment, Brown, Loving, Obergefell-style reasoning) loses its interpretive foundation — decades of jurisprudence built on the premise that 'equal' means more today than it meant in 1776 would need to be re-justified or reversed.
% FOUNDING_PROBLEM: The Declaration asserted 'all men are created equal' while the founding polity simultaneously enslaved people, excluded women from political participation, and restricted suffrage to property-holding men — creating an immediate and visible gap between stated principle and applied practice that required either abandoning the principle or reading it as a standard the polity had not yet met.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and constitutional scholars outside the civil rights movements themselves (including originalist critics who reject the universalist reading but agree the gap existed) corroborate that the founding generation's practice diverged from its own stated principle; abolitionist writings (Douglass's 'What to the Slave is the Fourth of July') and suffragist petitions from the 19th century independently attest the gap was recognized as live, not manufactured retroactively.
narrative_ontology:disappearance_verdict(all_men_created_equal__universalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(all_men_created_equal__universalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(all_men_created_equal__universalist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.42) reflecting genuine coordination cost: the iterative expansion machinery (litigation, amendment campaigns, doctrinal reinterpretation) requires real resources and imposes real costs on incumbents, but it is not primarily rent-extraction — it redistributes standing toward previously excluded groups rather than funneling value to a concentrated capturer. Suppression is moderate-to-declining (0.55 at founding, 0.38 by 2026) because early resistance to expansion (slavery's defenders, anti-suffrage forces) required significant coercive maintenance, while later-stage suppression is lower as expanded readings become normalized. Theater ratio rises modestly (0.10 to 0.28) reflecting an increasing gap between symbolic invocations of 'equality' in political rhetoric and the substantive pace of actual doctrinal expansion — some invocation of the principle becomes performative affirmation rather than a live claim for further inclusion. Accessibility collapse is moderate (0.35): the universalist reading does not foreclose alternative readings (originalist and textualist-paradox readings remain live in courts and scholarship), so alternatives have not collapsed. Resistance is substantial (0.62), reflecting the genuine and sustained political and doctrinal contestation the universalist reading has always faced from originalist counter-argument.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of a currently-included beneficiary group, the constraint reads as vindicated moral progress — proof the principle works as advertised. From the seat of a group still outside recognized scope, the same constraint reads as a promise indefinitely deferred, extracting patience and political labor without guaranteed payoff. From the incumbent seat, it reads as ongoing erosion of settled privilege justified by an ideal that keeps moving. The engine computes these divergent seat-level classifications from the structural power/exit data; the universalist claim itself does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Previously excluded groups and civil rights movements sit near the beneficiary end: the constraint's operation, when successful, expands their formal standing. Groups currently denied equal status pending future expansion sit near the full-target end: trapped, powerless, bearing the cost of an unclosed gap with no guaranteed timeline for resolution. Incumbent beneficiaries of narrower readings sit toward the target end on a different axis: they do not gain from expansion and their relative position erodes, though their absolute power remains high enough that their exit options are only constrained, not trapped. The federal judiciary is agenda-setting from an institutional position with the widest degrees of freedom (arbitrage-level exit from direct political consequence), which is why judicial doctrine can move faster or slower than either popular mobilization or original intent would predict.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a self-proclaimed universal principle applied to a visibly non-universal founding population) remains live by the story's own corroboration — it has not been resolved by any single expansion, because the logic of the universalist reading is that scope-questions recur (each successful inclusion claim reveals the next excluded group). This prevents mislabeling the constraint as pure extraction (a snare) because there is a genuine, repeatedly-vindicated coordination function — successive expansions (Reconstruction Amendments, women's suffrage, civil rights doctrine) demonstrably changed formal legal status for millions. It equally prevents mislabeling it as pure coordination (a rope) because the process visibly produces victims at every point in time: whoever is currently outside the recognized scope pays the cost of the gap, and incumbents who lose privilege actively resist. The tangled_rope classification holds both facts simultaneously without collapsing them.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founder_intent_relevance_to_scope,
    'Does the founders'' actual, documented intent regarding the equality clause''s scope bear on what the principle correctly requires today, or is intent irrelevant once the principle is stated in universal language?',
    'This is not empirically resolvable — it depends on a jurisprudential commitment (living constitutionalism vs. originalism) that the universalist reading assumes rather than proves. Sibling reading originalist_reading answers this question oppositely.',
    'If founder intent is dispositive, the universalist reading''s entire expansion program lacks legitimate grounding and the constraint collapses into judicial or political will unmoored from text. If intent is irrelevant, the universalist reading is the correct application of the text and its expansions are legitimate constitutional development.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founder_intent_relevance_to_scope, conceptual, 'Whether founder intent constrains or is superseded by the universal language of the equality clause.').

omega_variable(
    expansion_endpoint_or_infinite_regress,
    'Does the iterative expansion process have a natural terminus (a fully realized universal equality) or is it structurally open-ended, always generating a next excluded group?',
    'Historical pattern analysis: does each successful expansion (racial, gender, marital-status, sexual-orientation) reduce the rate of new claims, or does claim-generation persist at a stable rate? A declining rate would support a terminus; a stable or increasing rate would support structural open-endedness.',
    'If there is a terminus, the constraint is scaffold-like (transitional, working toward its own obsolescence). If structurally open-ended, the constraint is a permanent feature of the constitutional order and the tangled_rope classification is stable rather than transitional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expansion_endpoint_or_infinite_regress, empirical, 'Whether iterative equality expansion converges or is open-ended by structure.').

omega_variable(
    who_adjudicates_the_next_claim,
    'What determines which groups'' exclusion is recognized as an ''equality violation'' warranting expansion, versus which exclusions remain unrecognized or contested indefinitely?',
    'Comparative study of successful vs. stalled equality claims (e.g., disability rights vs. felon disenfranchisement) to identify what structural factors (organization, judicial receptivity, political salience) predict recognition.',
    'If recognition tracks organized political power rather than moral consistency, the universalist reading''s claim to principled, non-arbitrary expansion is weakened — expansion would track power, not universality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(who_adjudicates_the_next_claim, empirical, 'What structural factors govern which exclusion claims succeed under the universalist reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__universalist_reading, 1776, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(all__tr_t1776, all_men_created_equal__universalist_reading, theater_ratio, 1776, 0.1).
narrative_ontology:measurement(all__tr_t1865, all_men_created_equal__universalist_reading, theater_ratio, 1865, 0.15).
narrative_ontology:measurement(all__tr_t1920, all_men_created_equal__universalist_reading, theater_ratio, 1920, 0.18).
narrative_ontology:measurement(all__tr_t1965, all_men_created_equal__universalist_reading, theater_ratio, 1965, 0.22).
narrative_ontology:measurement(all__tr_t2000, all_men_created_equal__universalist_reading, theater_ratio, 2000, 0.26).
narrative_ontology:measurement(all__tr_t2026, all_men_created_equal__universalist_reading, theater_ratio, 2026, 0.28).

% Extraction over time
narrative_ontology:measurement(all__be_t1776, all_men_created_equal__universalist_reading, base_extractiveness, 1776, 0.15).
narrative_ontology:measurement(all__be_t1865, all_men_created_equal__universalist_reading, base_extractiveness, 1865, 0.28).
narrative_ontology:measurement(all__be_t1920, all_men_created_equal__universalist_reading, base_extractiveness, 1920, 0.33).
narrative_ontology:measurement(all__be_t1965, all_men_created_equal__universalist_reading, base_extractiveness, 1965, 0.4).
narrative_ontology:measurement(all__be_t2000, all_men_created_equal__universalist_reading, base_extractiveness, 2000, 0.44).
narrative_ontology:measurement(all__be_t2026, all_men_created_equal__universalist_reading, base_extractiveness, 2026, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(all__su_t1776, all_men_created_equal__universalist_reading, suppression_requirement, 1776, 0.55).
narrative_ontology:measurement(all__su_t1865, all_men_created_equal__universalist_reading, suppression_requirement, 1865, 0.6).
narrative_ontology:measurement(all__su_t1920, all_men_created_equal__universalist_reading, suppression_requirement, 1920, 0.5).
narrative_ontology:measurement(all__su_t1965, all_men_created_equal__universalist_reading, suppression_requirement, 1965, 0.48).
narrative_ontology:measurement(all__su_t2000, all_men_created_equal__universalist_reading, suppression_requirement, 2000, 0.4).
narrative_ontology:measurement(all__su_t2026, all_men_created_equal__universalist_reading, suppression_requirement, 2026, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(all_men_created_equal__universalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(all_men_created_equal__universalist_reading, 0.12).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, originalist_reading).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, textualist_paradox_reading).

% DUAL FORMULATION NOTE:
% This file is one of three siblings decomposing the natural-language 'all men are created equal' claim per the epsilon-invariance principle: universalist_reading (this file, tangled_rope, moderate extractiveness from expansion coordination costs), originalist_reading (bounded scope, different beneficiary/victim structure), and textualist_paradox_reading (treats the universal-language/restricted-application gap as irreconcilable contradiction rather than a legitimate expansion program). The three do not share one epsilon — each reading produces a structurally distinct constraint with its own metrics, and the network edges here record kernel-sibling relationships, not shared classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
