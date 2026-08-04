% ============================================================================
% CONSTRAINT STORY: deferential_realism_ontology__immutable_diagnostic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deferential_realism_ontology__immutable_diagnostic_reading, []).

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
 *   constraint_id: deferential_realism_ontology__immutable_diagnostic_reading
 *   human_readable: Deferential Realism Typology as Fixed-Referent Diagnostic Instrument
 *   domain: epistemology/normative_theory/institutional_design
 *
 * SUMMARY:
 *   The Deferential Realism engine itself operates under a background theory
 *   of what its own outputs mean. The immutable_diagnostic_reading holds that
 *   mountain/rope/snare status is a fact about the world, discoverable
 *   through correct measurement of extractiveness, suppression, and the other
 *   atoms — and that when two analysts disagree about a classification, at
 *   least one of them has made a measurement error, not a value choice. This
 *   reading has real coordination value: it lets disputants argue about
 *   checkable evidence instead of raw rhetorical force. But it also has a
 *   beneficiary structure — those who administer the diagnostic procedure
 *   gain authority that is insulated from having to defend its normative
 *   premises, and those whose cases fall into the genuinely contested
 *   periphery (where the hybrid reading says judgment enters) are told their
 *   objection is itself just bad measurement, foreclosing the objection
 *   rather than answering it.
 *
 * KEY AGENTS:
 *   - framework_engineers: agenda_setter (institutional/arbitrage) — design and maintain the diagnostic procedure
 *   - credentialed_analysts: beneficiary/agenda_setter (organized/mobile) — apply the procedure, gain professional authority from its presumed objectivity
 *   - institutions_seeking_neutral_cover: beneficiary (powerful/mobile) — cite favorable classifications to insulate arrangements from political contest
 *   - contested_case_litigants: payer (moderate/constrained) — bear the cost of having their normative objection reframed as a measurement dispute
 *   - marginal_framing_advocates: excluded (powerless/trapped) — hold the rhetorical_scaffold_reading's position, excluded by definitional fiat
 *   - affected_communities_awaiting_classification: payer (powerless/trapped) — bear real costs while classification is treated as a future-discoverable fact
 *   - meta_theoretical_observers: observer (analytical/universal) — see the diagnostic reading as one structurally-positioned reading among several, not a view from nowhere
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__immutable_diagnostic_reading, 0.42).
domain_priors:suppression_score(deferential_realism_ontology__immutable_diagnostic_reading, 0.71).
domain_priors:theater_ratio(deferential_realism_ontology__immutable_diagnostic_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__immutable_diagnostic_reading, tangled_rope).
narrative_ontology:human_readable(deferential_realism_ontology__immutable_diagnostic_reading, "Deferential Realism Typology as Fixed-Referent Diagnostic Instrument").
narrative_ontology:topic_domain(deferential_realism_ontology__immutable_diagnostic_reading, "epistemology/normative_theory/institutional_design").

domain_priors:requires_active_enforcement(deferential_realism_ontology__immutable_diagnostic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__immutable_diagnostic_reading, '196f57a8-2ab7-4658-9d4b-25ab2e9b9268').
narrative_ontology:cs_kernel_codification('196f57a8-2ab7-4658-9d4b-25ab2e9b9268', formalized).
narrative_ontology:cs_authority_grounding('196f57a8-2ab7-4658-9d4b-25ab2e9b9268', expertise).
narrative_ontology:cs_interpretation_layer_present('196f57a8-2ab7-4658-9d4b-25ab2e9b9268').
narrative_ontology:cs_reading_relation('196f57a8-2ab7-4658-9d4b-25ab2e9b9268', deferential_realism_ontology__rhetorical_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('196f57a8-2ab7-4658-9d4b-25ab2e9b9268', deferential_realism_ontology__hybrid_pragmatic_reading, influences).
narrative_ontology:cs_axiom('196f57a8-2ab7-4658-9d4b-25ab2e9b9268', foundational, classification_is_discovered_not_declared).
narrative_ontology:cs_axiom_status(classification_is_discovered_not_declared, holdable).
narrative_ontology:cs_axiom_grounding('196f57a8-2ab7-4658-9d4b-25ab2e9b9268', classification_is_discovered_not_declared, empirically_contingent).
narrative_ontology:cs_axiom('196f57a8-2ab7-4658-9d4b-25ab2e9b9268', secondary, misclassification_is_correctable_measurement_error).
narrative_ontology:cs_axiom_status(misclassification_is_correctable_measurement_error, holdable).
narrative_ontology:cs_axiom_grounding('196f57a8-2ab7-4658-9d4b-25ab2e9b9268', misclassification_is_correctable_measurement_error, instrumental).
narrative_ontology:cs_reference_frame('196f57a8-2ab7-4658-9d4b-25ab2e9b9268', classification_as_measurement_procedure).
narrative_ontology:cs_drift_state('196f57a8-2ab7-4658-9d4b-25ab2e9b9268', contemporary_contested_periphery_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('196f57a8-2ab7-4658-9d4b-25ab2e9b9268', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, framework_engineers).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, credentialed_analysts).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, institutions_seeking_neutral_cover).
narrative_ontology:constraint_victim(deferential_realism_ontology__immutable_diagnostic_reading, contested_case_litigants).
narrative_ontology:constraint_victim(deferential_realism_ontology__immutable_diagnostic_reading, marginal_framing_advocates).
narrative_ontology:constraint_victim(deferential_realism_ontology__immutable_diagnostic_reading, affected_communities_awaiting_classification).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__immutable_diagnostic_reading, epsilon_is_discoverable_not_constructed).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__immutable_diagnostic_reading, misclassification_is_measurement_error).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and maintain the classification engine, set the metric definitions (extractiveness, suppression, theater_ratio) and defend the position that these are discoverable properties of constraints rather than authored framings. They control what counts as valid structural data and can revise the engine's thresholds without needing to justify the revision as a value choice, since the instrument is presented as measuring rather than declaring.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, framework_engineers, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Apply the typology to real disputes and are paid, cited, and professionally advanced for producing 'objective' classifications. Their authority rests on the claim that mountain/snare/rope status is discovered through correct application of method, not chosen. Exit is easy for them individually — they can decline any given case — but the profession as a whole benefits from the instrument's authority remaining unquestioned.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, credentialed_analysts, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(deferential_realism_ontology__immutable_diagnostic_reading, credentialed_analysts, agenda_setter).

% Cite a 'mountain' or 'rope' classification produced by the diagnostic reading to insulate a contested arrangement from political contestation — if the typology says the arrangement is a physical/coordination invariant, no one needs to argue about whether it should exist. These institutions did not build the instrument but benefit whenever its verdicts happen to favor their preferred status quo.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, institutions_seeking_neutral_cover, beneficiary,
    powerful, biographical, mobile, national).

% Bring genuinely contested cases — is this arrangement a snare or a rope? — into the diagnostic frame and are told the answer is a matter of better measurement, not judgment. They cannot contest the classification on normative grounds because the reading has already defined normative contestation as a category error; their only available move is to dispute the metrics, which requires resources and expertise they often lack.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, contested_case_litigants, payer,
    moderate, biographical, constrained, national).

% Argue that a given constraint's classification depends on who is asking and what counts as a legitimate beneficiary — the rhetorical_scaffold_reading's position. Under the diagnostic reading, this argument is treated not as a competing account but as a symptom of insufficient measurement discipline, and is excluded from the adjudicating conversation by definitional fiat rather than by being rebutted on the merits.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, marginal_framing_advocates, excluded,
    powerless, biographical, trapped, regional).

% Live under an arrangement whose classification (mountain vs. snare) determines whether it can be challenged at all. While the diagnostic reading insists the correct classification is out there awaiting discovery, these communities bear the costs of the arrangement in real time regardless of which label eventually prevails, and the promise of eventual correction through 'better observation' offers no present remedy.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, affected_communities_awaiting_classification, payer,
    powerless, biographical, trapped, local).

% Study the typology itself as a social artifact — including this very story — and can see that the diagnostic reading's claim to be pure observation is itself a structural position with beneficiaries, distinguishable from the hybrid and rhetorical readings by its suppression of the constructedness question rather than by any privileged access to ground truth.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, meta_theoretical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(deferential_realism_ontology__immutable_diagnostic_reading, credentialed_analysts).
narrative_ontology:fixing_cost_class(deferential_realism_ontology__immutable_diagnostic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared vocabulary so that disputants in institutional-design disputes can point to a common diagnostic procedure rather than arguing past each other about values — in principle this reduces pure rhetorical warfare by routing disagreement through checkable metrics.
% TRANSFER_FUNCTION: Moves interpretive authority from contested normative deliberation (open to all affected parties) to credentialed measurement (open to those with the expertise and standing to dispute metrics), and moves the burden of proof onto whoever wants to contest a classification rather than onto whoever wants to impose one.
% ABSENT_VOICES: Marginal framing advocates and affected communities are structurally excluded from the adjudicating conversation because the diagnostic reading defines their central claim — that classification is partly constructed — as a category error rather than a competing position; they would object that the instrument's neutrality is itself unearned, but that objection has no standing within the frame.
% DISAPPEARANCE_RATIONALE: Framework engineers and institutions relying on 'settled' classifications would say the world rearranges badly — every previously-resolved dispute reopens as contestable. Marginal framing advocates and affected communities would say the world barely changes for them, since the diagnostic reading's promised eventual correction through better observation was never actually delivering remedies to them in the present; removing the claim to fixed referents just makes the underlying normative contest explicit instead of laundered through metrics.
% FOUNDING_PROBLEM: Early constraint analysis risked collapsing into pure partisan labeling — anything the speaker disliked called a 'snare,' anything they liked called a 'mountain,' with no shared procedure for adjudicating disagreement. The diagnostic reading was built to supply a checkable, revisable, non-arbitrary procedure so that classification disputes could be resolved by argument about evidence rather than by rhetorical force alone.
% FOUNDING_PROBLEM_CORROBORATION: Framework engineers and credentialed analysts attest the founding problem remains live and the instrument still solves it. Meta-theoretical observers, writing from outside the analyst guild, attest that the instrument has drifted from solving the labeling-collapse problem to actively suppressing the (real, unresolved) question of whether some classifications are irreducibly normative — citing the rhetorical_scaffold_reading and hybrid_pragmatic_reading as evidence that credentialed practitioners themselves disagree about where the fixed core ends and constructed periphery begins, which undercuts the diagnostic reading's claim that this is settled by measurement alone.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__immutable_diagnostic_reading, contested).
narrative_ontology:founding_problem_status(deferential_realism_ontology__immutable_diagnostic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__immutable_diagnostic_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-03',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(deferential_realism_ontology__immutable_diagnostic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(deferential_realism_ontology__immutable_diagnostic_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deferential_realism_ontology__immutable_diagnostic_reading_tests).
:- end_tests(deferential_realism_ontology__immutable_diagnostic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) and rises over the interval: the diagnostic reading's coordination value (a shared checkable vocabulary) is real at the outset, but its extraction component grows as more contested cases get folded under the label 'just needs better measurement,' quietly converting normative disputes into technical ones administered by credentialed analysts. Suppression is markedly higher (0.71) than extraction because the reading's core move is not extracting resources directly but suppressing an entire class of objection — the claim that classification is partly constructed — by defining it out of the conversation. Theater ratio is low-moderate (0.28): most of the diagnostic apparatus does real coordination work, but a growing minority of its activity is performative reassurance that a genuinely contested case has been 'measured' rather than 'decided.' Accessibility collapse (0.58) and resistance (0.55) are both mid-range, reflecting that alternative framings (hybrid, rhetorical) remain visible and actively argued by a professional minority — the collapse is real but not complete, consistent with a tangled_rope rather than a pure snare.
 *
 * PERSPECTIVAL GAP:
 *   From the framework-engineer seat, the diagnostic reading looks like a rope: a genuine, hard-won coordination solution to the labeling-collapse problem, applied faithfully and revisable on evidence. From the contested-case-litigant and marginal-framing-advocate seats, the same structure looks like a tangled rope shading toward snare: real coordination value exists for the settled core (mountains, ropes proper) but the periphery — precisely where the classification stakes are highest — is administered by a procedure that has pre-decided the metaquestion (is classification discovered or constructed?) in favor of the administering class. The engine computing different seat-types from the same structural data is exactly the phenomenon this story is about: the diagnostic reading's own self-description (pure observation, no construction) is the thing under test.
 *
 * DIRECTIONALITY LOGIC:
 *   Framework engineers and credentialed analysts sit near the beneficiary end: they administer the instrument, and their professional standing is enhanced by the instrument's presumed objectivity being unquestioned. Institutions seeking neutral cover benefit indirectly and opportunistically — they did not build the reading but exploit favorable verdicts. Contested-case litigants and affected communities sit near the target end: they bear the cost of having a normative dispute foreclosed as measurement error, with constrained or trapped exit because the classification apparatus is the only legitimate channel through which their dispute can even be heard. Marginal framing advocates are the clearest victims of the suppression mechanism specifically — they are not paying a resource cost so much as an epistemic-standing cost, excluded from the adjudicating conversation by the reading's own foundational premise.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — arbitrary partisan labeling with no shared adjudicative procedure — was real and the diagnostic reading's answer (checkable metrics, revisable classification) is not obsolete; genuinely settled cases (Mountain-grade physical/logical limits) still benefit from a procedure that resists relabeling on political whim. But the founding problem's status is contested rather than uniformly live: for the genuinely disputed periphery, the problem the diagnostic reading claims to solve (measurement uncertainty) has been substituted for a different, unacknowledged problem (whose normative judgment counts), and the reading's insistence that this substitution hasn't happened is itself the suppression mechanism. The corpus's own architecture is not exempt from the corpus's own analytical categories — this is not paradox, it is the intended self-application.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_choice_ambiguity,
    'Is the constraint typology best read as an immutable diagnostic instrument (this story), a fixed-core/contested-periphery hybrid (hybrid_pragmatic_reading), or a normative vocabulary for policy critique (rhetorical_scaffold_reading)?',
    'No single empirical test resolves this because the three readings disagree about what would count as evidence — the diagnostic reading treats analyst disagreement as measurement error to be reduced, the hybrid reading treats it as expected at the periphery, and the rhetorical reading treats it as confirming that classification is declared. Convergent practitioner behavior over long timescales (do analysts across traditions actually converge on classifications, or does disagreement persist indefinitely on the same cases?) is the best available signal, though even that signal is read differently by each reading''s own lights.',
    'If this reading is correct, apparent classification disputes are transient and resolvable by more data — suppression of alternative framings is epistemically justified housekeeping. If the hybrid or rhetorical readings are correct, the same suppression is itself an extractive move that forecloses legitimate normative contest by mislabeling it as an observational error.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_choice_ambiguity, conceptual, 'Which kernel reading of deferential_realism_ontology is structurally correct — this story deliberately does not resolve it, per Rule 1.').

omega_variable(
    epsilon_discoverability_vs_construction,
    'Are epsilon values (base extractiveness) discoverable facts about a constraint''s operation, or are they constructed through the choice of what counts as extraction, whose costs count, and over what time horizon?',
    'Test whether independent analysts using the same declared methodology but different observable proxies converge on the same epsilon for genuinely contested cases (per the ε-invariance principle this framework itself asserts). Persistent divergence despite methodological agreement would support construction; convergence would support discoverability.',
    'If epsilon is fully discoverable, the diagnostic reading''s suppression of alternative framings is justified — there is a fact to converge on and resistance to convergence is noise. If epsilon is partly constructed by the choice of observable, the diagnostic reading''s own foundational premise (fixed referents) is itself a normative choice dressed as observation, which is precisely the rhetorical_scaffold_reading''s critique.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_discoverability_vs_construction, empirical, 'Whether the framework''s own core metric is discovered or authored — the load-bearing empirical question underneath this reading''s claim.').

omega_variable(
    who_bears_cost_of_foreclosed_contestation,
    'When a contested classification is treated as a measurement problem rather than a judgment problem, who bears the cost of the foreclosure, and is that cost distribution itself evidence about the reading''s structural position?',
    'Track outcomes for contested_case_litigants and affected_communities_awaiting_classification across cases resolved under the diagnostic reading versus cases where a competing reading''s procedure was used instead; compare rates at which contestants'' underlying grievance was substantively addressed versus procedurally deferred.',
    'A finding that foreclosure systematically favors institutions_seeking_neutral_cover would support classifying this reading itself as tangled_rope rather than pure rope, corroborating the claimed_type authored here; a finding of no systematic asymmetry would weaken that claim and push the reading closer to a genuine rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(who_bears_cost_of_foreclosed_contestation, empirical, 'Distributional test of who pays for the diagnostic reading''s suppression of the construction question.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__immutable_diagnostic_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defe_tr_t0, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(defe_tr_t4, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 4, 0.13).
narrative_ontology:measurement(defe_tr_t8, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 8, 0.17).
narrative_ontology:measurement(defe_tr_t12, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 12, 0.2).
narrative_ontology:measurement(defe_tr_t16, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 16, 0.23).
narrative_ontology:measurement(defe_tr_t20, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(defe_tr_t24, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(defe_be_t0, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(defe_be_t4, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 4, 0.28).
narrative_ontology:measurement(defe_be_t8, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 8, 0.33).
narrative_ontology:measurement(defe_be_t12, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 12, 0.36).
narrative_ontology:measurement(defe_be_t16, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 16, 0.39).
narrative_ontology:measurement(defe_be_t20, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(defe_be_t24, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(defe_su_t0, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(defe_su_t4, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 4, 0.55).
narrative_ontology:measurement(defe_su_t8, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 8, 0.61).
narrative_ontology:measurement(defe_su_t12, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 12, 0.65).
narrative_ontology:measurement(defe_su_t16, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(defe_su_t20, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(defe_su_t24, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 24, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__immutable_diagnostic_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(deferential_realism_ontology__immutable_diagnostic_reading, 0.1).
narrative_ontology:affects_constraint(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology__hybrid_pragmatic_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology__rhetorical_scaffold_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language concept 'the Deferential Realism ontology' per the ε-invariance principle: immutable_diagnostic_reading (this file, claimed tangled_rope, epsilon 0.42), hybrid_pragmatic_reading (fixed core / contested periphery), and rhetorical_scaffold_reading (persuasive vocabulary, epsilon expected higher, classification-as-declaration). Each carries its own stable epsilon and its own beneficiary/victim structure rather than averaging across the framings. The three readings are linked bidirectionally via affects_constraints; each reading's authority over contested cases changes the resource and legitimacy environment the other readings operate in, which is the influences relation declared in cs_structure below.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
