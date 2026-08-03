% ============================================================================
% CONSTRAINT STORY: empirical_precedent_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_empirical_precedent_reading, []).

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
 *   constraint_id: empirical_precedent_reading
 *   human_readable: Fiat Efficacy Grounded in Historical Precedent (Debate Round Warrant)
 *   domain: debate theory/political philosophy
 *
 * SUMMARY:
 *   In competitive academic debate, the practice of 'fiat' allows debaters to
 *   argue as though a hypothetical policy has been enacted, bypassing
 *   questions of political feasibility. A recurring warrant defends fiat's
 *   argumentative value by appeal to historical precedent: BDS (Boycott,
 *   Divestment, Sanctions) activism generated sustained international
 *   attention to Palestinian rights; student and grassroots antiwar
 *   organizing is credited with contributing to U.S. withdrawal from Vietnam;
 *   and the Chicago School of economics, beginning as an academic and
 *   comparatively small ideological project, went on to found the
 *   intellectual infrastructure of global neoliberalism. Debaters cite these
 *   cases to argue that small-scale, resource-constrained collective action
 *   routinely produces large-scale material and political outcomes, and
 *   therefore that in-round fiat advocacy is not merely academic but
 *   analogous to efficacious real-world action. This constraint examines that
 *   specific warrant as a structural arrangement within the debate community
 *   — who benefits from its persistence, who pays its costs, and whether the
 *   historical analogy it depends on actually holds.
 *
 * KEY AGENTS:
 *   - competitive_debaters_running_fiat: primary agenda-setter and beneficiary (moderate/mobile) — deploys the warrant for competitive advantage
 *   - opposing_debaters_forced_to_engage_analogy: primary payer (moderate/constrained) — must contest or concede the framing under round-time pressure
 *   - coaching_programs_teaching_activism_framing: institutional beneficiary (organized/mobile) — trains and profits from the warrant's persistence
 *   - historical_movement_participants_instrumentalized_as_evidence: excluded victim (powerless/trapped) — the actual organizers whose history is cited without consultation or benefit
 *   - communities_named_in_analogized_movements_bds_vietnam_neoliberalism: excluded victim (powerless/trapped, global/civilizational scope) — the material referents whose suffering or political stakes become evidentiary shorthand
 *   - judges_and_tournament_administrators: analytical observer (institutional/analytical) — adjudicates which reading of fiat efficacy the community rewards
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(empirical_precedent_reading, 0.42).
domain_priors:suppression_score(empirical_precedent_reading, 0.31).
domain_priors:theater_ratio(empirical_precedent_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(empirical_precedent_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(empirical_precedent_reading, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(empirical_precedent_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(empirical_precedent_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(empirical_precedent_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(empirical_precedent_reading, tangled_rope).
narrative_ontology:human_readable(empirical_precedent_reading, "Fiat Efficacy Grounded in Historical Precedent (Debate Round Warrant)").
narrative_ontology:topic_domain(empirical_precedent_reading, "debate theory/political philosophy").

domain_priors:requires_active_enforcement(empirical_precedent_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(empirical_precedent_reading, '3cf9a510-9141-4143-b31b-6cbcbf1a8744').
narrative_ontology:cs_kernel_codification('3cf9a510-9141-4143-b31b-6cbcbf1a8744', distributed).
narrative_ontology:cs_authority_grounding('3cf9a510-9141-4143-b31b-6cbcbf1a8744', practice).
narrative_ontology:cs_interpretation_layer_present('3cf9a510-9141-4143-b31b-6cbcbf1a8744').
narrative_ontology:cs_reading_relation('3cf9a510-9141-4143-b31b-6cbcbf1a8744', fiat_efficacy_kernel__scholarship_reading, coexists_with).
narrative_ontology:cs_reading_relation('3cf9a510-9141-4143-b31b-6cbcbf1a8744', fiat_efficacy_kernel__truth_procedure_reading, influences).
narrative_ontology:cs_reading_relation('3cf9a510-9141-4143-b31b-6cbcbf1a8744', fiat_efficacy_kernel__predictive_synthesis_reading, coexists_with).
narrative_ontology:cs_reading_relation('3cf9a510-9141-4143-b31b-6cbcbf1a8744', fiat_efficacy_kernel__empathy_simulation_reading, coexists_with).
narrative_ontology:cs_reading_relation('3cf9a510-9141-4143-b31b-6cbcbf1a8744', fiat_efficacy_kernel__utopian_fiction_reading, forecloses).
narrative_ontology:cs_axiom('3cf9a510-9141-4143-b31b-6cbcbf1a8744', foundational, efficacy_requires_documented_causal_chain).
narrative_ontology:cs_axiom_status(efficacy_requires_documented_causal_chain, holdable).
narrative_ontology:cs_axiom_grounding('3cf9a510-9141-4143-b31b-6cbcbf1a8744', efficacy_requires_documented_causal_chain, empirically_contingent).
narrative_ontology:cs_axiom('3cf9a510-9141-4143-b31b-6cbcbf1a8744', secondary, historical_analogy_licenses_present_action_warrant).
narrative_ontology:cs_axiom_status(historical_analogy_licenses_present_action_warrant, holdable).
narrative_ontology:cs_axiom_grounding('3cf9a510-9141-4143-b31b-6cbcbf1a8744', historical_analogy_licenses_present_action_warrant, instrumental).
narrative_ontology:cs_reference_frame('3cf9a510-9141-4143-b31b-6cbcbf1a8744', policy_debate_stock_issues_tradition).
narrative_ontology:cs_drift_state('3cf9a510-9141-4143-b31b-6cbcbf1a8744', contemporary_critical_debate_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3cf9a510-9141-4143-b31b-6cbcbf1a8744', '').
narrative_ontology:cs_kernel_id(empirical_precedent_reading, fiat_efficacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(empirical_precedent_reading, competitive_debaters_running_fiat).
narrative_ontology:constraint_beneficiary(empirical_precedent_reading, coaching_programs_teaching_activism_framing).
narrative_ontology:constraint_beneficiary(empirical_precedent_reading, debate_institutions_claiming_civic_relevance).
narrative_ontology:constraint_victim(empirical_precedent_reading, opposing_debaters_forced_to_engage_analogy).
narrative_ontology:constraint_victim(empirical_precedent_reading, historical_movement_participants_instrumentalized_as_evidence).
narrative_ontology:constraint_victim(empirical_precedent_reading, communities_named_in_analogized_movements_bds_vietnam_neoliberalism).
narrative_ontology:constraint_vindicates(empirical_precedent_reading, small_scale_collective_action_causal_efficacy_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Constructs and delivers the causal-chain warrant in-round, citing BDS attention, Vietnam withdrawal, and Chicago School founding as proof that small-scale fiated action produces measurable downstream outcomes. Collects competitive success (ballots, rankings, scholarships) from deploying the warrant persuasively. Can abandon the framing for a different warrant if a judge pool disfavors it.
narrative_ontology:constraint_stakeholder(empirical_precedent_reading, competitive_debaters_running_fiat, agenda_setter,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(empirical_precedent_reading, competitive_debaters_running_fiat, beneficiary).

% Must spend limited round time contesting the historical analogy's validity or lose the argument by default; cannot simply refuse engagement without conceding ground. Bears the cost of the warrant's rhetorical efficiency even when they believe the analogy is structurally weak.
narrative_ontology:constraint_stakeholder(empirical_precedent_reading, opposing_debaters_forced_to_engage_analogy, payer,
    moderate, immediate, constrained, national).

% Trains successive cohorts of debaters to deploy the empirical-precedent warrant because it reliably wins rounds and reinforces the program's competitive reputation. Profits from the warrant's persistence regardless of whether the underlying historical analogy holds up to scrutiny.
narrative_ontology:constraint_stakeholder(empirical_precedent_reading, coaching_programs_teaching_activism_framing, beneficiary,
    organized, generational, mobile, national).

% Uses the framing of fiat-as-efficacious-precedent to justify competitive debate's civic and pedagogical value to funders, universities, and accrediting bodies. Benefits from the warrant's persuasive power independent of its empirical soundness.
narrative_ontology:constraint_stakeholder(empirical_precedent_reading, debate_institutions_claiming_civic_relevance, beneficiary,
    institutional, generational, arbitrage, national).

% The actual organizers and participants of BDS, Vietnam antiwar movements, and Chicago School economic advocacy did the underlying work being cited; they are not present in the debate round, have no say in how their history is analogized, and receive nothing from its rhetorical reuse.
narrative_ontology:constraint_stakeholder(empirical_precedent_reading, historical_movement_participants_instrumentalized_as_evidence, excluded,
    powerless, generational, trapped, global).

% Gaza-related populations, Vietnamese civilians and veterans, and populations affected by neoliberal restructuring are the material referents of the cited outcomes. Their suffering or political stakes are converted into evidentiary shorthand for a competitive activity that does not affect their circumstances.
narrative_ontology:constraint_stakeholder(empirical_precedent_reading, communities_named_in_analogized_movements_bds_vietnam_neoliberalism, excluded,
    powerless, civilizational, trapped, global).

% Evaluate whether the empirical-precedent warrant is persuasively executed and theoretically sound within debate norms. Can reward or penalize the framing through ballots, shaping which reading of fiat efficacy dominates community practice, but do not administer the underlying historical movements themselves.
narrative_ontology:constraint_stakeholder(empirical_precedent_reading, judges_and_tournament_administrators, observer,
    institutional, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(empirical_precedent_reading, diffuse).
narrative_ontology:fixing_cost_class(empirical_precedent_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides competitive debaters with a shared, legible standard for adjudicating whether fiat (hypothetical policy enactment) has argumentative weight: pointing to documented historical cases where small-scale organized action produced measurable large-scale outcomes gives judges and opponents a common empirical register to evaluate impact claims, rather than leaving efficacy purely asserted.
% TRANSFER_FUNCTION: Moves competitive legitimacy and institutional credibility from the historical record and its participants to the debaters and programs who successfully deploy the analogy; the historical movements' documented costs and risks are converted into rhetorical capital that accrues to debate performance rather than to the movements or affected populations.
% ABSENT_VOICES: BDS organizers, Vietnamese War veterans and civilians, and populations affected by Chicago School-derived austerity and structural adjustment are never present in the debate round to contest how their history is characterized, compressed, or analogized to a fifteen-minute speech act; their absence structurally licenses whatever framing wins ballots.
% DISAPPEARANCE_RATIONALE: Debate coaches and institutions would argue the competitive activity substantially reorganizes without this warrant-type — an entire genre of impact-calculus argumentation would need replacement. Critics of the analogy would argue the underlying competitive activity is fundamentally unchanged; only the rhetorical packaging shifts to a different fiat-efficacy reading (e.g. truth_procedure or scholarship framing), meaning the world of competitive debate stays materially the same while argumentative fashions rotate.
% FOUNDING_PROBLEM: Competitive debate needed a way to adjudicate whether a hypothetically-enacted policy or advocacy (fiat) could be said to 'do' anything in the world, given that no actual policy is enacted in a debate round — the empirical-precedent reading solved this by pointing to documented historical cases where comparably small-scale action produced measurable large-scale outcomes.
% FOUNDING_PROBLEM_CORROBORATION: Debate coaches and competitors (the beneficiary set) attest the warrant remains necessary and functionally sound. Debate theory scholars and critical pedagogy researchers writing from outside competitive-debate institutions (e.g. communication studies critiques of switch-side debate) argue the founding problem was never actually solved by historical analogy and that the warrant functions as competitive theater rather than genuine epistemic resolution of fiat's action-status; no corroboration exists from the historical movements' own participants, who are not consulted in debate scholarship at all.
narrative_ontology:disappearance_verdict(empirical_precedent_reading, contested).
narrative_ontology:founding_problem_status(empirical_precedent_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(empirical_precedent_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-03',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(empirical_precedent_reading, 'none', 1).
narrative_ontology:epsilon_provenance(empirical_precedent_reading, 0.42, 'claude-sonnet-5', 'fiat_efficacy_kernel_2026_20260803_102258', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(empirical_precedent_reading_tests).
:- end_tests(empirical_precedent_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42, rising modestly from 0.28 over the interval) reflects moderate but real extraction: the warrant converts historical suffering and organizing labor into competitive currency without reciprocal benefit to the movements cited, but the extraction operates at one remove (rhetorical instrumentalization, not direct material harm) rather than as direct predation. Suppression (0.31) is comparatively low — no one is coerced into accepting the analogy; opposing debaters can and do contest it, and the practice coexists with viable alternative warrants (the sibling readings). Theater ratio (0.48, rising from 0.30) is notable and rising: as the warrant becomes institutionalized in coaching literature and stock arguments, an increasing share of its deployment is rote citation-matching rather than genuine engagement with the historical record's complexity, evidencing Goodhart-style metric substitution (citing the label 'BDS/Vietnam/Chicago School' rather than engaging the actual causal mechanisms). Accessibility collapse (0.35) is moderate-low: alternative fiat-efficacy framings remain fully available and are regularly run in the same tournaments, so the empirical-precedent reading has not foreclosed its siblings. Resistance (0.62) is comparatively high, reflecting active theoretical contestation within debate circles (theory arguments, kritiks of the fiat-efficacy warrant itself, critical pedagogy literature) — this is a warrant under live methodological dispute, not a settled convention.
 *
 * PERSPECTIVAL GAP:
 *   From the competitive debater's seat, the warrant is a legitimate application of historical political science to a hypothetical scenario — genuine intellectual coordination. From the seat of the communities whose movements are cited, the same warrant (if they were ever to become aware of it) would likely register as extraction: their organizing risk, casualties, and political struggle repackaged as rhetorical ammunition in an activity structurally disconnected from their material circumstances. The engine's per-seat computation should reflect this: agenda_setter/beneficiary seats compute closer to rope-like coordination, while the excluded victim seats — if they could be measured from their own structural position — would compute much closer to snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Competitive debaters and coaching programs sit near the beneficiary end: they extract competitive and institutional value from deploying the warrant and can freely substitute a different framing if this one loses favor (mobile/arbitrage exit). Opposing debaters are moderate targets — constrained by round-time economics to engage the analogy on its terms even when skeptical. The clearest victims are structurally outside the debate room entirely: historical movement participants and the communities named in the cited movements (Palestinians, Vietnamese people, neoliberalism-affected populations) have trapped exit options not because they cannot leave a relationship, but because they have no standing in the arrangement to begin with — their history is used as evidentiary material in a process from which they are categorically absent. This is a directionality case where the highest-d agents are non-participants; the derivation is driven entirely by the beneficiary/victim declarations rather than by any observable behavior of the excluded parties within the constraint itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (adjudicating whether fiat 'does' anything without an actual policy enactment) remains genuinely live for the competitive debate community — the mandatrophy question is not whether the coordination function has vanished, but whether THIS SPECIFIC reading (empirical-precedent) is the version doing the real work or has become theatrical residue riding alongside sturdier alternative warrants. The rising theater_ratio suggests partial mandatrophy: the warrant increasingly functions as citation-shorthand rather than substantive historical analysis, even though the coordination problem it addresses is real. Classifying this as tangled_rope rather than snare preserves the genuine coordination function (debate needs SOME way to adjudicate fiat efficacy) while still registering the asymmetric extraction from instrumentalized historical actors who never consented to being evidentiary material.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    analogy_transfer_validity,
    'Does the causal chain observed in BDS/Vietnam/Chicago School actually transfer to a single debate round''s fiat action, or is the analogy doing rhetorical work that the underlying mechanism cannot support?',
    'Comparative case analysis of the actual organizational scale, duration, and institutional access of the cited movements versus a single competitive debate speech act; historiographical review of whether those movements'' efficacy depended on scale-thresholds absent in the debate context.',
    'If the transfer is invalid, this reading''s warrant collapses into a rhetorical move rather than a genuine empirical precedent claim, converting the constraint''s coordination function into pure persuasive theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(analogy_transfer_validity, empirical, 'Whether cited historical precedents actually license inference to debate-round fiat efficacy.').

omega_variable(
    kernel_reading_selection,
    'Is efficacy properly located in the empirical/causal chain (this reading) rather than in a truth-procedure, predictive-synthesis, scholarship, empathy-simulation, or utopian-fiction framing of fiat?',
    'Examine which framing the debate community''s own judging paradigms and coaching literature treat as authoritative — if judges consistently reward causal-historical warrants over alternative framings, this reading dominates the fiat_efficacy_kernel in practice.',
    'If judges/coaches predominantly reward a different reading (e.g. truth_procedure_reading), this reading''s claimed dominance is itself contested, and the beneficiary set (debaters/coaches who rely on this framing) shrinks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection, conceptual, 'Which sibling reading of the fiat_efficacy_kernel actually holds institutional authority in competitive debate practice.').

omega_variable(
    instrumentalization_of_movement_participants,
    'Does citing BDS, Vietnam antiwar activists, and Chicago School economists as ''evidence'' for a debate warrant extract rhetorical value from those movements'' actual costs and risks without any reciprocal benefit to the movements or the populations they affected?',
    'Track whether debate outcomes (ballots, scholarships, competitive rankings) ever translate into resources, attention, or accountability flowing back to the cited movements or affected populations (Palestinians, Vietnamese civilians, neoliberalism-affected populations).',
    'If no reciprocal flow exists, the ''victim'' framing of instrumentalized movement participants is empirically substantiated rather than merely rhetorical, strengthening the tangled_rope classification over a pure rope reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(instrumentalization_of_movement_participants, empirical, 'Whether historical movements cited as precedent receive any benefit from their citation in debate rounds.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(empirical_precedent_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(empi_tr_t0, empirical_precedent_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(empi_tr_t8, empirical_precedent_reading, theater_ratio, 8, 0.35).
narrative_ontology:measurement(empi_tr_t16, empirical_precedent_reading, theater_ratio, 16, 0.4).
narrative_ontology:measurement(empi_tr_t24, empirical_precedent_reading, theater_ratio, 24, 0.44).
narrative_ontology:measurement(empi_tr_t32, empirical_precedent_reading, theater_ratio, 32, 0.47).
narrative_ontology:measurement(empi_tr_t40, empirical_precedent_reading, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(empi_be_t0, empirical_precedent_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(empi_be_t8, empirical_precedent_reading, base_extractiveness, 8, 0.33).
narrative_ontology:measurement(empi_be_t16, empirical_precedent_reading, base_extractiveness, 16, 0.37).
narrative_ontology:measurement(empi_be_t24, empirical_precedent_reading, base_extractiveness, 24, 0.4).
narrative_ontology:measurement(empi_be_t32, empirical_precedent_reading, base_extractiveness, 32, 0.41).
narrative_ontology:measurement(empi_be_t40, empirical_precedent_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(empi_su_t0, empirical_precedent_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(empi_su_t8, empirical_precedent_reading, suppression_requirement, 8, 0.24).
narrative_ontology:measurement(empi_su_t16, empirical_precedent_reading, suppression_requirement, 16, 0.27).
narrative_ontology:measurement(empi_su_t24, empirical_precedent_reading, suppression_requirement, 24, 0.29).
narrative_ontology:measurement(empi_su_t32, empirical_precedent_reading, suppression_requirement, 32, 0.3).
narrative_ontology:measurement(empi_su_t40, empirical_precedent_reading, suppression_requirement, 40, 0.31).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(empirical_precedent_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(empirical_precedent_reading, 0.1).
narrative_ontology:affects_constraint(empirical_precedent_reading, scholarship_reading).
narrative_ontology:affects_constraint(empirical_precedent_reading, truth_procedure_reading).
narrative_ontology:affects_constraint(empirical_precedent_reading, predictive_synthesis_reading).
narrative_ontology:affects_constraint(empirical_precedent_reading, empathy_simulation_reading).
narrative_ontology:affects_constraint(empirical_precedent_reading, utopian_fiction_reading).

% DUAL FORMULATION NOTE:
% This story is one of six sibling readings decomposing the natural-language concept 'fiat efficacy' under the fiat_efficacy_kernel. Each sibling reading locates efficacy in a structurally distinct place (empirical causal chain, knowledge production, internal validity, predictive accuracy, empathic value, or imaginative modeling) and therefore carries its own ε, beneficiary/victim structure, and classification. The empirical_precedent_reading is distinguished by its causal-historical warrant and its uniquely severe exclusion of real-world movement participants from any voice in how their history is deployed — a structural feature the other readings (which do not depend on citing specific historical movements as evidence) do not share to the same degree.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(empirical_precedent_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
