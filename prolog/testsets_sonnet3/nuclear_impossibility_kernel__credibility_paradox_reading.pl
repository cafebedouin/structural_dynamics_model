% ============================================================================
% CONSTRAINT STORY: nuclear_impossibility_kernel__credibility_paradox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nuclear_impossibility_kernel__credibility_paradox_reading, []).

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
 *   constraint_id: nuclear_impossibility_kernel__credibility_paradox_reading
 *   human_readable: Nuclear Deterrence Credibility Paradox (Escalation-Ladder Reading)
 *   domain: strategic_studies/international_relations
 *
 * SUMMARY:
 *   This story instantiates the credibility_paradox_reading of the
 *   nuclear_impossibility_kernel: deterrence requires a credible threat to
 *   use nuclear weapons, but any actual use guarantees mutual destruction,
 *   making the threat structurally incredible at the point it would matter
 *   most. Rather than treating this as a stable equilibrium (the
 *   structural_contraction_reading) or a rational-choice off-ramp (the
 *   rational_dropout_reading), this reading holds that the paradox is
 *   UNSTABLE: great powers continuously re-engineer 'usable' nuclear options
 *   — counterforce targeting, flexible response, tactical/low-yield weapons,
 *   prompt global strike, escalation ladders — precisely to patch the
 *   incredibility problem and keep some rung of nuclear threat operationally
 *   credible. 'Unthinkability' is therefore read as rhetorical cover for
 *   arsenals and doctrines that are, in fact, built to keep war reachable
 *   through graduated escalation. This is a single, ε-invariant claim about
 *   the standing arrangement (Cold War through contemporary counterforce
 *   modernization) as this reading sees it — not an evaluation of what a
 *   stable minimal-deterrence regime would look like.
 *
 * KEY AGENTS:
 *   - nuclear_weapon_states_strategic_establishments: agenda_setter/beneficiary — designs escalation ladders to patch the credibility gap
 *   - defense_industrial_base: beneficiary — profits from continuous modernization justified by the credibility problem
 *   - non_nuclear_frontline_states: payer — geography becomes the escalation rung
 *   - civilian_populations_under_extended_deterrence: payer — bear risk from doctrines they cannot see or shape
 *   - future_generations_under_escalation_risk: payer — inherit accumulated tail risk
 *   - arms_control_negotiators: excluded — pushes for minimal deterrence, structurally sidelined from war-planning
 *   - strategic_studies_scholars: observer — documents the recurring credibility-patch cycle
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_impossibility_kernel__credibility_paradox_reading, 0.62).
domain_priors:suppression_score(nuclear_impossibility_kernel__credibility_paradox_reading, 0.71).
domain_priors:theater_ratio(nuclear_impossibility_kernel__credibility_paradox_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__credibility_paradox_reading, tangled_rope).
narrative_ontology:human_readable(nuclear_impossibility_kernel__credibility_paradox_reading, "Nuclear Deterrence Credibility Paradox (Escalation-Ladder Reading)").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__credibility_paradox_reading, "strategic_studies/international_relations").

domain_priors:requires_active_enforcement(nuclear_impossibility_kernel__credibility_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__credibility_paradox_reading, '02559d25-0639-4e10-8180-34ef3289dfbc').
narrative_ontology:cs_kernel_codification('02559d25-0639-4e10-8180-34ef3289dfbc', distributed).
narrative_ontology:cs_authority_grounding('02559d25-0639-4e10-8180-34ef3289dfbc', practice).
narrative_ontology:cs_interpretation_layer_present('02559d25-0639-4e10-8180-34ef3289dfbc').
narrative_ontology:cs_reading_relation('02559d25-0639-4e10-8180-34ef3289dfbc', nuclear_impossibility_kernel__structural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('02559d25-0639-4e10-8180-34ef3289dfbc', nuclear_impossibility_kernel__rational_dropout_reading, influences).
narrative_ontology:cs_axiom('02559d25-0639-4e10-8180-34ef3289dfbc', foundational, credibility_gap_drives_continuous_doctrinal_instability).
narrative_ontology:cs_axiom_status(credibility_gap_drives_continuous_doctrinal_instability, holdable).
narrative_ontology:cs_axiom_grounding('02559d25-0639-4e10-8180-34ef3289dfbc', credibility_gap_drives_continuous_doctrinal_instability, empirically_contingent).
narrative_ontology:cs_axiom('02559d25-0639-4e10-8180-34ef3289dfbc', foundational, unthinkability_is_rhetorical_not_structural).
narrative_ontology:cs_axiom_status(unthinkability_is_rhetorical_not_structural, holdable).
narrative_ontology:cs_axiom_grounding('02559d25-0639-4e10-8180-34ef3289dfbc', unthinkability_is_rhetorical_not_structural, empirically_contingent).
narrative_ontology:cs_reference_frame('02559d25-0639-4e10-8180-34ef3289dfbc', flexible_response_counterforce_doctrine).
narrative_ontology:cs_drift_state('02559d25-0639-4e10-8180-34ef3289dfbc', post_cold_war_modernization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('02559d25-0639-4e10-8180-34ef3289dfbc', '').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_weapon_states_strategic_establishments).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__credibility_paradox_reading, defense_industrial_base).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__credibility_paradox_reading, non_nuclear_frontline_states).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__credibility_paradox_reading, civilian_populations_under_extended_deterrence).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__credibility_paradox_reading, future_generations_under_escalation_risk).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and maintains counterforce postures, limited-war doctrines, and escalation-ladder war plans specifically because the pure mutual-annihilation threat is not credible enough to deter lesser aggressions. Builds flexible-response and tactical options that keep war 'thinkable' at intermediate rungs. Captures budgetary, bureaucratic, and geopolitical leverage from maintaining and modernizing these options; can exit into arms-control dialogue when convenient but is never structurally forced to.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_weapon_states_strategic_establishments, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_weapon_states_strategic_establishments, beneficiary).

% Manufactures the tactical warheads, delivery systems, and modernization programs the credibility paradox justifies — low-yield weapons, hypersonic glide vehicles, missile defense. Revenue flows directly from the perceived need to make deterrence 'usable' at lower rungs of the escalation ladder. Can pivot to adjacent contracts if a program is cancelled; the paradox is not existential to any single firm.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, defense_industrial_base, beneficiary,
    organized, biographical, mobile, national).

% Sit inside contested regions (the Baltics, Taiwan Strait, the Korean Peninsula) where the great powers' pursuit of 'usable' nuclear options and limited-war doctrines directly increases the odds their territory becomes an escalation rung. They have no seat in setting the doctrines that treat their geography as a testing ground for credibility. Cannot opt out of the alliance structures that place them there without losing conventional deterrence entirely.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, non_nuclear_frontline_states, payer,
    moderate, biographical, constrained, regional).

% Live under nuclear umbrellas whose credibility problem is 'solved' by keeping escalation thinkable — meaning war-fighting plans exist that put populations at risk precisely because leaders needed the threat to look usable. Have essentially no input into doctrine and no exit from the risk; migration does not remove the risk since the escalation ladders are transnational and second-strike targeting is comprehensive.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, civilian_populations_under_extended_deterrence, payer,
    powerless, biographical, trapped, global).

% Inherit whatever residual escalation risk accumulates from decades of maintaining 'usable' nuclear options as a credibility patch. Bear the tail risk of miscalculation on an escalation ladder built to make the incredible threat seem credible. Have no representation in current doctrine debates and cannot exit a risk profile set before their existence.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, future_generations_under_escalation_risk, payer,
    powerless, civilizational, trapped, universal).

% Argue that resolving the credibility paradox through escalation ladders and counterforce is itself destabilizing, and press for doctrines of minimal deterrence or no-first-use that would remove the 'usability' fix. Structurally sidelined whenever military establishments treat credibility engineering as an operational rather than a political question; their proposals are heard in treaty forums but rarely shape the war plans themselves.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, arms_control_negotiators, excluded,
    moderate, generational, constrained, global).

% Study the credibility paradox as a live structural instability — documenting how each generation of leaders re-engineers 'usable' options (flexible response, counterforce, prompt global strike, tactical nuclear modernization) to patch the incredibility of pure mutual destruction. Their analysis feeds doctrine debates but does not itself set policy.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, strategic_studies_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_weapon_states_strategic_establishments).
narrative_ontology:fixing_cost_class(nuclear_impossibility_kernel__credibility_paradox_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a genuine, real coordination function among nuclear-armed rivals: shared expectations about escalation thresholds, signaling protocols, and crisis-management channels that have prevented deliberate nuclear war for over seven decades by making mutual restraint calculable even under uncertainty.
% TRANSFER_FUNCTION: Moves security assurance from great-power arsenals to allied and client populations nominally, but moves actual escalation risk from the decision-making core (leaders, strategic establishments who can retreat to bunkers or negotiate) onto frontline states, civilian populations under extended deterrence, and unborn generations who bear the tail risk without having consented to the doctrine that produced it. Moves procurement revenue to the defense industrial base as escalation-ladder hardware is continuously modernized to keep the threat 'usable.'
% ABSENT_VOICES: Frontline populations in contested regions, civil society groups pressing for no-first-use or minimal deterrence, and future generations have no seat at the doctrine table. Arms control negotiators are formally present in treaty forums but structurally excluded from the war-planning process where escalation ladders are actually built.
% DISAPPEARANCE_RATIONALE: If the credibility-paradox management apparatus (counterforce postures, flexible response, limited-war doctrine, escalation ladders) disappeared overnight, nuclear postures would revert to pure existential deterrence or collapse into genuine incredibility — either forcing much more conservative crisis behavior (since leaders could no longer threaten graduated, 'usable' responses) or triggering a credibility crisis that reshapes alliance structures, extended-deterrence commitments, and defense budgets across the world.
% FOUNDING_PROBLEM: Pure mutual assured destruction is not a credible deterrent against anything short of existential attack — an adversary calling the bluff on a suicidal threat exposes the deterrer as unable to respond proportionately to limited aggression, salami-slicing, or regional conflict.
% FOUNDING_PROBLEM_CORROBORATION: Independent strategic studies scholars (Schelling's original credibility-of-commitment analysis, subsequent escalation-dominance literature) and multiple declassified war-planning archives corroborate that the credibility gap is a persistent, structurally real problem that each generation of planners has had to re-solve, not a rhetorical artifact invented by any single administration. Arms control advocates outside the beneficiary set corroborate the problem's persistence while disputing that escalation-ladder engineering is the right solution — their corroboration of the diagnosis, combined with their rejection of the cure, supports 'live' rather than 'dead.'
narrative_ontology:disappearance_verdict(nuclear_impossibility_kernel__credibility_paradox_reading, world_rearranges).
narrative_ontology:founding_problem_status(nuclear_impossibility_kernel__credibility_paradox_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nuclear_impossibility_kernel__credibility_paradox_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nuclear_impossibility_kernel__credibility_paradox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nuclear_impossibility_kernel__credibility_paradox_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_impossibility_kernel__credibility_paradox_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nuclear_impossibility_kernel__credibility_paradox_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nuclear_impossibility_kernel__credibility_paradox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62 at 2025) reflects that the coordination function (crisis stability, avoided deliberate nuclear war) is real but increasingly overlaid with escalation-ladder infrastructure whose primary beneficiaries are strategic establishments and the defense-industrial base, while the risk this infrastructure manages is borne disproportionately by frontline states and civilians who never consented to it. Suppression (0.71) is high because credibility-paradox management requires actively suppressing the alternative (minimal/finite deterrence, no-first-use) — treaty proposals for de-escalatory postures are routinely defeated inside war-planning bureaucracies even when politically popular. Theater ratio (0.58) captures that a substantial share of modernization programs (low-yield warheads, hypersonic systems) function more as credibility signaling than as operationally necessary — the theater IS the fix for the incredibility problem, not a byproduct of it. Accessibility collapse is only moderate (0.35) because genuine alternatives (minimal deterrence, disarmament regimes, no-first-use pledges) remain politically articulable and have been adopted by some states (China's NFU posture, for instance) — the paradox has not fully foreclosed alternative doctrine, which is precisely why this reading holds escalation is a choice made to patch incredibility rather than a physical inevitability.
 *
 * PERSPECTIVAL GAP:
 *   From the strategic establishment's seat, escalation-ladder engineering is prudent crisis management solving a genuine credibility deficit. From the frontline-state or civilian seat, the same infrastructure looks like being drafted, without consent, into someone else's credibility-signaling apparatus. The engine should compute these as structurally different experiences of the identical arrangement — this is exactly the kind of divergence tangled_rope classification is built to surface rather than average away.
 *
 * DIRECTIONALITY LOGIC:
 *   Strategic establishments and the defense-industrial base sit near the beneficiary end: they set the doctrines, capture the budgetary and bureaucratic returns, and retain institutional exit options (arms-control pivot, program cancellation without existential cost). Frontline states, extended-deterrence civilian populations, and future generations sit near the target end: they bear the escalation risk the ladder creates without having authored or consented to the doctrine, and their exit options range from constrained (alliance dependency) to fully trapped (no geographic or temporal exit at all). This maps a genuine structural asymmetry, not a rhetorical one — the credibility fix is engineered by and largely for the parties best insulated from its downside.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (pure MAD is not credible against limited aggression) remains live by this reading's own lights — it has not become obsolete zombie infrastructure. What has drifted is the SOLUTION: the escalation-ladder apparatus built to patch credibility has itself become an extraction vector, generating procurement rents and bureaucratic entrenchment beyond what the underlying coordination problem requires. Classifying this as tangled_rope rather than snare preserves the genuine coordination achievement (avoided deliberate nuclear war for 80 years) while flagging that the specific mechanism chosen to solve the credibility gap externalizes cost onto non-consenting parties — exactly the asymmetric-extraction-riding-on-real-coordination structure tangled_rope is meant to capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location_of_disagreement,
    'Do the three kernel readings (credibility_paradox, structural_contraction, rational_dropout) disagree about the FACTS of nuclear deterrence, or about which facts are load-bearing for classification?',
    'Compare declassified war-planning documents across the three readings'' preferred evidentiary bases: does documented pursuit of counterforce/limited-war options (this reading''s evidence) coexist with genuine leadership belief in guaranteed annihilation (structural_contraction''s evidence) and cost-benefit reasoning against war (rational_dropout''s evidence)? All three may be simultaneously true at different levels of the same decision apparatus (declaratory doctrine vs. operational war plans vs. individual leader psychology).',
    'If all three are compatible at different levels, the kernel itself may be under-specified — the disagreement is about WHICH LEVEL of the nuclear enterprise (declaratory doctrine, operational planning, leader cognition) is the correct referent for ''the'' deterrence constraint, not a factual dispute resolvable by more data.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location_of_disagreement, conceptual, 'Whether the kernel readings disagree on facts or on referent-level for classification.').

omega_variable(
    escalation_ladder_necessity_vs_extraction,
    'Is the ongoing construction of ''usable'' nuclear options (counterforce, low-yield, prompt global strike) a necessary response to a genuine and persistent credibility gap, or has it become self-sustaining bureaucratic and industrial extraction that outlives the marginal credibility benefit it once provided?',
    'Compare marginal deterrence value estimates (from independent strategic analysts, not vendor-funded studies) against procurement cost trajectories for successive generations of ''usable'' systems; also examine whether states with more restrained postures (declared minimal deterrence, no-first-use) experience measurably worse crisis outcomes.',
    'If restrained postures perform comparably in crisis stability terms, the escalation-ladder buildout is substantially rent-seeking layered on a real but bounded coordination problem — strengthening the tangled_rope classification over a pure rope reading in which the buildout is proportionate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(escalation_ladder_necessity_vs_extraction, empirical, 'Whether escalation-ladder modernization tracks genuine deterrence necessity or outpaces it.').

omega_variable(
    unthinkability_rhetorical_vs_operative,
    'Is ''nuclear unthinkability'' merely rhetorical cover maintained alongside operational war plans that treat use as reachable (this reading''s claim), or does declaratory unthinkability exert genuine causal restraint on operational planning independent of what war plans formally permit?',
    'Trace historical instances where declaratory taboo constrained or overrode operationally available options (e.g., non-use in Korea, Vietnam, Falklands despite tactical availability) versus instances where operational planning proceeded to build capability specifically to counteract the taboo''s restraining effect.',
    'If declaratory unthinkability demonstrably constrained real decisions independent of capability, the sharp rhetorical/operative distinction this reading draws is overstated, and the reading would need revision toward acknowledging a genuine (if imperfect) normative constraint layered atop the credibility engineering.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unthinkability_rhetorical_vs_operative, empirical, 'Whether the taboo against use has independent causal force beyond doctrine and capability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__credibility_paradox_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nucl_tr_t1945, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1945, 0.2).
narrative_ontology:measurement(nucl_tr_t1962, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1962, 0.3).
narrative_ontology:measurement(nucl_tr_t1983, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1983, 0.42).
narrative_ontology:measurement(nucl_tr_t1991, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1991, 0.35).
narrative_ontology:measurement(nucl_tr_t2010, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(nucl_tr_t2025, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 2025, 0.58).

% Extraction over time
narrative_ontology:measurement(nucl_be_t1945, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1945, 0.35).
narrative_ontology:measurement(nucl_be_t1962, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1962, 0.48).
narrative_ontology:measurement(nucl_be_t1983, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1983, 0.58).
narrative_ontology:measurement(nucl_be_t1991, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1991, 0.45).
narrative_ontology:measurement(nucl_be_t2010, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 2010, 0.52).
narrative_ontology:measurement(nucl_be_t2025, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 2025, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(nucl_su_t1945, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1945, 0.4).
narrative_ontology:measurement(nucl_su_t1962, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1962, 0.6).
narrative_ontology:measurement(nucl_su_t1983, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1983, 0.68).
narrative_ontology:measurement(nucl_su_t1991, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1991, 0.5).
narrative_ontology:measurement(nucl_su_t2010, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 2010, 0.55).
narrative_ontology:measurement(nucl_su_t2025, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 2025, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_impossibility_kernel__credibility_paradox_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(nuclear_impossibility_kernel__credibility_paradox_reading, 0.12).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_impossibility_kernel__structural_contraction_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_impossibility_kernel__rational_dropout_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__credibility_paradox_reading, extended_deterrence_alliance_commitments).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_nonproliferation_regime).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the nuclear_impossibility_kernel. structural_contraction_reading treats mutual annihilation as a genuine physical impossibility foreclosing rational war-seeking; rational_dropout_reading treats victory as theoretically possible but irrational given costs; this reading (credibility_paradox_reading) treats the deterrence threat as internally contradictory and therefore unstable, generating continuous escalation-ladder engineering. Each reading authors its own ε, beneficiary/victim structure, and claimed_type against the same underlying kernel (the paradox nuclear weapons pose for strategic theory) — they are not measurement variants of one constraint but three structurally distinct constraints sharing a contested commitment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
