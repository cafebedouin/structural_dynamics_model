% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__absolute_non_intervention
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalia_sovereignty__absolute_non_intervention, []).

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
 *   constraint_id: westphalia_sovereignty__absolute_non_intervention
 *   human_readable: Absolute Non-Intervention Norm (Categorical Territorial Inviolability)
 *   domain: international_law/political_theory/state_systems
 *
 * SUMMARY:
 *   The absolute non-intervention reading holds that external interference in
 *   domestic affairs is illegitimate per se, regardless of internal conduct:
 *   inviolability is categorical, unconditional, and reciprocal. This file
 *   instantiates that reading as a clean, epsilon-invariant constraint over
 *   the standing interstate arrangement; the sibling readings
 *   (conditional_responsibility, graded_sovereignty) are separate stories
 *   with their own victim sets and their own epsilon values, linked through
 *   the network section. The arrangement has a genuine, measurable
 *   coordination function — it underwrote the near-disappearance of
 *   interstate territorial conquest and gives weak states security without
 *   world government — and a genuine, measurable extraction: it converts
 *   domestic tyranny into externally invisible fact, sealing populations
 *   inside predatory states with no recourse. Beneficiary and victim
 *   declarations below are the structural input to the engine's per-seat
 *   computation; the claimed type and the metrics are authored independently.
 *   KEY AGENTS (by structural relationship): - authoritarian_regime_elites:
 *   Primary beneficiary (institutional/constrained) — collects immunity,
 *   administers the norm's defense - postcolonial_new_states: Secondary
 *   beneficiary (organized/constrained) — security dividend of categorical
 *   equality - established_great_powers: Beneficiary-administrator
 *   (institutional/arbitrage) — runs the enforcement architecture, reserves
 *   de facto exception - populations_under_authoritarian_rule: Primary target
 *   (powerless/trapped) — bears sealed-border costs -
 *   persecuted_minorities_facing_atrocity: Acute target (powerless/trapped,
 *   immediate horizon) — the reading's excluded protection set -
 *   domestic_opposition_movements: Target (moderate/constrained) — cut off
 *   from external leverage - humanitarian_intervention_advocates: Excluded
 *   seat (organized/constrained) — the rival reading's carriers, outside the
 *   conversation - international_order_analysts: Analytical observer
 *   (analytical/analytical) — sees the full structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__absolute_non_intervention, 0.66).
domain_priors:suppression_score(westphalia_sovereignty__absolute_non_intervention, 0.64).
domain_priors:theater_ratio(westphalia_sovereignty__absolute_non_intervention, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, extractiveness, 0.66).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__absolute_non_intervention, tangled_rope).
narrative_ontology:human_readable(westphalia_sovereignty__absolute_non_intervention, "Absolute Non-Intervention Norm (Categorical Territorial Inviolability)").
narrative_ontology:topic_domain(westphalia_sovereignty__absolute_non_intervention, "international_law/political_theory/state_systems").

domain_priors:requires_active_enforcement(westphalia_sovereignty__absolute_non_intervention).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__absolute_non_intervention, '0005e2d9-f001-48c8-a8dc-f2ace1d6c765').
narrative_ontology:cs_kernel_codification('0005e2d9-f001-48c8-a8dc-f2ace1d6c765', fixed_text).
narrative_ontology:cs_authority_grounding('0005e2d9-f001-48c8-a8dc-f2ace1d6c765', lineage).
narrative_ontology:cs_interpretation_layer_present('0005e2d9-f001-48c8-a8dc-f2ace1d6c765').
narrative_ontology:cs_reading_relation('0005e2d9-f001-48c8-a8dc-f2ace1d6c765', westphalia_sovereignty__conditional_responsibility, forecloses).
narrative_ontology:cs_reading_relation('0005e2d9-f001-48c8-a8dc-f2ace1d6c765', westphalia_sovereignty__graded_sovereignty, forecloses).
narrative_ontology:cs_axiom('0005e2d9-f001-48c8-a8dc-f2ace1d6c765', foundational, internal_conduct_irrelevant_to_inviolability).
narrative_ontology:cs_axiom_status(internal_conduct_irrelevant_to_inviolability, holdable).
narrative_ontology:cs_axiom_grounding('0005e2d9-f001-48c8-a8dc-f2ace1d6c765', internal_conduct_irrelevant_to_inviolability, conventional).
narrative_ontology:cs_axiom('0005e2d9-f001-48c8-a8dc-f2ace1d6c765', secondary, pluralist_order_requires_absolute_reciprocity).
narrative_ontology:cs_axiom_status(pluralist_order_requires_absolute_reciprocity, holdable).
narrative_ontology:cs_axiom_grounding('0005e2d9-f001-48c8-a8dc-f2ace1d6c765', pluralist_order_requires_absolute_reciprocity, deontological).
narrative_ontology:cs_reference_frame('0005e2d9-f001-48c8-a8dc-f2ace1d6c765', westphalian_categorical_inviolability).
narrative_ontology:cs_drift_state('0005e2d9-f001-48c8-a8dc-f2ace1d6c765', contemporary_post_r2p_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0005e2d9-f001-48c8-a8dc-f2ace1d6c765', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__absolute_non_intervention, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, authoritarian_regime_elites).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, postcolonial_new_states).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, established_great_powers).
narrative_ontology:constraint_victim(westphalia_sovereignty__absolute_non_intervention, populations_under_authoritarian_rule).
narrative_ontology:constraint_victim(westphalia_sovereignty__absolute_non_intervention, persecuted_minorities_facing_atrocity).
narrative_ontology:constraint_victim(westphalia_sovereignty__absolute_non_intervention, domestic_opposition_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rule territories under the norm's umbrella: whatever happens inside the border is legally invisible from outside. They collect immunity from external accountability, conduct-targeted pressure, and rescue operations aimed at their subjects, and they spend real diplomatic capital at the United Nations defending the categorical bar, because any crack in it — a conduct threshold, a capacity calibration — becomes precedent against every regime like theirs. Exit would mean accepting external judgment of internal conduct, which is precisely what the arrangement exists to prevent.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, authoritarian_regime_elites, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__absolute_non_intervention, authoritarian_regime_elites, agenda_setter).

% Won independence within living memory against powers that treated their territory as disposable. The absolute bar is their principal security guarantee: it converts material weakness into formal legal equality and makes reconquest or punitive expedition illegitimate per se. They vote as a bloc to defend it. The price they accept is that the same shield covers predatory neighbors and their own worst internal conduct.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, postcolonial_new_states, beneficiary,
    organized, generational, constrained, national).

% Drafted the Charter, hold veto power, and administer the enforcement architecture that makes the bar credible. They gain reciprocal immunity and stable spheres of influence. They also reserve a de facto exception: when vital interests engage they intervene anyway and absorb the legitimacy cost, so the formal norm constrains their rivals' excuses more reliably than their own practice. Their violation option is real but priced — credibility, coalition maintenance, precedent.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, established_great_powers, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__absolute_non_intervention, established_great_powers, agenda_setter).

% Live inside borders the norm seals from outside. When their rulers turn predatory, the categorical bar classifies their peril as an internal matter: no legal channel summons external protection, appeals for rescue read as collusion with foreigners, and flight is the only exit — itself criminalized. They bear the full cost of whoever holds territorial monopoly over them, with no outside referee.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, populations_under_authoritarian_rule, payer,
    powerless, biographical, trapped, national).

% Face mass killing, expulsion, or genocide on a timescale of weeks. Under the categorical bar, prevention requires the perpetrator's consent or a council unanimity the veto blocks; the historical record — Rwanda 1994 processed as an internal conflict while it ran — is the cost side of the reading's high barrier. Under this reading their protection exists only at the discretion of the party attacking them.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, persecuted_minorities_facing_atrocity, payer,
    powerless, immediate, trapped, regional).

% Organize against entrenched regimes and look outward for leverage: sanctuary, funding, recognition, pressure on their rulers. The categorical bar delegitimizes all of it as impermissible interference, cutting oppositions off from external resources and handing regimes a patriotism frame that equates dissent with foreign agency. Exile is available but severs them from the constituency they organize.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, domestic_opposition_movements, payer,
    moderate, biographical, constrained, national).

% Human-rights organizations, atrocity-prevention scholars, and protection-doctrine proponents argue that inviolability should be conditional on populations being protected. Under the absolute reading their entire doctrinal project is illegitimate per se: they hold no seat in the norm's administration, their documentation enters official channels only as interference advocacy, and their one institutional foothold — the 2005 summit language — is what this reading's adherents are working to roll back.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, humanitarian_intervention_advocates, excluded,
    organized, generational, constrained, global).

% Historians, international lawyers, and scholars of international relations trace the norm's genealogy from the 1648 settlement through the Charter's domestic-jurisdiction clause, measure the gap between doctrine and practice, and adjudicate whether categorical inviolability is law, aspiration, or elite shield. They collect nothing and pay nothing; their classifications feed the contest between readings.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, international_order_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalia_sovereignty__absolute_non_intervention, authoritarian_regime_elites).
narrative_ontology:fixing_cost_class(westphalia_sovereignty__absolute_non_intervention, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the interstate anarchy problem: by making every state's territorial monopoly universally recognized and categorically inviolable, it removes the standing license of conquest, crusade, and punitive rescue that made pre-modern order impossible; lets radically unequal regimes deal with each other as formal equals; and gives weak states security against strong ones without any world government.
% TRANSFER_FUNCTION: Moves immunity and legitimacy upward and recourse-denial downward: rulers acquire external-accountability-proof status, while the costs of their conduct concentrate wholly on their own subjects, who lose any external channel of protection or appeal. Security for weak states is paid for by foreclosed protection of at-risk populations everywhere else.
% ABSENT_VOICES: The populations the norm seals inside predatory states were never seated: Westphalia was negotiated among princes over confessional populations nobody consulted, and the Charter was written by victorious great powers while colonized peoples were objects of the settlement, not its authors. Their standing objection — that the inviolability of rulers is not the safety of the ruled — has no institutional address; it speaks only through the excluded advocacy seat.
% DISAPPEARANCE_RATIONALE: Borders become negotiable overnight: revisionist powers move on neighbors under rescued-minority or capacity-fixing pretexts, small states race for patrons or nuclear weapons, and the treaty, diplomatic, and commercial architecture built on mutual recognition loses its foundation. Whatever replaced it — a conditional or graded regime — would itself be a new construction, not a reversion to a natural state.
% FOUNDING_PROBLEM: The wars of religion: for a century and a half, every prince claimed a duty to rescue co-religionists inside other princes' lands, making any stable European order impossible. Westphalia's solution was mutual recognition of territorial monopoly — stop adjudicating what happens inside another sovereign's borders and the transborder killing between sovereigns stops. The problem was re-founded in 1945: never again should peoples be objects disposed of by great-power conference.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the Thirty Years War and of the 1945 settlement corroborate, from outside the benefiting parties, that the founding problems were real and that the norm measurably contributed to solving them (the near-disappearance of interstate territorial conquest after 1945 is documented independently of regime elites). Whether the problem remains live is disputed along the reading lines: regime elites and postcolonial blocs attest liveness, citing persistent great-power predation; atrocity scholars and humanitarian advocates — also outside the benefiting parties — attest that the operative problem the arrangement now solves is elite impunity, not interstate chaos. Corroboration for the founding problem's reality is strong; corroboration for its continued primacy comes only from beneficiaries.
narrative_ontology:disappearance_verdict(westphalia_sovereignty__absolute_non_intervention, world_rearranges).
narrative_ontology:founding_problem_status(westphalia_sovereignty__absolute_non_intervention, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__absolute_non_intervention, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(westphalia_sovereignty__absolute_non_intervention, 'none', 1).
narrative_ontology:epsilon_provenance(westphalia_sovereignty__absolute_non_intervention, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalia_sovereignty__absolute_non_intervention_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalia_sovereignty__absolute_non_intervention, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalia_sovereignty__absolute_non_intervention_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.66: substantial but bounded — the norm delivers real interstate goods while concentrating impunity upward and recourse-denial downward; the victim set is enormous (billions live under regimes the norm shields) but the coordination dividend is equally real. Suppression 0.64: the bar is maintained by reciprocal self-interest plus veto architecture, and where challenged it mobilizes hard; alternatives remain live, so not maximal. Theater 0.52: a growing share of norm-invocation is ritual — General Assembly sovereignty rhetoric delivered by states simultaneously violating the norm, summit language adopted without machinery — a Goodhart-drift signal that invocation is decoupling from observance. Accessibility collapse 0.40: once the norm is understood, the conditional and graded alternatives remain institutionally reachable (R2P language exists; Kosovo happened outside charter authorization), so alternatives are partly suppressed, not collapsed. Resistance 0.58: sustained doctrinal and political resistance from the human-rights complex, atrocity-prevention scholarship, and intervention coalitions. The temporal series run on one shared eight-point grid (1648, 1815, 1919, 1945, 1960, 1994, 2005, 2026) with every tracked metric authored at every point. The 1994-to-2005-to-2026 oscillation is not noise: it is the kernel contest itself — each atrocity scandal pressures the bar, each controversial intervention triggers restoration. Extraction peaked at Rwanda (the full cost of the categorical bar made visible), dipped after the 2005 summit admitted the conditional reading's language, and rose again after the Libya backlash re-hardened it. Base properties are taken at interval end, in the post-backlash resealing phase. Coalition note: the victim seats are coalition-incapable by design — trapped, dispersed across jurisdictions, with no international franchise — which is part of the structure, not an accident of measurement.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats compute different constraints from identical structural data. From the trapped population seats, the norm is a sealed border: total closure of external recourse, experienced most acutely by atrocity-facing minorities whose time horizon is immediate. From the regime-elite seat, the same structure is earned immunity and the dignity of formal equality. From the great-power seat, it is binding-on-others convenience — real restraint costs, absorbed selectively, kept affordable by arbitrage exit. The excluded advocacy seat perceives the deepest gap: persons misclassified as jurisdiction. The engine computes this per-seat divergence from the power and exit atoms; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Regime elites, postcolonial states, and great powers are declared beneficiaries and derive low directionality — the arrangement subsidizes them, so effective extraction is damped or inverted for their seats. Trapped populations, atrocity-facing minorities, and constrained oppositions are declared victims and derive high directionality — effective extraction is amplified for them, further amplified by the arrangement's global scope, which raises verification difficulty. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled. No directionality overrides are used: the derivation from beneficiary/victim declarations plus exit atoms captures every seat, including the great-power nuance (their restraint costs are real but net out on the beneficiary side given arbitrage-grade exit).
 *
 * MANDATROPHY ANALYSIS:
 *   The classification discipline prevents two opposite mislabels. Calling this a pure coordination mechanism would erase the trapped victim set: the interstate peace dividend does not reach sealed populations, and the coordination story is partly cover for elite impunity. Calling it pure extraction would erase the genuine, independently corroborated function — the near-disappearance of interstate conquest, the real security the norm gave decolonized states — which no cover story manufactured. The hybrid reading holds both: real coordination function, asymmetric extraction through the same structure, active enforcement required to maintain it. On the R5 mismatch check: founding_problem_status is contested and disappearance_verdict is world_rearranges, so no zombie flag fires — the arrangement demonstrably rearranges the world if removed — but the contested status records that the founding problem's continued primacy is now asserted mainly by the arrangement's beneficiaries, which is itself signal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading (absolute_non_intervention) of the contested kernel westphalia_sovereignty; sibling readings conditional_responsibility and graded_sovereignty instantiate different constraints over the same interstate practice. Which reading''s structure best describes the operative international order?',
    'Track Security Council authorization patterns, General Assembly voting blocs, and state justificatory language for interventions across decades: rising conduct-threshold justifications indicate migration toward the conditional reading; capacity-calibrated language indicates the graded reading; categorical internal-affair language indicates this reading''s persistence.',
    'If the conditional reading becomes operative, the protected set expands to atrocity-threatened populations and the barrier drops — a different constraint with different epsilon and different classification. This file''s verdict holds only for the absolute reading''s tenure; the siblings are separate stories, not parameters of this one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one of three rival readings of the sovereignty kernel; classification is reading-indexed.').

omega_variable(
    inviolability_attachment_locus,
    'Does territorial inviolability attach to the state-as-ruler (this reading''s premise) or to the population-as-territorial-community (the conditional reading''s premise)? This is the precise structural element on which the kernel contest turns.',
    'Doctrinal analysis of the protection vocabulary of international law — whether territorial integrity and domestic jurisdiction grammatically protect institutions or persons — combined with case studies of how atrocity episodes resolved the ambiguity in practice.',
    'If inviolability attaches to populations, this reading''s victim set is miscounted: the shielded are rights-holders betrayed rather than mere bystanders, and the cost profile shifts from subsidy-to-elites toward betrayal-of-subjects, changing effective extraction for every domestic seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inviolability_attachment_locus, conceptual, 'Locus of the kernel disagreement: ruler-attachment versus population-attachment of inviolability.').

omega_variable(
    naturality_of_anarchic_order,
    'Is categorical inviolability a discovered requirement of any ordered international life under anarchy (remove it and universal war returns), or a constructed bargain whose beneficiaries are identifiable state elites?',
    'Comparative analysis of interstate systems lacking the norm (pre-1648 Europe, the Warring States period, interwar breach episodes) against periods of strict observance; test whether order tracks the norm or the underlying distribution of power.',
    'If the norm is load-bearing natural law, its costs are the price of order and the hybrid reading overstates harm; if constructed, the false-summit question activates — identifiable beneficiaries collecting under a naturality cover story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturality_of_anarchic_order, empirical, 'Natural law of anarchy versus elite-constructed bargain.').

omega_variable(
    enforcement_selectivity_asymmetry,
    'The norm binds weak states far more than great powers, who retain arbitrage-grade violation options — is the measured extraction a property of the norm itself or of selective enforcement layered atop it?',
    'Code intervention and sanction events by target-state power tier across the interval; compare violation rates and consequences across tiers.',
    'If selectivity dominates, the arrangement operates as coordination-for-the-strong and closure-for-the-weak simultaneously — per-seat classifications diverge maximally and aggregate metrics mislead; remedies shift from norm revision to enforcement equalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_selectivity_asymmetry, empirical, 'Whether extraction reflects the norm or its selective application.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__absolute_non_intervention, 1648, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ws_ani_tr_t1648, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 1648, 0.15).
narrative_ontology:measurement(ws_ani_tr_t1815, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 1815, 0.2).
narrative_ontology:measurement(ws_ani_tr_t1919, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 1919, 0.28).
narrative_ontology:measurement(ws_ani_tr_t1945, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 1945, 0.32).
narrative_ontology:measurement(ws_ani_tr_t1960, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 1960, 0.35).
narrative_ontology:measurement(ws_ani_tr_t1994, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 1994, 0.45).
narrative_ontology:measurement(ws_ani_tr_t2005, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 2005, 0.48).
narrative_ontology:measurement(ws_ani_tr_t2026, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 2026, 0.52).

% Extraction over time
narrative_ontology:measurement(ws_ani_be_t1648, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 1648, 0.34).
narrative_ontology:measurement(ws_ani_be_t1815, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 1815, 0.37).
narrative_ontology:measurement(ws_ani_be_t1919, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 1919, 0.41).
narrative_ontology:measurement(ws_ani_be_t1945, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 1945, 0.46).
narrative_ontology:measurement(ws_ani_be_t1960, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 1960, 0.52).
narrative_ontology:measurement(ws_ani_be_t1994, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 1994, 0.68).
narrative_ontology:measurement(ws_ani_be_t2005, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 2005, 0.6).
narrative_ontology:measurement(ws_ani_be_t2026, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 2026, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(ws_ani_su_t1648, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 1648, 0.4).
narrative_ontology:measurement(ws_ani_su_t1815, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 1815, 0.45).
narrative_ontology:measurement(ws_ani_su_t1919, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 1919, 0.3).
narrative_ontology:measurement(ws_ani_su_t1945, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 1945, 0.5).
narrative_ontology:measurement(ws_ani_su_t1960, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 1960, 0.62).
narrative_ontology:measurement(ws_ani_su_t1994, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 1994, 0.55).
narrative_ontology:measurement(ws_ani_su_t2005, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 2005, 0.5).
narrative_ontology:measurement(ws_ani_su_t2026, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 2026, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__absolute_non_intervention, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalia_sovereignty__absolute_non_intervention, conditional_responsibility).
narrative_ontology:affects_constraint(westphalia_sovereignty__absolute_non_intervention, graded_sovereignty).

% DUAL FORMULATION NOTE:
% The colloquial label 'Westphalian sovereignty' conflates three structurally distinct claims (epsilon-invariance decomposition): categorical inviolability regardless of internal conduct (this file), conduct-conditioned inviolability forfeited on protection failure (conditional_responsibility), and capacity-calibrated inviolability (graded_sovereignty). Each has a distinct victim set, distinct epsilon, and distinct enforcement implications, so each is authored as a separate story. The absolute reading is the historical baseline from which the conditional reading emerged as explicit revision (2005 summit) and against which the graded reading defines itself; this file links both siblings as required for constraint-family membership.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
