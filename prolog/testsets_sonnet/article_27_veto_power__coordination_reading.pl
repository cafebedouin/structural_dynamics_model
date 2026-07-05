% ============================================================================
% CONSTRAINT STORY: article_27_veto_power__coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_27_veto_power__coordination_reading, []).

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
 *   constraint_id: article_27_veto_power__coordination_reading
 *   human_readable: UN Charter Article 27(3) P5 Veto — Great-Power War Prevention Reading
 *   domain: international_relations/institutional_design/constitutional_law
 *
 * SUMMARY:
 *   This story instantiates the coordination reading of the Article 27(3)
 *   veto kernel: the veto as the specific mechanism that keeps the Security
 *   Council from ever becoming a legal instrument capable of compelling a
 *   nuclear-armed permanent member into a war it has not chosen to fight.
 *   Under this reading, the veto's function is measured against the
 *   counterfactual of majoritarian compulsion of a nuclear power, not against
 *   the counterfactual of an egalitarian Council. The coordination good is
 *   real and specific: avoided direct great-power confrontation triggered by
 *   Council authorization. Two sibling readings of the same kernel text
 *   (oligopoly_reading, sovereignty_reading) are NOT part of this constraint
 *   — they are separate stories with separate ε values, linked via
 *   network.affects_constraints, because the same clause supports
 *   structurally distinct claims about what is being coordinated versus
 *   extracted versus asserted as sovereign prerogative.
 *
 * KEY AGENTS:
 *   - p5_states: primary beneficiary and agenda-setter (institutional/arbitrage) — hold mutual forbearance guarantee
 *   - un_member_states: secondary beneficiary (moderate/constrained) — benefit from system stability without holding veto power themselves
 *   - global_civilian_population: diffuse ultimate beneficiary (powerless/trapped) — bears no formal role but is the party war-avoidance actually protects
 *   - security_council_secretariat: analytical observer (institutional/analytical) — records mechanism operation without power over it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__coordination_reading, 0.18).
domain_priors:suppression_score(article_27_veto_power__coordination_reading, 0.22).
domain_priors:theater_ratio(article_27_veto_power__coordination_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__coordination_reading, rope).
narrative_ontology:human_readable(article_27_veto_power__coordination_reading, "UN Charter Article 27(3) P5 Veto — Great-Power War Prevention Reading").
narrative_ontology:topic_domain(article_27_veto_power__coordination_reading, "international_relations/institutional_design/constitutional_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__coordination_reading, '29747ffc-3a4b-4fe5-80e0-b0d5b6bff6b4').
narrative_ontology:cs_kernel_codification('29747ffc-3a4b-4fe5-80e0-b0d5b6bff6b4', formalized).
narrative_ontology:cs_authority_grounding('29747ffc-3a4b-4fe5-80e0-b0d5b6bff6b4', lineage).
narrative_ontology:cs_interpretation_layer_present('29747ffc-3a4b-4fe5-80e0-b0d5b6bff6b4').
narrative_ontology:cs_reading_relation('29747ffc-3a4b-4fe5-80e0-b0d5b6bff6b4', article_27_veto_power__oligopoly_reading, coexists_with).
narrative_ontology:cs_reading_relation('29747ffc-3a4b-4fe5-80e0-b0d5b6bff6b4', article_27_veto_power__sovereignty_reading, influences).
narrative_ontology:cs_axiom('29747ffc-3a4b-4fe5-80e0-b0d5b6bff6b4', foundational, great_power_unanimity_necessary_for_stable_enforcement).
narrative_ontology:cs_axiom_status(great_power_unanimity_necessary_for_stable_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('29747ffc-3a4b-4fe5-80e0-b0d5b6bff6b4', great_power_unanimity_necessary_for_stable_enforcement, empirically_contingent).
narrative_ontology:cs_axiom('29747ffc-3a4b-4fe5-80e0-b0d5b6bff6b4', secondary, coercive_authorization_against_nuclear_state_risks_escalation_to_general_war).
narrative_ontology:cs_axiom_status(coercive_authorization_against_nuclear_state_risks_escalation_to_general_war, holdable).
narrative_ontology:cs_axiom_grounding('29747ffc-3a4b-4fe5-80e0-b0d5b6bff6b4', coercive_authorization_against_nuclear_state_risks_escalation_to_general_war, empirically_contingent).
narrative_ontology:cs_reference_frame('29747ffc-3a4b-4fe5-80e0-b0d5b6bff6b4', san_francisco_founding_settlement).
narrative_ontology:cs_drift_state('29747ffc-3a4b-4fe5-80e0-b0d5b6bff6b4', post_cold_war_unipolar_and_multipolar_transition, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('29747ffc-3a4b-4fe5-80e0-b0d5b6bff6b4', '').
narrative_ontology:cs_kernel_id(article_27_veto_power__coordination_reading, article_27_veto_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, un_member_states).
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, global_civilian_population).
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, p5_states).
narrative_ontology:constraint_vindicates(article_27_veto_power__coordination_reading, great_power_unanimity_prevents_bloc_war).
narrative_ontology:constraint_vindicates(article_27_veto_power__coordination_reading, security_council_legitimacy_requires_p5_consent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Each of the five permanent members can block any Security Council resolution that would authorize collective action against it or its core interests. This gives each a guarantee that the Council cannot be used as a legal instrument to assemble a coalition compelling it into a war it has not chosen. They also bear the reciprocal cost: each accepts that the other four hold the same guarantee against them, which caps what the Council can accomplish when great-power interests diverge.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, p5_states, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article_27_veto_power__coordination_reading, p5_states, agenda_setter).

% Non-permanent members and the wider UN membership operate inside a system whose central value proposition is that it does not collapse into a great-power war of the kind the League of Nations failed to prevent. They cannot themselves wield a veto, but they benefit from a system architecture in which no single resolution can drag two nuclear-armed permanent members into direct confrontation via legal compulsion. Their exit from the UN system entirely is theoretically available but not practically exercised — the constraint is accepted as the price of a functioning collective security forum at all.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, un_member_states, beneficiary,
    moderate, generational, constrained, global).

% Ordinary people worldwide have no direct standing in Security Council deliberation and no exit from the consequences of great-power war should one occur. They are the ultimate beneficiaries of any structural feature that raises the threshold before nuclear-armed states are drawn into direct military confrontation through an international legal mechanism. Their interest in this reading is entirely indirect and cannot be represented except through state intermediaries.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, global_civilian_population, beneficiary,
    powerless, civilizational, trapped, universal).

% Administers Council procedure, records vetoes, and reports on Council paralysis versus Council action. It observes the mechanism operate without power to override it, and can document when the veto functions as advertised (blocking action that would produce a great-power clash) versus when it blocks unrelated humanitarian or regional matters — a boundary this reading treats as outside its scope.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, security_council_secretariat, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents the Security Council from becoming a legal mechanism by which a coalition of states could authorize collective military action compelling a nuclear-armed permanent member into a confrontation it has not consented to enter — the central collective-action failure the veto is built to avert is bloc-versus-bloc escalation triggered by Council authorization.
% TRANSFER_FUNCTION: The arrangement does not principally move resources between parties; it withholds a capability (binding collective authorization against a P5 state) that would otherwise exist, in exchange for universal exemption from that same capability being used against any other P5 state. What moves is a mutual forbearance, not a rent.
% ABSENT_VOICES: States that are neither P5 nor able to secure non-permanent seats have no formal voice in whether the veto is exercised in a given case, but this reading holds their interest is served by the mechanism's existence in principle even where they cannot object to a specific exercise of it.
% DISAPPEARANCE_RATIONALE: If Article 27(3) vanished overnight, the Council could in principle authorize binding collective action against any state including a P5 member by simple majority. Whether this would produce more effective collective security or would instead produce exactly the great-power confrontation the veto was designed to prevent is the live empirical dispute this reading cannot settle by itself — hence 'contested' rather than a clean verdict.
% FOUNDING_PROBLEM: The League of Nations required unanimity for enforcement action and had no mechanism preventing great powers from simply leaving when constrained; both flaws contributed to its inability to prevent World War II. The 1945 founders sought a mechanism ensuring the great powers with the capacity to fight a new world war would remain inside the institution and never be legally compelled by it into a war they had not chosen.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the San Francisco Conference (outside any P5 government) and multiple non-P5 UN member state delegations have on record affirmed that great-power participation was conditioned explicitly on the veto guarantee, and that the absence of any P5 defection from the UN system since 1945 — unlike League defections in the 1930s — is offered as ongoing corroboration that the founding problem remains the operative logic, not merely a P5 talking point.
narrative_ontology:disappearance_verdict(article_27_veto_power__coordination_reading, contested).
narrative_ontology:founding_problem_status(article_27_veto_power__coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__coordination_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_27_veto_power__coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_27_veto_power__coordination_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_27_veto_power__coordination_reading_tests).
:- end_tests(article_27_veto_power__coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low (0.18) under this reading because the coordination reading holds that no party is a net payer of the veto's core function — every state, including each individual P5 member, benefits from the reciprocal guarantee that no bloc-authorized war can be legally imposed on any nuclear power without its consent. Suppression is moderate-low (0.22): the veto does suppress the alternative of majoritarian Council enforcement against a P5 state, but under this reading that suppression is the coordination mechanism itself, not an extraction device, since it runs symmetrically across all five permanent members rather than asymmetrically against a subset. Theater ratio is low and essentially flat (0.08 to 0.12 over 80 years) because under this reading the veto is not primarily performative — it is invoked and its blocking function is real and load-bearing, not decorative. Accessibility collapse (0.4) and resistance (0.35) sit at moderate levels: alternatives to unanimity-among-nuclear-powers were seriously debated at San Francisco and remain debated in reform proposals, so this is not mountain-grade natural necessity, but neither is it trivially contested — the mechanism has held without amendment for 80 years despite sustained reform pressure.
 *
 * DIRECTIONALITY LOGIC:
 *   Under this reading beneficiaries include the P5 states themselves (each protected from compulsion by the veto of the others in mirror image), the wider UN membership (who benefit from a Council that has not fractured into competing blocs at war), and the global civilian population (the ultimate beneficiary of avoided great-power war, though with no direct standing). There is no declared victim class in this reading: the coordination reading's central claim is that the cost the veto imposes — blocked Council action in specific cases — is the necessary price of the benefit it produces, paid diffusely by everyone who wanted a particular resolution passed, not concentrated on an identifiable target group extracted from by an identifiable extractor.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as Rope under this reading depends on the founding problem (preventing repeat of League-era great-power defection and forced-unanimity paralysis) remaining live, which the six_questions interview marks 'live' with corroboration from historians and non-P5 states independent of P5 self-interest. If the founding problem were dead — if, say, credible non-veto mechanisms for avoiding great-power military compulsion had emerged and matured — while the veto persisted regardless, this reading's classification would degrade toward piton or the oligopoly_reading's tangled_rope/snare framing would become the operative account. The coordination reading is explicitly the reading that holds this has not yet happened.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_oligopoly_framing_choice,
    'Is the veto''s near-8-decade persistence without amendment evidence that it is solving a genuine, still-live collective-action problem (coordination_reading), or evidence that the five beneficiaries of Charter immutability have successfully blocked the amendment mechanism (Article 108/109, which itself requires P5 ratification) that would let anyone test whether the problem is still live (oligopoly_reading)?',
    'There is no clean empirical test: any counterfactual reform attempt is itself blocked by the mechanism under study, so the persistence data is consistent with both readings by construction. Partial evidence could come from close comparative analysis of the small number of historical near-amendment episodes (e.g. 1960s non-permanent seat expansion) and whether P5 resistance in those episodes tracked genuine war-avoidance concerns or naked authority preservation.',
    'If the oligopoly reading is closer to true, this coordination_reading''s classification of the identical clause as low-extraction Rope is a false summit at the level of the natural-language ''the veto'' label — but per the ε-invariance principle this does not contaminate this story''s own ε, because this story measures a structurally distinct claim (the war-prevention function) from the oligopoly reading''s claim (the rent-extraction function). Both can be simultaneously true of overlapping real-world conduct.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_oligopoly_framing_choice, conceptual, 'Whether persistence-without-amendment is evidence for genuine coordination necessity or for successful entrenchment — the two readings cannot be disentangled by the amendment record alone.').

omega_variable(
    counterfactual_war_avoidance_causal_claim,
    'Has the veto actually prevented a great-power war that would otherwise have occurred via Council-authorized collective action, or has great-power war been avoided since 1945 for entirely independent reasons (nuclear deterrence, economic interdependence, bipolar/unipolar stability) such that the veto''s war-prevention function is a post-hoc justification rather than a load-bearing mechanism?',
    'Historical case analysis of Cold War and post-Cold War crises where Council authorization against a P5 state''s interests was seriously contemplated (Korea 1950 Soviet absence, Suez 1956, Kosovo 1999, Syria 2011-onward) and whether the veto''s presence or absence in each counterfactual plausibly changed the war-or-no-war outcome.',
    'If nuclear deterrence and bipolarity/unipolarity independently explain great-power war avoidance, the veto''s contribution to the coordination good this reading credits it with may be marginal — moving this story''s ε upward toward the oligopoly reading''s territory as the coordination story loses independent explanatory weight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_war_avoidance_causal_claim, empirical, 'Whether the veto is causally load-bearing for great-power war avoidance or a redundant/post-hoc mechanism given other deterrence structures.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__coordination_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1945, article_27_veto_power__coordination_reading, theater_ratio, 1945, 0.08).
narrative_ontology:measurement(arti_tr_t1961, article_27_veto_power__coordination_reading, theater_ratio, 1961, 0.09).
narrative_ontology:measurement(arti_tr_t1977, article_27_veto_power__coordination_reading, theater_ratio, 1977, 0.1).
narrative_ontology:measurement(arti_tr_t1993, article_27_veto_power__coordination_reading, theater_ratio, 1993, 0.11).
narrative_ontology:measurement(arti_tr_t2009, article_27_veto_power__coordination_reading, theater_ratio, 2009, 0.12).
narrative_ontology:measurement(arti_tr_t2025, article_27_veto_power__coordination_reading, theater_ratio, 2025, 0.12).

% Extraction over time
narrative_ontology:measurement(arti_be_t1945, article_27_veto_power__coordination_reading, base_extractiveness, 1945, 0.15).
narrative_ontology:measurement(arti_be_t1961, article_27_veto_power__coordination_reading, base_extractiveness, 1961, 0.16).
narrative_ontology:measurement(arti_be_t1977, article_27_veto_power__coordination_reading, base_extractiveness, 1977, 0.17).
narrative_ontology:measurement(arti_be_t1993, article_27_veto_power__coordination_reading, base_extractiveness, 1993, 0.17).
narrative_ontology:measurement(arti_be_t2009, article_27_veto_power__coordination_reading, base_extractiveness, 2009, 0.18).
narrative_ontology:measurement(arti_be_t2025, article_27_veto_power__coordination_reading, base_extractiveness, 2025, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(article_27_veto_power__coordination_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_27_veto_power__coordination_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_27_veto_power__coordination_reading, 0.12).
narrative_ontology:affects_constraint(article_27_veto_power__coordination_reading, article_27_veto_power__oligopoly_reading).
narrative_ontology:affects_constraint(article_27_veto_power__coordination_reading, article_27_veto_power__sovereignty_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the article_27_veto_power kernel (UN Charter Article 27(3)). coordination_reading (this story) treats the veto as a war-avoidance mechanism with low extraction and no victim class. oligopoly_reading treats the identical clause as entrenched rent-extraction via Charter immutability, with beneficiaries (P5) and victims (states excluded from permanent power, reform-seeking coalitions) and higher ε. sovereignty_reading treats it as a Westphalian consent principle, a distinct normative claim again with its own ε. Per the ε-invariance principle, these are three separate constraints sharing one natural-language label, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
