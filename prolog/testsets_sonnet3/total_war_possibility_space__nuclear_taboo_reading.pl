% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__nuclear_taboo_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__nuclear_taboo_reading, []).

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
 *   constraint_id: total_war_possibility_space__nuclear_taboo_reading
 *   human_readable: The Nuclear Taboo: Constructed Normative Prohibition on Total War
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   Constructivist IR scholarship (most prominently Nina Tannenwald) argues
 *   that the seventy-plus-year absence of nuclear weapon use in conflict
 *   cannot be explained by material deterrence logic alone, since deterrence
 *   theory struggles to account for restraint in cases of profound arsenal
 *   asymmetry (e.g., the US in Korea and Vietnam, where using nuclear weapons
 *   carried no risk of nuclear retaliation). The taboo reading holds that a
 *   normative prohibition — built through survivor testimony, activist
 *   campaigning, diplomatic ritual, and treaty law — became an independent
 *   causal force constraining state behavior, generating its own
 *   institutional infrastructure (the NPT regime, no-first-use pledges,
 *   nuclear-weapon-free zones) that must be actively maintained rather than
 *   following automatically from capability calculations.
 *
 * KEY AGENTS:
 *   - existing_nuclear_weapon_states: institutional beneficiary and co-administrator of the taboo's enforcement machinery
 *   - norm_entrepreneur_ngos: organized agenda-setters who constructed and sustain the taboo's normative vocabulary
 *   - non_proliferation_treaty_secretariat: institutional administrator whose relevance depends on continued taboo maintenance
 *   - threshold_states_denied_arsenals: moderate-power payers bearing sanctions and isolation for crossing the normative line
 *   - deterrence_theorists_and_realist_strategists: excluded analytical voices offering a rival materialist causal account
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__nuclear_taboo_reading, 0.42).
domain_priors:suppression_score(total_war_possibility_space__nuclear_taboo_reading, 0.58).
domain_priors:theater_ratio(total_war_possibility_space__nuclear_taboo_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__nuclear_taboo_reading, tangled_rope).
narrative_ontology:human_readable(total_war_possibility_space__nuclear_taboo_reading, "The Nuclear Taboo: Constructed Normative Prohibition on Total War").
narrative_ontology:topic_domain(total_war_possibility_space__nuclear_taboo_reading, "international_relations/strategic_studies").

domain_priors:requires_active_enforcement(total_war_possibility_space__nuclear_taboo_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__nuclear_taboo_reading, '4b29520d-15dc-4604-b411-2e0ae4d45ee7').
narrative_ontology:cs_kernel_codification('4b29520d-15dc-4604-b411-2e0ae4d45ee7', distributed).
narrative_ontology:cs_authority_grounding('4b29520d-15dc-4604-b411-2e0ae4d45ee7', distributed).
narrative_ontology:cs_reading_relation('4b29520d-15dc-4604-b411-2e0ae4d45ee7', total_war_possibility_space__deterrence_equilibrium_reading, coexists_with).
narrative_ontology:cs_reading_relation('4b29520d-15dc-4604-b411-2e0ae4d45ee7', total_war_possibility_space__space_contraction_reading, coexists_with).
narrative_ontology:cs_axiom('4b29520d-15dc-4604-b411-2e0ae4d45ee7', foundational, normative_prohibition_has_independent_causal_force).
narrative_ontology:cs_axiom_status(normative_prohibition_has_independent_causal_force, holdable).
narrative_ontology:cs_axiom_grounding('4b29520d-15dc-4604-b411-2e0ae4d45ee7', normative_prohibition_has_independent_causal_force, empirically_contingent).
narrative_ontology:cs_axiom('4b29520d-15dc-4604-b411-2e0ae4d45ee7', secondary, taboo_persistence_requires_active_norm_entrepreneurship).
narrative_ontology:cs_axiom_status(taboo_persistence_requires_active_norm_entrepreneurship, holdable).
narrative_ontology:cs_axiom_grounding('4b29520d-15dc-4604-b411-2e0ae4d45ee7', taboo_persistence_requires_active_norm_entrepreneurship, empirically_contingent).
narrative_ontology:cs_reference_frame('4b29520d-15dc-4604-b411-2e0ae4d45ee7', post_hiroshima_normative_vacuum).
narrative_ontology:cs_drift_state('4b29520d-15dc-4604-b411-2e0ae4d45ee7', post_cold_war_proliferation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4b29520d-15dc-4604-b411-2e0ae4d45ee7', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__nuclear_taboo_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, existing_nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, norm_entrepreneur_ngos).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, non_proliferation_treaty_secretariat).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, threshold_states_denied_arsenals).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_states_under_extended_deterrence).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_states_under_extended_deterrence).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, no_first_use_signatory_states).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, no_first_use_signatory_states).
narrative_ontology:constraint_vindicates(total_war_possibility_space__nuclear_taboo_reading, social_construction_of_security_norms).
narrative_ontology:constraint_vindicates(total_war_possibility_space__nuclear_taboo_reading, taboo_independent_of_material_capability_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold arsenals acquired before the taboo hardened, and now benefit from a normative order that discourages new entrants while their own possession is treated as a grandfathered fact. They administer the non-proliferation regime, sit on the NPT's recognized nuclear-weapon-state list, and can invoke or relax the taboo's enforcement machinery (sanctions, diplomatic isolation) largely at their own discretion.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, existing_nuclear_weapon_states, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__nuclear_taboo_reading, existing_nuclear_weapon_states, agenda_setter).

% Campaign organizations, survivor networks (hibakusha), and disarmament advocacy bodies that built and sustain the taboo's normative vocabulary through public shaming, treaty advocacy, and moral framing. They gain reputational and institutional standing from the taboo's persistence and can shift attention or funding elsewhere if the norm loses salience.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, norm_entrepreneur_ngos, agenda_setter,
    organized, generational, mobile, global).

% Administers verification, safeguards, and review conferences that operationalize the taboo into treaty law. Its institutional relevance depends on the taboo continuing to require active maintenance; it enforces compliance reviews and inspection regimes against signatories.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, non_proliferation_treaty_secretariat, beneficiary,
    institutional, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__nuclear_taboo_reading, non_proliferation_treaty_secretariat, agenda_setter).

% States with the technical capacity to build weapons but who face sanctions, export controls, and diplomatic isolation if they cross the threshold. They bear the taboo's enforcement costs directly — lost trade access, frozen assets, military strikes on facilities — while militarily weaker non-signatory nuclear states outside the regime face milder consequences.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, threshold_states_denied_arsenals, payer,
    moderate, biographical, trapped, national).

% Rely on a nuclear patron's extended deterrence umbrella rather than pursuing their own arsenal, accepting reduced strategic autonomy and alliance dependency as the price of shelter under the taboo's enforcement structure. They benefit from war avoidance but pay in diminished sovereignty over their own security posture and no voice in the patron's doctrine.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_states_under_extended_deterrence, payer,
    powerless, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_states_under_extended_deterrence, beneficiary).

% Bind their own future options by pledging no-first-use, accepting a reputational and strategic constraint on their own arsenal in exchange for reinforcing the taboo's credibility. Reversing the pledge is possible but costs international standing built over decades.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, no_first_use_signatory_states, payer,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__nuclear_taboo_reading, no_first_use_signatory_states, beneficiary).

% Argue the taboo is epiphenomenal to material deterrence calculations, not an independent normative force, and that framing it as constructed taboo obscures the real work done by mutual assured destruction. Their competing causal account is marginalized in the norm-entrepreneur-dominated discourse that authored the taboo's institutional apparatus.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, deterrence_theorists_and_realist_strategists, excluded,
    organized, generational, analytical, global).

% Study the taboo's emergence (Hiroshima/Nagasaki memory, Cold War near-misses, activist campaigns) as a case of norm construction and diffusion, tracing how a normative prohibition became institutionally self-sustaining independent of the underlying strategic logic.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, historical_institutionalist_analysts, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, non-material Schelling point against total war: rather than relying solely on each state's independent calculation of mutual destruction, the taboo supplies a normative floor that coordinates restraint even where material deterrence logic alone would leave ambiguous cases (asymmetric arsenals, non-nuclear aggressors, crisis miscalculation).
% TRANSFER_FUNCTION: Moves strategic autonomy and arsenal-acquisition options away from threshold and aspiring states toward the existing nuclear weapon states, whose possession is grandfathered and normalized while new acquisition is treated as taboo violation; moves reputational capital and institutional standing toward norm-entrepreneur organizations and treaty secretariats who administer and police the boundary.
% ABSENT_VOICES: Deterrence theorists and realist strategists who attribute war-avoidance to material capability rather than constructed norms are structurally marginalized in a discourse built and staffed by norm entrepreneurs; threshold states subject to sanctions rarely get to define the taboo's terms, only to be judged against them.
% DISAPPEARANCE_RATIONALE: Norm entrepreneurs and treaty administrators would say the world rearranges catastrophically — restraint depended on the taboo's moral force and its collapse invites proliferation and use. Deterrence theorists would say the world stays largely unchanged, since material MAD logic was doing the actual restraining work all along and the taboo was epiphenomenal narrative layered on top of it. The kernel-level dispute is precisely which reading is correct; this reading takes the taboo as doing independent causal work.
% FOUNDING_PROBLEM: After Hiroshima and Nagasaki, and intensifying through Cold War near-misses (Cuban Missile Crisis, false-alarm incidents), the problem was that pure material deterrence logic was seen by activists and some strategists as too brittle and too silent on cases outside symmetric superpower confrontation — crisis miscalculation, non-nuclear aggressors, accidental escalation — leaving no normative backstop against use.
% FOUNDING_PROBLEM_CORROBORATION: Norm entrepreneur organizations and much of the constructivist IR literature (Tannenwald's taboo scholarship) attest the founding problem remains live and the taboo continues necessary independent work. Realist and deterrence-school strategists, writing from outside the norm-entrepreneur community, dispute that the problem was ever solved by normative construction rather than material capability, and some argue the taboo's apparent success is unfalsifiable absent a counterfactual test.
narrative_ontology:disappearance_verdict(total_war_possibility_space__nuclear_taboo_reading, contested).
narrative_ontology:founding_problem_status(total_war_possibility_space__nuclear_taboo_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__nuclear_taboo_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(total_war_possibility_space__nuclear_taboo_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__nuclear_taboo_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__nuclear_taboo_reading_tests).
:- end_tests(total_war_possibility_space__nuclear_taboo_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at 2025) rather than low or high because the taboo's coordination function is genuine — it plausibly does independent restraining work in asymmetric cases where pure deterrence logic is silent — but its enforcement machinery has hardened into a structure that asymmetrically burdens threshold states relative to grandfathered nuclear powers, which is the extractive residue. Suppression is higher (0.58) and has risen steadily because the taboo's institutional apparatus (sanctions regimes, export control cartels, verification demands) has intensified over the interval, consistent with a norm that requires increasing active maintenance to hold rather than one that has become self-enforcing. Theater ratio (0.31) reflects that a meaningful share of enforcement activity — review conferences, symbolic pledges — has become ritualized performance layered on top of substantive verification.
 *
 * PERSPECTIVAL GAP:
 *   From the nuclear weapon states' seat, the taboo looks like a rope they help sustain — a genuine coordination good they voluntarily reinforce through no-first-use pledges. From the threshold states' seat, the same structure looks like a tangled rope or worse — real war-avoidance benefit bundled with an enforcement apparatus that locks in an unequal arsenal hierarchy under moral cover. The engine's per-seat computation should register this divergence rather than resolve it.
 *
 * DIRECTIONALITY LOGIC:
 *   Existing nuclear weapon states and the NPT secretariat sit near the beneficiary end: they administer the taboo's enforcement and their own possession is normalized rather than penalized. Threshold states and non-nuclear states under extended deterrence sit nearer the target end: they bear the taboo's costs (sanctions, lost strategic autonomy) without comparable voice in setting its terms. Norm entrepreneur NGOs are beneficiaries in institutional-standing terms but hold mobile exit — they can shift causes if the taboo's salience fades, which differentiates them from the treaty secretariat's constrained exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (need for a normative backstop against use in asymmetric or crisis-miscalculation scenarios) is contested as still-live: no confirmed use since 1945 is consistent both with continued taboo function and with the taboo becoming institutionally self-perpetuating largely independent of any residual restraining function, since a genuine natural experiment (removing the taboo while holding material capability constant) has never occurred and cannot ethically occur. This is the central mandatrophy question for this reading: has the norm's protective function persisted, or has an initially functional coordination device calcified into an enforcement hierarchy whose primary current effect is maintaining nuclear apartheid between recognized and threshold states?
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    taboo_independent_causal_force_or_epiphenomenal,
    'Does the normative taboo do independent causal work restraining total war, or is observed restraint fully explained by material deterrence and the taboo is a post-hoc normative narrative layered onto materially-determined outcomes?',
    'Comparative case analysis of asymmetric-capability conflicts (US in Korea/Vietnam, Israel''s undeclared arsenal in regional conflicts, Russia''s rhetoric in Ukraine) where material deterrence logic alone would not predict restraint; convergent process-tracing of decision-maker testimony citing normative versus material reasoning.',
    'If the taboo is confirmed as an independent causal force, the tangled_rope classification (genuine coordination bundled with asymmetric enforcement costs) holds. If fully epiphenomenal to material deterrence, the entire enforcement apparatus reduces to pure extraction dressed in normative language, pushing the reading toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taboo_independent_causal_force_or_epiphenomenal, empirical, 'Core kernel-contest question: whether the taboo is a real mechanism or a narrative gloss on material deterrence.').

omega_variable(
    committer_structure_kernel_reading_divergence,
    'This constraint is one of three readings of the total_war_possibility_space kernel (nuclear_taboo_reading here; siblings deterrence_equilibrium_reading and space_contraction_reading are separate constraints). What would adopting a sibling reading change structurally?',
    'Compare the three readings'' predicted failure modes against actual historical stress-tests: taboo_reading predicts weakening if norm entrepreneurs exit or moral framing loses salience; deterrence_equilibrium_reading predicts weakening only if mutual vulnerability is broken (e.g., effective missile defense); space_contraction_reading predicts weakening only if the strategic imagination itself shifts (e.g., normalization of limited nuclear use doctrine). Track which predictor best explains observed post-Cold War arsenal expansions and doctrine changes (Russia''s 2020 nuclear doctrine revisions, North Korea''s arsenal development).',
    'If the deterrence_equilibrium_reading''s predictor tracks reality better, this taboo-reading constraint''s claimed_type and beneficiary structure should be understood as narrative superstructure on a materially-determined base, substantially lowering confidence in the independent extraction attributed to norm-entrepreneur institutions here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading_divergence, conceptual, 'Documents the committer/kernel structure: which reading is adopted determines the causal mechanism, the beneficiary set, and the enforcement architecture attributed to nuclear restraint.').

omega_variable(
    threshold_state_sanctions_proportionality,
    'Is the sanctions and enforcement burden placed on threshold states proportionate to the marginal risk their acquisition would add, or disproportionate relative to the risk tolerated from existing arsenal-holders?',
    'Comparative risk-modeling of threshold-state versus recognized-nuclear-state arsenal characteristics (command-and-control maturity, doctrine transparency, regional stability) against the sanctions intensity each faces.',
    'If disproportionate, strengthens the tangled_rope/asymmetric-extraction reading of the enforcement apparatus; if proportionate to genuinely higher marginal risk (e.g., weaker C2 infrastructure), weakens the extraction claim and supports a more genuine-coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_state_sanctions_proportionality, empirical, 'Whether enforcement asymmetry reflects genuine risk differential or entrenched hierarchy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__nuclear_taboo_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(tota_tr_t1962, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1962, 0.14).
narrative_ontology:measurement(tota_tr_t1970, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1970, 0.18).
narrative_ontology:measurement(tota_tr_t1991, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1991, 0.22).
narrative_ontology:measurement(tota_tr_t2003, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 2003, 0.26).
narrative_ontology:measurement(tota_tr_t2015, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 2015, 0.29).
narrative_ontology:measurement(tota_tr_t2025, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 2025, 0.31).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1945, 0.15).
narrative_ontology:measurement(tota_be_t1962, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1962, 0.22).
narrative_ontology:measurement(tota_be_t1970, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1970, 0.3).
narrative_ontology:measurement(tota_be_t1991, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1991, 0.34).
narrative_ontology:measurement(tota_be_t2003, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 2003, 0.38).
narrative_ontology:measurement(tota_be_t2015, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 2015, 0.4).
narrative_ontology:measurement(tota_be_t2025, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1945, 0.2).
narrative_ontology:measurement(tota_su_t1962, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1962, 0.3).
narrative_ontology:measurement(tota_su_t1970, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1970, 0.4).
narrative_ontology:measurement(tota_su_t1991, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1991, 0.48).
narrative_ontology:measurement(tota_su_t2003, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 2003, 0.53).
narrative_ontology:measurement(tota_su_t2015, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 2015, 0.56).
narrative_ontology:measurement(tota_su_t2025, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 2025, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__nuclear_taboo_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_possibility_space__nuclear_taboo_reading, 0.12).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, deterrence_equilibrium_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, space_contraction_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, non_proliferation_treaty_regime).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, no_first_use_pledge_mechanism).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language claim 'the nuclear taboo prevents total war' (kernel: total_war_possibility_space). deterrence_equilibrium_reading holds that mutual vulnerability, not normative construction, does the restraining work — a materialist account with a different beneficiary structure (arsenal-symmetric states benefit; norm-entrepreneurs have no independent causal role). space_contraction_reading holds that nuclear weapons removed total war from the strategically thinkable altogether, a cognitive/epistemic account distinct from both the material-equilibrium and normative-taboo mechanisms. Each reading carries its own ε, beneficiary/victim structure, and predicted failure mode; they are linked here rather than merged because measuring 'the nuclear taboo' by different observables (state rhetoric vs. arsenal deployment patterns vs. crisis-decision process tracing) yields different ε values, which per the ε-invariance principle means they are different constraints, not one constraint viewed three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
