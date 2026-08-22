% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__endogenous_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_practice_standardization__endogenous_displacement_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: legitimacy_of_practice_standardization__endogenous_displacement_reading
 *   human_readable: Legitimacy of Endogenously Driven Practice Standardization
 *   domain: political_history/institutional_change
 *
 * SUMMARY:
 *   The constraint under examination is ONE READING of a contested kernel
 *   about the legitimacy of practice standardization (calendar reform, dress
 *   codes, metric systems, naming conventions). This story instantiates the
 *   ENDOGENOUS DISPLACEMENT READING: practice change is legitimate when it
 *   emerges from voluntary adoption driven by perceived utility or cultural
 *   evolution. The sibling readings — EXOGENOUS OVERRIDE (state decree for
 *   collective benefit) and DUAL PRACTICE EQUILIBRIUM (domain-partitioned
 *   legitimacy) — ground legitimacy differently and would classify the same
 *   empirical interval as different constraint types. The endogenous reading
 *   frames standardization as rope (genuine coordination that agents choose
 *   because they benefit from it); exogenous reading frames it as
 *   tangled_rope or snare (state extraction disguised as modernization);
 *   dual_practice frames it as two compatible constraint systems, each
 *   legitimate in its domain. This story does NOT adjudicate between the
 *   readings. It authors the endogenous reading as a structurally clean
 *   constraint with its own ε, beneficiaries, and legitimacy mechanics. The
 *   committer structure (which kernel, which reading, how the readings
 *   relate) is routed through omega variables and cs_structure fields, as
 *   required by the kernel-reading rules.
 *
 * KEY AGENTS:
 *   - Early adopters (moderate power, mobile exit): perceive utility in the new practice, gain social status and efficiency, drive network effects that make adoption self-reinforcing.
 *   - Utility beneficiaries (organized power, constrained exit): merchants, administrators, technicians who benefit from coordination and can credibly claim the new practice solves real problems.
 *   - Tradition guardians (moderate power, identity-locked exit): custodians of prior practice whose authority and identity rest on mastery of the old system; face delegitimization and identity loss.
 *   - Mass population (powerless, constrained exit): ordinary users who experience friction during transition but lack institutional stake in either system.
 *   - State authority (institutional power, arbitrage exit): registers, facilitates, and legitimizes voluntary shifts without initiating change (in this reading).
 *   - Rival authority systems (organized power, trapped exit): excluded from defining legitimacy; would argue the change is illegitimate by definition.
 *   - Analytical observer (analytical seat): examines whether adoption curves match voluntary-diffusion signature or manufactured-consent signature.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.42).
domain_priors:suppression_score(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.28).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__endogenous_displacement_reading, rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__endogenous_displacement_reading, "Legitimacy of Endogenously Driven Practice Standardization").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__endogenous_displacement_reading, "political_history/institutional_change").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__endogenous_displacement_reading, 'dcbf9de2-d3c4-42bb-a92b-1bef3e368df2').
narrative_ontology:cs_kernel_codification('dcbf9de2-d3c4-42bb-a92b-1bef3e368df2', distributed).
narrative_ontology:cs_authority_grounding('dcbf9de2-d3c4-42bb-a92b-1bef3e368df2', practice).
narrative_ontology:cs_interpretation_layer_present('dcbf9de2-d3c4-42bb-a92b-1bef3e368df2').
narrative_ontology:cs_reading_relation('dcbf9de2-d3c4-42bb-a92b-1bef3e368df2', legitimacy_of_practice_standardization__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('dcbf9de2-d3c4-42bb-a92b-1bef3e368df2', legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, coexists_with).
narrative_ontology:cs_axiom('dcbf9de2-d3c4-42bb-a92b-1bef3e368df2', foundational, voluntary_adoption_legitimates_change).
narrative_ontology:cs_axiom_status(voluntary_adoption_legitimates_change, holdable).
narrative_ontology:cs_axiom_grounding('dcbf9de2-d3c4-42bb-a92b-1bef3e368df2', voluntary_adoption_legitimates_change, conventional).
narrative_ontology:cs_axiom('dcbf9de2-d3c4-42bb-a92b-1bef3e368df2', foundational, utility_perception_drives_legitimate_standardization).
narrative_ontology:cs_axiom_status(utility_perception_drives_legitimate_standardization, holdable).
narrative_ontology:cs_axiom_grounding('dcbf9de2-d3c4-42bb-a92b-1bef3e368df2', utility_perception_drives_legitimate_standardization, empirically_contingent).
narrative_ontology:cs_reference_frame('dcbf9de2-d3c4-42bb-a92b-1bef3e368df2', community_practice_autonomy).
narrative_ontology:cs_drift_state('dcbf9de2-d3c4-42bb-a92b-1bef3e368df2', post_adoption_maturation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('dcbf9de2-d3c4-42bb-a92b-1bef3e368df2', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, early_adopters).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, utility_beneficiaries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, mass_population).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__endogenous_displacement_reading, tradition_guardians).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__endogenous_displacement_reading, mass_population).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__endogenous_displacement_reading, voluntary_adoption_legitimacy).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__endogenous_displacement_reading, cultural_evolution_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% First movers who perceive utility in the new practice (calendar reform, standardized dress, metric measurement). They gain efficiency, reduce friction with other adopters, and experience social status from being 'modern' or 'practical'. Their exit is to return to the old practice, which becomes progressively less attractive as the network effects of the new standard grow.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, early_adopters, beneficiary,
    moderate, biographical, mobile, regional).

% Organized groups (merchants, administrators, technicians, professionals) who benefit from standardized practice: reduced transaction costs, interoperability, simplified record-keeping. They can credibly claim the new practice solves real coordination problems they face. Their interests align with gradual but steady adoption.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, utility_beneficiaries, beneficiary,
    organized, generational, constrained, national).

% Custodians of prior practice whose authority and identity rest on mastery of the old system: religious authorities, master craftspeople, cultural elders. The new practice delegitimizes their accumulated knowledge and social position. Exit means surrendering identity-constitutive expertise; many sustain 'double life' (public standard, private ritual) to buffer the transition.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, tradition_guardians, payer,
    moderate, generational, identity_locked, regional).

% Ordinary people who use practice daily but lack institutional stake in either system. They experience friction from the transition period: learning new norms, maintaining two systems simultaneously, social pressure to conform. As adoption reaches critical mass, they benefit from the coordination gains but bear real switching costs along the way.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, mass_population, payer,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__endogenous_displacement_reading, mass_population, beneficiary).

% Governments that can accelerate or decelerate practice standardization through policy but in this reading do not decree it. Instead they register, facilitate, and legitimize voluntary shifts: they recognize the new calendar, validate the new standard, remove legal impediments to adoption. Their role is to lock in coordination once it becomes dominant, not to initiate change.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, state_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Religious or traditional authorities whose legitimacy rests on the old practice being immutable or divinely ordained. They are excluded from defining what counts as 'legitimate change' in this reading — the endogenous reading treats their claims to immutability as descriptive claims about their followers' preferences, not as binding constraints. Their exclusion is the core contestation with the exogenous reading.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, rival_authority_systems, excluded,
    organized, generational, trapped, regional).

% Historians, institutional analysts, and social scientists examining whether practice change in this case genuinely emerged from endogenous utility perception and cultural drift, or whether it was state-driven with manufactured consent. This seat examines the adoption curve, regional variation patterns, and whether resistance came from material friction or coordinated opposition.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies measurement, timekeeping, or social convention across previously fragmented regional/institutional domains so that exchange, administration, and communication operate on shared frames. The Gregorian calendar reduction of leap-year drift, metric standardization of weights/measures, and dress code standardization all solve the same fundamental problem: agents operating across regions face transaction costs when systems don't interlock.
% TRANSFER_FUNCTION: Transfers authority over practice legitimacy from traditional/ritual authorities (those who held mastery of the prior system) to utility-driven networks and state recognition. The old system's social-status and knowledge-gatekeeping value flow to the early adopters and utility beneficiaries; tradition guardians lose the exclusive claim to legitimate practice.
% ABSENT_VOICES: Rival authority systems (religious authorities claiming immutability, isolationist communities without incentive to coordinate across boundaries) are not consulted on whether change is legitimate — they would argue the change is illegitimate by definition because it violates their authority to define practice. In exogenous readings they get a seat; in endogenous reading their exclusion is the point.
% DISAPPEARANCE_RATIONALE: If the legitimacy constraint (that endogenous adoption makes practice change legitimate) vanished, the mechanism for standardization would depend entirely on state decree or external coercion. Historical cases show that when state authority tried to impose practice change WITHOUT perceived utility or gradual adoption (e.g., radical calendar disruption in French Revolution, forced dress codes without infrastructure), adoption collapsed once enforcement eased. The constraint's disappearance would require constant enforcement to maintain any standard.
% FOUNDING_PROBLEM: Coordination across heterogeneous regions and institutions with different prior practices created friction: merchants couldn't interoperate, administrators faced record incompatibilities, and communicating across regions required costly translation. Early modern states and trading networks faced growing pressure to synchronize.
% FOUNDING_PROBLEM_CORROBORATION: Merchants' records and administrative archives document rising costs of incompatibility. Trade historians and institutional economists outside the benefiting parties (early adopters) corroborate that coordination friction was real. Rival authority systems dispute the framing: they claim the 'friction' is exaggerated and the real problem is loss of control.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__endogenous_displacement_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__endogenous_displacement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__endogenous_displacement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__endogenous_displacement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_practice_standardization__endogenous_displacement_reading_tests).
:- end_tests(legitimacy_of_practice_standardization__endogenous_displacement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) because the constraint operates as genuine coordination (network effects, transaction cost reduction) but produces asymmetric gains: early adopters capture status and first-mover advantages; tradition guardians lose identity-constitutive expertise; mass population bears transition costs. Suppression is low (0.28) because the endogenous reading claims adoption is voluntary, resistance is temporary friction rather than organized coercion, and exit options remain open (though increasingly costly as adoption spreads). Theater is minimal (0.18) because the constraint's function (coordination) is real and persists — it is not theatrically maintained. Accessibility collapse is moderate-high (0.65) because once the standard reaches critical mass, alternatives collapse: you cannot conduct commerce on the old calendar if everyone else uses the new one. Resistance is substantial (0.52) because identity-locked populations (tradition guardians, insular communities) mount real resistance even as the new standard spreads — the constraint does not dissolve resistance, it marginalizes resisters. The measurement series tracks the interval from early adoption (t=0, low extractiveness, minimal suppression) through maturation (t=40, moderate extractiveness, low suppression). Extractiveness rises as the new standard becomes dominant and switching costs mount for laggards; then plateaus as the constraint settles. Theater stays low because the coordination function remains real. Suppression rises slightly during the transition period (people learning two systems, social friction) then plateaus at a low level because the endogenous reading requires enforcement to be minimal. One shared time grid so every metric is authored at every examined point (OQ-105 alignment rule).
 *
 * PERSPECTIVAL GAP:
 *   The early adopters and utility beneficiaries experience this as rope (genuine coordination, mutual benefit) and perceive the extraction as fair compensation for transition-bearing. Tradition guardians experience it as tangled_rope (coordination story covering loss of status and authority) and perceive the extraction as delegitimization imposed on them without their consent. The state experiences it as passive facilitation but the analytical observer can detect (via resistance measurements and adoption curves) whether the state was actually active-enforcing the new standard from the start. The engine computes per-seat classifications from the same structural data: early adopter seat computes lower d (beneficiary); tradition guardian seat computes higher d (target); state seat's d depends on whether the observer detects the state's actual role. This divergence is the core of what different readings MEAN — different seats' experience of legitimacy.
 *
 * DIRECTIONALITY LOGIC:
 *   Early adopters: low d (near beneficiary end) because they choose adoption before lock-in, experience utility gains, and exit remains voluntary. Utility beneficiaries: slightly higher d than adopters because their benefit comes from others' adoption (network effect) — they do not drive change but capture rents from it. Tradition guardians: high d (near target end) because exit is identity-locked (surrendering expertise and social role), the constraint extracts status and authority, and resistance fails to reverse adoption. Mass population: near-symmetric d (0.5) because they experience both gains (coordination benefits) and costs (transition friction) in rough balance, though the costs are front-loaded and concentrated on learning. State authority: low d if genuinely passive (registering without coercing); higher d if actually active-enforcing (state is then not a beneficiary but an agenda-setter extracting legitimacy from the coordination story). Analytical observer: d is purely analytical (sits outside the constraint). Directionality overrides are not needed; the structural derivation from beneficiary/victim declarations captures the relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The endogenous reading does NOT face mandatrophy. The founding problem (coordination friction across regions) is live and the constraint persists because it solves it. The constraint's legitimacy rests on the solving, not on state decree or tradition — so if the founding problem vanished (regions became disconnected again, trade collapsed), the constraint would lose force. The threat to the endogenous reading comes not from mandatrophy but from the exogenous reading's evidence: if state enforcement is actually active (and early), the founding problem is not the real driver. Mandatrophy would emerge if the sibling exogenous reading is correct — then the founding problem is a cover story for state extraction, and once the state achieves its real goal (control, revenue, international alignment), the constraint persists without solving the founding problem. The endogenous reading avoids mandatrophy by committing to the claim that adoption is voluntary and utility-driven; if that claim fails (resistance is too high, adoption curves show state direction), the reading is false, not mandatrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    endogenous_vs_manufactured_consent,
    'Did early adoption genuinely reflect perceived utility and cultural preference shift, or was it state-facilitated adoption with manufactured appearance of voluntariness?',
    'Examine adoption curves by region and social class: true endogenous adoption shows elite-first diffusion, regional variation correlated with trade/coordination incentives, and resistance concentrated in isolated communities; manufactured consent shows uniform rates, state-led rollout infrastructure, and resistance from organized traditional authorities.',
    'If manufactured, the constraint reclassifies from rope (voluntary coordination) toward tangled_rope or snare (coordination story covering state extraction); the legitimacy claim is false, and the other readings (exogenous/dual_practice) become more plausible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(endogenous_vs_manufactured_consent, empirical, 'Whether voluntary adoption or state direction drove standardization.').

omega_variable(
    kernel_reading_contest,
    'This constraint instantiates ONE reading (endogenous_displacement) of a contested kernel (legitimacy_of_practice_standardization). The sibling readings — exogenous_override and dual_practice_equilibrium — ground legitimacy differently. What determines which reading is the correct account of THIS case of practice change?',
    'The readings are FRAMINGS of the same empirical interval, not empirical claims. Endogenous reading wins if adoption curves and resistance patterns match the voluntary-diffusion signature; exogenous reading wins if state decree precedes (and drives) adoption; dual_practice reading wins if long-term coexistence of old and new systems shows no displacement. The contest is not resolvable by data alone — it depends on what the reading''s tradition COUNTS as legitimate authority (cultural preference vs. state decree vs. domain partition). Different seats will endorse different readings.',
    'The classification of the constraint depends on which reading is adopted: endogenous reads this as rope (legitimate coordination); exogenous reads it as tangled_rope or snare (extraction disguised as legitimacy); dual_practice reads it as two constraints, each a rope in its domain. The engine computes per-reading, but each story must author its own ε independently.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading''s theory of legitimacy is correct — the framing determines the type.').

omega_variable(
    transition_cost_distribution,
    'Who bears the real costs of the transition period (learning, maintaining two systems, losing status), and does the endogenous reading''s appeal to ''cultural evolution'' adequately account for those costs as legitimate?',
    'Measure resistance intensity by social group: if tradition_guardians and mass_population show sustained resistance even after coordination benefits accrue to them, the costs are higher than the reading''s ''voluntary adoption'' framing acknowledges. If identity-locked populations refuse even after economic incentive, the constraint''s extraction component is being suppressed (internalized) rather than dissolved.',
    'High sustained resistance and identity-locked refusal suggest the constraint operates more like tangled_rope (coordination story masking extraction from identity-locked populations) than rope. The reading''s legitimacy claim depends on minimizing or naturalizing these costs; resistance measurements expose the claim''s limits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transition_cost_distribution, empirical, 'Whether transition costs are distributed fairly or concentrated on identity-locked groups.').

omega_variable(
    authority_shift_mechanism,
    'Does the state actually remain passive (registering voluntary change) as the endogenous reading claims, or does it active-enforce the new standard by removing legal recognition of the old practice?',
    'Examine state actions: passive registration (accepting dual practice, no penalty for old system) vs. active enforcement (removing legal standing for old practice, imposing fines or exclusions). Passive action supports endogenous reading; active enforcement (especially early in the interval) supports exogenous reading.',
    'If state enforcement is active and early, the constraint reclassifies: the endogenous legitimacy claim is false, and exogenous reading becomes the accurate account. The ''voluntary adoption'' framing masks state decree.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_shift_mechanism, empirical, 'Whether state role is passive-registration or active-enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(legi_tr_t0, observed).
narrative_ontology:measurement(legi_tr_t5, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 5, 0.11).
narrative_ontology:measurement_basis(legi_tr_t5, observed).
narrative_ontology:measurement(legi_tr_t10, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 10, 0.13).
narrative_ontology:measurement_basis(legi_tr_t10, observed).
narrative_ontology:measurement(legi_tr_t15, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 15, 0.15).
narrative_ontology:measurement_basis(legi_tr_t15, observed).
narrative_ontology:measurement(legi_tr_t25, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 25, 0.17).
narrative_ontology:measurement_basis(legi_tr_t25, observed).
narrative_ontology:measurement(legi_tr_t35, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 35, 0.18).
narrative_ontology:measurement_basis(legi_tr_t35, observed).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement_basis(legi_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(legi_be_t0, observed).
narrative_ontology:measurement(legi_be_t5, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 5, 0.25).
narrative_ontology:measurement_basis(legi_be_t5, observed).
narrative_ontology:measurement(legi_be_t10, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement_basis(legi_be_t10, observed).
narrative_ontology:measurement(legi_be_t15, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 15, 0.37).
narrative_ontology:measurement_basis(legi_be_t15, observed).
narrative_ontology:measurement(legi_be_t25, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 25, 0.41).
narrative_ontology:measurement_basis(legi_be_t25, observed).
narrative_ontology:measurement(legi_be_t35, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 35, 0.42).
narrative_ontology:measurement_basis(legi_be_t35, observed).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 40, 0.42).
narrative_ontology:measurement_basis(legi_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement_basis(legi_su_t0, observed).
narrative_ontology:measurement(legi_su_t5, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 5, 0.18).
narrative_ontology:measurement_basis(legi_su_t5, observed).
narrative_ontology:measurement(legi_su_t10, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 10, 0.22).
narrative_ontology:measurement_basis(legi_su_t10, observed).
narrative_ontology:measurement(legi_su_t15, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 15, 0.26).
narrative_ontology:measurement_basis(legi_su_t15, observed).
narrative_ontology:measurement(legi_su_t25, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 25, 0.28).
narrative_ontology:measurement_basis(legi_su_t25, observed).
narrative_ontology:measurement(legi_su_t35, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 35, 0.28).
narrative_ontology:measurement_basis(legi_su_t35, observed).
narrative_ontology:measurement(legi_su_t40, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 40, 0.28).
narrative_ontology:measurement_basis(legi_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__endogenous_displacement_reading, resource_allocation).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization__exogenous_override_reading).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of a three-reading kernel family. All three readings address the same empirical phenomenon (practice standardization) but ground legitimacy differently. The endogenous reading treats state role as passive facilitation; exogenous reading treats state decree as primary; dual_practice reading treats coexistence as the stable outcome. These are not alternative measurements of the same constraint — they are different constraints over the same interval. The shared network links all three readings so corpus analysis can track how the readings' classifications diverge and what empirical features correlate with different reading endorsements.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
