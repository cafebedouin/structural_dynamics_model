% ============================================================================
% CONSTRAINT STORY: legitimacy_of_imposed_practice__hybrid_scaffolding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_imposed_practice__hybrid_scaffolding_reading, []).

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
 *   constraint_id: legitimacy_of_imposed_practice__hybrid_scaffolding_reading
 *   human_readable: Scaffolded Dress-Reform Mandate (Elite Modeling + Ideological Framing)
 *   domain: political_history/state_formation/cultural_imposition
 *
 * SUMMARY:
 *   This story instantiates the hybrid_scaffolding reading of the
 *   imposed-practice kernel, applied to a dress-reform mandate (distinguished
 *   explicitly from the calendar-reform case, which is the unscaffolded
 *   exogenous_override reading and fails). Here the state pairs legal decree
 *   with elite modeling, patronage, and sustained ideological messaging
 *   framing the new dress as the marker of modern national identity. Theater
 *   is high early (performative compliance, staged unveilings, showcase urban
 *   districts) and falls as genuine quasi-endogenous adoption takes hold
 *   among the scaffolded population — but this falling theater ratio masks a
 *   widening geographic split: urban elites internalize the marker while
 *   rural populations, never given scaffolding, remain formally noncompliant
 *   or adopt hybrid practices under threat of penalty rather than persuasion.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.58).
domain_priors:suppression_score(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.62).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, "Scaffolded Dress-Reform Mandate (Elite Modeling + Ideological Framing)").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, "political_history/state_formation/cultural_imposition").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__hybrid_scaffolding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, '1a6ddf8f-7305-4407-8494-4db4336afc04').
narrative_ontology:cs_kernel_codification('1a6ddf8f-7305-4407-8494-4db4336afc04', formalized).
narrative_ontology:cs_authority_grounding('1a6ddf8f-7305-4407-8494-4db4336afc04', extraction).
narrative_ontology:cs_interpretation_layer_present('1a6ddf8f-7305-4407-8494-4db4336afc04').
narrative_ontology:cs_reading_relation('1a6ddf8f-7305-4407-8494-4db4336afc04', legitimacy_of_imposed_practice__exogenous_override_reading, influences).
narrative_ontology:cs_reading_relation('1a6ddf8f-7305-4407-8494-4db4336afc04', legitimacy_of_imposed_practice__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_axiom('1a6ddf8f-7305-4407-8494-4db4336afc04', foundational, ideological_scaffolding_generates_quasi_endogenous_pull).
narrative_ontology:cs_axiom_status(ideological_scaffolding_generates_quasi_endogenous_pull, holdable).
narrative_ontology:cs_axiom_grounding('1a6ddf8f-7305-4407-8494-4db4336afc04', ideological_scaffolding_generates_quasi_endogenous_pull, empirically_contingent).
narrative_ontology:cs_axiom('1a6ddf8f-7305-4407-8494-4db4336afc04', secondary, pure_decree_without_scaffolding_fails).
narrative_ontology:cs_axiom_status(pure_decree_without_scaffolding_fails, holdable).
narrative_ontology:cs_axiom_grounding('1a6ddf8f-7305-4407-8494-4db4336afc04', pure_decree_without_scaffolding_fails, empirically_contingent).
narrative_ontology:cs_reference_frame('1a6ddf8f-7305-4407-8494-4db4336afc04', state_directed_modernization_with_elite_patronage_scaffolding).
narrative_ontology:cs_drift_state('1a6ddf8f-7305-4407-8494-4db4336afc04', post_scaffolding_maturation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1a6ddf8f-7305-4407-8494-4db4336afc04', '').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, urban_westernizing_elites).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, state_modernization_bureaucracy).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, rural_populations).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, traditionalist_clergy_and_notables).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, provincial_administrators).
narrative_ontology:constraint_vindicates(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, national_modernization_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bureaucrats, officers, and professionals in the capital and provincial centers who are given hats, suits, and civil-service posts contingent on visible dress reform. They gain access to state patronage, foreign-facing prestige, and career advancement by modeling the new dress publicly. Their exit options are wide: they can perform compliance selectively, adopt hybrid styles, or lean fully into the new marker to accelerate advancement.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, urban_westernizing_elites, beneficiary,
    powerful, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, urban_westernizing_elites, agenda_setter).

% Designs and enforces the dress mandate as a visible marker of national modernization, pairs the decree with schools, media, and ideological campaigns framing old dress as backward, and channels patronage toward compliant elites to manufacture the appearance of organic adoption. It controls the scaffolding infrastructure — training, subsidized clothing, urban distribution networks — and decides who receives it.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, state_modernization_bureaucracy, agenda_setter,
    institutional, generational, analytical, national).

% Villagers and provincial townspeople are nominally subject to the same decree but receive none of the scaffolding infrastructure: no subsidized garments, no local elite models, no ideological outreach adapted to rural life. They face legal penalties or social stigma for noncompliance while lacking the resources or access to comply in the sanctioned form, and are excluded from the patronage that makes compliance rewarding for urban elites.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, rural_populations, payer,
    powerless, biographical, trapped, regional).

% Religious and local authority figures whose status was tied to traditional dress and custom lose standing as the state reframes their markers as backward. They can resist rhetorically or through local noncompliance but face escalating legal and reputational pressure, and have no equivalent scaffolding infrastructure offered to them to ease a face-saving transition.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, traditionalist_clergy_and_notables, payer,
    moderate, generational, constrained, regional).

% Mid-level officials tasked with enforcing the mandate in areas without scaffolding resources. They must produce compliance reports with little of the patronage or ideological infrastructure available in the capital, often improvising local enforcement or quietly tolerating hybrid or partial compliance to avoid unrest they lack resources to manage.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, provincial_administrators, agenda_setter,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, provincial_administrators, payer).

% Compare this case to pure-decree failures (unscaffolded calendar reform) and slow endogenous-climb cases elsewhere, using differential success rates across scaffolded versus unscaffolded mandates within the same regime to isolate the scaffolding variable.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, historians_of_state_formation, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, legible national identity marker intended to signal modernization to domestic elites and foreign observers, coordinated through elite patronage networks so that compliance becomes self-reinforcing among those with access to the scaffolding.
% TRANSFER_FUNCTION: Moves prestige, career access, and state patronage toward urban elites who adopt the sanctioned dress, while moving legal exposure, social stigma, and loss of local status onto rural populations and traditional authorities who cannot access the same scaffolding.
% ABSENT_VOICES: Rural communities and provincial religious authorities have no representation in the ideological campaign design or patronage allocation; their objections surface only as noncompliance statistics or unrest reports funneled upward through provincial administrators who have their own incentive to underreport friction.
% DISAPPEARANCE_RATIONALE: Urban elites would likely retain the adopted markers as embedded status signals even absent enforcement (constraint largely internalized among them), while rural populations would experience little change since the mandate was never functionally extended to them — the world rearranges for the enforcement apparatus and legal exposure but not for lived practice at either extreme.
% FOUNDING_PROBLEM: The state needed a visible, rapid marker of national modernization to project legitimacy domestically and to foreign powers, and judged that waiting for organic cultural change was too slow given geopolitical pressure.
% FOUNDING_PROBLEM_CORROBORATION: State modernization bureaucracy and urban elite beneficiaries attest the problem is substantially solved — dress now functions as an internalized marker among the urban professional class. Independent historians and provincial administrators' own compliance reports (produced for enforcement purposes, not advocacy) corroborate that rural areas never achieved the claimed displacement, indicating the founding problem's 'solution' is geographically confined to the scaffolded population rather than nationally resolved as claimed.
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, contested).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_imposed_practice__hybrid_scaffolding_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_imposed_practice__hybrid_scaffolding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises over the interval (0.35 to 0.58) as the mandate's costs increasingly fall on unscaffolded rural populations and traditional authorities even as urban compliance becomes voluntary-seeming. Suppression falls (0.75 to 0.62) as ideological pull reduces the need for raw coercion among elites, but remains substantial because rural enforcement never transitions away from coercion — the aggregate suppression figure blends a genuinely internalized urban population with a still-coerced rural one. Theater ratio falls from 0.6 to 0.44 as the coordination function among elites becomes real rather than performed, while the scaffolding gap itself is never closed.
 *
 * DIRECTIONALITY LOGIC:
 *   Urban elites are near-full beneficiaries: patronage, prestige, and eventually internalized identity accrue to them with low ongoing cost, and their exit options (arbitrage — they can adopt selectively or fully) keep directionality low. Rural populations are near-full targets: legal exposure without patronage, no scaffolding, and trapped exit options push directionality high. The state bureaucracy sits as an analytical/institutional agenda-setter benefiting from the appearance of national modernization (a vindicated proposition, not a rent-collecting actor in itself) while administering unequal costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid-scaffolding reading resists two mislabelings at once: it does not let the visible urban success (real internalization, low theater by mid-interval) get read as proof the whole mandate is a genuine Rope — the founding_problem_corroboration shows the rural population's experience contradicts that reading. It also does not let the rural coercion (real suppression, trapped exit) collapse the whole story into pure Snare — the urban seat's structural data show authentic coordination-function uptake, not merely coerced compliance. Tangled Rope holds both halves without averaging them into a false middle.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    internalization_authenticity,
    'Is the urban elite adoption of the new dress marker genuine internalized identity change, or sophisticated performance sustained by ongoing patronage incentives that would collapse if patronage were withdrawn?',
    'Withdrawal natural experiment: track compliance persistence among urban elites in periods of state weakness or patronage interruption; persistence without patronage indicates genuine internalization, rapid reversion indicates performance.',
    'If reversion is rapid, the falling theater_ratio in this story is itself theater — the ''quasi-endogenous pull'' claimed by the hybrid-scaffolding reading would be overstated and the constraint would sit closer to exogenous_override with better PR.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalization_authenticity, empirical, 'Whether urban dress adoption is internalized or patronage-contingent performance.').

omega_variable(
    scaffolding_allocation_intentionality,
    'Was the exclusion of rural populations from scaffolding infrastructure a deliberate resource-allocation choice by the state (targeting the urban elite as the legitimacy-projecting audience) or an unintended byproduct of state capacity limits?',
    'Archival review of state planning documents and resource allocation debates at the time of mandate design; evidence of explicit urban-first prioritization versus evidence of failed attempts to extend scaffolding rurally.',
    'Deliberate exclusion strengthens the tangled_rope reading (asymmetric extraction by design); capacity-limited exclusion would push the rural-facing portion of this constraint closer to a scaffold reading with an unmet, rather than absent, sunset intention.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scaffolding_allocation_intentionality, conceptual, 'Whether rural exclusion from scaffolding was designed or incidental.').

omega_variable(
    cs_framing_kernel_vs_layer,
    'Should the CS framing treat the state modernization bureaucracy itself as the kernel-authority, or should it treat the deeper ideological narrative of national progress (which the bureaucracy merely administers) as the true kernel, with the bureaucracy as an interpretive layer beneath it?',
    'Compare the persistence of the dress norm across changes of bureaucratic personnel and regime: if the norm persists through bureaucratic turnover because the progress narrative remains intact, the narrative is the kernel; if the norm decays with the bureaucracy, the bureaucracy is the kernel.',
    'Treating the progress narrative as kernel would classify this as authority_grounding: extraction with the bureaucracy as an interpretation layer; treating the bureaucracy itself as kernel authority (as authored here) makes the ideological messaging a tool rather than the ground of legitimacy. The two framings assign different loci to where drift would register.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_kernel_vs_layer, conceptual, 'Alternative CS framings: bureaucracy-as-kernel versus progress-narrative-as-kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 0, 0.6).
narrative_ontology:measurement(legi_tr_t8, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 8, 0.52).
narrative_ontology:measurement(legi_tr_t16, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 16, 0.46).
narrative_ontology:measurement(legi_tr_t24, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 24, 0.44).
narrative_ontology:measurement(legi_tr_t32, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 32, 0.44).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 40, 0.44).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(legi_be_t8, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(legi_be_t16, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(legi_be_t24, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(legi_be_t32, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 32, 0.57).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(legi_su_t8, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 8, 0.7).
narrative_ontology:measurement(legi_su_t16, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 16, 0.65).
narrative_ontology:measurement(legi_su_t24, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 24, 0.63).
narrative_ontology:measurement(legi_su_t32, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 32, 0.62).
narrative_ontology:measurement(legi_su_t40, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.08).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, legitimacy_of_imposed_practice__exogenous_override_reading).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, legitimacy_of_imposed_practice__endogenous_climb_reading).

% DUAL FORMULATION NOTE:
% This story, the exogenous_override_reading (calendar mandate, unscaffolded, failed), and the endogenous_climb_reading (bottom-up internalization requirement) form a three-member reading family over the same kernel: legitimacy_of_imposed_practice. Each authors a distinct epsilon and beneficiary/victim structure rather than averaging across the contest — this reading's epsilon (0.58, rising) reflects a partial, geographically bifurcated success that neither sibling reading can represent: the exogenous reading would predict failure throughout, the endogenous reading would predict slow uniform climb, and neither predicts the urban/rural split this reading documents.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
