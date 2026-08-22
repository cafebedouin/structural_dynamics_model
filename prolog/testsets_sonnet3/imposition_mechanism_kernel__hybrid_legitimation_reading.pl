% ============================================================================
% CONSTRAINT STORY: imposition_mechanism_kernel__hybrid_legitimation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_mechanism_kernel__hybrid_legitimation_reading, []).

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
 *   constraint_id: imposition_mechanism_kernel__hybrid_legitimation_reading
 *   human_readable: Hybrid Legitimation Reading of Imperial Norm Imposition (Symbolic Authority + Institutional Incentive)
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This story instantiates the hybrid_legitimation_reading of the
 *   imposition_mechanism_kernel: a new social or administrative norm spreads
 *   through the empire not because the masses demanded it (the
 *   endogenous_climb_reading) and not because the state imposed it by naked
 *   coercion (the exogenous_override_reading), but because the emperor's own
 *   visible adoption of the norm functioned as symbolic capital that the
 *   court and provincial administration then wired into institutional
 *   incentives — office, honorifics, marriage alliance eligibility. Adoption
 *   is stratified: aristocrats and administrators near the
 *   symbolic-and-institutional reward structure convert quickly; peasants and
 *   peripheral clans, who cannot access the institutional reward channel and
 *   only receive the compliance burden, adopt slowly and under a rising, if
 *   moderate, enforcement gradient. Extraction and suppression start low
 *   (charismatic diffusion phase) and rise moderately as the state converts
 *   informal prestige-following into administrative expectation with real
 *   sanctions for holdouts — but the mechanism never reaches the coercive
 *   intensity the exogenous_override_reading would describe.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.52).
domain_priors:suppression_score(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.44).
domain_priors:theater_ratio(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_mechanism_kernel__hybrid_legitimation_reading, tangled_rope).
narrative_ontology:human_readable(imposition_mechanism_kernel__hybrid_legitimation_reading, "Hybrid Legitimation Reading of Imperial Norm Imposition (Symbolic Authority + Institutional Incentive)").
narrative_ontology:topic_domain(imposition_mechanism_kernel__hybrid_legitimation_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(imposition_mechanism_kernel__hybrid_legitimation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_mechanism_kernel__hybrid_legitimation_reading, '62dad60d-77ca-40dc-a388-76bb065e2cd4').
narrative_ontology:cs_kernel_codification('62dad60d-77ca-40dc-a388-76bb065e2cd4', distributed).
narrative_ontology:cs_authority_grounding('62dad60d-77ca-40dc-a388-76bb065e2cd4', lineage).
narrative_ontology:cs_interpretation_layer_present('62dad60d-77ca-40dc-a388-76bb065e2cd4').
narrative_ontology:cs_reading_relation('62dad60d-77ca-40dc-a388-76bb065e2cd4', imposition_mechanism_kernel__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('62dad60d-77ca-40dc-a388-76bb065e2cd4', imposition_mechanism_kernel__exogenous_override_reading, influences).
narrative_ontology:cs_axiom('62dad60d-77ca-40dc-a388-76bb065e2cd4', foundational, legitimacy_transfers_through_exemplary_practice).
narrative_ontology:cs_axiom_status(legitimacy_transfers_through_exemplary_practice, holdable).
narrative_ontology:cs_axiom_grounding('62dad60d-77ca-40dc-a388-76bb065e2cd4', legitimacy_transfers_through_exemplary_practice, conventional).
narrative_ontology:cs_axiom('62dad60d-77ca-40dc-a388-76bb065e2cd4', foundational, institutional_incentive_is_necessary_complement_to_symbolic_authority).
narrative_ontology:cs_axiom_status(institutional_incentive_is_necessary_complement_to_symbolic_authority, holdable).
narrative_ontology:cs_axiom_grounding('62dad60d-77ca-40dc-a388-76bb065e2cd4', institutional_incentive_is_necessary_complement_to_symbolic_authority, instrumental).
narrative_ontology:cs_reference_frame('62dad60d-77ca-40dc-a388-76bb065e2cd4', charismatic_exemplar_authority).
narrative_ontology:cs_drift_state('62dad60d-77ca-40dc-a388-76bb065e2cd4', post_administrative_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('62dad60d-77ca-40dc-a388-76bb065e2cd4', '').
narrative_ontology:cs_kernel_id(imposition_mechanism_kernel__hybrid_legitimation_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_court).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__hybrid_legitimation_reading, aristocratic_early_adopters).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__hybrid_legitimation_reading, provincial_administrators).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, peasant_communities).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, peripheral_clans).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, non_conforming_local_elites).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and personifies the new norm through the emperor's own visible practice — dress, ritual, marriage form, or administrative conduct. Pairs the symbolic example with concrete institutional levers (appointments, tax exemptions, honorific titles) that reward conformity. Does not enforce by direct coercion at the mass level; enforces by controlling who advances within the state apparatus.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_court, agenda_setter,
    institutional, generational, analytical, national).

% Adopt the emperor's example quickly because proximity to the court makes both the symbolic capital and the institutional rewards immediately available. Convert early conformity into office, marriage alliances, and status ranking above slower-adopting peers. Retain the option of never fully complying without catastrophic cost, but forfeit the accelerated advancement if they don't.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, aristocratic_early_adopters, beneficiary,
    powerful, biographical, mobile, national).

% Transmit the norm downward into the provinces, translating imperial example into local administrative practice and reporting compliance upward. Gain career advancement and local authority from being effective conduits; their standing depends on visible enthusiasm for the new norm regardless of private conviction.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, provincial_administrators, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(imposition_mechanism_kernel__hybrid_legitimation_reading, provincial_administrators, agenda_setter).

% Encounter the norm last, mediated through provincial administrators, often as a set of new obligations (dress, dues, ritual observance) with none of the institutional rewards available to elites. Adoption is slower and more coerced-feeling at this level because the symbolic incentive (imperial favor) is functionally inaccessible to them; noncompliance risks local sanction without any compensating access to advancement.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, peasant_communities, payer,
    powerless, biographical, trapped, local).

% Maintain older kinship-based normative structures that the new imperial norm displaces or subordinates. Face a choice between costly conformity that erodes their own authority basis, or resistance that risks exclusion from imperial patronage networks their neighbors are entering. Geographic distance from the court gives partial but shrinking room to delay.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, peripheral_clans, payer,
    moderate, generational, constrained, regional).

% Local notables whose status predates the new norm and who lose relative standing as imperial-aligned norms redefine what counts as prestige. They bear a status cost and, in cases of overt resistance, risk being administratively bypassed in favor of more compliant rivals.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, non_conforming_local_elites, payer,
    moderate, biographical, constrained, regional).

% Record the emperor's exemplary acts and the pace of adoption across strata, producing the documentary record later historians use to reconstruct whether legitimacy flowed from charisma, coercion, or popular will. Their framing choices shape which reading of the kernel later observers find most persuasive.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, court_historians_and_chroniclers, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, legible normative standard (the emperor's exemplary practice) around which disparate elite factions and provincial administrations can coordinate advancement, status competition, and administrative reporting, replacing a patchwork of locally variable norms with one imperially anchored reference point.
% TRANSFER_FUNCTION: Moves status, office, and patronage toward those who visibly and early adopt the norm (court, aristocracy, cooperative administrators), and moves compliance costs, status loss, and administrative burden onto peasants, peripheral clans, and non-conforming elites who adopt late or not at all.
% ABSENT_VOICES: Peasant communities and peripheral clans have no direct channel to the imperial court and are not consulted on the norm's content; their objections, where recorded, survive only through hostile administrative reports characterizing resistance as backwardness rather than as reasoned dissent.
% DISAPPEARANCE_RATIONALE: If the hybrid mechanism vanished — if the emperor's example stopped functioning as a status signal and the institutional rewards attached to it were withdrawn — the entire status ladder built on early/late adoption would collapse, aristocratic rank orderings tied to conformity timing would need to be renegotiated, and provincial administrators would lose their primary lever for demonstrating loyalty upward.
% FOUNDING_PROBLEM: The imperial center needed a way to standardize norms and behavior across a large, administratively heterogeneous territory without either the cost of comprehensive coercive enforcement or the delay of waiting for organic bottom-up convergence.
% FOUNDING_PROBLEM_CORROBORATION: Court chroniclers and the imperial administration attest the mechanism worked as intended, citing rapid elite conformity as proof of successful integration. Independent evidence is thinner outside the benefiting strata: later administrative records of peasant petitions and clan genealogies compiled by non-court scribes suggest slower, more resentful compliance at the periphery, and some modern historians reading the same archive conclude the 'legitimacy' was substantially manufactured retrospectively by the very chroniclers who benefited from imperial patronage — corroboration from outside the court-and-aristocracy nexus is limited and contested.
narrative_ontology:disappearance_verdict(imposition_mechanism_kernel__hybrid_legitimation_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_mechanism_kernel__hybrid_legitimation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_mechanism_kernel__hybrid_legitimation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(imposition_mechanism_kernel__hybrid_legitimation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_mechanism_kernel__hybrid_legitimation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imposition_mechanism_kernel__hybrid_legitimation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imposition_mechanism_kernel__hybrid_legitimation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises modestly from 0.30 to 0.52 across the interval: initial diffusion is driven by voluntary elite emulation (low extraction), but as provincial administrators formalize compliance reporting the arrangement increasingly extracts unrewarded conformity costs from peasants and peripheral clans who see none of the institutional upside. Theater ratio starts relatively high (0.55) — much of early 'adoption' is performative court display — and falls as the norm sediments into genuine administrative practice with real consequences (0.40 by interval end). Suppression starts low (0.20, genuinely charismatic/voluntary at the elite level) and climbs to a moderate 0.44 as the institutional incentive structure hardens into something closer to an expectation backed by administrative sanction — consistent with the reading's expected delta of 'moderate enforcement costs,' clearly distinguished from the higher suppression the exogenous_override_reading would author for the same historical moment.
 *
 * DIRECTIONALITY LOGIC:
 *   Imperial court and aristocratic early adopters sit near the beneficiary end: they set or ride the symbolic signal and capture the institutional rewards (office, status, patronage) with mobile or analytical exit options. Provincial administrators are dual-positioned — beneficiaries of the conduit role but also agenda-setters at the regional level, since their career incentive is to enforce visible compliance downward regardless of genuine belief. Peasant communities, peripheral clans, and non-conforming elites are targets: they bear the compliance cost (ritual, dues, status subordination) without institutional access to the reward side, and their exit options range from constrained (elites, clans, who can still negotiate) to trapped (peasants, who cannot).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — standardizing behavior across an administratively fragmented territory cheaply — never fully disappears (empires persist facing this coordination problem), but its live/dead status is genuinely contested: from the court's perspective the mechanism remains necessary; from the periphery's perspective the coordination need was substantially met early and what persists afterward is inertial status-signaling machinery riding on the original charismatic transfer. This is precisely the ambiguity a Tangled Rope classification is built to hold: real coordination function (a legible, non-locally-variable standard) coexists with real asymmetric extraction (peasants and clans pay disproportionately for a system whose institutional rewards flow elsewhere) — collapsing this into pure Rope would erase the periphery's payer status, and collapsing it into pure Snare would erase the genuine coordination the elite emulation cascade provided.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    charisma_or_coercion_at_periphery,
    'At the periphery — peasant communities and peripheral clans — did adoption ultimately occur through diffused symbolic prestige (a delayed but genuine version of the hybrid mechanism) or through local administrative coercion that the hybrid reading undercounts because it centers the elite experience?',
    'Comparative analysis of local administrative correspondence and petition records versus court chronicles: if peripheral compliance correlates with local sanction records rather than exposure to imperial symbolism, the exogenous_override_reading better describes the periphery even if the hybrid reading holds for the elite tier.',
    'If peripheral adoption is substantially coercive, this reading''s ε and suppression values may understate the periphery''s actual experience, suggesting the kernel''s readings may need to be scoped by social stratum rather than treated as empire-wide alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(charisma_or_coercion_at_periphery, empirical, 'Whether the hybrid mechanism actually extends to peripheral adoption or is an elite-tier phenomenon misapplied empire-wide.').

omega_variable(
    committer_framing_disagreement_location,
    'Where exactly do the three kernel readings disagree — is it about the CAUSE of legitimacy (charisma vs. demand vs. force), the SEQUENCE of adoption (elite-first vs. mass-first vs. simultaneous-imposed), or the MEASURE OF COERCION (moderate vs. none vs. high)?',
    'Cross-reading structural comparison: hold adoption-sequence data fixed and vary only the causal-attribution claim to see whether the three readings are actually making incompatible empirical claims or incompatible interpretive claims about the same sequence.',
    'If the disagreement is purely interpretive (same sequence, different causal story), the three readings coexist as genuinely different lenses on identical facts. If the disagreement is empirical (different actual adoption sequences claimed), one reading may be straightforwardly more accurate than the others for a given historical case.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_framing_disagreement_location, conceptual, 'Locating whether the kernel contest is about causation, sequence, or coercion-measurement.').

omega_variable(
    natural_diffusion_vs_constructed_legitimacy,
    'Is the imperial court''s beneficiary status here a natural byproduct of any coordination mechanism (someone has to set the standard) or is ''the emperor''s example'' a constructed legitimating fiction specifically designed to disguise what is functionally a directive from a coercive apparatus?',
    'Examine whether the institutional incentive structure (offices, exemptions) existed prior to and independent of the emperor''s symbolic act, or was newly constructed alongside it — prior independent existence would support genuine coordination; simultaneous construction would support a manufactured-legitimacy account.',
    'If simultaneously constructed, the coordination function claimed for this reading is substantially cover for what is closer to the exogenous_override_reading wearing charismatic packaging, which would push this constraint toward snare rather than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_diffusion_vs_constructed_legitimacy, conceptual, 'Whether imperial charismatic legitimacy is a genuine coordination signal or a constructed cover for coercive imposition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_mechanism_kernel__hybrid_legitimation_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(impo_tr_t8, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 8, 0.5).
narrative_ontology:measurement(impo_tr_t16, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 16, 0.46).
narrative_ontology:measurement(impo_tr_t24, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 24, 0.43).
narrative_ontology:measurement(impo_tr_t32, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 32, 0.41).
narrative_ontology:measurement(impo_tr_t40, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(impo_be_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(impo_be_t8, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(impo_be_t16, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 16, 0.44).
narrative_ontology:measurement(impo_be_t24, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 24, 0.48).
narrative_ontology:measurement(impo_be_t32, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 32, 0.5).
narrative_ontology:measurement(impo_be_t40, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(impo_su_t8, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 8, 0.28).
narrative_ontology:measurement(impo_su_t16, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 16, 0.35).
narrative_ontology:measurement(impo_su_t24, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 24, 0.4).
narrative_ontology:measurement(impo_su_t32, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 32, 0.43).
narrative_ontology:measurement(impo_su_t40, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 40, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_mechanism_kernel__hybrid_legitimation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.1).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__hybrid_legitimation_reading, imposition_mechanism_kernel__endogenous_climb_reading).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__hybrid_legitimation_reading, imposition_mechanism_kernel__exogenous_override_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'how did the new norm achieve legitimacy' into structurally distinct constraints per the ε-invariance principle: endogenous_climb_reading (bottom-up, low suppression, mass-first), exogenous_override_reading (coercive, high suppression, state-first), and this hybrid_legitimation_reading (moderate suppression, elite-first stratified adoption via charismatic-plus-institutional mechanism). Each carries its own ε and stakeholder structure; none is the 'correct' reading of a single constraint — they are three different constraints sharing a contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
