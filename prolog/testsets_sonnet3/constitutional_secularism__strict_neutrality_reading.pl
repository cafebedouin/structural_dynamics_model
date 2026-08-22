% ============================================================================
% CONSTRAINT STORY: constitutional_secularism__strict_neutrality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_secularism__strict_neutrality_reading, []).

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
 *   constraint_id: constitutional_secularism__strict_neutrality_reading
 *   human_readable: Constitutional Secularism — Strict Equidistance Reading
 *   domain: constitutional_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This story instantiates the strict neutrality reading of the
 *   constitutional secularism kernel: the state commits to equal distance
 *   from all religions, declining both preferential treatment and
 *   interference. This reading is distinct from the
 *   principled_intervention_reading (state may intervene for social reform)
 *   and the reformist_reading (state has an affirmative duty to eliminate
 *   oppressive practices) — those are separate constraints with separate ε
 *   values, not alternative measurements of this one. Under strict
 *   neutrality, the state's restraint is uniform in form but not in effect:
 *   it leaves the majority community's largely self-reforming institutions
 *   untouched while also leaving minority-community internal dissenters,
 *   especially women under personal law, without an external forum, since the
 *   same restraint applies regardless of a community's internal power
 *   structure or reform capacity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_secularism__strict_neutrality_reading, 0.42).
domain_priors:suppression_score(constitutional_secularism__strict_neutrality_reading, 0.38).
domain_priors:theater_ratio(constitutional_secularism__strict_neutrality_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__strict_neutrality_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_secularism__strict_neutrality_reading, "Constitutional Secularism — Strict Equidistance Reading").
narrative_ontology:topic_domain(constitutional_secularism__strict_neutrality_reading, "constitutional_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(constitutional_secularism__strict_neutrality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__strict_neutrality_reading, '078b296c-403f-45a4-bed7-91584c192e9f').
narrative_ontology:cs_kernel_codification('078b296c-403f-45a4-bed7-91584c192e9f', formalized).
narrative_ontology:cs_authority_grounding('078b296c-403f-45a4-bed7-91584c192e9f', lineage).
narrative_ontology:cs_interpretation_layer_present('078b296c-403f-45a4-bed7-91584c192e9f').
narrative_ontology:cs_reading_relation('078b296c-403f-45a4-bed7-91584c192e9f', constitutional_secularism__principled_intervention_reading, coexists_with).
narrative_ontology:cs_reading_relation('078b296c-403f-45a4-bed7-91584c192e9f', constitutional_secularism__reformist_reading, coexists_with).
narrative_ontology:cs_axiom('078b296c-403f-45a4-bed7-91584c192e9f', foundational, state_neutrality_requires_uniform_non_interference).
narrative_ontology:cs_axiom_status(state_neutrality_requires_uniform_non_interference, holdable).
narrative_ontology:cs_axiom_grounding('078b296c-403f-45a4-bed7-91584c192e9f', state_neutrality_requires_uniform_non_interference, deontological).
narrative_ontology:cs_axiom('078b296c-403f-45a4-bed7-91584c192e9f', secondary, religious_autonomy_presumptively_outweighs_reform_mandate).
narrative_ontology:cs_axiom_status(religious_autonomy_presumptively_outweighs_reform_mandate, holdable).
narrative_ontology:cs_axiom_grounding('078b296c-403f-45a4-bed7-91584c192e9f', religious_autonomy_presumptively_outweighs_reform_mandate, conventional).
narrative_ontology:cs_reference_frame('078b296c-403f-45a4-bed7-91584c192e9f', post_independence_anti_establishment_settlement).
narrative_ontology:cs_drift_state('078b296c-403f-45a4-bed7-91584c192e9f', contemporary_rights_jurisprudence_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('078b296c-403f-45a4-bed7-91584c192e9f', '').
narrative_ontology:cs_kernel_id(constitutional_secularism__strict_neutrality_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__strict_neutrality_reading, majority_religious_establishment).
narrative_ontology:constraint_beneficiary(constitutional_secularism__strict_neutrality_reading, state_judiciary_and_executive).
narrative_ontology:constraint_victim(constitutional_secularism__strict_neutrality_reading, intra_community_dissenters).
narrative_ontology:constraint_victim(constitutional_secularism__strict_neutrality_reading, women_under_personal_law).
narrative_ontology:constraint_victim(constitutional_secularism__strict_neutrality_reading, religious_minority_reform_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the equidistance doctrine, declining to intervene in internal religious practice on the theory that touching any one faith's practices would breach neutrality toward all. Sets the threshold for what counts as 'religious' versus 'secular' regulable conduct, and thereby controls how much protection the doctrine actually delivers to anyone inside a religious community.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, state_judiciary_and_executive, agenda_setter,
    institutional, generational, analytical, national).

% Holds enough numeric and institutional weight that the equidistance rule mostly ratifies its existing practices as the unmarked baseline against which 'equal treatment' is measured. Benefits from a state posture that treats non-intervention as fairness, since its internal reform movements are self-funded and do not depend on state action.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, majority_religious_establishment, beneficiary,
    organized, civilizational, arbitrage, national).

% Members within a given religious community who are subject to internal religious authority on matters of personal status, inheritance, or excommunication. Because the state declines to intervene in the name of neutrality, they have no external forum to appeal internal religious rulings that harm them; their only exits are formal apostasy (often socially catastrophic) or endurance.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, intra_community_dissenters, payer,
    powerless, biographical, trapped, local).

% Governed by religion-specific personal laws on marriage, divorce, and inheritance that the equidistance doctrine treats as internal religious matters exempt from uniform civil scrutiny. Bear the cost of whatever gender-inequitable provisions a given community's personal law contains, with the state declining to harmonize or override on neutrality grounds. Exit requires either litigation framed as a rights claim (uncertain, slow) or leaving the religious community altogether.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, women_under_personal_law, payer,
    powerless, biographical, constrained, national).

% Internal reformers within minority religious communities who want state backing to overturn oppressive practices but find the equidistance doctrine used against them: the state declines to assist reform on the theory that doing so would be interference, even though the same restraint is not exercised evenly given the majority's greater capacity for self-directed reform. Their voice is structurally unheard in the doctrine's own terms, since it recognizes only 'the community' as the relevant unit, not factions within it.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, religious_minority_reform_movements, excluded,
    moderate, generational, constrained, national).

% Analyze whether formal equidistance produces substantive equality, tracking case law and comparative constitutional practice across jurisdictions that have adopted equidistance versus interventionist models.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, secular_constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_secularism__strict_neutrality_reading, majority_religious_establishment).
narrative_ontology:fixing_cost_class(constitutional_secularism__strict_neutrality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, uniformly applied rule the state can invoke to avoid being drawn into sectarian favoritism: by declining to prefer or interfere with any religion, it solves the coordination problem of religious pluralism without requiring the state to adjudicate which religious practices are legitimate.
% TRANSFER_FUNCTION: Moves the burden of internal religious reform from the state onto individuals inside religious communities — particularly women and dissenters under personal law — who cannot invoke state power to challenge practices the state has classified as internal religious matters exempt from neutrality-preserving scrutiny.
% ABSENT_VOICES: Intra-community dissenters and reform factions are structurally absent from the doctrine's unit of analysis, which recognizes 'the religion' as a bloc rather than the contested internal politics within it; their objections do not register because the equidistance frame has no seat for intra-community conflict.
% DISAPPEARANCE_RATIONALE: If strict equidistance were abandoned overnight in favor of an interventionist posture, personal-law regimes, exemption doctrines, and non-interference case law would all become contestable; religious institutions would face direct civil scrutiny of internal practices, and the political calculus around minority protection versus majority-imposed reform would shift substantially.
% FOUNDING_PROBLEM: Newly independent, religiously plural states needed a rule to prevent the state itself from becoming an instrument of majority religious domination or sectarian favoritism, especially given histories of state-established religion and communal violence.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional framers and the state itself attest the founding problem — preventing state-sponsored majoritarian religious domination — remains live. Feminist legal scholars, minority reform movements, and comparative constitutional analysts (outside the beneficiary set) attest that in its strict equidistance form the doctrine has drifted into a shield for internal communal hierarchies, protecting community leadership rather than individual community members from state scrutiny.
narrative_ontology:disappearance_verdict(constitutional_secularism__strict_neutrality_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_secularism__strict_neutrality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_secularism__strict_neutrality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_secularism__strict_neutrality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_secularism__strict_neutrality_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_secularism__strict_neutrality_reading_tests).
:- end_tests(constitutional_secularism__strict_neutrality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.42) rather than severe because the doctrine's stated coordination function — preventing the state from becoming a sectarian instrument — is genuine and operative; the extraction is a byproduct of who bears the cost of state restraint, not a direct expropriation. Suppression sits moderate (0.38): the doctrine does not use force to silence dissenters, but it structurally forecloses an entire avenue of redress (state intervention) that would otherwise be available, and this foreclosure compounds over time as case law hardens the non-interference posture into settled doctrine — hence the rising measurement series.
 *
 * PERSPECTIVAL GAP:
 *   From the state's administrative seat, equidistance looks like principled restraint and even-handedness. From the seat of a woman governed by an unreformed personal law, the identical rule looks like the state actively declining to protect her because doing so would look like favoring one religion's reform trajectory over another's. The engine computes these as structurally different experiences of the same constraint from the declared power/exit data, not from a claim either seat makes about itself.
 *
 * DIRECTIONALITY LOGIC:
 *   The majority religious establishment benefits because its self-directed reform capacity means the equidistance rule mostly just ratifies its status quo; it experiences the constraint as low-cost or subsidizing. Intra-community dissenters, women under personal law, and minority reform movements are structurally targeted because the same non-interference rule denies them precisely the external lever they would need to change internal practices; their exit options range from trapped to constrained, pushing their derived directionality toward the full-target end.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope (rather than snare) preserves the doctrine's genuine coordination function — averting state-sponsored majoritarian religious domination is a real problem this reading solves — while still registering that the same structure imposes asymmetric costs on those with the least power to escape it. Collapsing this into a pure snare reading would erase the coordination value the doctrine provides against explicit state establishment of religion; collapsing it into a pure rope reading would erase the documented asymmetric burden on intra-community dissenters.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    formal_versus_substantive_equality,
    'Does uniform state non-interference across religious communities produce substantive equal treatment, given that communities differ in internal power structure and self-reform capacity?',
    'Comparative empirical study of outcomes for intra-community dissenters (particularly women under personal law) across communities with differing internal reform capacities, holding the equidistance rule constant.',
    'If formal uniformity systematically produces substantively unequal protection because communities differ in internal reform capacity, that supports the reformist critique that strict neutrality entrenches rather than resolves communal hierarchy. If protections converge despite differing community capacities, that supports the strict neutrality reading''s claim to genuine even-handedness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formal_versus_substantive_equality, empirical, 'Whether formal equidistance yields substantive equality across communities of differing internal power.').

omega_variable(
    kernel_framing_state_action_versus_state_omission,
    'Is the relevant kernel object ''what the state does to religions'' (in which case equidistance among state actions is the natural framing) or ''what happens to individuals inside religions as a function of state omission'' (in which case equidistance is a category error, since omission has asymmetric effects)?',
    'This is a conceptual framing question rather than an empirical one — it depends on whether the constitutional text and interpretive tradition treat state inaction as a form of state action for equal-protection purposes. Comparative constitutional doctrine on state action doctrine versus positive rights doctrine would inform but not settle it.',
    'Under the state-action framing, this reading appears as genuine neutrality (mountain-adjacent, low extraction). Under the state-omission framing, the identical doctrine appears as the state selectively declining to protect a subset of its citizens, which is much closer to a tangled_rope or snare reading. The classification given here (tangled_rope) adopts the state-omission framing as the more descriptively accurate one for this reading, but the state-action framing is a live alternative held by adherents of this reading themselves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_state_action_versus_state_omission, conceptual, 'Whether equidistance is properly measured against state action or against the asymmetric effects of state omission.').

omega_variable(
    false_neutrality_beneficiary_concentration,
    'Is the equidistance doctrine genuinely neutral background law, or does its practical operation concentrate benefit in the numerically and institutionally dominant religious community by treating that community''s existing practices as the unmarked baseline?',
    'Track which community''s practices most frequently anchor ''ordinary'' or ''default'' legal treatment in equidistance case law versus which communities'' practices are most frequently flagged as requiring special accommodation or exemption.',
    'If majority practices function as the unmarked baseline while minority practices are consistently treated as exceptions requiring accommodation, the doctrine''s claimed neutrality is partly cosmetic — a false-summit-adjacent structure where the ''neutral'' rule quietly encodes majority advantage.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(false_neutrality_beneficiary_concentration, conceptual, 'Whether formal equidistance conceals a structural tilt toward the majority community as the default reference point.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_secularism__strict_neutrality_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_secularism__strict_neutrality_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(cons_tr_t8, constitutional_secularism__strict_neutrality_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(cons_tr_t16, constitutional_secularism__strict_neutrality_reading, theater_ratio, 16, 0.23).
narrative_ontology:measurement(cons_tr_t24, constitutional_secularism__strict_neutrality_reading, theater_ratio, 24, 0.26).
narrative_ontology:measurement(cons_tr_t32, constitutional_secularism__strict_neutrality_reading, theater_ratio, 32, 0.28).
narrative_ontology:measurement(cons_tr_t40, constitutional_secularism__strict_neutrality_reading, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(cons_be_t8, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 8, 0.33).
narrative_ontology:measurement(cons_be_t16, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 16, 0.36).
narrative_ontology:measurement(cons_be_t24, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 24, 0.39).
narrative_ontology:measurement(cons_be_t32, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 32, 0.41).
narrative_ontology:measurement(cons_be_t40, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(cons_su_t8, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 8, 0.3).
narrative_ontology:measurement(cons_su_t16, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 16, 0.32).
narrative_ontology:measurement(cons_su_t24, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 24, 0.34).
narrative_ontology:measurement(cons_su_t32, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 32, 0.36).
narrative_ontology:measurement(cons_su_t40, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_secularism__strict_neutrality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_secularism__strict_neutrality_reading, constitutional_secularism__principled_intervention_reading).
narrative_ontology:affects_constraint(constitutional_secularism__strict_neutrality_reading, constitutional_secularism__reformist_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the constitutional_secularism kernel. constitutional_secularism__principled_intervention_reading authors the state's affirmative capacity to intervene for social reform as a separate constraint with its own beneficiary/victim structure and lower extraction toward reform-seeking minorities but higher extraction toward religious autonomy claims. constitutional_secularism__reformist_reading authors an affirmative state duty to eliminate oppressive practices, which would show still higher extraction toward religious institutional autonomy but lower extraction toward intra-community dissenters. Each story carries its own stable ε; do not average across them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
