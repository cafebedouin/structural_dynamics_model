% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__civic_republican_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_arms_right__civic_republican_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: second_amendment_arms_right__civic_republican_reading
 *   human_readable: Second Amendment as Civic-Republican Armed Citizenship Right
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the civic-republican reading of the Second
 *   Amendment kernel: the right to keep and bear arms is protected because
 *   armed citizenship is a structural prerequisite for republican
 *   self-governance, not because arms-bearing is a pre-political individual
 *   liberty (the individual-right reading) and not because the constitutional
 *   text protects only organized state militia authority (the
 *   collective-right reading). Under this reading, the citizen who trains,
 *   organizes, and stands ready for collective defense occupies the fullest
 *   protected category — the right and the civic duty are two faces of one
 *   status. This creates a distinctive extraction profile: moderate, not
 *   high, because the reading does not license unlimited state control (as
 *   the collective-right reading might) nor does it forbid any conditioning
 *   of access (as the individual-right reading does); it authorizes training
 *   and qualification requirements tied to the civic-participatory rationale
 *   specifically. ε rises across the 20th century as the National Guard
 *   system professionalized militia service away from the general citizenry,
 *   creating tension between the reading's civic ideal and the institutional
 *   reality of who actually 'serves.'
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__civic_republican_reading, 0.38).
domain_priors:suppression_score(second_amendment_arms_right__civic_republican_reading, 0.42).
domain_priors:theater_ratio(second_amendment_arms_right__civic_republican_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__civic_republican_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_arms_right__civic_republican_reading, "Second Amendment as Civic-Republican Armed Citizenship Right").
narrative_ontology:topic_domain(second_amendment_arms_right__civic_republican_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(second_amendment_arms_right__civic_republican_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__civic_republican_reading, '9d86fc8a-ebea-4ade-90d0-3bc9b12aa65c').
narrative_ontology:cs_kernel_codification('9d86fc8a-ebea-4ade-90d0-3bc9b12aa65c', fixed_text).
narrative_ontology:cs_authority_grounding('9d86fc8a-ebea-4ade-90d0-3bc9b12aa65c', lineage).
narrative_ontology:cs_interpretation_layer_present('9d86fc8a-ebea-4ade-90d0-3bc9b12aa65c').
narrative_ontology:cs_reading_relation('9d86fc8a-ebea-4ade-90d0-3bc9b12aa65c', second_amendment_arms_right__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('9d86fc8a-ebea-4ade-90d0-3bc9b12aa65c', second_amendment_arms_right__collective_right_reading, coexists_with).
narrative_ontology:cs_axiom('9d86fc8a-ebea-4ade-90d0-3bc9b12aa65c', foundational, armed_citizenship_constitutive_of_self_governance).
narrative_ontology:cs_axiom_status(armed_citizenship_constitutive_of_self_governance, holdable).
narrative_ontology:cs_axiom_grounding('9d86fc8a-ebea-4ade-90d0-3bc9b12aa65c', armed_citizenship_constitutive_of_self_governance, conventional).
narrative_ontology:cs_axiom('9d86fc8a-ebea-4ade-90d0-3bc9b12aa65c', foundational, right_conditioned_on_civic_participation_not_unconditional).
narrative_ontology:cs_axiom_status(right_conditioned_on_civic_participation_not_unconditional, holdable).
narrative_ontology:cs_axiom_grounding('9d86fc8a-ebea-4ade-90d0-3bc9b12aa65c', right_conditioned_on_civic_participation_not_unconditional, instrumental).
narrative_ontology:cs_reference_frame('9d86fc8a-ebea-4ade-90d0-3bc9b12aa65c', founding_era_civic_militia_synthesis).
narrative_ontology:cs_drift_state('9d86fc8a-ebea-4ade-90d0-3bc9b12aa65c', post_heller_individual_rights_ascendance, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9d86fc8a-ebea-4ade-90d0-3bc9b12aa65c', '').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__civic_republican_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__civic_republican_reading, civic_militia_participants).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__civic_republican_reading, republican_self_governance_polity).
narrative_ontology:constraint_victim(second_amendment_arms_right__civic_republican_reading, unaffiliated_gun_owners_seeking_pure_individual_right).
narrative_ontology:constraint_victim(second_amendment_arms_right__civic_republican_reading, unorganized_citizens_excluded_from_militia_norm).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(second_amendment_arms_right__civic_republican_reading, civic_militia_participants).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__civic_republican_reading, civic_republican_theory_of_armed_citizenship).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__civic_republican_reading, militia_as_constitutive_of_self_governance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Citizens who train, organize, and maintain readiness under this reading receive the right's protection precisely because they discharge the civic duty of potential militia service. They benefit from a constitutionally grounded claim to keep and bear arms, but that claim is bundled with an implicit expectation of training, discipline, and civic participation — the right is conditioned on the duty, so it costs something to fully occupy the protected category.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, civic_militia_participants, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_arms_right__civic_republican_reading, civic_militia_participants, payer).

% The abstract political order this reading is meant to secure — a citizenry capable of collective self-defense against tyranny and external threat, understood as a structural precondition of republican government. It is not an actor that collects anything itself; it is the vindicated end the arrangement claims to serve.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, republican_self_governance_polity, beneficiary,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(second_amendment_arms_right__civic_republican_reading, republican_self_governance_polity).

% Citizens who want to own and carry firearms purely for personal self-defense, sport, or autonomy, disconnected from any civic-participatory or militia framing. Under this reading their claim is weaker than under an individual-right reading: regulators can condition, license, or qualify access by reference to training and civic-readiness norms that a pure individual-liberty framing would treat as illegitimate burdens. They bear the cost of a right that is read as conditional rather than unconditional.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, unaffiliated_gun_owners_seeking_pure_individual_right, payer,
    moderate, biographical, constrained, national).

% People who, for reasons of disability, poverty, geography, or simple disinclination, cannot or do not participate in anything resembling militia readiness. Under a civic-republican framing their claim to protection is structurally weaker than that of engaged citizen-militia members, even though the constitutional text nominally protects 'the people.' They cannot buy their way into full protection and cannot easily exit the jurisdiction that applies this reading.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, unorganized_citizens_excluded_from_militia_norm, payer,
    powerless, biographical, trapped, national).

% Legislatures and agencies that write firearms regulation are, under this reading, granted a distinctive kind of authority: they may impose training, licensing, and organizational requirements that track the civic-republican rationale (readiness, accountability, competence) without those requirements being treated as infringements of an unconditional individual liberty. This gives regulators a middle path unavailable under either sibling reading — more latitude than the individual-right reading permits, but grounded in citizenship duty rather than mere state militia control.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, state_and_federal_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Courts that must adjudicate between the three competing kernel readings when litigants invoke the Second Amendment. Under the civic-republican reading, courts evaluate regulations by asking whether they promote or undermine the civic-participatory purpose of an armed, self-governing citizenry — a different inquiry than either pure individual-liberty balancing or pure militia-institutional deference.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% Advocacy organizations and scholars committed to the pre-political individual-liberty reading are structurally excluded from shaping doctrine under this reading's dominant framing; they would object that conditioning the right on civic participation smuggles in a duty the founding-era text does not impose on the individual holder. They are well-resourced and can litigate and lobby elsewhere, but within a jurisdiction adopting this reading their preferred framing has no purchase.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, originalist_individual_rights_advocates, excluded,
    powerful, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_arms_right__civic_republican_reading, diffuse).
narrative_ontology:fixing_cost_class(second_amendment_arms_right__civic_republican_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Secures a citizenry capable of collective self-defense and resistance to tyranny by tying the constitutional protection of arms-bearing to participation in the civic project of republican self-governance — coordinating individual armament with a shared political end rather than treating either the individual or the state militia as the sole locus of the right.
% TRANSFER_FUNCTION: Moves interpretive latitude from courts applying strict individual-liberty scrutiny toward legislatures and regulators who may condition firearms access on training, organizational participation, and civic-readiness criteria; correspondingly moves protection away from citizens who hold arms outside any civic-participatory frame and toward those who can be characterized as engaged in militia-adjacent civic activity.
% ABSENT_VOICES: Individual-rights litigants who view any conditioning of the right on civic duty as an illegitimate reintroduction of state permission into a pre-political liberty are not accommodated within this reading's framework — their objection would be that 'the people' means each person, full stop, not each person insofar as they participate in collective defense. Collective-right proponents who would restrict the protection entirely to organized, state-authorized militia service are similarly unaccommodated on the other side — they would object that recognizing any individual-facing dimension already concedes too much.
% DISAPPEARANCE_RATIONALE: If this specific reading vanished, jurisdictions applying it would not become gun-free zones — they would simply re-sort into either the individual-right or collective-right framework, each of which resolves cases differently (broader individual latitude under one, narrower individual claims under the other). Regulators, courts, and civic-militia advocacy groups that have built doctrine and organizational identity around the civic-republican middle path would lose their distinctive interpretive home; individual-rights advocates would gain ground; collective-right advocates would also gain ground relative to this reading specifically. Whether 'the world rearranges' depends on which sibling reading fills the vacuum, which is itself contested.
% FOUNDING_PROBLEM: The founding-era problem was securing a citizenry capable of resisting both domestic tyranny and foreign threat without relying on a permanent professional standing army, understood by republican political theory as inherently dangerous to liberty — arms-bearing citizens organized (at least notionally) as militia were the alternative.
% FOUNDING_PROBLEM_CORROBORATION: Historians of republican political thought and some constitutional scholars outside the gun-rights advocacy space attest that the civic-republican framing accurately describes founding-era anxieties about standing armies and civic virtue. Individual-rights advocacy organizations dispute that this framing should control modern doctrine, arguing the civic-participatory problem is largely obsolete (no serious standing-army-versus-militia debate exists today) while the individual self-defense interest remains fully live; they attest, from outside this reading's own beneficiary set, that the founding problem this reading names is substantially dead even though the reading itself persists in academic and some judicial discourse.
narrative_ontology:disappearance_verdict(second_amendment_arms_right__civic_republican_reading, contested).
narrative_ontology:founding_problem_status(second_amendment_arms_right__civic_republican_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__civic_republican_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_arms_right__civic_republican_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_arms_right__civic_republican_reading, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__civic_republican_reading_tests).
:- end_tests(second_amendment_arms_right__civic_republican_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38) because the civic-republican reading licenses real regulatory burden — training mandates, organizational registration, qualification standards — that a pure individual-right reading would treat as infringement, but caps that burden at what serves the civic-readiness rationale rather than permitting open-ended state control. Suppression (0.42) reflects that citizens outside the militia-adjacent framing face a genuinely narrower claim, enforced through licensing and permitting regimes that reference civic-participation criteria. Theater ratio (0.28) captures that some 'civic readiness' framing in modern regulation is rhetorical rather than functional — few jurisdictions actually operationalize militia service as a live institution, so invoking the civic-republican rationale to justify training requirements is partly performative continuity with an 18th-century institutional reality that has largely disappeared.
 *
 * DIRECTIONALITY LOGIC:
 *   Civic militia participants are near-symmetric beneficiaries: they receive real protection but carry the corresponding duty-cost, so directionality sits closer to the coordination midpoint than a pure beneficiary would. Unaffiliated individual-rights-seeking gun owners and unorganized citizens are targets under this reading specifically — not because the reading is hostile to them, but because their claim is structurally weaker than it would be under the individual-right reading, and weaker still than the civic-militia participant's claim. Regulators are the agenda-setting seat: this reading hands them a distinctive tool (civic-rationale-linked conditioning) unavailable under either sibling reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — securing a citizen militia as an alternative to a standing professional army — is substantially dead in practical military terms (the United States has maintained a large standing army for over a century, and the National Guard is federally integrated and professionalized, not a general citizen levy). Yet the civic-republican reading persists in constitutional doctrine and advocacy, which is precisely the founding-problem/disappearance-verdict mismatch this schema is built to surface: founding_problem_status is contested-trending-dead while the reading's institutional life continues, suggesting parts of its modern application function as inherited doctrinal architecture rather than live civic-defense necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    civic_republican_vs_individual_right_boundary_location,
    'Where exactly does the civic-republican reading''s tolerance for regulatory conditioning end and illegitimate infringement on individual liberty begin — and is that boundary principled or simply wherever courts applying this reading happen to draw it?',
    'Comparative doctrinal analysis of jurisdictions explicitly adopting civic-republican reasoning versus individual-right reasoning: track whether the civic-republican boundary tracks a coherent theory (training tied to actual readiness function) or drifts toward whatever burden level courts find politically tolerable.',
    'If the boundary is principled and tracks civic-readiness function, this reading is structurally distinct from both siblings with a real coordination logic (tangled_rope with genuine coordination component). If the boundary is unprincipled, this reading functions as a rhetorical middle path that lets courts reach individual-right or collective-right outcomes while claiming a third theory, and the coordination function is largely cosmetic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civic_republican_vs_individual_right_boundary_location, conceptual, 'Whether the civic-republican reading has a principled regulatory boundary or is doctrinally indeterminate.').

omega_variable(
    kernel_framing_which_reading_the_text_actually_states,
    'Does the constitutional text and its founding-era context more strongly support the civic-republican reading, or is this reading itself a modern scholarly synthesis imposed on a text that more cleanly supports one of the other two readings?',
    'Historical-linguistic analysis of 18th-century usage of ''bear arms'' and ''well regulated militia,'' cross-referenced against contemporaneous state constitutional provisions and ratification debates, compared across all three readings'' historical claims.',
    'If the civic-republican reading is a later synthesis rather than the original public meaning, its claim to constitutional grounding is weaker than either sibling''s claim to textual fidelity, which would affect how much interpretive weight courts should give it relative to the competing readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_which_reading_the_text_actually_states, empirical, 'Whether the civic-republican reading reflects original constitutional meaning or is a later interpretive construction.').

omega_variable(
    founding_problem_obsolescence_degree,
    'Is the civic-republican rationale (citizen militia as alternative to standing army) fully obsolete given the permanent professional military and federalized National Guard, or does it retain a live function in some residual form (e.g., civil unrest response, disaster response militia-adjacent activity)?',
    'Survey of actual state militia statutes still in force and their invocation history over the past 50 years; assess whether any jurisdiction has activated general-citizen militia provisions outside the National Guard framework.',
    'If fully obsolete, the reading''s persistence in doctrine is closer to inertial/scaffold-like continuation of a dead rationale; if a residual function exists, the tangled_rope coordination component is more defensible as live rather than vestigial.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_obsolescence_degree, empirical, 'Whether the civic-republican rationale retains any live institutional function today.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__civic_republican_reading, 1791, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1791, second_amendment_arms_right__civic_republican_reading, theater_ratio, 1791, 0.1).
narrative_ontology:measurement_basis(seco_tr_t1791, observed).
narrative_ontology:measurement(seco_tr_t1900, second_amendment_arms_right__civic_republican_reading, theater_ratio, 1900, 0.15).
narrative_ontology:measurement_basis(seco_tr_t1900, observed).
narrative_ontology:measurement(seco_tr_t1939, second_amendment_arms_right__civic_republican_reading, theater_ratio, 1939, 0.2).
narrative_ontology:measurement_basis(seco_tr_t1939, observed).
narrative_ontology:measurement(seco_tr_t1980, second_amendment_arms_right__civic_republican_reading, theater_ratio, 1980, 0.24).
narrative_ontology:measurement_basis(seco_tr_t1980, observed).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_arms_right__civic_republican_reading, theater_ratio, 2008, 0.3).
narrative_ontology:measurement_basis(seco_tr_t2008, observed).
narrative_ontology:measurement(seco_tr_t2026, second_amendment_arms_right__civic_republican_reading, theater_ratio, 2026, 0.28).
narrative_ontology:measurement_basis(seco_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 1791, 0.15).
narrative_ontology:measurement_basis(seco_be_t1791, observed).
narrative_ontology:measurement(seco_be_t1900, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 1900, 0.22).
narrative_ontology:measurement_basis(seco_be_t1900, observed).
narrative_ontology:measurement(seco_be_t1939, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 1939, 0.3).
narrative_ontology:measurement_basis(seco_be_t1939, observed).
narrative_ontology:measurement(seco_be_t1980, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 1980, 0.32).
narrative_ontology:measurement_basis(seco_be_t1980, observed).
narrative_ontology:measurement(seco_be_t2008, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 2008, 0.4).
narrative_ontology:measurement_basis(seco_be_t2008, observed).
narrative_ontology:measurement(seco_be_t2026, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 2026, 0.38).
narrative_ontology:measurement_basis(seco_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1791, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 1791, 0.2).
narrative_ontology:measurement_basis(seco_su_t1791, observed).
narrative_ontology:measurement(seco_su_t1900, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 1900, 0.28).
narrative_ontology:measurement_basis(seco_su_t1900, observed).
narrative_ontology:measurement(seco_su_t1939, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 1939, 0.35).
narrative_ontology:measurement_basis(seco_su_t1939, observed).
narrative_ontology:measurement(seco_su_t1980, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 1980, 0.36).
narrative_ontology:measurement_basis(seco_su_t1980, observed).
narrative_ontology:measurement(seco_su_t2008, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 2008, 0.45).
narrative_ontology:measurement_basis(seco_su_t2008, observed).
narrative_ontology:measurement(seco_su_t2026, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 2026, 0.42).
narrative_ontology:measurement_basis(seco_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__civic_republican_reading, identity_coordination).
narrative_ontology:affects_constraint(second_amendment_arms_right__civic_republican_reading, second_amendment_arms_right__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__civic_republican_reading, second_amendment_arms_right__collective_right_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language 'Second Amendment right to keep and bear arms' per the ε-invariance principle: individual_right_reading (pre-political individual liberty, low regulatory tolerance), collective_right_reading (state militia authority only, no individual claim outside organized service), and this civic_republican_reading (dual right-and-duty citizenship status, moderate regulatory tolerance tied to civic-readiness rationale). Each carries a distinct ε and distinct beneficiary/victim structure; they are linked here rather than merged because measuring 'the right' by different observables (individual self-defense capacity vs. state militia control vs. civic-participatory function) yields materially different extraction and suppression profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
