% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__living_constitutionalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_living_constitutionalism, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: magna_carta_constraint_authority__living_constitutionalism_reading
 *   human_readable: Magna Carta Constraint Authority (Living Constitutionalism Reading)
 *   domain: constitutional/legal/political
 *
 * SUMMARY:
 *   The living-constitutionalism reading holds that Magna Carta establishes
 *   an inherited, evolving constraint on executive power that binds all
 *   subsequent rulers through juridical precedent and continuous
 *   interpretation. In this frame, the constraint is not a static historical
 *   artifact but a living principle: its core commitment—that even the
 *   highest authority is subject to law—transmutes across centuries through
 *   judicial interpretation, legislative codification, and practice. This
 *   reading competes with two siblings: the feudal-obsolescence reading
 *   (which treats Magna Carta as a spent baronial compact from 1215 with no
 *   binding authority over modern sovereignty) and the
 *   parliamentary-sovereignty reading (which absorbs Magna Carta into
 *   parliamentary statute law, giving legislators the power to revise or
 *   repeal any charter principle). The living-constitutionalism reading is
 *   structurally distinct because it claims the constraint persists through
 *   INTERPRETATION rather than through parliamentary inscription—judges and
 *   the juridical order itself are the custodians, not legislatures.
 *
 * KEY AGENTS:
 *   - juridical_subjects_and_citizens — Inherit the due-process shield and the hereditary legal standing that bind all rulers
 *   - executive_prerogative_authority — Constrained from unilateral action; must operate within a juridical framework
 *   - judicial_interpreters — The agenda-setting institutional seat; courts decide what due process and lawful restraint mean as circumstances change
 *   - legislative_authority — Both beneficiary and agenda-setter; it refines the constraint but is also constrained by it
 *   - royal_prerogative_advocates — Excluded from the conversation; they would repudiate inherited constitutional restraint entirely
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__living_constitutionalism_reading, 0.38).
domain_priors:suppression_score(magna_carta_constraint_authority__living_constitutionalism_reading, 0.22).
domain_priors:theater_ratio(magna_carta_constraint_authority__living_constitutionalism_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__living_constitutionalism_reading, rope).
narrative_ontology:human_readable(magna_carta_constraint_authority__living_constitutionalism_reading, "Magna Carta Constraint Authority (Living Constitutionalism Reading)").
narrative_ontology:topic_domain(magna_carta_constraint_authority__living_constitutionalism_reading, "constitutional/legal/political").

domain_priors:requires_active_enforcement(magna_carta_constraint_authority__living_constitutionalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__living_constitutionalism_reading, '6998702b-acce-4fef-8238-a2b60953590e').
narrative_ontology:cs_kernel_codification('6998702b-acce-4fef-8238-a2b60953590e', fixed_text).
narrative_ontology:cs_authority_grounding('6998702b-acce-4fef-8238-a2b60953590e', lineage).
narrative_ontology:cs_interpretation_layer_present('6998702b-acce-4fef-8238-a2b60953590e').
narrative_ontology:cs_reading_relation('6998702b-acce-4fef-8238-a2b60953590e', magna_carta_constraint_authority__feudal_obsolescence_reading, forecloses).
narrative_ontology:cs_reading_relation('6998702b-acce-4fef-8238-a2b60953590e', magna_carta_constraint_authority__parliamentary_sovereignty_reading, influences).
narrative_ontology:cs_axiom('6998702b-acce-4fef-8238-a2b60953590e', foundational, inherited_text_binds_future_rulers).
narrative_ontology:cs_axiom_status(inherited_text_binds_future_rulers, holdable).
narrative_ontology:cs_axiom_grounding('6998702b-acce-4fef-8238-a2b60953590e', inherited_text_binds_future_rulers, deontological).
narrative_ontology:cs_axiom('6998702b-acce-4fef-8238-a2b60953590e', foundational, interpretation_preserves_constitutional_force).
narrative_ontology:cs_axiom_status(interpretation_preserves_constitutional_force, holdable).
narrative_ontology:cs_axiom_grounding('6998702b-acce-4fef-8238-a2b60953590e', interpretation_preserves_constitutional_force, instrumental).
narrative_ontology:cs_reference_frame('6998702b-acce-4fef-8238-a2b60953590e', magna_carta_as_binding_inherited_law).
narrative_ontology:cs_drift_state('6998702b-acce-4fef-8238-a2b60953590e', contemporary_democratic_constitutionalism, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('6998702b-acce-4fef-8238-a2b60953590e', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__living_constitutionalism_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__living_constitutionalism_reading, juridical_subjects_and_citizens).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__living_constitutionalism_reading, executive_prerogative_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__living_constitutionalism_reading, legislative_authority).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__living_constitutionalism_reading, rule_of_law_doctrine).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__living_constitutionalism_reading, inherited_constitutional_restraint).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Subjects and citizens inherit a constitutional shield against arbitrary executive power. The Magna Carta framework guarantees due process before deprivation of rights, access to justice, and restraint on royal/executive discretion. They benefit from predictable rule application and hereditary legal standing. Their exit from this arrangement would mean renouncing citizenship itself.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, juridical_subjects_and_citizens, beneficiary,
    organized, generational, constrained, national).

% The sitting executive (monarch, president, or executive branch leadership) inherits an office whose powers are constitutionally circumscribed. They cannot act by pure discretion; actions must rest on law and due process. This constrains policy speed and unilateral action. They bear the costs of justifying executive acts within a juridical framework rather than by prerogative alone. Exiting the constraint would require constitutional amendment or repudiation — an extraordinary act.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, executive_prerogative_authority, payer,
    powerful, biographical, constrained, national).

% Courts interpret and apply Magna Carta's constraints across generations. They hold the authority to decide what due process requires, what lawful restraint means, and how these principles evolve with changing circumstances. Their interpretive work is the living mechanism through which the constraint persists and adapts. They balance fidelity to the inherited text with responsiveness to new conditions.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, judicial_interpreters, agenda_setter,
    institutional, generational, analytical, national).

% Parliament or the legislature is both a beneficiary (it inherits the Magna Carta framework that constrains competing power centers) and an agenda-setter (it can codify, expand, or refine the constraint through statute). In this reading, legislative authority is checked but also enabled by the Magna Carta framework: it cannot exercise power arbitrarily either, but it can make law that refines due process.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, legislative_authority, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_constraint_authority__living_constitutionalism_reading, legislative_authority, agenda_setter).

% Those who argue for strong executive prerogative independent of juridical restraint (monarchist traditionalists, absolute-sovereignty advocates) are structurally excluded from the living-constitutionalism reading. They would contest that any inherited text can bind current rulers or that judicial review of executive acts is legitimate. They are not in the conversation this reading enables.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, royal_prerogative_advocates, excluded,
    powerful, biographical, trapped, national).

% The broader common-law and precedent-based legal tradition carries and transmits the Magna Carta constraint across jurisdictions and centuries. As an analytical observer, the tradition itself does not act or benefit, but it is the epistemic ground on which this reading rests: that inherited juridical principles can bind future practice through interpretation and precedent.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, common_law_tradition, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the medieval and enduring problem of arbitrary executive power: how to bind a sitting ruler to law rather than discretion, and how to do it in a way that survives the turnover of individual rulers. Coordinates subjects under a predictable, inherited due-process framework rather than royal whim.
% TRANSFER_FUNCTION: Transfers authority from unilateral executive prerogative to a juridical order in which all power (executive, legislative, subject claims) are mediated through law and due process. The constraint moves the cost of executive action from 'I command it' to 'law permits it,' and moves the benefit of restraint from bargained exception to hereditary right.
% ABSENT_VOICES: Absolute-sovereignty advocates, executive-prerogative defenders, and those who view the Magna Carta as purely historical artifact without binding force are structurally excluded from the living-constitutionalism reading. They would argue that inherited constraints cannot bind current rulers and that modern law supersedes medieval charters. Their absence from the conversation is itself a structural feature of how the reading maintains itself.
% DISAPPEARANCE_RATIONALE: If the Magna Carta constraint disappeared — if the inherited due-process framework were repudiated and executive authority reverted to pure prerogative — the relationship between ruler and ruled would restructure immediately. Citizens would lose the hereditary shield; executives would recover unilateral discretion; the juridical order itself would collapse and need to be rebuilt on entirely different premises. Majorities and minorities alike depend on this constraint's persistence for predictable legal standing.
% FOUNDING_PROBLEM: Medieval executives (monarchs) ruled by discretion and prerogative, constrained only by immediate military balance. Subjects had no inherited legal standing or predictable right to due process. Disputes were settled by force, not law. The Magna Carta was forged to establish that even the highest executive is bound by inherited law and cannot act in pure prerogative.
% FOUNDING_PROBLEM_CORROBORATION: The executive overreach litmus tests of modern constitutionalism (presidential power grabs, emergency prerogative claims, bypass of due process) and the consistent judicial pushback against them (separation of powers doctrine, rule-of-law jurisprudence) corroborate that the founding problem persists. Legal scholars, historians outside the judiciary, and legislative oversight bodies all attest that executives still attempt to expand prerogative and that inherited constitutional restraint is the mechanism that catches and corrects those attempts. No corroborating source claims the problem is solved; the debate is whether the constraint still works, not whether the threat is gone.
narrative_ontology:disappearance_verdict(magna_carta_constraint_authority__living_constitutionalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_constraint_authority__living_constitutionalism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__living_constitutionalism_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(magna_carta_constraint_authority__living_constitutionalism_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_constraint_authority__living_constitutionalism_reading_tests).
:- end_tests(magna_carta_constraint_authority__living_constitutionalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-low (0.38 at 2025) because the constraint's function is coordination around inherited restraint, not rent-collection. The primary beneficiary (subjects and citizens) gains predictable legal standing and access to justice; the primary payer (executive prerogative) loses unilateral discretion but still retains power to act lawfully. Suppression has declined dramatically (0.65 in 1215 to 0.22 in 2025) because the constraint's enforcement has moved from coercion (the Crown suppressing dissent, nobles enforcing charter) to institutionalization (courts, legislatures, and internalized rule-of-law norms). Theater is low and stable (0.08–0.18 across the interval) because judicial interpretation, though performative in some respects, genuinely adjudicates disputes rather than merely staging them. The measurement series shows the trajectory of a coordination constraint that became normalized: suppression declined as legal institutions matured; extractiveness rose slightly as the constraint's scope widened and its behavioral demands on executives increased. All metrics are authored on a single shared time grid (1215, 1400, 1688, 1832, 1945, 2025) so temporal analysis is grid-coherent.
 *
 * PERSPECTIVAL GAP:
 *   From the executive seat, the constraint is an inherited restraint that enables lawful rule; from the subject seat, it is a shield against arbitrary power. From the judicial seat, it is an interpretive mission that evolves with circumstances. These are not divergent measurements of the same constraint—they are the constraint experienced from different structural positions. The living-constitutionalism reading allows all three perspectives to cohere: the constraint coordinates them all by binding everyone (including judges and legislatures) to the principle that law, not discretion, governs.
 *
 * DIRECTIONALITY LOGIC:
 *   Subjects and citizens sit at the beneficiary end (d near 0.0) because they gain inherited legal standing and due-process protection; they lose nothing and their exit (renouncing citizenship) is extraordinarily costly. The executive authority sits at the target end (d near 1.0 on the prerogative axis) because it loses unilateral discretion and is constrained by law; executives cannot exit the office without abandoning their power. Judicial interpreters and legislatures sit near the middle but asymmetrically: they are constrained but also empowered by the framework. The constraint's d-profile is unusual because it coordinates around a structural asymmetry—rulers cannot exit, and subjects cannot freely relocate—that persists across centuries. Identity-lock is present for judicial and legislative actors: their institutional identity is constituted through the role of interpreting and refining inherited law; abandoning that role would unmake the institutions themselves.
 *
 * MANDATROPHY ANALYSIS:
 *   The living-constitutionalism reading resists mandatrophy by maintaining that the founding problem (arbitrary executive power) is permanently live and that the constraint's function (binding rulers to law through inherited juridical principle) remains operationally necessary. The measurement series captures this: even as suppression declined (institutions matured, rule of law internalized), extractiveness did not rise dramatically because the constraint remained genuinely coordinative—it was not performing a spent function theatrically, but adapting to new contexts (parliamentary representation, democratic elections, expanded due process). The theater-ratio remained low because judicial interpretation, while interpretive and mutable, was grounded in genuine dispute resolution and constitutional adjudication, not merely ceremonial. This reading risks mandatrophy only if the founding problem is genuinely solved—if executive prerogative is permanently tamed and ruling executives no longer attempt to circumvent due process. The living-constitutionalism reading claims that risk is not yet realized; periodic executive overreach (emergency powers, surveillance expansion, regulatory bypass) demonstrates the threat is live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretation_vs_original_intent,
    'Does the authority of Magna Carta rest on fidelity to the original 1215 baronial intent, or on the capacity of judges to reinterpret the principle of lawful restraint for new contexts?',
    'Genealogy of leading cases: do courts justify departures from medieval specifics by appeal to original intent or by appeal to evolved principle? Does the constraint''s persistence depend on original meaning or on interpretive freedom?',
    'If dependent on original intent, the constraint decays as contexts diverge (moves toward feudal-obsolescence reading). If dependent on interpretive freedom, the constraint is alive as long as judges honor the principle of restraint (supports living-constitutionalism reading).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interpretation_vs_original_intent, conceptual, 'Whether the constraint''s authority is rooted in original meaning or evolved interpretation.').

omega_variable(
    judicial_vs_legislative_custodianship,
    'Who is the custodian of the constraint in the modern era: courts interpreting inherited principle, or legislatures codifying and revising statutory due process?',
    'Empirical comparison: in jurisdictions where legislatures have codified extensive statutory due process (largely replacing common-law Magna Carta), does the constraint persist with the same force? How much does modern due process rest on judicial Magna Carta interpretation vs. statutory codification?',
    'Strong dependence on legislative codification supports parliamentary-sovereignty reading; strong dependence on judicial interpretation supports living-constitutionalism reading. Mixed dependence suggests the constraint operates through both mechanisms simultaneously.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_vs_legislative_custodianship, empirical, 'Whether custodianship rests with courts or legislatures.').

omega_variable(
    executive_prerogative_resilience,
    'Is executive prerogative genuinely constrained by inherited Magna Carta principle, or does it persist beneath the surface through emergency powers, state-secrets doctrines, and prosecutorial discretion?',
    'Structural analysis of how executives circumvent due process (emergency measures, classified procedures, administrative discretion); measurement of actual constraint vs. nominal constraint; examination of whether modern executives face real costs for due-process violations.',
    'If prerogative is genuinely constrained, the constraint functions as rope (coordination). If executives routinely evade it and pay no cost, the constraint is theater (piton). If coercion is required to enforce it against powerful resistance, it may be tangled-rope or even snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(executive_prerogative_resilience, empirical, 'Whether executive power is actually constrained by Magna Carta principle or merely nominally restrained.').

omega_variable(
    identity_lock_on_judicial_authority,
    'Is judicial institutional identity so fused with the role of interpreting inherited constitutional restraint that judges cannot renounce Magna Carta authority without unmaking their own institutional role?',
    'Historical counterfactual: could a judiciary cease to treat Magna Carta as binding while remaining ''a judiciary'' in the same institutional sense? How much of judicial identity is constituted by custodianship of inherited principle?',
    'High identity-lock supports interpretation that judges'' exit options are identity-locked (not merely constrained or mobile), which affects directionality calculation and type assessment. It also explains why the constraint persists even against executive pressure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_lock_on_judicial_authority, conceptual, 'Whether judicial institutional identity is constituted by custodianship of inherited restraint.').

omega_variable(
    living_vs_obsolete_reading_contest,
    'Is the living-constitutionalism reading itself a contestable reading, or is it the only defensible modern interpretation?',
    'Genealogy of modern challenges: do contemporary authorities still argue for feudal obsolescence or parliamentary-complete-absorption? Or has the contest been decisively won by living-constitutionalism? What would modern feudal-obsolescence look like as a live position?',
    'If feudal-obsolescence is a dead position, the reading has won and mandatrophy is low. If it remains a live position held by serious actors, the constraint''s future is still contested and mandatrophy is higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(living_vs_obsolete_reading_contest, empirical, 'Whether the living-constitutionalism reading remains contested or has become settled doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__living_constitutionalism_reading, 1215, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1215, 0.08).
narrative_ontology:measurement_basis(magn_tr_t1215, projected).
narrative_ontology:measurement(magn_tr_t1400, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1400, 0.11).
narrative_ontology:measurement_basis(magn_tr_t1400, projected).
narrative_ontology:measurement(magn_tr_t1688, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1688, 0.14).
narrative_ontology:measurement_basis(magn_tr_t1688, projected).
narrative_ontology:measurement(magn_tr_t1832, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1832, 0.16).
narrative_ontology:measurement_basis(magn_tr_t1832, observed).
narrative_ontology:measurement(magn_tr_t1945, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1945, 0.17).
narrative_ontology:measurement_basis(magn_tr_t1945, observed).
narrative_ontology:measurement(magn_tr_t2025, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 2025, 0.18).
narrative_ontology:measurement_basis(magn_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1215, 0.15).
narrative_ontology:measurement_basis(magn_be_t1215, projected).
narrative_ontology:measurement(magn_be_t1400, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1400, 0.22).
narrative_ontology:measurement_basis(magn_be_t1400, projected).
narrative_ontology:measurement(magn_be_t1688, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1688, 0.28).
narrative_ontology:measurement_basis(magn_be_t1688, projected).
narrative_ontology:measurement(magn_be_t1832, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1832, 0.32).
narrative_ontology:measurement_basis(magn_be_t1832, observed).
narrative_ontology:measurement(magn_be_t1945, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1945, 0.35).
narrative_ontology:measurement_basis(magn_be_t1945, observed).
narrative_ontology:measurement(magn_be_t2025, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 2025, 0.38).
narrative_ontology:measurement_basis(magn_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1215, 0.65).
narrative_ontology:measurement_basis(magn_su_t1215, projected).
narrative_ontology:measurement(magn_su_t1400, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1400, 0.58).
narrative_ontology:measurement_basis(magn_su_t1400, projected).
narrative_ontology:measurement(magn_su_t1688, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1688, 0.45).
narrative_ontology:measurement_basis(magn_su_t1688, projected).
narrative_ontology:measurement(magn_su_t1832, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1832, 0.28).
narrative_ontology:measurement_basis(magn_su_t1832, observed).
narrative_ontology:measurement(magn_su_t1945, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1945, 0.24).
narrative_ontology:measurement_basis(magn_su_t1945, observed).
narrative_ontology:measurement(magn_su_t2025, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 2025, 0.22).
narrative_ontology:measurement_basis(magn_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__living_constitutionalism_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_constraint_authority__living_constitutionalism_reading, 0.12).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__living_constitutionalism_reading, magna_carta_constraint_authority__feudal_obsolescence_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__living_constitutionalism_reading, magna_carta_constraint_authority__parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% Three constraint stories form a kernel family: the contested Magna Carta constraint authority. Each reading instantiates a different constraint with different ε values, beneficiary/victim structures, and type classifications. The feudal-obsolescence reading treats the constraint as inert (mountain-toward-piton, low extraction); the parliamentary-sovereignty reading treats it as absorbed into legislative authority (rope, moderate extraction); the living-constitutionalism reading (this one) treats it as actively interpreted by courts (rope, low-to-moderate extraction). All three stories must be authored to capture the full contest. Each story's network.affects_constraints array links to its sibling readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
