% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__living_constitutionalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_constraint_authority__living_constitutionalism_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: magna_carta_constraint_authority__living_constitutionalism_reading
 *   human_readable: Magna Carta Constraint Authority (Living Constitutionalism Reading)
 *   domain: constitutional_history/legal_philosophy/political_theory
 *
 * SUMMARY:
 *   This constraint is the living constitutionalist reading of Magna Carta:
 *   the charter establishes inherited, binding restraint on all subsequent
 *   rulers through the mechanism of juridical precedent and evolutionary
 *   interpretation. Under this reading, the charter is not a feudal compact
 *   frozen in 1215 but a fundamental law that each generation of judges and
 *   parliamentarians reinterprets to bind new exercises of executive power.
 *   The constraint's operation is coordination around the principle that
 *   rulers must act within law, not above it. Extractiveness is
 *   low-to-moderate because the coordination function is genuine (all parties
 *   benefit from rule-of-law predictability) and the suppression required is
 *   diminishing over time as the constraint becomes normalized and
 *   internalized. The reading stands opposed to a feudal-obsolescence reading
 *   (which treats the charter as a dead historical artifact with no binding
 *   authority) and a parliamentary-sovereignty reading (which absorbs the
 *   charter's restraint into parliament's own revocable statute law). This
 *   constraint measures the living constitutionalist instantiation alone.
 *
 * KEY AGENTS:
 *   - Subjects as rights-bearers (powerless, identity-locked to the realm; beneficiary)
 *   - Crown/Executive authority (institutional, constrained exit; payer)
 *   - Judiciary (institutional, arbitrage-capable; beneficiary and agenda-setter)
 *   - Parliament (institutional, mobile exit; beneficiary and agenda-setter)
 *   - Subsequent rulers (institutional, constrained by inheritance; payers)
 *   - Feudal nobility (powerful, now excluded as the constraint universalizes)
 *   - Legal scholars and constitutionalists (analytical observers mediating interpretation)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__living_constitutionalism_reading, 0.28).
domain_priors:suppression_score(magna_carta_constraint_authority__living_constitutionalism_reading, 0.15).
domain_priors:theater_ratio(magna_carta_constraint_authority__living_constitutionalism_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__living_constitutionalism_reading, rope).
narrative_ontology:human_readable(magna_carta_constraint_authority__living_constitutionalism_reading, "Magna Carta Constraint Authority (Living Constitutionalism Reading)").
narrative_ontology:topic_domain(magna_carta_constraint_authority__living_constitutionalism_reading, "constitutional_history/legal_philosophy/political_theory").

domain_priors:requires_active_enforcement(magna_carta_constraint_authority__living_constitutionalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__living_constitutionalism_reading, 'd8ebcfc7-5f49-4013-a5f4-ffd0413cc754').
narrative_ontology:cs_kernel_codification('d8ebcfc7-5f49-4013-a5f4-ffd0413cc754', fixed_text).
narrative_ontology:cs_authority_grounding('d8ebcfc7-5f49-4013-a5f4-ffd0413cc754', lineage).
narrative_ontology:cs_interpretation_layer_present('d8ebcfc7-5f49-4013-a5f4-ffd0413cc754').
narrative_ontology:cs_reading_relation('d8ebcfc7-5f49-4013-a5f4-ffd0413cc754', magna_carta_constraint_authority__feudal_obsolescence_reading, coexists_with).
narrative_ontology:cs_reading_relation('d8ebcfc7-5f49-4013-a5f4-ffd0413cc754', magna_carta_constraint_authority__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('d8ebcfc7-5f49-4013-a5f4-ffd0413cc754', foundational, inherited_constitutional_restraint_binding).
narrative_ontology:cs_axiom_status(inherited_constitutional_restraint_binding, holdable).
narrative_ontology:cs_axiom_grounding('d8ebcfc7-5f49-4013-a5f4-ffd0413cc754', inherited_constitutional_restraint_binding, conventional).
narrative_ontology:cs_axiom('d8ebcfc7-5f49-4013-a5f4-ffd0413cc754', foundational, evolutionary_interpretation_legitimate).
narrative_ontology:cs_axiom_status(evolutionary_interpretation_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('d8ebcfc7-5f49-4013-a5f4-ffd0413cc754', evolutionary_interpretation_legitimate, conventional).
narrative_ontology:cs_reference_frame('d8ebcfc7-5f49-4013-a5f4-ffd0413cc754', charter_as_binding_precedent).
narrative_ontology:cs_drift_state('d8ebcfc7-5f49-4013-a5f4-ffd0413cc754', contemporary_constitutional_practice, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d8ebcfc7-5f49-4013-a5f4-ffd0413cc754', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__living_constitutionalism_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__living_constitutionalism_reading, subjects_as_rights_bearers).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__living_constitutionalism_reading, judicial_power).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__living_constitutionalism_reading, parliamentary_institutions).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__living_constitutionalism_reading, absolute_executive_prerogative).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__living_constitutionalism_reading, arbitrary_sovereign_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__living_constitutionalism_reading, subsequent_rulers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain a shield against arbitrary executive action and unlawful detention. Their security depends on the constraint's persistence as a binding precedent across generations. Exit is impossible — they cannot leave the realm; their stake is constitutive of their identity as subjects under law rather than arbitrary will. The constraint secures their claim to due process without requiring active vigilance on their part.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, subjects_as_rights_bearers, beneficiary,
    powerless, generational, identity_locked, national).

% Gains independence from executive override and authority to interpret the charter's restraints evolutionarily. Courts become the institutional seat that reads the charter forward through generations, updating its meaning to new circumstances while claiming to preserve its original intent. Judges accumulate interpretive power precisely by maintaining the fiction that they are merely discovering the charter's timeless meaning.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, judicial_power, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_constraint_authority__living_constitutionalism_reading, judicial_power, agenda_setter).

% Gain legislative authority grounded in the charter's principle of restraint-through-law. Parliament becomes the body that enacts binding statute law and can claim constitutional legitimacy by reference to the inherited charter. They accumulate power to make law while remaining theoretically subordinate to a fundamental document — a covering narrative that lets them expand authority under the banner of defending tradition.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, parliamentary_institutions, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_constraint_authority__living_constitutionalism_reading, parliamentary_institutions, agenda_setter).

% The crown's formerly unlimited power to rule by will alone bears the cost of the constraint. Prerogative survives in residual form but is persistently narrowed by judicial interpretation and parliamentary statute. The monarchy's freedom of action faces continuous boundary renegotiation; what was once unfettered discretion becomes discretion exercised 'within the law.' The exit from this constraint — reasserting absolute prerogative — becomes increasingly costly as constitutional practice solidifies.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, absolute_executive_prerogative, payer,
    institutional, civilizational, constrained, national).

% Is the abstract principle of unfettered sovereign will. This is not an agent but rather a configuration the constraint forecloses. As the constraint operates, the sovereign's claim to be above law becomes progressively harder to assert without facing resistance. The cost is not paid by any one actor but distributed across all attempts to escape the constraint's binding force.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, arbitrary_sovereign_authority, payer,
    institutional, civilizational, trapped, national).

% Each new monarch inherits the obligation to rule within the charter's restraints. They cannot revoke or ignore the charter without triggering resistance and loss of legitimacy. Their freedom to govern is calibrated to this binding document. The constraint persists not because any single ruler chooses it but because the alternative — open repudiation of the charter — would cost more in legitimacy and resistance than compliance.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, subsequent_rulers, payer,
    institutional, generational, constrained, national).

% Analyze the charter's meaning and trace its evolution through case law and statutory development. They mediate between the historical charter and contemporary constitutional practice. Their readings become the intellectual grounds on which judges and parliamentarians base new interpretations.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, legal_scholars_and_constitutionalists, observer,
    analytical, generational, analytical, national).

% Were originally the charter's direct beneficiaries and framers in 1215 but became progressively excluded from the constraint's operation as it was reinterpreted away from baronial privilege toward universal due process. Their original interest in restraining royal prerogative was absorbed into a principle that now applies equally to all subjects, not just the magnates. They have no exit.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, feudal_nobility_and_baronage, excluded,
    powerful, civilizational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_constraint_authority__living_constitutionalism_reading, judicial_power).
narrative_ontology:fixing_cost_class(magna_carta_constraint_authority__living_constitutionalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a binding framework that all rulers — including the initially exempt crown — must operate within law rather than by fiat. This solves the collective-action problem of arbitrary rule: subjects gain security from capricious punishment and seizure; rulers gain predictability and legitimacy; legal institutions gain authority to mediate conflicts without deferring to raw power. The coordination is not about what the law says but that there IS law that binds everyone.
% TRANSFER_FUNCTION: Transfers restraint-bearing capacity from the crown (which loses absolute discretion) to legal institutions (which gain interpretive authority) and subjects (who gain rights-claims). What moves is legitimacy: the right to rule becomes conditional on ruling within the charter's constraints. As the charter is reinterpreted, power shifts from executive will to judicial interpretation and parliamentary statute-making.
% ABSENT_VOICES: Feudal magnates who originally negotiated the charter are progressively excluded as it is universalized; they have no seat at the table of modern constitutional interpretation. Non-property-holding subjects (women, the poor, non-landowners) were excluded from the original compact and remain absent from the constraint's operative framing even as it expands in principle to protect all subjects. Rival executive traditions from non-English common-law systems have no voice in interpreting this charter.
% DISAPPEARANCE_RATIONALE: If the charter's binding force vanished overnight, executives would revert to unfettered prerogative, subjects would lose their constitutional shield against arbitrary action, judicial authority would collapse, and parliamentary sovereignty would rest on no inherited foundation. The entire constitutional order that developed through centuries of interpretation would lose its legitimizing precedent. Subjects and institutions organized around the charter's protections would face immediate renegotiation of their rights and constraints.
% FOUNDING_PROBLEM: Unbounded royal prerogative allowed arbitrary detention, seizure of property, and rule by will rather than law. Magnates and subjects had no recourse against unfettered crown discretion. The charter was built to solve this by making the crown subject to written constraints that could not be revoked unilaterally.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians and legal scholars outside the conservative legal establishment attest that the original founding problem (arbitrary prerogative) was substantially constrained over centuries of parliamentary development and judicial interpretation. Modern executive practice in constitutional democracies shows executive power significantly restrained by law, statute, and judicial review — corroborating that the constraint's original problem was solved. However, scholars and civil liberties advocates continue to dispute whether the constraint remains live: emergency powers, executive privilege claims, and surveillance authorities suggest the problem partially persists in modern form.
narrative_ontology:disappearance_verdict(magna_carta_constraint_authority__living_constitutionalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_constraint_authority__living_constitutionalism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__living_constitutionalism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(magna_carta_constraint_authority__living_constitutionalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_constraint_authority__living_constitutionalism_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness starts high (0.65 at t=0, immediately after charter enactment) because the crown views the charter as baronial impositions limiting its prerogative — a costly restraint on power it formerly exercised freely. Over 800 years, extractiveness declines sharply (to 0.28 at t=800) as the charter becomes internalized as the normal legal framework, as parliamentary development absorbs and expands the principle of restraint, and as judicial interpretation layers the charter's protection across an expanding domain of rights. Suppression follows a similar trajectory: initially high (0.72) because the crown must actively resist or suppress the charter's binding claim (civil war, repudiation attempts, assertions of prerogative override), suppression falls dramatically (to 0.15) as the constraint becomes part of constitutional tradition and new rulers inherit the assumption that they rule under law. Theater remains low and stable (0.08–0.12) throughout, indicating that the coordination function is genuine: judges actually interpret cases, parliament actually enacts laws binding the crown, subjects actually have recourse through courts — the constraint is not maintained by pure performative activity. The measurement series author one shared time grid, with every metric at every examined time point, enabling temporal analysis of the constraint's normalization.
 *
 * PERSPECTIVAL GAP:
 *   From the crown's seat (t=0), the charter appears as an external imposition, a loss of prerogative, a reduction in decision-making freedom — a snare-like extraction of power. From the judicial seat, the charter appears as a coordinate authority structure: courts are given power to interpret and apply it, judges become constitutional authorities rather than mere royal agents. From the subject's seat, the charter appears as pure benefit: a shield they never had to negotiate for, whose costs (restraint of executive) are borne by the crown, and whose benefits (due process) accrue to them. The engine's per-seat classification should diverge sharply: from the executive's position, the constraint may compute as tangled-rope or snare (coordination cover for extraction); from the judicial and subject seats, it should compute as rope (genuine coordination with modest asymmetry favoring judges). This divergence is the signal that the constraint's structure contains genuinely incompatible interests — not everyone benefits equally.
 *
 * DIRECTIONALITY LOGIC:
 *   Subjects gain due process shield without active participation — they are beneficiaries by receiving a right-claim against arbitrary action, identity-locked to the realm, powerless to negotiate differently. The crown (as absolute executive prerogative) is the target: it loses unfettered discretion and must submit to law. Judges and parliament split: they are beneficiaries because they gain authority to interpret and enact law (compared to their pre-charter subordination), but they are also agenda-setters because their interpretive power IS the mechanism by which the constraint persists and evolves. The feudal nobility, originally beneficiaries, become excluded as the charter's meaning shifts from baronial privilege to universal due process — their original structural position is erased. The directionality of the executive is unambiguous: d near 1.0 (full target) because the constraint's central operation is restraining executive will. Judges and parliament sit near 0.3–0.4 (beneficiary-leaning): they collect authority and legitimacy but do not bear the extraction cost the executive pays.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unbounded prerogative enabling arbitrary rule) was live at t=0 and substantially solved by t=800 (executives now rule under law, subject to parliamentary statute and judicial review). However, the constraint persists and indeed expands in scope — modern executives invoke emergency powers, claim state secrets privilege, and attempt unilateral action in ways the bare charter does not obviously prevent. This suggests a mandatrophy dynamic: the original problem is dead (arbitrary prerogative without any legal constraint is gone), but the constraint persists as a principle that now governs a much broader domain (all government action, not just crown discretion in feudal contexts). The constraint has NOT become a piton: extractiveness has declined, suppression has declined, and the constraint is actively interpreted and applied rather than theatrically maintained. Instead, the constraint has become a generative principle: its solution to the original problem (rule under law) now extends to problems that did not exist in 1215 (administrative state, executive emergency powers, surveillance). The mandatrophy is resolved because the constraint's persistence is justified not by the founding problem alone but by continuous generative reinterpretation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    living_vs_fixed_interpretation,
    'Does the constraint derive its binding authority from the original 1215 charter''s fixed meaning, or from the evolutionary interpretations judges and parliaments have layered onto it?',
    'Examine case law and constitutional doctrine to see whether courts defer to historical intent or treat the charter as a living text whose meaning expands with new circumstances. If courts cite modern applications not obviously present in 1215, evolutionary interpretation is operative.',
    'If fixed: the constraint is narrower and declines over time as modern problems diverge from 13th-century grievances (moving toward piton or feudal-obsolescence reading). If evolutionary: the constraint is generative and expands with new exercises of power (supporting the living-constitutionalism reading).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(living_vs_fixed_interpretation, conceptual, 'Whether the constraint''s authority is grounded in textual fixity or interpretive evolution.').

omega_variable(
    charter_binding_vs_parliamentary_absorption,
    'Does the charter''s restraint persist as an independent constitutional principle that binds parliament itself, or has parliament absorbed the charter''s content into statute law that parliament can revise?',
    'Trace whether courts have struck down or limited parliamentary statutes on the grounds that they violate charter principles. If courts refuse to enforce even parliamentary acts that violate due process or fundamental rights grounded in the charter, the charter retains independent authority (living-constitutionalism reading). If parliament can repeal any charter-derived protection by statute, the charter has been absorbed into parliamentary sovereignty.',
    'If independent: the constraint is genuinely binding on parliament (supports rope-type coordination with enforceable limits). If absorbed: parliament''s choice is the only true restraint (supports parliamentary-sovereignty reading, and the constraint becomes a piton maintained by parliamentary choice rather than inherited obligation).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(charter_binding_vs_parliamentary_absorption, empirical, 'Whether the charter retains constitutional force independent of parliamentary statute.').

omega_variable(
    feudal_vs_universal_scope,
    'Does the constraint operate as a protection for all subjects equally, or primarily as a restraint on crown prerogative in contexts that affect feudal property and baronial interests?',
    'Examine the charter''s application to modern subjects without property: do the due process and restraint principles apply to welfare recipients, prisoners, detainees, and others the original charter did not contemplate? If courts extend charter protections universally, it is universal; if application remains narrower, it retains feudal-era boundaries.',
    'If universal: the constraint has been successfully reinterpreted across 800 years as a binding principle for all governance (supporting living-constitutionalism). If feudal-bounded: the constraint''s expansion is narrower than the reading claims, and modern subjects gain less protection than the generative interpretation suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feudal_vs_universal_scope, empirical, 'Whether the constraint''s protections are universal or bounded to feudal-property contexts.').

omega_variable(
    reading_vs_sibling_foreclosure,
    'Do the living-constitutionalism reading''s core premises logically rule out the sibling readings, or can competing readings coexist as different parties'' constitutional narratives?',
    'Examine whether a single institutional actor (a court or parliament) can hold both the living-constitutionalism reading and a sibling reading simultaneously, or whether commitment to one reading necessarily forecloses the others. If institutional actors genuinely hold different readings in parallel, readings coexist; if one reading''s victory in courts or parliament causes the others to vanish, the winning reading forecloses.',
    'If coexistence: the readings are alternative framings of the same constraint, each held by different factions or parties (the constraint has multiple simultaneous instantiations). If foreclosure: one reading will eventually triumph and the others will be formally rejected (the constraint''s true type will be settled).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_sibling_foreclosure, conceptual, 'Whether sibling readings can coexist or whether commitment to one forecloses the others.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__living_constitutionalism_reading, 0, 800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t0, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(magn_tr_t100, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 100, 0.09).
narrative_ontology:measurement(magn_tr_t200, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 200, 0.1).
narrative_ontology:measurement(magn_tr_t400, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 400, 0.11).
narrative_ontology:measurement(magn_tr_t600, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 600, 0.11).
narrative_ontology:measurement(magn_tr_t800, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 800, 0.12).

% Extraction over time
narrative_ontology:measurement(magn_be_t0, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(magn_be_t100, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 100, 0.58).
narrative_ontology:measurement(magn_be_t200, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 200, 0.48).
narrative_ontology:measurement(magn_be_t400, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 400, 0.38).
narrative_ontology:measurement(magn_be_t600, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 600, 0.32).
narrative_ontology:measurement(magn_be_t800, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 800, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t0, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(magn_su_t100, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 100, 0.68).
narrative_ontology:measurement(magn_su_t200, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 200, 0.58).
narrative_ontology:measurement(magn_su_t400, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 400, 0.42).
narrative_ontology:measurement(magn_su_t600, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 600, 0.28).
narrative_ontology:measurement(magn_su_t800, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 800, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__living_constitutionalism_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_constraint_authority__living_constitutionalism_reading, 0.12).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__living_constitutionalism_reading, magna_carta_constraint_authority__feudal_obsolescence_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__living_constitutionalism_reading, magna_carta_constraint_authority__parliamentary_sovereignty_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__living_constitutionalism_reading, due_process_doctrine_common_law).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__living_constitutionalism_reading, rule_of_law_constraint_judicial_review).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Magna Carta kernel alongside feudal_obsolescence_reading and parliamentary_sovereignty_reading. The three readings share a referent (the charter text and its historical operation) but diverge on whether the constraint is bindingly evolutionary (living-constitutionalism), historically obsolete (feudal-obsolescence), or absorbed into parliament's authority (parliamentary-sovereignty). Each reading instantiates a distinct constraint with different ε values, beneficiary/victim structures, and types. The three are linked as a constraint family via network.affects_constraints to enable cross-reading analysis of how the kernel's interpretation shifted over 800 years and how the three readings' relative dominance changed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(magna_carta_constraint_authority__living_constitutionalism_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
