% ============================================================================
% CONSTRAINT STORY: state_killing_legitimacy__abolition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_legitimacy__abolition_reading, []).

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
 *   constraint_id: state_killing_legitimacy__abolition_reading
 *   human_readable: State Killing as Categorical Violation of Human Dignity (Abolition Reading)
 *   domain: criminal_justice/political_philosophy/legal_theory
 *
 * SUMMARY:
 *   The abolition reading of state killing legitimacy presents capital
 *   punishment as a categorical violation of human dignity that cannot be
 *   justified by desert, deterrence, or public safety rationales. This
 *   reading constitutes the condemned person as an inalienable rights-bearer
 *   whose fundamental dignity is violated by state killing regardless of the
 *   crime committed. From this perspective, the state killing apparatus is
 *   itself a victim — victim of the abolition constraint, which forecloses
 *   the state's claimed right to execute. The constraint exhibits snare
 *   classification from the powerless (condemned person), moderate (victim's
 *   family), institutional (killing authority), and moderate institutional
 *   (justice system) perspectives, piton from the long-term legitimacy
 *   apparatus view, and risks false summitry from the natural law
 *   perspective. The abolition reading is ONE of three sibling readings of
 *   the contested kernel 'state killing legitimacy': the retributive reading
 *   justifies execution through proportional desert (lex talionis), and the
 *   deterrence reading justifies it as a rational signal preventing future
 *   murders. These three readings coexist as live positions held by different
 *   parties in contemporary legal and political philosophy, though the
 *   abolition reading claims to foreclose the retributive reading's core
 *   premise (that desert can override inalienable rights).
 *
 * KEY AGENTS:
 *   - Condemned Persons (Rights-Bearers): Primary victims (powerless/trapped) — face categorical extraction of life itself; the abolition reading constitutes them as bearers of inalienable rights that cannot be forfeited regardless of crime
 *   - State Killing Authority: Institutional actor experiencing the constraint as victim — the abolition prohibition forecloses the authority's claimed right to execute; maintains suppression mechanisms (legal finality, death row procedures) to sustain killing capacity
 *   - Victim's Families: Competing dignity bearers (moderate/constrained) — abolition reading reframes their retributive justice claims as secondary to the condemned person's dignity right; face suppression of alternative justice framings
 *   - Criminal Justice System: Institutional actor (institutional/constrained) — performs genuine coordination functions (adjudication, proportional sentencing) but the abolition reading treats the killing apparatus as corrupting the system's entire legitimacy claim
 *   - Retributive Reading Community: Institutional actor (institutional/arbitrage) — holds that desert-based forfeiture justifies execution; the abolition reading's core axiom directly contradicts their foundational premise
 *   - Deterrence Reading Community: Institutional actor (institutional/arbitrage) — holds that execution deters murder; empirical refutation weakens but does not logically foreclose their position
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the abolition axiom as immutable law rather than contingent normative commitment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__abolition_reading, 0.82).
domain_priors:suppression_score(state_killing_legitimacy__abolition_reading, 0.88).
domain_priors:theater_ratio(state_killing_legitimacy__abolition_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__abolition_reading, snare).
narrative_ontology:human_readable(state_killing_legitimacy__abolition_reading, "State Killing as Categorical Violation of Human Dignity (Abolition Reading)").
narrative_ontology:topic_domain(state_killing_legitimacy__abolition_reading, "criminal_justice/political_philosophy/legal_theory").

domain_priors:requires_active_enforcement(state_killing_legitimacy__abolition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__abolition_reading, '17df7539-ba0c-4662-b9ff-7ac998a4d23f').
narrative_ontology:cs_kernel_codification('17df7539-ba0c-4662-b9ff-7ac998a4d23f', formalized).
narrative_ontology:cs_authority_grounding('17df7539-ba0c-4662-b9ff-7ac998a4d23f', lineage).
narrative_ontology:cs_interpretation_layer_present('17df7539-ba0c-4662-b9ff-7ac998a4d23f').
narrative_ontology:cs_reading_relation('17df7539-ba0c-4662-b9ff-7ac998a4d23f', state_killing_legitimacy__retributive_reading, forecloses).
narrative_ontology:cs_reading_relation('17df7539-ba0c-4662-b9ff-7ac998a4d23f', state_killing_legitimacy__deterrence_reading, coexists_with).
narrative_ontology:cs_axiom('17df7539-ba0c-4662-b9ff-7ac998a4d23f', foundational, human_dignity_inalienable).
narrative_ontology:cs_axiom_status(human_dignity_inalienable, holdable).
narrative_ontology:cs_axiom_grounding('17df7539-ba0c-4662-b9ff-7ac998a4d23f', human_dignity_inalienable, deontological).
narrative_ontology:cs_axiom('17df7539-ba0c-4662-b9ff-7ac998a4d23f', foundational, state_killing_corrupts_legitimacy).
narrative_ontology:cs_axiom_status(state_killing_corrupts_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('17df7539-ba0c-4662-b9ff-7ac998a4d23f', state_killing_corrupts_legitimacy, deontological).
narrative_ontology:cs_reference_frame('17df7539-ba0c-4662-b9ff-7ac998a4d23f', rights_respecting_state_model).
narrative_ontology:cs_drift_state('17df7539-ba0c-4662-b9ff-7ac998a4d23f', contemporary_retentionist_jurisdictions, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('17df7539-ba0c-4662-b9ff-7ac998a4d23f', '').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__abolition_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__abolition_reading, condemned_persons_dignity_bearers).
narrative_ontology:constraint_victim(state_killing_legitimacy__abolition_reading, condemned_persons_dignity_bearers).
narrative_ontology:constraint_victim(state_killing_legitimacy__abolition_reading, state_killing_authority_itself).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONDEMNED PERSON / RIGHTS-BEARER (SNARE) — Faces categorical extraction of life itself grounded not in proportional desert but in state killing power's own self-perpetuation. The condemned has no exit option; the constraint suppresses alternatives (clemency, commutation, rehabilitation framing) through legal and procedural barriers. Experiences maximum extraction: the state's killing power is enacted directly upon their person. The abolition reading constitutes them as rights-bearing subject whose dignity is violated regardless of the crime committed — this status cannot be forfeited or traded. Trapped exit option combined with high suppression (legal finality, irreversibility, social isolation) and high extraction (loss of life) produces snare classification.
constraint_indexing:constraint_classification(state_killing_legitimacy__abolition_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: VICTIM'S FAMILY / COMPETING DIGNITY CLAIMS (SNARE) — From the abolition reading, the family's claim to retributive justice is structurally reframed as a separate violation of human dignity. The constraint suppresses alternative framings (restorative justice, victim voice in non-lethal accountability) by channeling grief into the state killing mechanism. Constrained exit: the family can oppose execution but faces social pressure, institutional barriers, and the state's presumptive authority over justice. Experiences extraction through subordination of their own moral claims to the state's killing power. Moderate power and constrained exit (can organize, advocate, but at high cost) produces snare rather than mountain from this perspective.
constraint_indexing:constraint_classification(state_killing_legitimacy__abolition_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CRIMINAL JUSTICE SYSTEM / HYBRID COORDINATION-EXTRACTION (TANGLED ROPE) — The system performs genuine coordination functions: adjudication of guilt, proportional sentencing, incapacitation through imprisonment. But the killing apparatus layers asymmetric extraction onto this coordination. The abolition reading reframes execution as a violation that corrupts the entire system's legitimacy — the system cannot both uphold human dignity as a foundational principle AND practice categorical killing. Constrained exit at the generational level: individual judges and prosecutors face retaliation or career damage if they refuse participation; the institution can reform but only through legislative/constitutional change at multi-generational cost. Tangled rope classification reflects the genuine coordination function (legitimate punishment, public safety) embedded within an extractive mechanism (killing) that the abolition reading presents as incompatible with the coordination's own foundational premise (human dignity).
constraint_indexing:constraint_classification(state_killing_legitimacy__abolition_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE KILLING AUTHORITY / EXTRACTIVE POWER STRUCTURE (SNARE) — This perspective reveals the constraint from the position of the killing apparatus itself. The abolition reading treats the state's killing power as a victim — victim of its own perpetuation, its own claim to legitimacy. The state killing authority extracts legitimacy from the condemned person's death, and the authority's survival depends on suppressing alternative mechanisms (imprisonment, restorative justice, pardon). High suppression (legal monopoly on execution, closure of clemency pathways, death row finality) and high extraction (the killing power sustains itself through the act of killing) characterize this perspective. Arbitrage exit: the state can theoretically abolish capital punishment but faces institutional path-dependence and the vested interests of prosecutors, judges, and law enforcement in maintaining the power. The snare classification here reveals the constraint as fundamentally about the self-perpetuation of state killing capacity, not about punishment proportionality.
constraint_indexing:constraint_classification(state_killing_legitimacy__abolition_reading, snare,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGITIMACY APPARATUS / VESTIGIAL JUSTIFICATION (PITON) — From the long view, the state killing authority's justification apparatus (retributive theory, deterrence theory, incapacitation) is increasingly theatrical. Most abolition democracies have functionally abandoned capital punishment while retaining its legal form as symbolic power. The theater ratio here reflects the gap between the legitimacy claims made (justice, deterrence, proportionality) and the actual function performed (maintenance of state killing capacity and demonstration of state power over life). High theater (0.65) because the modern execution is increasingly a ritualized performance of state sovereignty rather than an active mechanism for public safety. The constraint persists through institutional inertia and symbolic necessity, not because the stated justifications remain coherent.
constraint_indexing:constraint_classification(state_killing_legitimacy__abolition_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, the abolition reading presents human dignity as an immutable principle that cannot be forfeited, traded, or overridden by state interest. This perspective classifies the constraint as a natural law: the categorical prohibition on state killing is not contingent on policy preferences or utilitarian calculation but follows from the irreducible status of human persons as rights-bearing entities. However, this perspective risks false summitry — the 'natural law' framing naturalizes what is actually a contestable normative commitment (the abolition axiom). The engine's false summit detector will identify this as a naturalization of the axiom rather than a discovery of natural law.
constraint_indexing:constraint_classification(state_killing_legitimacy__abolition_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_legitimacy__abolition_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(state_killing_legitimacy__abolition_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(state_killing_legitimacy__abolition_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_legitimacy__abolition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(state_killing_legitimacy__abolition_reading, TR),
    TR >= 0.70.

:- end_tests(state_killing_legitimacy__abolition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82): Very high. The abolition reading presents execution as the maximal form of state extraction — the taking of human life itself grounded in state power's self-perpetuation rather than legitimate punishment. The constraint's extractiveness rises over the interval (0.68 → 0.82) as suppression mechanisms intensify (death row procedures, legal finality, clemency restrictions eliminate alternatives). The extractiveness value reflects that the abolition reading treats execution as categorical violation, not as a proportional response to crime. Suppression (0.88): Very high. The constraint suppresses alternatives through multiple mechanisms: legal closure (death row finality, restricted clemency), institutional barriers (prosecutor discretion, appellate deference), and conceptual suppression (reframing retributive justice claims as secondary to dignity rights). Theater ratio (0.65): Moderate-high. The legitimacy apparatus surrounding execution has become increasingly theatrical in abolition-trajectory democracies. Justification through deterrence, proportionality, and public safety coexists with evidence that these rationales do not drive actual execution practices (which are determined by geography, prosecutor discretion, race, and class). The theater increases over time (0.52 → 0.65) as the empirical gaps between stated justifications and actual functions become more visible. Mandatrophy (resolved): The abolition reading resolves the mandatrophy by presenting execution as incompatible with the state's foundational legitimacy claim (rights protection). The state cannot coherently claim to be a rights-protecting institution while practicing categorical killing. This resolution is structural, not empirical — it depends on accepting the abolition axiom that rights are inalienable and dignity cannot be forfeited.
 *
 * PERSPECTIVAL GAP:
 *   The abolition reading produces maximum perspectival gaps because it reframes the entire legitimacy structure of capital punishment. The condemned person's perspective (snare) and the killing authority's perspective (snare) converge on the same classification but for opposite reasons: the condemned experiences maximum extraction; the authority experiences the constraint as victim of the prohibition. The justice system perspective (tangled rope) reveals the internal contradiction within the system's legitimacy claim: it performs genuine coordination functions (adjudication, proportional sentencing) but layers an extractive killing apparatus onto this coordination that the abolition reading treats as fundamentally incompatible with the coordination's own foundation (human dignity). The retributive reading community would classify the same constraint as rope or even scaffold (a temporary problem solvable through refined proportionality standards), but the abolition reading treats retributive justification as logically foreclosed by the axiom that rights are inalienable. The deterrence reading community would classify as scaffold (execution as a contingent means to the end of crime prevention), but the abolition reading treats deterrence efficacy as irrelevant to the categorical prohibition. The piton perspective reveals the constraint's increasingly theatrical character: legitimation rationales persist despite empirical gaps because the state killing apparatus sustains itself through institutional inertia and symbolic necessity. The natural law perspective risks false summitry by presenting the abolition axiom as discovered immutable principle rather than contingent normative commitment.
 *
 * DIRECTIONALITY LOGIC:
 *   The abolition reading's directionality structure is complex because it redefines who is beneficiary and who is victim. Standard retributive and deterrence readings treat the state (and society through crime prevention) as beneficiary and the condemned as victim. The abolition reading reverses this: the condemned person's dignity (the inalienable right to life) is the beneficiary of the constraint, and the state killing authority is the victim (of the prohibition). This directional inversion produces high extractiveness from the killing authority's perspective (it experiences the prohibition as extraction of its claimed right) while the condemned person simultaneously benefits from the constraint's protection of their dignity and suffers extraction through the state's resistance to abolition. The powerless condemned person's high d value (0.95 in the trapped exit case) reflects that they bear maximum cost of the killing apparatus's resistance to the constraint. The institutional killing authority's moderate d value (0.40-0.60 constrained exit) reflects their institutional position: they can theoretically adopt abolition but face path-dependence and vested interests. The justice system's d value (0.55-0.65 constrained exit) reflects the mixed position: the system benefits from coordination functions but experiences the constraint as corrupting to its legitimacy claim.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    desert_forfeiture_dispute,
    'Can a person forfeit their right to life through committing murder, or is the right to life inalienable regardless of desert?',
    'This is a foundational axiom dispute between the abolition and retributive readings. No empirical data resolves it — it is a conceptual question about the structure of rights and personhood. Resolution comes through coherence analysis: which framework (inalienable rights vs. desert-contingent rights) produces fewer internal contradictions when applied consistently across cases.',
    'If life-right is inalienable: abolition reading''s entire structure holds; retributive reading is logically foreclosed. If life-right is forfeitable: retributive reading becomes viable; abolition reading must treat desert-forfeiture as a false legitimation claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(desert_forfeiture_dispute, conceptual, 'Whether human rights to life are inalienable or forfeitable through desert').

omega_variable(
    state_dignity_corruption,
    'Does the state''s practice of killing under law corrupt its own legitimacy claim to be a rights-protecting institution, or is killing a legitimate state function that can coexist with rights protection in other domains?',
    'Coherence analysis: Can a state simultaneously claim to protect human dignity as a foundational principle AND practice categorical killing? This is a question about the internal consistency of state legitimacy frameworks, not an empirical question. The abolition reading treats this as incoherent; the retributive and deterrence readings treat it as coherent. Resolution depends on which framework one adopts.',
    'If state killing corrupts state legitimacy: abolition reading''s account of the constraint as self-referentially contradictory holds. If state can legitimately kill while protecting rights in other domains: retributive and deterrence readings remain coherent alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_dignity_corruption, conceptual, 'Whether state killing corrupts the state''s legitimacy as a rights-protecting institution').

omega_variable(
    deterrence_efficacy_empirical_binding,
    'Does empirical evidence of deterrence failure decisively rebut the deterrence reading''s justification, or do deterrent effects persist at a level that sustains the deterrence reading''s coherence?',
    'Meta-analysis of deterrence studies; historical comparison of jurisdictions before/after abolition for homicide rate changes; international cross-sectional analysis of execution rates vs. murder rates.',
    'If empirical evidence decisively falsifies deterrence claims: the deterrence reading loses its primary justification, weakening its position in the kernel contest. The abolition reading''s position strengthens (one sibling justification is eliminated). If deterrence effects are real but modest: the deterrence reading remains coherent as a policy position, and the kernel contest remains contested.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deterrence_efficacy_empirical_binding, empirical, 'Whether execution has measurable deterrent effect on homicide rates').

omega_variable(
    reading_asymmetry_reversibility,
    'Can a jurisdiction that abolishes capital punishment later restore it without internal contradiction, or does abolition represent an irreversible moral commitment?',
    'Historical analysis of jurisdictions that abolished and later restored capital punishment (e.g., France 1981-1993, US state-level reversals); analysis of stated reasons for restoration or non-restoration in contemporary abolition debates.',
    'If abolition is reversible: the reading represents a policy preference, not a foundational axiom. If abolition is presented as irreversible: the reading claims to instantiate a permanent moral commitment. The irreversibility claim affects how the abolition reading relates to its siblings (does it foreclose or coexist?).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_asymmetry_reversibility, conceptual, 'Whether abolition of capital punishment is irreversible or represents contingent policy choice').

omega_variable(
    axiom_grounding_mixed_type,
    'Is the abolition axiom (state killing categorically violates human dignity) grounded primarily in deontological principle (inalienable rights) or in institutional practice and convention (the dignity-respecting state as a historically emergent form)?',
    'Textual/doctrinal analysis of abolition traditions: do they ground themselves in natural rights philosophy (deontological) or in the practice of rights-respecting states (conventional)? Both groundings are present in contemporary abolition theory.',
    'If deontological: the axiom is not subject to empirical refutation or policy reversal. If conventional: the axiom can be revised if state practice changes. The grounding affects how the axiom relates to drift (can it be overridden by practice drift, or does practice drift against the axiom undermine the state''s legitimacy rather than the axiom itself?).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(axiom_grounding_mixed_type, conceptual, 'Whether the abolition axiom is grounded in deontology or institutional convention').

omega_variable(
    condemn_person_as_victim_status,
    'In the abolition reading, is the condemned person classified as a beneficiary or a victim of the constraint? The kernel context indicates ''condemned person as rights-bearer beneficiary,'' but the structural logic treats them as target of extraction.',
    'Clarify the directional interpretation: the constraint protects the condemned person''s dignity (beneficiary function), but the mechanism of this protection is enforcement of a prohibition on state killing (the condemned person is not the direct beneficiary of enforcement — the prohibition is enforced FOR them, not BY them). The structural data should distinguish between the beneficiary of the constraint''s normative force (dignity-as-inalienable) and the agent who benefits from its enforcement.',
    'If condemned person is beneficiary: directionality derivation shifts toward lower d for institutional perspectives that would enforce abolition. If condemned person is victim/target: directionality remains high extraction for the perpetrator perspective (state killing authority). The omega clarifies this distinction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(condemn_person_as_victim_status, conceptual, 'Whether condemned person is beneficiary or victim of the abolition constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__abolition_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_legitimacy__abolition_reading, theater_ratio, 0, 0.52).
narrative_ontology:measurement(stat_tr_t2, state_killing_legitimacy__abolition_reading, theater_ratio, 2, 0.58).
narrative_ontology:measurement(stat_tr_t4, state_killing_legitimacy__abolition_reading, theater_ratio, 4, 0.62).
narrative_ontology:measurement(stat_tr_t6, state_killing_legitimacy__abolition_reading, theater_ratio, 6, 0.65).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_legitimacy__abolition_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(stat_be_t2, state_killing_legitimacy__abolition_reading, base_extractiveness, 2, 0.75).
narrative_ontology:measurement(stat_be_t4, state_killing_legitimacy__abolition_reading, base_extractiveness, 4, 0.8).
narrative_ontology:measurement(stat_be_t6, state_killing_legitimacy__abolition_reading, base_extractiveness, 6, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_legitimacy__abolition_reading, suppression_requirement, 0, 0.82).
narrative_ontology:measurement(stat_su_t3, state_killing_legitimacy__abolition_reading, suppression_requirement, 3, 0.85).
narrative_ontology:measurement(stat_su_t6, state_killing_legitimacy__abolition_reading, suppression_requirement, 6, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_legitimacy__abolition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_legitimacy__abolition_reading, state_killing_legitimacy__retributive_reading).
narrative_ontology:affects_constraint(state_killing_legitimacy__abolition_reading, state_killing_legitimacy__deterrence_reading).

% DUAL FORMULATION NOTE:
% The kernel 'state_killing_legitimacy' decomposes into three constraint stories, one per reading. Each reading instantiates a distinct ε value and beneficiary/victim structure. The abolition reading (this file) has ε=0.82 (snare classification), treats execution as categorical violation. The retributive reading has lower ε (proportional punishment justification, not pure extraction) and different beneficiary/victim structure (murderer forfeits right through desert). The deterrence reading has contingent ε (depends on empirical deterrence effects) and treats execution as a means to crime prevention, not as extraction. All three are linked via network.affects_constraints and share the same kernel_id. The sibling readings are structurally distinct constraints with different temporal dynamics, different compression of evidence, and different logical structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
