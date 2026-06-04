% ============================================================================
% CONSTRAINT STORY: reserved_powers_amendments__ninth_amendment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reserved_powers_amendments__ninth_amendment, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: reserved_powers_amendments__ninth_amendment
 *   human_readable: Ninth Amendment Unenumerated Rights Doctrine
 *   domain: political/constitutional/legal
 *
 * SUMMARY:
 *   The Ninth Amendment instantiates one reading of the
 *   reserved_powers_amendments kernel — the claim that rights not enumerated
 *   in the Constitution are retained by the people and protected from
 *   government infringement. This reading competes with the Tenth Amendment
 *   reading, which focuses on reserved powers to states and people rather
 *   than rights. The Ninth Amendment declares: 'The enumeration of certain
 *   rights shall not be construed to deny or disparage others retained by the
 *   people.' This text creates a structural constraint: it coordinates the
 *   principle that enumeration does not exhaust rights, while simultaneously
 *   suppressing the competing principle (enumeration-as-exhaustion) that
 *   strict interpreters have advanced since the Founding. The constraint
 *   exhibits asymmetric extraction: beneficiaries are claimants of unlisted
 *   liberties (ACLU, privacy advocates, libertarians defending unarticulated
 *   freedoms), who benefit from a doctrine that recognizes rights beyond the
 *   Bill of Rights. Victims are strict enumeration interpreters
 *   (originalists, constitutional minimalists, Scalia-aligned textualists),
 *   who benefit from a doctrine treating the Ninth as meaningless theater.
 *   The extractiveness score (0.38) reflects the intermediate status: the
 *   Ninth Amendment is sometimes enforceable (Griswold v. Connecticut
 *   grounded privacy in the Ninth, though later cases backed away), sometimes
 *   ignored (contemporary originalists treat it as a nullity), and
 *   perpetually contested. The theater ratio (0.65) reflects that significant
 *   jurisprudential effort has gone into explaining why the Ninth Amendment
 *   either is or is not enforceable — elaborate theater to naturalize what is
 *   actually a doctrinal choice.
 *
 * KEY AGENTS:
 *   - Claimants of Unlisted Liberties (powerless/trapped): Citizens asserting unenumerated rights (privacy, bodily autonomy, freedom of association beyond First Amendment scope). Bear full extraction cost when courts dismiss their claims as lacking textual hook. Cannot exit the ambiguity the Ninth Amendment creates.
 *   - Originalist & Textualist Interpreters (institutional/arbitrage): Scholars and judges treating the Ninth Amendment as a non-justiciable interpretive principle, thereby suppressing unenumerated rights doctrine. Beneficiaries — they extract by redefining the Amendment as meaningless. Arbitrage exit: they can deploy strict enumeration doctrine across constitutional disputes.
 *   - Civil Rights & Libertarian Coalitions (organized/mobile): ACLU, Institute for Justice, libertarian legal scholars. Beneficiaries — they benefit from a Ninth Amendment doctrine that opens space for unlisted rights claims. Mobile exit: they can litigate or shift strategies.
 *   - Appellate Judiciary (institutional/constrained): Courts face the constraint of having to decide which unlisted rights qualify. Active enforcement required but structurally contested. Constrained by precedent and the Ninth's textual silence on which rights are protected.
 *   - Originalist Academic Establishment (institutional/arbitrage): Federalist Society, originalist scholars. Institutional beneficiary of the piton perspective — they maintain elaborate argumentation for why the Ninth Amendment is unenforceable, theater that has decayed as the arguments are increasingly recognized as motivated reasoning.
 *   - Analytical Observer (analytical/analytical): Sees the Ninth Amendment as expressing a necessary logical principle (enumeration ≠ exhaustion) that appears mountain-like but is actually contested institutional terrain — a false summit.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reserved_powers_amendments__ninth_amendment, 0.38).
domain_priors:suppression_score(reserved_powers_amendments__ninth_amendment, 0.52).
domain_priors:theater_ratio(reserved_powers_amendments__ninth_amendment, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reserved_powers_amendments__ninth_amendment, extractiveness, 0.38).
narrative_ontology:constraint_metric(reserved_powers_amendments__ninth_amendment, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(reserved_powers_amendments__ninth_amendment, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reserved_powers_amendments__ninth_amendment, tangled_rope).
narrative_ontology:human_readable(reserved_powers_amendments__ninth_amendment, "Ninth Amendment Unenumerated Rights Doctrine").
narrative_ontology:topic_domain(reserved_powers_amendments__ninth_amendment, "political/constitutional/legal").

domain_priors:requires_active_enforcement(reserved_powers_amendments__ninth_amendment).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reserved_powers_amendments__ninth_amendment, '5934f23c-51ab-4efb-9905-f4f604671d7b').
narrative_ontology:cs_kernel_codification('5934f23c-51ab-4efb-9905-f4f604671d7b', fixed_text).
narrative_ontology:cs_authority_grounding('5934f23c-51ab-4efb-9905-f4f604671d7b', lineage).
narrative_ontology:cs_interpretation_layer_present('5934f23c-51ab-4efb-9905-f4f604671d7b').
narrative_ontology:cs_reading_relation('5934f23c-51ab-4efb-9905-f4f604671d7b', reserved_powers_amendments__tenth_amendment, coexists_with).
narrative_ontology:cs_axiom('5934f23c-51ab-4efb-9905-f4f604671d7b', foundational, rights_transcend_enumeration).
narrative_ontology:cs_axiom_status(rights_transcend_enumeration, holdable).
narrative_ontology:cs_axiom_grounding('5934f23c-51ab-4efb-9905-f4f604671d7b', rights_transcend_enumeration, deontological).
narrative_ontology:cs_axiom('5934f23c-51ab-4efb-9905-f4f604671d7b', foundational, people_retain_unarticulated_liberties).
narrative_ontology:cs_axiom_status(people_retain_unarticulated_liberties, holdable).
narrative_ontology:cs_axiom_grounding('5934f23c-51ab-4efb-9905-f4f604671d7b', people_retain_unarticulated_liberties, deontological).
narrative_ontology:cs_reference_frame('5934f23c-51ab-4efb-9905-f4f604671d7b', natural_rights_constitutional_recognition).
narrative_ontology:cs_drift_state('5934f23c-51ab-4efb-9905-f4f604671d7b', contemporary_originalist_dominance, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5934f23c-51ab-4efb-9905-f4f604671d7b', '').
narrative_ontology:cs_kernel_id(reserved_powers_amendments__ninth_amendment, reserved_powers_amendments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reserved_powers_amendments__ninth_amendment, claimants_of_unlisted_liberties).
narrative_ontology:constraint_victim(reserved_powers_amendments__ninth_amendment, strict_enumeration_interpreters).
narrative_ontology:constraint_victim(reserved_powers_amendments__ninth_amendment, originalist_textualists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CITIZEN CLAIMING UNENUMERATED LIBERTY (SNARE) — No exit from the Ninth Amendment's structural ambiguity. A citizen invoking an unlisted right faces the existential extraction: the court may dismiss the claim as lacking textual hook, treating the Ninth Amendment as a dead letter. Maximal suppression — trapped by the constraint's refusal to specify which unlisted rights are protected. The beneficiary structure of the Ninth (claimants of unlisted liberties) appears powerless because they cannot exit the interpretive framework that denies their rights recognition.
constraint_indexing:constraint_classification(reserved_powers_amendments__ninth_amendment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: APPELLATE JUDICIARY AS CONSTRAINED INTERPRETER (TANGLED ROPE) — Moderate power, constrained by precedent and interpretive tradition. Courts experience genuine coordination: the Ninth Amendment text does coordinate the principle that enumeration does not exhaust rights. But enforcement is costly — judges must articulate which unlisted rights qualify, risking overreach charges. Active enforcement required but structurally contested. Extraction runs in both directions: the court is constrained by the amendment's silence on which rights are protected, and it extracts power by deciding which unlisted rights to recognize.
constraint_indexing:constraint_classification(reserved_powers_amendments__ninth_amendment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ORGANIZED CIVIL RIGHTS & LIBERTARIAN COALITIONS (ROPE) — Organized agents (ACLU, Institute for Justice, libertarian legal scholars) benefit from the Ninth Amendment's open-ended structure. It provides a textual hook for expanding liberty protections beyond enumerated rights. Mobile exit option: coalitions can deploy the Ninth strategically in litigation or exit to other constitutional provisions. The coordination function is genuine: the Ninth coordinates diverse liberty claims under a unified principle. Beneficiary status — coalitions benefit from a doctrine that permits unlisted rights recognition.
constraint_indexing:constraint_classification(reserved_powers_amendments__ninth_amendment, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: ORIGINALIST ACADEMIC ESTABLISHMENT (PITON) — Institutional actors (originalist scholars, Federalist Society, textualist jurists) treat the Ninth Amendment as a dead letter. Randy Barnett's 2004 Restoring the Lost Constitution began revival, but the dominant originalist position was decades of insisting the Ninth Amendment is meaningless or unenforceable — 'the rights that are enumerated are the rights,' as if enumeration were exhaustive. This perspective shows theater ratio at 0.80+: elaborate jurisprudential arguments for why the Ninth means nothing substantive, maintained through institutional inertia and ideological commitment. Theater has decayed because the arguments are increasingly recognized as motivated reasoning rather than textualism. The originalist establishment sees the Ninth as degraded — incoherent if enforced as written, so treated as aesthetic rather than functional.
constraint_indexing:constraint_classification(reserved_powers_amendments__ninth_amendment, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: STRICT ENUMERATION INTERPRETERS / CONSTITUTIONAL MINIMALISTS (TANGLED ROPE) — Institutional voice (Justice Scalia's textualism, constitutional minimalists) claiming that enumeration is exhaustive — the Bill of Rights specifies rights, and unlisted liberties lack constitutional standing. This perspective experiences the Ninth Amendment as constraint + extraction: the Amendment's text coordinates a principle (enumeration does not exhaust rights) that victims of strict enumeration must acknowledge, yet they extract by reinterpreting the Amendment as conferring no enforceable rights. Beneficiary: strict enumeration interpreters (they get to claim the Ninth means nothing). Victim: claimants of unlisted liberties (who are suppressed by the doctrine that their rights have no constitutional hook).
constraint_indexing:constraint_classification(reserved_powers_amendments__ninth_amendment, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, the Ninth Amendment appears to express an immutable principle: it is logically incoherent to claim that enumerating some rights implies the absence of all others. The principle is not contingent on judicial recognition — it expresses a necessary truth about the relationship between enumeration and completeness. However, the structural data contradicts the mountain classification. The engine's false summit detector identifies this as naturalization: the 'logical incoherence' framing conceals that the Ninth Amendment's enforceability is deeply contested, that institutional actors (originalists, strict enumeration interpreters) have substantial power to suppress its doctrine, and that beneficiaries are identifiable. This is a false summit, not a mountain.
constraint_indexing:constraint_classification(reserved_powers_amendments__ninth_amendment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reserved_powers_amendments__ninth_amendment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(reserved_powers_amendments__ninth_amendment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(reserved_powers_amendments__ninth_amendment, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(reserved_powers_amendments__ninth_amendment, TR),
    TR >= 0.70.

:- end_tests(reserved_powers_amendments__ninth_amendment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The Ninth Amendment is sometimes enforceable (Griswold v. Connecticut, though on shaky foundation), sometimes ignored (contemporary Supreme Court rarely cites it), and perpetually contested in scholarly debate. The extractiveness is not high because the constraint has not fully calcified — judicial practice oscillates between recognizing and suppressing unenumerated rights doctrine. It is not low because claimants face systematic suppression when they invoke the Ninth Amendment (many courts dismiss such claims as frivolous or insufficiently grounded). Suppression (0.52): Moderate-high. Originalist and textualist doctrine has successfully suppressed Ninth Amendment enforceability for decades. Griswold's privacy doctrine (1965) relied on the Ninth but subsequent cases (Bowers v. Hardwick, 1986) abandoned Ninth Amendment grounding, treating it as a dead letter. The suppression is structural: strict enumeration interpreters have institutional power (originalism dominates contemporary SCOTUS and lower federal courts) to dismiss Ninth Amendment claims. Theater ratio (0.65): Moderate-high. Significant energy has gone into explaining why the Ninth Amendment either does or does not protect substantive rights. The originalist position (that the Ninth is a non-justiciable interpretive principle) requires elaborate argumentation — if the Amendment truly means nothing, why does it appear in the Constitution? Why were the Framers so concerned to state that enumeration does not exhaust rights? The originalist position inverts the text's apparent meaning, creating a performative gap between what the Amendment says and what originalists claim it does. This gap is the theater.
 *
 * PERSPECTIVAL GAP:
 *   The Ninth Amendment generates maximal perspectival divergence. Citizens claiming unlisted liberties see a snare: they face systematic dismissal. Civil rights coalitions see a rope: the Amendment coordinates a principle enabling their advocacy. Originalists see a piton: they perform elaborate arguments that the Amendment is meaningless. Courts see tangled rope: genuine coordination (the principle that enumeration doesn't exhaust rights) mixed with enforcement extraction (the cost and controversy of deciding which unlisted rights qualify). The analytical observer risks seeing a mountain (a logical necessity: enumeration cannot mean exhaustion) but this naturalizes what is actually a contested institutional terrain. The perspectival gap reveals that the Ninth Amendment's classification depends entirely on whether one views the constraint as a source of enforceable rights or as a hermeneutic principle. This is the classic compression-to-single-type problem: the same text generates six different classifications from six observer positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's structural relationship to the constraint. Citizens claiming unlisted liberties: powerless/trapped, no exit, victim status → high d (0.90+) → high f(d) → experienced as snare. Civil rights coalitions: organized/mobile, beneficiary status → low d (0.25) → low f(d) → experienced as rope. Originalists: institutional/arbitrage, beneficiary of the doctrine that the Ninth is meaningless → very low d (0.05) → negative f(d) → experienced as beneficiary protection (rope from their perspective, but classified as piton because of the theater gate). Courts: institutional/constrained, both beneficiary (courts expand power to decide unlisted rights) and victim (courts face controversy and precedent constraints) → moderate d (0.50+) → moderate f(d) → experienced as tangled rope. The analytical observer: analytical/analytical, no structural position in the constraint, observes from outside → canonical d (0.73) → high f(d) → would experience high extractiveness if the mountain classification held, but the constraint's false summit status means the analytical view itself is captured by naturalizing arguments.
 *
 * MANDATROPHY ANALYSIS:
 *   The Ninth Amendment resolves the mandatrophy by showing that multiple DR types are legitimate perspectival readings of the same constitutional text. The question is not 'which type is correct?' but 'which structural position are you occupying?' From the position of a powerless citizen asserting an unlisted right, the constraint is a snare (systematic suppression, no exit). From the position of an organized civil rights coalition, it is a rope (genuine coordination of the principle that enumeration doesn't exhaust rights). From the position of an originalist scholar, it is a piton (performative arguments for meaninglessness). From the position of an appellate court, it is tangled rope (genuine coordination mixed with extraction power). From the position of a strict enumeration interpreter, it is tangled rope (the Ninth Amendment forces acknowledgment of the coordination principle while allowing reinterpretation to extract meaning away from substantive rights protection). The false summit perspective shows why analytical observers must be careful not to naturalize contingent institutional arrangements as logical necessities. The Ninth Amendment appears mountain-like (a logical principle: enumeration ≠ exhaustion) but this naturalizes what is actually a fully contested institutional terrain where powerful originalist actors have successfully suppressed the Amendment's apparent meaning for decades.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ninth_amendment_enforceability,
    'Is the Ninth Amendment enforceable as a source of substantive constitutional rights, or does it merely express a non-justiciable principle about interpretation?',
    'Historical trajectory of Ninth Amendment jurisprudence (Griswold v. Connecticut to contemporary privacy doctrine); whether courts have cited the Ninth as an independent basis for rights recognition vs. merely as an interpretive principle',
    'If enforceable: Ninth Amendment is a coordinate source of rights with the enumerated rights (Tangled Rope from citizen perspective becomes Rope). If non-justiciable: the Amendment is purely performative theater (citizen perspective becomes Snare with higher theater, shifting toward Piton). Extractiveness would shift from 0.38 to either lower (if enforceable) or higher (if purely theater).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ninth_amendment_enforceability, empirical, 'Whether the Ninth Amendment provides enforceable substantive rights').

omega_variable(
    unenumerated_rights_specification_problem,
    'Can the Ninth Amendment specify which unlisted liberties qualify as protected rights without itself enumerating them (defeating its purpose), or does its open-endedness make it impossible to distinguish protected unenumerated rights from unprotected liberty claims?',
    'Analysis of successful Ninth Amendment claims (if any); examination of whether courts have developed principled criteria for which unenumerated rights qualify vs. ad hoc case-by-case adjudication',
    'If specification is possible: the Ninth Amendment permits coherent doctrine (moderate extractiveness). If specification is impossible: the Amendment remains structurally incoherent (high extractiveness, high theater, victim status for clarity-seeking interpreters). Classification may shift to Piton if unresolvable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unenumerated_rights_specification_problem, conceptual, 'Whether the Ninth Amendment can specify which unlisted rights are protected').

omega_variable(
    natural_rights_versus_positivist_grounding,
    'Does the Ninth Amendment presuppose a natural rights framework (rights exist pre-constitutionally and the Constitution merely acknowledges them), or is it a positivist claim (the Ninth Amendment creates or recognizes no substantive rights, merely a hermeneutic principle)?',
    'Originalist analysis of Ninth Amendment drafting history and ratifier intent; comparison to contemporary constitutional interpretation (original public meaning vs. living constitution); whether natural rights framing produces enforceable doctrine vs. positivist framing',
    'If natural rights: the Ninth Amendment has substantive content (lower extractiveness, Rope/Tangled Rope). If positivist (no substantive content): the Ninth is purely interpretive theater (higher extractiveness, Piton). The grounding choice determines whether the constraint protects unlisted liberties or merely instructs judges not to treat enumeration as foreclosure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_rights_versus_positivist_grounding, conceptual, 'Natural rights vs. positivist grounding of unenumerated rights').

omega_variable(
    originalist_foreclosure_of_ninth,
    'Is the contemporary originalist rejection of Ninth Amendment enforceability (treating it as a non-justiciable interpretive principle) a genuine textualist position, or a motivated reasoning that protects strict enumeration doctrine from the Amendment''s clear language?',
    'Textual analysis: does ''retained by the people'' in the Ninth Amendment grammatically permit the originalist reading that no rights are actually retained? Comparison to originalist enforcement of other constitutional silences (where silence is treated as prohibition vs. permission). Historical evidence of whether founding-era originalists treated the Ninth as non-justiciable or as enforceable.',
    'If motivated reasoning: the originalist position is a false summit masking institutional extraction (suppressing Ninth Amendment doctrine to protect enumeration-exhaustion). Extractiveness and suppression would be recomputed higher. If genuine: the originalist position is a coherent constitutional reading (extractiveness lower). This omega is diagnostic of whether the strict enumeration interpreter perspective is legitimate or extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalist_foreclosure_of_ninth, empirical, 'Whether originalist rejection of the Ninth Amendment is genuine textualism or motivated foreclosure').

omega_variable(
    reading_kernel_contest_framing,
    'Does the Ninth Amendment foreclose, coexist with, or influence the Tenth Amendment reading of reserved powers?',
    'Constitutional interpretation: the Ninth protects unenumerated rights retained by the people; the Tenth reserves undelegated powers to states and people. Do these readings compete for the same textual/doctrinal space, or do they address structurally distinct domains (individual rights vs. federal/state power allocation)?',
    'If foreclose: only one reading can hold in a single constitutional framework (rare but possible if rights doctrine and federalism doctrine are viewed as mutually constraining). If coexist: both readings remain live in different interpretive traditions or litigational contexts (most likely). If influence: Ninth enforcement shapes how the Tenth is read (e.g., stronger Ninth = pressure to read Tenth as protecting individual liberty, weaker Ninth = Tenth read as protecting state power). The reading_relations field in cs_structure encodes the sibling relationship; this omega documents the structural basis for the choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_contest_framing, conceptual, 'Structural relationship between Ninth Amendment and Tenth Amendment readings of reserved powers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reserved_powers_amendments__ninth_amendment, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ninth_tr_t0, reserved_powers_amendments__ninth_amendment, theater_ratio, 0, 0.5).
narrative_ontology:measurement(ninth_tr_t30, reserved_powers_amendments__ninth_amendment, theater_ratio, 30, 0.65).
narrative_ontology:measurement(ninth_tr_t60, reserved_powers_amendments__ninth_amendment, theater_ratio, 60, 0.75).

% Extraction over time
narrative_ontology:measurement(ninth_be_t0, reserved_powers_amendments__ninth_amendment, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(ninth_be_t30, reserved_powers_amendments__ninth_amendment, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(ninth_be_t60, reserved_powers_amendments__ninth_amendment, base_extractiveness, 60, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reserved_powers_amendments__ninth_amendment, identity_coordination).
narrative_ontology:affects_constraint(reserved_powers_amendments__ninth_amendment, reserved_powers_amendments__tenth_amendment).

% DUAL FORMULATION NOTE:
% The Ninth Amendment reading of reserved_powers_amendments kernel focuses on individual rights retention and protection from federal infringement. The Tenth Amendment reading (sibling) addresses governmental power allocation between federal and state levels. Both readings derive from the kernel's commitment to reserved powers, but instantiate different constraint structures (individual rights vs. governmental federalism). The two constraints are linked via network.affects_constraints as siblings in the reserved_powers_amendments constraint family. The Ninth reading has extractiveness 0.38 (moderate, contested enforceability). The Tenth reading would have different extractiveness reflecting the federalism domain's structure (likely lower, as federalism doctrine is more established and less contested than unenumerated rights doctrine). See reading_relations in cs_structure for the structural relationship between readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reserved_powers_amendments__ninth_amendment, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
