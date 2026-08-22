% ============================================================================
% CONSTRAINT STORY: qualified_immunity_doctrine__protective_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qualified_immunity_doctrine__protective_scaffold_reading, []).

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
 *   constraint_id: qualified_immunity_doctrine__protective_scaffold_reading
 *   human_readable: Qualified Immunity as Protective Scaffold for Good-Faith Law Enforcement
 *   domain: constitutional_law/civil_rights/law_enforcement_policy
 *
 * SUMMARY:
 *   This story instantiates the protective-scaffold reading of the qualified
 *   immunity kernel: immunity is framed as a necessary structural protection
 *   allowing officers to exercise good-faith discretion under ambiguous legal
 *   standards without the threat of personal financial ruin, and allowing
 *   municipalities to staff and budget without unpredictable litigation
 *   exposure. Under this reading, the doctrine has a genuine coordination
 *   function — de-risking split-second discretionary judgment in dangerous
 *   encounters — but that coordination is bought by externalizing the cost of
 *   unremedied constitutional violations onto survivors whose claims fail the
 *   'clearly established' test regardless of the actual reasonableness of the
 *   officer's conduct. This is precisely why the reading computes as
 *   tangled_rope rather than pure rope: a real coordination function coexists
 *   with asymmetric extraction from an identifiable victim class, sustained
 *   by active judicial and legislative enforcement of the doctrine's scope.
 *
 * KEY AGENTS:
 *   - law_enforcement_officers: primary beneficiary (moderate power/constrained exit) — protected from personal liability for good-faith judgment calls
 *   - municipal_governments: institutional beneficiary and co-agenda-setter — reduced litigation exposure and insurance costs
 *   - police_unions: organized beneficiary and agenda-setter — actively lobby to preserve and extend the doctrine
 *   - constitutional_violation_survivors: primary victim (powerless/trapped) — bear the uncompensated cost of violations dismissed on immunity grounds
 *   - federal_judiciary: agenda-setter — controls the doctrine's actual scope through case-by-case 'clearly established' determinations
 *   - civil_rights_litigators: excluded voice — document the doctrine's drift but rarely reshape it directly
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualified_immunity_doctrine__protective_scaffold_reading, 0.48).
domain_priors:suppression_score(qualified_immunity_doctrine__protective_scaffold_reading, 0.55).
domain_priors:theater_ratio(qualified_immunity_doctrine__protective_scaffold_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_immunity_doctrine__protective_scaffold_reading, tangled_rope).
narrative_ontology:human_readable(qualified_immunity_doctrine__protective_scaffold_reading, "Qualified Immunity as Protective Scaffold for Good-Faith Law Enforcement").
narrative_ontology:topic_domain(qualified_immunity_doctrine__protective_scaffold_reading, "constitutional_law/civil_rights/law_enforcement_policy").

domain_priors:requires_active_enforcement(qualified_immunity_doctrine__protective_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qualified_immunity_doctrine__protective_scaffold_reading, '23757772-d142-4bf5-969e-241888cae75a').
narrative_ontology:cs_kernel_codification('23757772-d142-4bf5-969e-241888cae75a', formalized).
narrative_ontology:cs_authority_grounding('23757772-d142-4bf5-969e-241888cae75a', lineage).
narrative_ontology:cs_interpretation_layer_present('23757772-d142-4bf5-969e-241888cae75a').
narrative_ontology:cs_reading_relation('23757772-d142-4bf5-969e-241888cae75a', qualified_immunity_doctrine__accountability_void_reading, coexists_with).
narrative_ontology:cs_reading_relation('23757772-d142-4bf5-969e-241888cae75a', qualified_immunity_doctrine__constitutional_fidelity_reading, coexists_with).
narrative_ontology:cs_axiom('23757772-d142-4bf5-969e-241888cae75a', foundational, discretionary_judgment_requires_liability_shield).
narrative_ontology:cs_axiom_status(discretionary_judgment_requires_liability_shield, holdable).
narrative_ontology:cs_axiom_grounding('23757772-d142-4bf5-969e-241888cae75a', discretionary_judgment_requires_liability_shield, instrumental).
narrative_ontology:cs_axiom('23757772-d142-4bf5-969e-241888cae75a', secondary, clearly_established_standard_is_calibrated_not_categorical).
narrative_ontology:cs_axiom_status(clearly_established_standard_is_calibrated_not_categorical, holdable).
narrative_ontology:cs_axiom_grounding('23757772-d142-4bf5-969e-241888cae75a', clearly_established_standard_is_calibrated_not_categorical, empirically_contingent).
narrative_ontology:cs_reference_frame('23757772-d142-4bf5-969e-241888cae75a', harlow_fitzgerald_objective_reasonableness_standard).
narrative_ontology:cs_drift_state('23757772-d142-4bf5-969e-241888cae75a', post_2009_pearson_discretionary_order_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('23757772-d142-4bf5-969e-241888cae75a', '').
narrative_ontology:cs_kernel_id(qualified_immunity_doctrine__protective_scaffold_reading, qualified_immunity_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__protective_scaffold_reading, law_enforcement_officers).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__protective_scaffold_reading, municipal_governments).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__protective_scaffold_reading, police_unions).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__protective_scaffold_reading, constitutional_violation_survivors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Make split-second judgment calls in ambiguous, often dangerous encounters. Immunity shields them from personal financial liability and protracted litigation for actions taken in good faith under unclear or evolving legal standards, provided the right violated was not 'clearly established' at the time. They did not design the doctrine but rely on it to make discretionary decisions without fear that any mistaken judgment call bankrupts them personally.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, law_enforcement_officers, beneficiary,
    moderate, biographical, constrained, national).

% Fund police departments, defend officers in litigation, and often indemnify judgments. Immunity reduces the frequency and size of successful suits against officers, containing municipal insurance costs and preserving departmental operations. Governments lobby to preserve the doctrine and shape its application through litigation strategy.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, municipal_governments, beneficiary,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(qualified_immunity_doctrine__protective_scaffold_reading, municipal_governments, agenda_setter).

% Advocate for preserving and broadening immunity protections as a core condition of employment, arguing officers cannot perform the job effectively if every discretionary act invites personal-capacity lawsuits. They lobby legislatures and file amicus briefs defending the doctrine's scope.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, police_unions, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(qualified_immunity_doctrine__protective_scaffold_reading, police_unions, agenda_setter).

% Have suffered an actual constitutional violation — excessive force, unlawful search, wrongful arrest — but find their claim dismissed because no prior case held materially identical conduct unlawful. They bear the full cost of the violation with no compensatory remedy, and the dismissal creates no precedent to protect the next victim either, since the right remains unestablished.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, constitutional_violation_survivors, payer,
    powerless, biographical, trapped, local).

% Adjudicates the 'clearly established' standard case by case, exercising discretion over how specifically prior precedent must match the facts at hand. Courts frequently resolve cases on the immunity question alone, avoiding the underlying constitutional merits and thereby controlling the pace at which new rights become 'clearly established.'
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Represent plaintiffs against officers and argue the doctrine as currently applied insulates misconduct rather than good-faith judgment calls. Their arguments for reform are heard in litigation and legislative testimony but rarely reshape the doctrine's judicial application, since the Supreme Court has been reluctant to revisit it directly.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, civil_rights_litigators, excluded,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows officers to exercise discretionary judgment in ambiguous, time-pressured situations without the chilling effect of potential personal financial ruin from every good-faith mistake, and allows municipalities to budget and staff departments without unpredictable litigation exposure for reasonable, if ultimately incorrect, split-second decisions.
% TRANSFER_FUNCTION: Moves the cost of unremedied constitutional violations from officers and municipal insurance pools onto the individuals whose rights were violated, in cases where the violated right was not yet 'clearly established' by materially similar precedent.
% ABSENT_VOICES: Constitutional violation survivors whose specific claims were dismissed are procedurally absent from the doctrine-shaping conversation once their case is disposed of on immunity grounds — the merits are never reached, so their experience never becomes precedent informing future cases. Civil rights litigators voice this pattern in aggregate but individual dismissed plaintiffs have no further voice in their own case.
% DISAPPEARANCE_RATIONALE: If immunity vanished overnight, officers and their unions argue policing would become excessively defensive, departments would face insurance and recruitment crises, and municipalities would face unpredictable liability exposure — the operational posture of policing and its financial architecture would have to be rebuilt around a different risk allocation.
% FOUNDING_PROBLEM: Officers needed to be able to make discretionary, split-second judgment calls in dangerous and ambiguous situations without the deterrent effect of personal liability for every reasonable mistake, so that vigorous, good-faith enforcement of the law would not be chilled by litigation risk.
% FOUNDING_PROBLEM_CORROBORATION: Police unions, municipal risk managers, and some sitting judges attest the founding problem — chilled, overly cautious policing from litigation exposure — remains live and immunity remains necessary to it. Civil rights litigators, legal scholars analyzing dismissal-rate data, and several federal appellate judges in separate writings attest the doctrine has drifted from protecting good-faith judgment calls toward blanket dismissal of meritorious claims regardless of officer culpability, corroborated by independent academic tracking of qualified immunity outcomes.
narrative_ontology:disappearance_verdict(qualified_immunity_doctrine__protective_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(qualified_immunity_doctrine__protective_scaffold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qualified_immunity_doctrine__protective_scaffold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(qualified_immunity_doctrine__protective_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qualified_immunity_doctrine__protective_scaffold_reading, 0.48, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qualified_immunity_doctrine__protective_scaffold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qualified_immunity_doctrine__protective_scaffold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qualified_immunity_doctrine__protective_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored as moderate (0.48) rather than high: under this reading, judicial discretion in applying the 'clearly established' standard is a real filtering mechanism intended to distinguish genuinely reasonable judgment calls from culpable misconduct, not a blanket shield. The extraction that does occur is real (survivors of genuine violations are denied remedy when precedent is insufficiently specific) but the reading holds this as a byproduct of an imperfect but necessary standard, not as the doctrine's purpose. Suppression (0.55) reflects that survivors have essentially no alternative remedy path once immunity is granted — the claim is simply extinguished, not merely made harder. Theater ratio is modest (0.28) and rising: over the interval, a growing share of judicial and legislative energy defends the doctrine's continued existence and scope rather than refining the standard's line-drawing, which is the theatrical drift this reading concedes even while defending the underlying coordination function.
 *
 * PERSPECTIVAL GAP:
 *   From the officer and municipal beneficiary seats, the doctrine is functioning exactly as designed: predictable protection for good-faith discretion. From the survivor payer seat, the same doctrine operates as a categorical bar to remedy regardless of the actual reasonableness of the conduct in their specific case. The engine should compute meaningfully different seat-level classifications from these structurally opposed positions even within this single reading — that divergence is the tangled-rope signature, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Officers, municipalities, and police unions are declared beneficiaries because the doctrine's central mechanism — barring personal-capacity suits absent clearly established precedent — directly reduces their financial and litigation exposure; their exit options (constrained-to-mobile) reflect that they can lobby or relocate employment but cannot simply opt out of the doctrine's protective effect while serving as officers. Constitutional violation survivors are declared victims because the same mechanism extinguishes their remedy regardless of the merits of the underlying violation; their exit options are trapped because a dismissed claim has no alternative forum. The federal judiciary sits as agenda-setter with analytical exit options, since it administers the standard's application but is not itself extracted from or benefiting materially.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists collapsing into either pure snare or pure rope: the coordination function (enabling good-faith discretionary judgment without chilling effects) is real and independently defensible from the officer/municipal seat, which is why requires_active_enforcement, beneficiaries, and victims are all authored together rather than omitting one leg of the tangled-rope gate. Treating this purely as extraction would erase the genuine operational problem the doctrine was built to solve; treating it purely as coordination would erase the uncompensated victim class this reading itself names. The founding_problem_status is authored as contested rather than resolved because the reading's own corroboration record shows disagreement about whether the original problem (chilled policing) remains as pressing as the doctrine's current scope suggests.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    clearly_established_standard_line_drawing,
    'Does judicial discretion in applying the ''clearly established'' standard function as a genuine, reasonably calibrated filter distinguishing good-faith judgment calls from culpable misconduct, or has it drifted into a near-automatic dismissal mechanism regardless of underlying culpability?',
    'Empirical tracking of case outcomes: compare dismissal rates and stated rationales across decades against independently coded severity/culpability of the underlying conduct, and assess whether increasingly specific precedent-matching requirements track any genuine change in officer conduct or purely reflect doctrinal hardening.',
    'If the standard functions as a calibrated filter, this reading''s moderate extractiveness score is well-supported. If it has drifted toward near-automatic dismissal, the effective extraction under this reading''s own metrics is understated and the tangled-rope classification would trend toward snare even within this reading''s framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(clearly_established_standard_line_drawing, empirical, 'Whether the clearly-established standard still discriminates reasonably between good-faith and culpable conduct.').

omega_variable(
    founding_problem_persistence,
    'Is the original chilling-effect problem the doctrine was built to solve still empirically live at current levels of severity, or has decades of doctrinal accretion outpaced the problem it was meant to address?',
    'Comparative study of officer decision-making and recruitment/retention outcomes in jurisdictions or historical periods with narrower immunity doctrine versus the current federal standard.',
    'If the founding problem has substantially receded relative to the doctrine''s current breadth, this reading''s own protective framing becomes harder to sustain and the mandatrophy_resolved question moves toward true even under this reading''s own terms.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether the doctrine''s current scope is proportionate to a still-live founding problem.').

omega_variable(
    committer_kernel_disagreement_locus,
    'This constraint is one of three readings of the qualified_immunity_doctrine kernel — the accountability_void_reading, the constitutional_fidelity_reading, and this protective_scaffold_reading. The three readings disagree at the root over whether the doctrine has ANY genuine coordination function at all (this reading says yes; accountability_void_reading says no) and separately over whether the doctrine''s judicial origin is legitimate regardless of its policy effects (constitutional_fidelity_reading says no, independent of the coordination question this reading and accountability_void_reading dispute).',
    'The coordination-function disagreement is partly empirical (resolvable by the clearly_established_standard_line_drawing omega above) but the legitimacy-of-origin disagreement in constitutional_fidelity_reading is a distinct, non-empirical question about the judiciary''s authority to fashion immunity doctrine absent statutory text — it would not be resolved even if this reading''s coordination claims were fully vindicated.',
    'If accountability_void_reading is correct that no genuine coordination function exists, this story''s claimed_type should be read as an artifact of this reading''s own framing rather than a settled structural fact. If constitutional_fidelity_reading is correct, the doctrine''s illegitimacy at the level of judicial authority would apply regardless of how this reading''s extraction metrics come out.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_kernel_disagreement_locus, conceptual, 'Locates where this reading''s disagreement with its two siblings actually lies: coordination-function existence (vs. accountability_void_reading) and judicial legitimacy of origin (vs. constitutional_fidelity_reading) are separate axes of dispute.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualified_immunity_doctrine__protective_scaffold_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qual_tr_t1967, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 1967, 0.1).
narrative_ontology:measurement(qual_tr_t1982, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 1982, 0.14).
narrative_ontology:measurement(qual_tr_t1997, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 1997, 0.19).
narrative_ontology:measurement(qual_tr_t2009, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 2009, 0.23).
narrative_ontology:measurement(qual_tr_t2018, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 2018, 0.26).
narrative_ontology:measurement(qual_tr_t2024, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(qual_be_t1967, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 1967, 0.22).
narrative_ontology:measurement(qual_be_t1982, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 1982, 0.3).
narrative_ontology:measurement(qual_be_t1997, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 1997, 0.38).
narrative_ontology:measurement(qual_be_t2009, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 2009, 0.44).
narrative_ontology:measurement(qual_be_t2018, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 2018, 0.46).
narrative_ontology:measurement(qual_be_t2024, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 2024, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(qual_su_t1967, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 1967, 0.4).
narrative_ontology:measurement(qual_su_t1982, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 1982, 0.46).
narrative_ontology:measurement(qual_su_t1997, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 1997, 0.5).
narrative_ontology:measurement(qual_su_t2009, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 2009, 0.52).
narrative_ontology:measurement(qual_su_t2018, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 2018, 0.54).
narrative_ontology:measurement(qual_su_t2024, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qualified_immunity_doctrine__protective_scaffold_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__protective_scaffold_reading, accountability_void_reading).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__protective_scaffold_reading, constitutional_fidelity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the qualified_immunity_doctrine kernel. accountability_void_reading authors materially higher extractiveness and suppression, denies any genuine coordination function, and treats officers as pure capturers rather than beneficiaries of a defensible protective function. constitutional_fidelity_reading brackets the extraction question entirely and instead treats the doctrine's judicial (rather than legislative or constitutional) origin as dispositive of illegitimacy regardless of policy consequences. All three share the same underlying kernel (the judicially-administered 'clearly established' immunity standard) but instantiate structurally distinct constraints with different ε, different beneficiary/victim framing, and different claimed types — per the ε-invariance principle, they are authored as separate stories linked here rather than as one story with a contested classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
