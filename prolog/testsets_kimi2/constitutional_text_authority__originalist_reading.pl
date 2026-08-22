% ============================================================================
% CONSTRAINT STORY: constitutional_text_authority__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text_authority__originalist_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: constitutional_text_authority__originalist_reading
 *   human_readable: Originalist Reading of Constitutional Text Authority
 *   domain: constitutional/law/legal_theory
 *
 * SUMMARY:
 *   This constraint instantiates the originalist reading of the
 *   constitutional_text_authority kernel: constitutional meaning is fixed at
 *   ratification and authority derives from historical public understanding.
 *   It operates as an interpretive methodology that binds federal judges,
 *   creating a rigid constraint on judicial discretion. Historical evidence
 *   gates permissible outcomes, making unenumerated rights difficult to
 *   recognize and channeling post-ratification social change into the Article
 *   V amendment process. The constraint is claimed as necessary coordination
 *   against arbitrary judicial power, while critics read it as asymmetric
 *   extraction that empowers originalist institutions and dead-hand control.
 *
 * KEY AGENTS:
 *   - originalist_judiciary: Primary agenda_setter (institutional/identity_locked) â administers and enforces original meaning methodology
 *   - conservative_political_coalitions: Primary beneficiary (powerful/mobile) â captures policy stability and blocked progressive rights
 *   - originalist_legal_academy: Secondary beneficiary (organized/constrained) â receives institutional prestige and gatekeeping power
 *   - litigants_novel_rights: Primary target (powerless/trapped) â bear the cost of frozen constitutional meaning
 *   - living_constitutionalist_jurists: Secondary target (moderate/constrained) â bear methodological delegitimization
 *   - progressive_social_movements: Tertiary target (powerless/trapped) â democratic change channeled into Article V amendment process
 *   - constitutional_historians: Analytical observer (analytical/analytical) â provides evidentiary inputs without bearing costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text_authority__originalist_reading, 0.62).
domain_priors:suppression_score(constitutional_text_authority__originalist_reading, 0.71).
domain_priors:theater_ratio(constitutional_text_authority__originalist_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text_authority__originalist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text_authority__originalist_reading, "Originalist Reading of Constitutional Text Authority").
narrative_ontology:topic_domain(constitutional_text_authority__originalist_reading, "constitutional/law/legal_theory").

domain_priors:requires_active_enforcement(constitutional_text_authority__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text_authority__originalist_reading, '34fb14f0-17ae-49c6-b96a-79018e72882b').
narrative_ontology:cs_kernel_codification('34fb14f0-17ae-49c6-b96a-79018e72882b', fixed_text).
narrative_ontology:cs_authority_grounding('34fb14f0-17ae-49c6-b96a-79018e72882b', lineage).
narrative_ontology:cs_interpretation_layer_present('34fb14f0-17ae-49c6-b96a-79018e72882b').
narrative_ontology:cs_reading_relation('34fb14f0-17ae-49c6-b96a-79018e72882b', constitutional_text_authority__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('34fb14f0-17ae-49c6-b96a-79018e72882b', constitutional_text_authority__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('34fb14f0-17ae-49c6-b96a-79018e72882b', foundational, original_public_meaning_authoritative).
narrative_ontology:cs_axiom_status(original_public_meaning_authoritative, holdable).
narrative_ontology:cs_axiom_grounding('34fb14f0-17ae-49c6-b96a-79018e72882b', original_public_meaning_authoritative, conventional).
narrative_ontology:cs_axiom('34fb14f0-17ae-49c6-b96a-79018e72882b', secondary, article_v_sole_vehicle_for_legal_change).
narrative_ontology:cs_axiom_status(article_v_sole_vehicle_for_legal_change, holdable).
narrative_ontology:cs_axiom_grounding('34fb14f0-17ae-49c6-b96a-79018e72882b', article_v_sole_vehicle_for_legal_change, conventional).
narrative_ontology:cs_reference_frame('34fb14f0-17ae-49c6-b96a-79018e72882b', ratification_era_public_meaning).
narrative_ontology:cs_drift_state('34fb14f0-17ae-49c6-b96a-79018e72882b', contemporary_jurisprudence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('34fb14f0-17ae-49c6-b96a-79018e72882b', '').
narrative_ontology:cs_kernel_id(constitutional_text_authority__originalist_reading, constitutional_text_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, conservative_political_coalitions).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, originalist_legal_academy).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, litigants_novel_rights).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, living_constitutionalist_jurists).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, progressive_social_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control constitutional interpretation through appointment and doctrinal gatekeeping; enforce original public meaning as the exclusive valid method; their professional identity and institutional authority are fused with the interpretive framework.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, originalist_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).

% Receive constitutional interpretations that block regulatory and rights evolution they oppose; their policy preferences are insulated from judicial revision by the fixed-meaning constraint.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, conservative_political_coalitions, beneficiary,
    powerful, biographical, mobile, national).

% Receive institutional prestige, funding, and clerkship pipeline advantages from the constraint's dominance; their scholarly output sets the terms of what counts as valid constitutional argument in federal courts.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, originalist_legal_academy, beneficiary,
    organized, biographical, constrained, national).

% Bring claims for unenumerated or evolving rights; face a structural barrier because historical evidence from 1787 or 1868 is mobilized to defeat their claims regardless of contemporary moral urgency or democratic consensus.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, litigants_novel_rights, payer,
    powerless, immediate, trapped, national).

% Judges and scholars who view constitutional interpretation as requiring integration of contemporary values; their methodological preference is ruled illegitimate in originalist-dominated forums, constraining their discretion and professional advancement.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, living_constitutionalist_jurists, payer,
    moderate, biographical, constrained, national).

% Seek constitutional change through adjudication; find their claims channeled into a supermajoritarian Article V amendment process because judicial interpretation is foreclosed to evolving rights claims.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, progressive_social_movements, payer,
    powerless, generational, trapped, national).

% Provide historical evidence used by all sides in constitutional litigation; do not themselves benefit from or pay for the interpretive constraint, serving an evidentiary rather than normative function.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates constitutional interpretation across time and judges by binding them to a fixed historical meaning, preventing constitutional law from fluctuating with the personal values of unelected judges or transient majorities.
% TRANSFER_FUNCTION: Transfers interpretive authority from contemporary democratic majorities and moral philosophers to historians and archival evidence of late-eighteenth-century public meaning; transfers the cost of constitutional adaptation from the judiciary to litigants and social movements who must secure Article V supermajorities.
% ABSENT_VOICES: Progressive social movements arguing for evolving rights, and living constitutionalist jurists advocating methodological pluralism, are structurally excluded from the authoritative interpretive conversation in originalist-dominated courts; their arguments are treated as categorically illegitimate rather than merely incorrect.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight, constitutional interpretation would immediately reorient around contemporary values, pragmatic balancing, or pluralist methods; unenumerated rights would become judicially recognizable; the political economy of judicial appointments and clerkships would shift away from historical expertise; and the dead-hand control of ratification-era majorities would dissolve.
% FOUNDING_PROBLEM: The Warren Court and early-twentieth-century judiciary were perceived as imposing personal policy preferences under the guise of constitutional interpretation, producing unstable, illegitimate law and democratically unaccountable judicial supremacy.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars and Federalist Society affiliates attest the problem remains live, citing continuing judicial overreach. Progressive legal historians and living constitutionalist jurists attest the problem was either historically exaggerated or has been superseded by a counter-problem of historical fetishism and democratic rigidity; political scientists outside the legal academy note the founding problem has been replaced by counter-majoritarian difficulty in a polarized era.
narrative_ontology:disappearance_verdict(constitutional_text_authority__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text_authority__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text_authority__originalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_text_authority__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text_authority__originalist_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text_authority__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text_authority__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_text_authority__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.25 to 0.62 over the interval, tracking originalism's shift from a critical methodology to an institutional enforcement regime. Suppression rises from 0.35 to 0.71 as originalist courts increasingly treat non-originalist arguments as categorically illegitimate. Theater_ratio rises from 0.20 to 0.45, reflecting the growing performative dimension of law-office history deployed to justify predetermined outcomes. Accessibility_collapse is authored at 0.65: alternatives persist intellectually but collapse institutionally once a case reaches an originalist bench. Resistance is 0.68 because the constraint meets sustained methodological opposition from living constitutionalists and progressive social movements.
 *
 * PERSPECTIVAL GAP:
 *   From the originalist judiciary and legal academy, the constraint is experienced as necessary coordination that prevents arbitrary judicial power and stabilizes constitutional law. From the payer seats â litigants seeking novel rights, living constitutionalist jurists, and progressive social movements â the same structure is experienced as extraction: it transfers the cost of constitutional adaptation to them and channels their claims into a politically impossible amendment process. The engine computes this divergence from the structural asymmetry in power and exit options; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (conservative_political_coalitions, originalist_legal_academy) are structurally subsidized by the constraint and sit near the low-d end. The agenda_setter (originalist_judiciary) is identity-locked into the framework, giving it low directional extraction despite its power. Payers (litigants_novel_rights, progressive_social_movements) are powerless and trapped, sitting near full-target. Living_constitutionalist_jurists are moderate but constrained, giving them a high but not maximal d. The historian observer sits near neutral.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint was founded to solve judicial activism (Warren Court era). That founding problem is contested as still live or already superseded. If dead, the constraint risks mandatrophy; however, it still provides a genuine coordination function (interpretive stability across judges) that prevents reclassification as piton or snare. The tangled_rope classification captures this duality: the coordination function is real, but the asymmetric extraction â channeling all adaptive pressure to Article V â has intensified as the constraint became institutionally dominant. The founding_problem_status of contested preserves the ambiguity rather than pre-empting it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_knowledge_limit,
    'Can the historical public meaning of constitutional clauses be recovered with sufficient precision to genuinely constrain judicial discretion, or does the historical record underdetermine outcomes in ways that allow covert contemporary values to drive results?',
    'Systematic audit of originalist judicial opinions against professional historical consensus; if divergence is systematic, the constraint''s coordination claim is weakened and its extraction function (agenda-setting by selective history) is revealed.',
    'If historical meaning is irreducibly indeterminate, the constraint''s extraction component dominates and it functions more like a snare; if determinate, the coordination claim is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_knowledge_limit, empirical, 'Whether historical recovery is precise enough to support the coordination claim').

omega_variable(
    originalist_kernel_position,
    'This constraint is the originalist reading of the constitutional_text_authority kernel; does it foreclose the living constitutionalist reading within a single interpretive framework, or do they merely coexist across different institutional factions?',
    'Analyze whether a single judge can coherently hold both original public meaning and evolving standards as simultaneously binding methods of constitutional interpretation.',
    'If foreclosed, the kernel generates zero-sum institutional capture dynamics; if coexistent, the constraint family should be modeled as competitive readings rather than logical contradictions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalist_kernel_position, conceptual, 'Structural relationship between originalist and living constitutionalist readings').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the dominance of originalist methodology maintained by structural institutional gatekeeping (appointments, clerkship pipelines, funding networks) or by internalized professional belief among legal elites?',
    'Track interpretive-method adoption rates before and after institutional exposure (law school curriculum, clerkships); if dominance collapses when institutional rewards shift, suppression is structural.',
    'If internalized, effective suppression is higher than structural measure suggests â judges and scholars carry the constraint even without external enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of alternative interpretive methods').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text_authority__originalist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(originalist_tr_t0, constitutional_text_authority__originalist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(originalist_tr_t10, constitutional_text_authority__originalist_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(originalist_tr_t20, constitutional_text_authority__originalist_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(originalist_tr_t30, constitutional_text_authority__originalist_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(originalist_tr_t40, constitutional_text_authority__originalist_reading, theater_ratio, 40, 0.43).
narrative_ontology:measurement(originalist_tr_t50, constitutional_text_authority__originalist_reading, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(originalist_be_t0, constitutional_text_authority__originalist_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(originalist_be_t10, constitutional_text_authority__originalist_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(originalist_be_t20, constitutional_text_authority__originalist_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(originalist_be_t30, constitutional_text_authority__originalist_reading, base_extractiveness, 30, 0.48).
narrative_ontology:measurement(originalist_be_t40, constitutional_text_authority__originalist_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(originalist_be_t50, constitutional_text_authority__originalist_reading, base_extractiveness, 50, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(originalist_su_t0, constitutional_text_authority__originalist_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(originalist_su_t10, constitutional_text_authority__originalist_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(originalist_su_t20, constitutional_text_authority__originalist_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(originalist_su_t30, constitutional_text_authority__originalist_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(originalist_su_t40, constitutional_text_authority__originalist_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement(originalist_su_t50, constitutional_text_authority__originalist_reading, suppression_requirement, 50, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, living_constitutionalist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, positivist_reading).

% DUAL FORMULATION NOTE:
% This constraint is the originalist reading of the constitutional_text_authority kernel. It is structurally distinct from the living constitutionalist reading (which treats meaning as evolving) and the positivist reading (which grounds validity in enactment procedure alone). Decomposition follows the epsilon-invariance principle: the three readings have different epsilon values, different stakeholder structures, and different failure modes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
