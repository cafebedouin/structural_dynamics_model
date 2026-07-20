% ============================================================================
% CONSTRAINT STORY: constitutional_text__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__judicial_supremacy_reading, []).

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
 *   constraint_id: constitutional_text__judicial_supremacy_reading
 *   human_readable: Constitutional Text â Judicial Supremacy Reading
 *   domain: constitutional theory / political philosophy / comparative law
 *
 * SUMMARY:
 *   Constitutional text grants courts final interpretive authority; judicial
 *   invalidation of legislation is the conclusive determination of
 *   constitutional meaning. This constraint instantiates the
 *   judicial_supremacy_reading of the constitutional_text kernel, in which
 *   the constitutional judiciary serves as the gatekeeper of constitutional
 *   meaning, legislative override is structurally impossible, and
 *   rights-claimants gain a judicial veto against majoritarian overreach at
 *   the cost of democratic responsiveness. Sibling readings assign final
 *   authority to legislative assemblies or to the constituent people.
 *
 * KEY AGENTS:
 *   - constitutional_judiciary: Agenda-setter (institutional/constrained) â administers final interpretive authority
 *   - rights_claimants: Primary beneficiary (moderate/constrained) â receives rights-protection coordination
 *   - legislative_assemblies: Primary target (institutional/constrained) â bears loss of legislative finality
 *   - electorate_majorities: Secondary target (organized/constrained) â bears democratic responsiveness reduction
 *   - constitutional_scholars: Analytical observer (analytical/analytical) â evaluates authority allocation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__judicial_supremacy_reading, 0.62).
domain_priors:suppression_score(constitutional_text__judicial_supremacy_reading, 0.58).
domain_priors:theater_ratio(constitutional_text__judicial_supremacy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text__judicial_supremacy_reading, "Constitutional Text â Judicial Supremacy Reading").
narrative_ontology:topic_domain(constitutional_text__judicial_supremacy_reading, "constitutional theory / political philosophy / comparative law").

domain_priors:requires_active_enforcement(constitutional_text__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__judicial_supremacy_reading, 'f9bd499a-d71e-4336-b898-145b14c83241').
narrative_ontology:cs_kernel_codification('f9bd499a-d71e-4336-b898-145b14c83241', fixed_text).
narrative_ontology:cs_authority_grounding('f9bd499a-d71e-4336-b898-145b14c83241', lineage).
narrative_ontology:cs_interpretation_layer_present('f9bd499a-d71e-4336-b898-145b14c83241').
narrative_ontology:cs_reading_relation('f9bd499a-d71e-4336-b898-145b14c83241', constitutional_text__legislative_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('f9bd499a-d71e-4336-b898-145b14c83241', constitutional_text__popular_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('f9bd499a-d71e-4336-b898-145b14c83241', foundational, judicial_finality_principle).
narrative_ontology:cs_axiom_status(judicial_finality_principle, holdable).
narrative_ontology:cs_axiom_grounding('f9bd499a-d71e-4336-b898-145b14c83241', judicial_finality_principle, conventional).
narrative_ontology:cs_axiom('f9bd499a-d71e-4336-b898-145b14c83241', foundational, constitutional_settlement_supremacy).
narrative_ontology:cs_axiom_status(constitutional_settlement_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('f9bd499a-d71e-4336-b898-145b14c83241', constitutional_settlement_supremacy, instrumental).
narrative_ontology:cs_reference_frame('f9bd499a-d71e-4336-b898-145b14c83241', constitutional_settlement_framework).
narrative_ontology:cs_drift_state('f9bd499a-d71e-4336-b898-145b14c83241', contemporary_political_mobilization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f9bd499a-d71e-4336-b898-145b14c83241', '').
narrative_ontology:cs_kernel_id(constitutional_text__judicial_supremacy_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__judicial_supremacy_reading, rights_claimants).
narrative_ontology:constraint_victim(constitutional_text__judicial_supremacy_reading, legislative_assemblies).
narrative_ontology:constraint_victim(constitutional_text__judicial_supremacy_reading, electorate_majorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets constitutional text and invalidates legislation found inconsistent with it. Operates under precedent and established interpretive methodology. Cannot easily abdicate the finality role without constitutional amendment or institutional collapse.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, constitutional_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Invoke constitutional rights before courts to challenge legislation. Receive binding remedies when courts agree. Depend entirely on judicial access and favorable doctrine; cannot secure rights protection through the ordinary political process alone under this reading.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, rights_claimants, beneficiary,
    moderate, biographical, constrained, national).

% Enact legislation subject to judicial review. Laws found unconstitutional are nullified and cannot be reinstated by simple majority. No legislative override mechanism exists; must amend constitution or await doctrinal reversal.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, legislative_assemblies, payer,
    institutional, biographical, constrained, national).

% Express policy preferences through elections that translate into legislation, but see those preferences overridden when courts find constitutional infirmity. Constitutional amendment is the only direct recourse, typically requiring supermajorities.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, electorate_majorities, payer,
    organized, biographical, constrained, national).

% Analyze and critique doctrines of judicial supremacy, legislative supremacy, and popular constitutionalism. Produce comparative and historical scholarship on authority allocation without direct institutional control over the constraint.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Settles constitutional meaning across political cycles and provides a stable forum for rights-claimants to challenge legislation that violates entrenched constitutional protections, reducing uncertainty about fundamental boundaries.
% TRANSFER_FUNCTION: Moves interpretive authority over constitutional meaning from elected legislatures and electorates to appointed courts; moves policy outcomes from majoritarian preference to rights-protected settlement.
% ABSENT_VOICES: Legislative supremacy advocates and popular constitutionalists who would argue that final interpretive authority should rest with elected bodies or the constituent people directly; they are structurally marginalized by the doctrine of judicial finality and excluded from the interpretive monopoly.
% DISAPPEARANCE_RATIONALE: If judicial supremacy vanished, legislatures would operate without judicial nullification, rights-claimants would lose a forum for vetoing legislation, and constitutional interpretation would migrate to political processes; the separation of powers and rights-protection architecture would reorganize around legislative or popular supremacy.
% FOUNDING_PROBLEM: How to prevent transient legislative majorities from violating fundamental rights and to ensure settled constitutional meaning persists across political cycles without being renegotiated by every new majority.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians and rights theorists outside the judiciary attest the problem of majoritarian overreach remains live; democratic theorists and legislative supremacy proponents attest the founding problem has shifted into judicial overreach and counter-majoritarian difficulty. Comparative constitutional scholars provide external corroboration that rights enforcement is a genuine coordination need, while political scientists document the democratic deficit.
narrative_ontology:disappearance_verdict(constitutional_text__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__judicial_supremacy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_text__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text__judicial_supremacy_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_text__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is substantial because judicial supremacy permanently removes legislative override from the policy space, transferring significant authority to unelected courts. Suppression (0.58) reflects the active doctrinal and institutional work required to maintain that legislatures cannot simply override constitutional interpretations. Theater ratio (0.42) captures the performative dimension of constitutional interpretation â doctrinal originality and interpretive methodology that increasingly serves to justify the authority structure itself. Accessibility collapse (0.72) is high because alternatives (legislative override, popular constitutionalism) are formally closed off in this reading. Resistance (0.55) reflects sustained majoritarian and populist pushback against judicial finality. Measurements trace the Warren Court expansion and subsequent normalization of judicial review as a high-extraction equilibrium.
 *
 * PERSPECTIVAL GAP:
 *   The constitutional judiciary seat experiences this constraint as a necessary coordination mechanism protecting rights against volatile majorities. The legislative and electorate seats experience the same structure as asymmetric extraction of democratic authority. The engine computes this divergence from structural data: beneficiaries with constrained exit versus victims with constrained exit produce opposed directionalities despite similar mobility profiles.
 *
 * DIRECTIONALITY LOGIC:
 *   Rights_claimants are declared beneficiaries and have constrained exit; they sit near the beneficiary pole (low d), receiving protective coordination. Legislative_assemblies and electorate_majorities are declared victims with constrained exit; they sit near the target pole (high d), experiencing extraction of democratic responsiveness. The constitutional_judiciary is agenda_setter but not declared beneficiary; without beneficiary status it derives a moderate d from its institutional power and constrained exit, reflecting that it administers rather than captures the extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   Without acknowledging the coordination function (rights protection, settlement across cycles), this would read as a pure snare â courts seizing power. Without acknowledging the victim side (democratic responsiveness, majoritarian exclusion), it would read as a rope â pure coordination. The Tangled Rope classification captures the structural reality: a genuine coordination function (rights enforcement) is fused with asymmetric extraction (democratic authority transfer), held in place by active enforcement (doctrine of finality, contempt power, institutional self-defense).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Does the constitutional text kernel structurally support the judicial_supremacy_reading, or do sibling readings (legislative_sovereignty, popular_sovereignty) better capture the kernel''s authority distribution?',
    'Historical-textual analysis of constitutional conventions and comparative constitutional practice to determine whether the kernel was designed for judicial finality or a different allocation.',
    'Resolution would shift classification to a different reading (and constraint story) if the kernel is found to support another seat''s finality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Whether judicial supremacy is the kernel''s intended reading').

omega_variable(
    sibling_reading_structural_delta,
    'Would assigning final authority to legislatures or to the constituent people eliminate the extractive asymmetry currently borne by majoritarian institutions, or would it simply relocate extraction to different seats?',
    'Comparative analysis of jurisdictions with legislative override or popular constitutionalism to measure democratic responsiveness and rights-protection outcomes.',
    'If extraction relocates rather than disappears, the kernel itself is tangled; if it disappears, judicial supremacy is the extractive locus.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Whether extraction is kernel-intrinsic or reading-specific').

omega_variable(
    judicial_finality_mandate_source,
    'Is judicial finality grounded in the constitutional text itself, or is it a doctrinal construction by the judiciary that extracts authority beyond the kernel''s explicit grant?',
    'Textual analysis of constitutional provisions empowering courts, combined with historical practice review of early judicial review.',
    'If textually grounded, the coordination function has stronger legitimacy; if doctrinally constructed, the extraction component is larger than the coordination justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_finality_mandate_source, empirical, 'Textual versus constructed basis for judicial finality').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__judicial_supremacy_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ctjsr_tr_t0, constitutional_text__judicial_supremacy_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ctjsr_tr_t16, constitutional_text__judicial_supremacy_reading, theater_ratio, 16, 0.25).
narrative_ontology:measurement(ctjsr_tr_t32, constitutional_text__judicial_supremacy_reading, theater_ratio, 32, 0.3).
narrative_ontology:measurement(ctjsr_tr_t48, constitutional_text__judicial_supremacy_reading, theater_ratio, 48, 0.35).
narrative_ontology:measurement(ctjsr_tr_t64, constitutional_text__judicial_supremacy_reading, theater_ratio, 64, 0.4).
narrative_ontology:measurement(ctjsr_tr_t80, constitutional_text__judicial_supremacy_reading, theater_ratio, 80, 0.42).

% Extraction over time
narrative_ontology:measurement(ctjsr_be_t0, constitutional_text__judicial_supremacy_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(ctjsr_be_t16, constitutional_text__judicial_supremacy_reading, base_extractiveness, 16, 0.52).
narrative_ontology:measurement(ctjsr_be_t32, constitutional_text__judicial_supremacy_reading, base_extractiveness, 32, 0.61).
narrative_ontology:measurement(ctjsr_be_t48, constitutional_text__judicial_supremacy_reading, base_extractiveness, 48, 0.65).
narrative_ontology:measurement(ctjsr_be_t64, constitutional_text__judicial_supremacy_reading, base_extractiveness, 64, 0.6).
narrative_ontology:measurement(ctjsr_be_t80, constitutional_text__judicial_supremacy_reading, base_extractiveness, 80, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(ctjsr_su_t0, constitutional_text__judicial_supremacy_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(ctjsr_su_t16, constitutional_text__judicial_supremacy_reading, suppression_requirement, 16, 0.45).
narrative_ontology:measurement(ctjsr_su_t32, constitutional_text__judicial_supremacy_reading, suppression_requirement, 32, 0.55).
narrative_ontology:measurement(ctjsr_su_t48, constitutional_text__judicial_supremacy_reading, suppression_requirement, 48, 0.6).
narrative_ontology:measurement(ctjsr_su_t64, constitutional_text__judicial_supremacy_reading, suppression_requirement, 64, 0.58).
narrative_ontology:measurement(ctjsr_su_t80, constitutional_text__judicial_supremacy_reading, suppression_requirement, 80, 0.59).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_text__judicial_supremacy_reading, legislative_sovereignty_reading).
narrative_ontology:affects_constraint(constitutional_text__judicial_supremacy_reading, popular_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is the judicial_supremacy_reading of the constitutional_text kernel. The kernel decomposes into three structurally distinct constraints because the same constitutional text is read by different commitment frameworks assigning final authority to different seats. Each reading has distinct beneficiaries, victims, and Îµ profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
