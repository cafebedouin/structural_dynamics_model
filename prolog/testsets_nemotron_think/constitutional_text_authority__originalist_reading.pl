% ============================================================================
% CONSTRAINT STORY: constitutional_text_authority__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: constitutional_text_authority__originalist_reading
 *   human_readable: Originalist Constitutional Authority (Fixed Meaning at Ratification)
 *   domain: constitutional_law/interpretive_jurisprudence
 *
 * SUMMARY:
 *   The originalist reading of constitutional authority instantiates a
 *   constraint that fixes constitutional meaning at the moment of
 *   ratification, treating historical public understanding as the sole
 *   legitimate ground for judicial decision-making. This reading emerged from
 *   the conservative legal movement's response to mid-20th century judicial
 *   activism and has become the dominant methodology on the U.S. Supreme
 *   Court. The constraint claims to be a neutral, mountain-like limit on
 *   judicial power — but its operation systematically benefits the
 *   conservative legal movement and originalist judges while extracting from
 *   progressive litigants, unenumerated rights seekers, and living
 *   constitutionalist judges. It requires active enforcement through judicial
 *   appointments, methodological policing, and institutional infrastructure
 *   (Federalist Society, originalist academic centers). The claim/metric
 *   divergence is deliberate: the reading claims mountain; the authored
 *   metrics describe a substantially extractive, actively enforced
 *   coordination mechanism — a tangled rope. The engine will compute per-seat
 *   classifications from the structural data; the analytical seat sees
 *   tangled rope, the originalist seat sees mountain, the progressive seat
 *   sees snare.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text_authority__originalist_reading, 0.68).
domain_priors:suppression_score(constitutional_text_authority__originalist_reading, 0.72).
domain_priors:theater_ratio(constitutional_text_authority__originalist_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text_authority__originalist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text_authority__originalist_reading, "Originalist Constitutional Authority (Fixed Meaning at Ratification)").
narrative_ontology:topic_domain(constitutional_text_authority__originalist_reading, "constitutional_law/interpretive_jurisprudence").

domain_priors:requires_active_enforcement(constitutional_text_authority__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text_authority__originalist_reading, 'cd346124-b5cf-44a4-8130-1df117a34d39').
narrative_ontology:cs_kernel_codification('cd346124-b5cf-44a4-8130-1df117a34d39', fixed_text).
narrative_ontology:cs_authority_grounding('cd346124-b5cf-44a4-8130-1df117a34d39', lineage).
narrative_ontology:cs_interpretation_layer_present('cd346124-b5cf-44a4-8130-1df117a34d39').
narrative_ontology:cs_reading_relation('cd346124-b5cf-44a4-8130-1df117a34d39', constitutional_text_authority__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('cd346124-b5cf-44a4-8130-1df117a34d39', constitutional_text_authority__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('cd346124-b5cf-44a4-8130-1df117a34d39', foundational, original_public_meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(original_public_meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('cd346124-b5cf-44a4-8130-1df117a34d39', original_public_meaning_fixed_at_ratification, empirically_contingent).
narrative_ontology:cs_axiom('cd346124-b5cf-44a4-8130-1df117a34d39', secondary, judicial_restraint_requires_historical_tether).
narrative_ontology:cs_axiom_status(judicial_restraint_requires_historical_tether, holdable).
narrative_ontology:cs_axiom_grounding('cd346124-b5cf-44a4-8130-1df117a34d39', judicial_restraint_requires_historical_tether, deontological).
narrative_ontology:cs_reference_frame('cd346124-b5cf-44a4-8130-1df117a34d39', founding_era_public_understanding).
narrative_ontology:cs_drift_state('cd346124-b5cf-44a4-8130-1df117a34d39', contemporary_originalist_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('cd346124-b5cf-44a4-8130-1df117a34d39', '').
narrative_ontology:cs_kernel_id(constitutional_text_authority__originalist_reading, constitutional_text_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, originalist_judges).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, conservative_legal_movement).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, originalist_scholars).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, progressive_litigants).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, unenumerated_rights_seekers).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, living_constitutionalist_judges).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, general_citizenry).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, general_citizenry).
narrative_ontology:constraint_vindicates(constitutional_text_authority__originalist_reading, judicial_restraint_doctrine).
narrative_ontology:constraint_vindicates(constitutional_text_authority__originalist_reading, democratic_legitimacy_of_fixed_meaning).
narrative_ontology:constraint_vindicates(constitutional_text_authority__originalist_reading, rule_of_law_as_fixed_rules).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control the authoritative interpretation of constitutional text through judicial opinions. Their professional identity and legitimacy within the conservative legal movement are fused with originalist methodology. Exit means abandoning a career-defining commitment and the institutional network that elevated them.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, originalist_judges, agenda_setter,
    institutional, generational, identity_locked, national).

% Built a decades-long pipeline (Federalist Society, judicial nominations, academic centers) that produces and protects originalist judges. Gains policy outcomes (gun rights, religious liberty, regulatory limits, abortion restrictions) that democratic majorities might not sustain. Exit means dismantling the movement's crown jewel.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, conservative_legal_movement, beneficiary,
    organized, generational, constrained, national).

% Produce the historical evidence and theoretical frameworks that legitimate originalist decisions. Gain professional status, funding, and influence within the conservative legal ecosystem. Their work is cited in opinions; their methodology becomes the standard. Exit means professional marginalization.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, originalist_scholars, beneficiary,
    organized, biographical, constrained, national).

% Bring claims for rights not recognized at ratification (reproductive autonomy, LGBTQ+ equality, voting rights protections, criminal procedure protections). Face a structural barrier: originalism treats their claims as policy preferences, not constitutional rights. Their exit option is legislative politics, where they face countermajoritarian obstacles.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, progressive_litigants, payer,
    moderate, biographical, constrained, national).

% Individuals and groups whose fundamental interests (bodily autonomy, dignity, equality) find no foothold in 1789 or 1868 public understanding. The constraint treats their exclusion as a feature, not a bug. No judicial exit; legislative exit blocked by the very structures originalism protects (malapportionment, veto points).
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, unenumerated_rights_seekers, payer,
    powerless, biographical, trapped, national).

% Judges whose interpretive commitments require engaging evolving moral principles and contemporary circumstances. Originalism's dominance marginalizes their methodology, treats their reasoning as illegitimate, and threatens their institutional legitimacy. Their identity as judges is bound to a rival interpretive tradition; exit means surrendering their judicial philosophy.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, living_constitutionalist_judges, payer,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text_authority__originalist_reading, living_constitutionalist_judges, excluded).

% Some benefit from stable, predictable constitutional rules that protect property, speech, and gun rights. Others pay when originalism blocks recognition of rights they experience as fundamental (privacy, equality, dignity). Exit is collective constitutional amendment (Article V) — functionally impossible on contested issues.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, general_citizenry, beneficiary,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text_authority__originalist_reading, general_citizenry, payer).

% Analyze the structural operation of originalism as a constraint system. Track its coordination benefits (determinacy, democratic legitimacy claims) and its extraction patterns (systematic disfavoring of modern rights claims, entrenchment of 18th/19th century power distributions). Do not collect rents or bear costs from its operation.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, constitutional_theorists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a determinate, historically anchored standard that constrains judicial discretion and claims democratic legitimacy by tethering constitutional meaning to the understandings of the ratifying public, rather than to the unconstrained moral judgments of unelected judges.
% TRANSFER_FUNCTION: Moves interpretive authority from contemporary judicial moral reasoning to historical linguistic and public-understanding evidence. Moves policy outcomes from majoritarian or evolving standards to fixed historical understandings — systematically transferring power from groups seeking recognition of new rights to groups defending the status quo ante.
% ABSENT_VOICES: The enslaved, women, indigenous peoples, and propertyless persons who were excluded from the ratifying public whose 'original understanding' now governs. Their descendants and analogous groups today (LGBTQ+ persons, undocumented immigrants, felons denied voting rights) are structurally excluded from the originalist conversation because the constraint defines the relevant 'public' as the historical one. They would object that a constitution they had no hand in ratifying is being used to deny them rights.
% DISAPPEARANCE_RATIONALE: If originalism vanished overnight, judicial discretion would expand dramatically. Unenumerated rights claims (privacy, autonomy, dignity, equality) would become cognizable. The Supreme Court's docket and doctrine would shift toward living constitutionalist or pragmatic frameworks. The conservative legal movement would lose its primary jurisprudential anchor. Democratic majorities would face fewer judicial vetoes on regulatory and social policy. The entire architecture of 21st-century constitutional law would reorganize.
% FOUNDING_PROBLEM: The perceived crisis of judicial activism in the mid-20th century (Warren/Burger Courts): unelected judges imposing policy preferences under the guise of constitutional interpretation, lacking a principled constraint on their discretion, and undermining democratic legitimacy.
% FOUNDING_PROBLEM_CORROBORATION: Originalists attest the problem is live, citing ongoing substantive due process expansions and non-originalist methodology. Critics (living constitutionalists, progressive scholars, political scientists) attest the founding problem is substantially solved or misdiagnosed: the Warren Court's activism was a historical episode; modern courts are more restrained; originalism itself is selectively applied to achieve conservative policy outcomes. Independent corroboration from political science (Judicial Behavior literature) shows originalist judges vote ideologically at rates comparable to non-originalists, undermining the 'constraint' claim.
narrative_ontology:disappearance_verdict(constitutional_text_authority__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text_authority__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text_authority__originalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_text_authority__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text_authority__originalist_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.68) is substantial because the constraint systematically channels constitutional outcomes toward 1789/1868 understandings, disfavoring modern rights claims whose recognition would require judicial creativity. The coordination function (determinacy, democratic legitimacy) is real but partial — originalist judges still disagree on historical evidence, and the methodology is selectively applied. Suppression (0.72) is high because the constraint's persistence depends on active exclusion of rival methodologies: living constitutionalism is treated as illegitimate, not merely wrong; non-originalist judges face legitimacy challenges; the appointment process filters for methodological conformity. Theater ratio (0.45) is moderate and rising: early originalism (1970s-80s) was scholarly and theoretical; contemporary originalism performs historical analysis while often reaching predetermined ideological outcomes. Accessibility collapse (0.62) is significant — once the originalist frame is accepted, alternatives (living constitutionalism, pragmatism) appear illegitimate, not just mistaken. Resistance (0.58) is real: living constitutionalist scholars, progressive litigants, and some judges contest originalism's historical claims and its selective application.
 *
 * PERSPECTIVAL GAP:
 *   The originalist seat experiences this constraint as a mountain — a fixed, natural limit on judicial power that happens to produce outcomes they favor. The progressive litigant seat experiences it as a snare — a coercive barrier that denies their rights while claiming neutrality. The analytical seat (this story) computes tangled rope: genuine coordination (determinacy, constraint on discretion) fused with asymmetric extraction (systematic favoring of founding-era power distributions). The engine's per-seat computation will capture this divergence; the authored metrics describe the constraint's aggregate operation, not any single seat's experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist judges and the conservative legal movement are structural beneficiaries (d near 0.0-0.2): they collect the constraint's gains (policy outcomes, institutional power, professional status) and control its enforcement. Progressive litigants and unenumerated rights seekers are structural targets (d near 0.8-1.0): they bear the constraint's costs (denied rights, blocked claims) with trapped or constrained exit. Living constitutionalist judges are targets with identity_locked exit — their professional identity is fused with the rival methodology. General citizenry sits near symmetric (d ~0.5): some benefit from stability, others pay from excluded rights. Constitutional theorists are analytical observers (d=0.5 by definition). The engine derives these directionalities from the beneficiary/victim declarations and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (judicial activism / democratic legitimacy deficit) is contested. Originalists argue it persists; critics argue originalism has become its own form of judicial activism — imposing 18th-century policy preferences on 21st-century society. The constraint persists not because the founding problem is universally acknowledged, but because the conservative legal movement has built a self-reinforcing institutional ecosystem (appointments pipeline, methodological orthodoxy, funding network) that makes originalism self-sustaining. This is mandatrophy: the arrangement's original justification is contested, but its institutional inertia and extraction infrastructure maintain it. The constraint would not survive if its beneficiaries did not actively maintain it — hence requires_active_enforcement: true.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    originalism_selective_application,
    'Does originalism function as a neutral constraint on judicial discretion, or is it selectively applied to reach conservative policy outcomes while ignoring originalist evidence that would produce liberal outcomes?',
    'Systematic coding of originalist opinions: rate of originalist methodology use when historical evidence supports conservative vs. liberal outcomes; citation patterns of historical sources; comparison with non-originalist judges'' voting patterns on same issues.',
    'If selective application is demonstrated, the constraint''s coordination function is compromised — it operates as a cover for ideological extraction. The claimed_type would shift from tangled_rope toward snare for the progressive seat. The originalist seat''s mountain claim would be falsified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalism_selective_application, empirical, 'Whether originalism''s methodological neutrality is genuine or performative.').

omega_variable(
    historical_evidence_underdetermination,
    'Is the historical record of original public meaning sufficiently determinate to constrain judicial outcomes, or does its underdetermination allow judges to reach preferred results under originalist cover?',
    'Inter-coder reliability studies among originalist scholars on contested provisions; analysis of disagreement rates in originalist opinions on the same historical question; comparison with non-originalist interpretive disagreement rates.',
    'If historical evidence is radically underdeterminate, the coordination function collapses — the constraint provides no more determinacy than living constitutionalism. The constraint would reclassify toward snare (pure extraction masquerading as coordination). If determinate, the coordination function is vindicated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(historical_evidence_underdetermination, conceptual, 'Whether the constraint''s coordination mechanism (historical evidence) has the determinacy it claims.').

omega_variable(
    kernel_framing_underdetermination,
    'Does the constitutional_text_authority kernel admit a single coherent framing (fixed text + lineage authority), or do rival framings (fixed text + extraction authority; distributed kernel + diffuse authority) produce different cs_pattern classifications for the same originalist reading?',
    'Compare cs_structure classifications under alternative framings: (a) fixed_text + lineage (declared) vs. (b) fixed_text + extraction (originalism as institutional power maintenance) vs. (c) distributed kernel (no single adjudicating authority). Assess whether reading_relations and axiom_overriding drift computations change.',
    'If alternative framings yield different terminal states (e.g., legitimate_preservation vs. captured_repudiation), the cs_pattern classification is framing-dependent. This would require documenting the framing ambiguity as a conceptual omega and treating the cs_pattern as underdetermined until framing is resolved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the commitment-system classification of this reading is stable across defensible framings of the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text_authority__originalist_reading, 0, 54).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(originalist_reading_tr_t0, constitutional_text_authority__originalist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(originalist_reading_tr_t9, constitutional_text_authority__originalist_reading, theater_ratio, 9, 0.2).
narrative_ontology:measurement(originalist_reading_tr_t18, constitutional_text_authority__originalist_reading, theater_ratio, 18, 0.28).
narrative_ontology:measurement(originalist_reading_tr_t27, constitutional_text_authority__originalist_reading, theater_ratio, 27, 0.35).
narrative_ontology:measurement(originalist_reading_tr_t36, constitutional_text_authority__originalist_reading, theater_ratio, 36, 0.4).
narrative_ontology:measurement(originalist_reading_tr_t45, constitutional_text_authority__originalist_reading, theater_ratio, 45, 0.43).
narrative_ontology:measurement(originalist_reading_tr_t54, constitutional_text_authority__originalist_reading, theater_ratio, 54, 0.45).

% Extraction over time
narrative_ontology:measurement(originalist_reading_be_t0, constitutional_text_authority__originalist_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(originalist_reading_be_t9, constitutional_text_authority__originalist_reading, base_extractiveness, 9, 0.35).
narrative_ontology:measurement(originalist_reading_be_t18, constitutional_text_authority__originalist_reading, base_extractiveness, 18, 0.45).
narrative_ontology:measurement(originalist_reading_be_t27, constitutional_text_authority__originalist_reading, base_extractiveness, 27, 0.55).
narrative_ontology:measurement(originalist_reading_be_t36, constitutional_text_authority__originalist_reading, base_extractiveness, 36, 0.62).
narrative_ontology:measurement(originalist_reading_be_t45, constitutional_text_authority__originalist_reading, base_extractiveness, 45, 0.66).
narrative_ontology:measurement(originalist_reading_be_t54, constitutional_text_authority__originalist_reading, base_extractiveness, 54, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(originalist_reading_su_t0, constitutional_text_authority__originalist_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(originalist_reading_su_t9, constitutional_text_authority__originalist_reading, suppression_requirement, 9, 0.4).
narrative_ontology:measurement(originalist_reading_su_t18, constitutional_text_authority__originalist_reading, suppression_requirement, 18, 0.52).
narrative_ontology:measurement(originalist_reading_su_t27, constitutional_text_authority__originalist_reading, suppression_requirement, 27, 0.6).
narrative_ontology:measurement(originalist_reading_su_t36, constitutional_text_authority__originalist_reading, suppression_requirement, 36, 0.65).
narrative_ontology:measurement(originalist_reading_su_t45, constitutional_text_authority__originalist_reading, suppression_requirement, 45, 0.69).
narrative_ontology:measurement(originalist_reading_su_t54, constitutional_text_authority__originalist_reading, suppression_requirement, 54, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text_authority__originalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(constitutional_text_authority__originalist_reading, 0.08).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, constitutional_text_authority__living_constitutionalist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, constitutional_text_authority__positivist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, judicial_review_legitimacy).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, unenumerated_rights_doctrine).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, stare_decisis_constraint).

% DUAL FORMULATION NOTE:
% This constraint is the originalist_reading of the constitutional_text_authority kernel. It decomposes the colloquial 'originalism' into a structurally precise claim: constitutional meaning fixed at ratification, enforced through originalist methodology. The living_constitutionalist_reading instantiates a rival constraint (evolving meaning, moral principles). The positivist_reading instantiates a different-axis constraint (validity from enactment). All three share the kernel but instantiate different constraints with different ε, beneficiaries, victims, and types. This decomposition follows the ε-invariance principle: the label 'constitutional interpretation' covers multiple structurally distinct claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_text_authority__originalist_reading, institutional, 0.15).
constraint_indexing:directionality_override(constitutional_text_authority__originalist_reading, organized, 0.2).
constraint_indexing:directionality_override(constitutional_text_authority__originalist_reading, moderate, 0.7).
constraint_indexing:directionality_override(constitutional_text_authority__originalist_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
