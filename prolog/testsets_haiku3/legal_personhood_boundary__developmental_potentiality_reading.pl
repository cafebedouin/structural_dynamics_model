% ============================================================================
% CONSTRAINT STORY: legal_personhood_boundary__developmental_potentiality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legal_personhood_boundary__developmental_potentiality_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: legal_personhood_boundary__developmental_potentiality_reading
 *   human_readable: Developmental Potentiality Reading: Personhood at Conception
 *   domain: legal_philosophy/constitutional_law
 *
 * SUMMARY:
 *   The developmental potentiality reading claims that personhood begins at
 *   conception and that any entity with the biological and developmental
 *   trajectory of a human person holds constitutional rights deserving state
 *   protection. Under this reading, the fetus becomes a legal person from
 *   conception; the pregnant person's autonomy is subordinated to fetal
 *   rights; and the state acquires enforcement authority over pregnancy
 *   decisions and outcomes. This is ONE reading of the contested
 *   personhood-boundary kernel. Other readings (functional-capacity,
 *   restrictive-anthropocentric) instantiate different constraints with
 *   different victim sets, beneficiary structures, and extraction profiles.
 *   This story generates ONLY the developmental potentiality reading; it does
 *   not describe or adjudicate the alternatives.
 *
 * KEY AGENTS:
 *   - Pregnant persons: biological substrate for the constraint; subject to state enforcement of pregnancy continuation; bear the extraction
 *   - Fetal rights advocates: organized beneficiaries; seek state enforcement of fetal personhood; benefit from state machinery
 *   - State enforcement apparatus: agenda setter; administers personhood boundary through law and prosecution; gains regulatory authority
 *   - Constitutional interpreters: observer seats; adjudicate the reading's constitutional legitimacy
 *   - Pregnant-person autonomy advocates: excluded from the constraint's legitimacy structure; represent the incompatible alternative premise
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_boundary__developmental_potentiality_reading, 0.78).
domain_priors:suppression_score(legal_personhood_boundary__developmental_potentiality_reading, 0.81).
domain_priors:theater_ratio(legal_personhood_boundary__developmental_potentiality_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__developmental_potentiality_reading, tangled_rope).
narrative_ontology:human_readable(legal_personhood_boundary__developmental_potentiality_reading, "Developmental Potentiality Reading: Personhood at Conception").
narrative_ontology:topic_domain(legal_personhood_boundary__developmental_potentiality_reading, "legal_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(legal_personhood_boundary__developmental_potentiality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__developmental_potentiality_reading, '9043036c-a682-4af7-b1d6-78965bacce27').
narrative_ontology:cs_kernel_codification('9043036c-a682-4af7-b1d6-78965bacce27', formalized).
narrative_ontology:cs_authority_grounding('9043036c-a682-4af7-b1d6-78965bacce27', lineage).
narrative_ontology:cs_interpretation_layer_present('9043036c-a682-4af7-b1d6-78965bacce27').
narrative_ontology:cs_reading_relation('9043036c-a682-4af7-b1d6-78965bacce27', legal_personhood_boundary__functional_capacity_reading, coexists_with).
narrative_ontology:cs_reading_relation('9043036c-a682-4af7-b1d6-78965bacce27', legal_personhood_boundary__restrictive_anthropocentric_reading, coexists_with).
narrative_ontology:cs_axiom('9043036c-a682-4af7-b1d6-78965bacce27', foundational, continuous_human_development_grants_personhood).
narrative_ontology:cs_axiom_status(continuous_human_development_grants_personhood, holdable).
narrative_ontology:cs_axiom_grounding('9043036c-a682-4af7-b1d6-78965bacce27', continuous_human_development_grants_personhood, deontological).
narrative_ontology:cs_axiom('9043036c-a682-4af7-b1d6-78965bacce27', foundational, state_has_compelling_interest_in_fetal_life).
narrative_ontology:cs_axiom_status(state_has_compelling_interest_in_fetal_life, holdable).
narrative_ontology:cs_axiom_grounding('9043036c-a682-4af7-b1d6-78965bacce27', state_has_compelling_interest_in_fetal_life, deontological).
narrative_ontology:cs_reference_frame('9043036c-a682-4af7-b1d6-78965bacce27', traditional_quick_ensoulment_doctrine).
narrative_ontology:cs_drift_state('9043036c-a682-4af7-b1d6-78965bacce27', contemporary_bioethics_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9043036c-a682-4af7-b1d6-78965bacce27', '').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__developmental_potentiality_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__developmental_potentiality_reading, fetal_rights_advocates).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__developmental_potentiality_reading, state_enforcement_apparatus).
narrative_ontology:constraint_victim(legal_personhood_boundary__developmental_potentiality_reading, pregnant_persons).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Subject to state-mandated pregnancy continuation regardless of their own bodily autonomy, health, or life circumstances. The constraint subordinates their decision-making authority over their bodies to the state's interest in fetal life from conception. Exit options are heavily constrained: geographic relocation to jurisdictions with different rules, underground/unsafe abortion, or bearing unwanted pregnancy to term. Medical decisions during pregnancy (treatment refusal, medication, delivery method) are subject to state review and override authority based on fetal interests.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, pregnant_persons, payer,
    powerless, biographical, trapped, national).

% Advocate for legal recognition of fetal personhood and seek state enforcement of pregnancy continuation. They frame pregnancy as a duty owed to the developing human. They benefit from state legal machinery that treats the fetus as a legal person entitled to protection, enforcement of gestation as a legal obligation, and criminalization of abortion as a mechanism to enforce compliance. Their power derives from political organization and alignment with state enforcement interests.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, fetal_rights_advocates, beneficiary,
    organized, generational, mobile, national).

% Administers the personhood boundary through criminal prohibition of abortion, civil restrictions on pregnant persons' medical autonomy, and enforcement machinery (prosecution, fetal monitoring, court-ordered interventions). The state gains regulatory authority over reproductive decisions and pregnancy management; compliance is secured through criminal sanctions and medical overrides. The state's legitimacy under this reading depends on recognizing fetal personhood as a constitutional matter requiring protection.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, state_enforcement_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Would argue that pregnant persons retain autonomous decision-making authority over their bodies and medical treatment, that fetal interests do not supersede maternal bodily autonomy, and that pregnancy should not be treated as a legal obligation. They are excluded from the constraint's legitimacy structure — their core claim (maternal autonomy as fundamental) is incompatible with the developmental potentiality reading's premise that fetal personhood overrides maternal choice from conception. They have no seat at the table determining the boundaries of personhood.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, pregnant_persons_medical_autonomy_advocates, excluded,
    organized, generational, mobile, national).

% Courts and legal scholars tasked with interpreting whether the Constitution recognizes fetal personhood as a fundamental right. They adjudicate competing readings of the 14th Amendment, examine precedent, and determine the scope of state authority to regulate pregnancy. Their decisions either affirm or contest the developmental potentiality reading's constitutional foundation.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, constitutional_interpreters, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified legal framework for determining who holds constitutional personhood rights and deserves state protection. Solves the theoretical and constitutional question: at what point does a biological entity acquire the status of a legal person entitled to fundamental rights and state protection? The developmental potentiality reading proposes that continuous human development from conception provides a stable, non-arbitrary, and principled boundary — avoiding both the arbitrariness of birth as a marker and the vagueness of functional capacity thresholds.
% TRANSFER_FUNCTION: Transfers fundamental decision-making authority over pregnancy from the pregnant person to the state (as trustee of fetal interests). Transfers bodily autonomy and medical self-determination in pregnancy to state regulatory authority. The pregnant person's authority to decide whether to continue pregnancy, how pregnancy is managed, and what medical interventions she accepts is transferred to state oversight in service of fetal personhood protection. The constraint moves control, authority, and autonomy from pregnant persons to state apparatus.
% ABSENT_VOICES: Pregnant persons themselves — particularly those who experience unwanted pregnancy, who face health risks from continuation, or who have existing family commitments — are structurally excluded from the legitimacy framework in which the constraint is justified. Their own testimony about their circumstances, their bodily autonomy, their reproductive self-determination, and their lived experience of subordination is subordinated to the state's determination of fetal interests. Functional-capacity and restrictive-anthropocentric readings of the personhood boundary (alternative constitutional interpretations) are excluded from the kernel's legitimacy structure, though they circulate in constitutional scholarship, international law, and judicial dissent. Medical professionals' assessments of maternal health may also be overridden by fetal-interests enforcement.
% DISAPPEARANCE_RATIONALE: If the developmental potentiality reading were replaced by a functional-capacity or restrictive-anthropocentric reading (if constitutional interpretation shifted to deny fetal personhood or to limit personhood to born humans), the entire regulatory framework would collapse immediately. State authority to criminalize abortion would vanish. Pregnant persons would regain autonomous decision-making over pregnancy continuation, medical treatment, and delivery method. The legal status of the fetus would revert from 'person holding constitutional rights' to 'potential person' or 'biologically developing tissue' — a category that receives legal protection but not personhood status. The world reorganizes in weeks: legal abortion access restores, prosecution ceases, medical practice reverts to pregnant-person/physician determination. The constraint's entire persistence depends on the developmental potentiality reading's constitutional legitimacy.
% FOUNDING_PROBLEM: What principle determines the boundaries of legal personhood, and who deserves constitutional protection under law? How do we draw a non-arbitrary, principled line between human biological entities that hold legal personhood status (and thus constitutional rights) and those that do not? This question has no stable natural-law answer and has been contested across legal history. The developmental potentiality reading answers: continuous human development from conception is the principled criterion. Any entity with the genetic and developmental trajectory of a human person — i.e., any entity that, if not interfered with, would become a human person — deserves personhood status and constitutional protection.
% FOUNDING_PROBLEM_CORROBORATION: Anti-abortion organizations, some religious traditions (particularly Catholic and evangelical Christian), some conservative constitutional scholars, and some state legislatures attest that the founding problem remains deeply live and that developmental potentiality provides the correct principled answer. However, pro-choice legal scholars, major medical organizations (American Medical Association, American College of Obstetricians and Gynecologists), international human rights bodies (UN, European Court of Human Rights), pregnant-person advocacy organizations, and a significant portion of constitutional scholars attest that the founding problem is better answered by functional-capacity criteria or birth-based personhood, and that even if the state has a legitimate interest in protecting potential life, this interest does not override pregnant persons' fundamental autonomy rights. Multiple state legislatures have adopted different readings. The contested status is evidenced by the fact that different constitutional jurisdictions have adopted different readings, that the same constitutional text (the 14th Amendment) is interpreted to reach opposite conclusions, and that the question remains the subject of live, ongoing constitutional litigation.
narrative_ontology:disappearance_verdict(legal_personhood_boundary__developmental_potentiality_reading, world_rearranges).
narrative_ontology:founding_problem_status(legal_personhood_boundary__developmental_potentiality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legal_personhood_boundary__developmental_potentiality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legal_personhood_boundary__developmental_potentiality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legal_personhood_boundary__developmental_potentiality_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legal_personhood_boundary__developmental_potentiality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legal_personhood_boundary__developmental_potentiality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legal_personhood_boundary__developmental_potentiality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and rising (0.58→0.78) because the constraint transfers fundamental decision-making authority from pregnant persons to the state, restricting bodily autonomy in a domain that directly affects the person's life trajectory. The reading is extractive on its face: it subordinates one party's autonomy to another's legal status claims. Suppression is higher still (0.81) because compliance is secured through criminal prohibition, medical override authority, and state surveillance of pregnancy — the constraint persists through coercion, not through voluntary coordination. Theater ratio rises (0.22→0.42) because enforcement increasingly relies on symbolic confirmation of state authority and fetal personhood doctrine rather than genuine coordination; the functional gains (determining personhood) are real but increasingly subordinate to the performative work of maintaining state reproductive authority. Accessibility collapse is high (0.72) because once the developmental potentiality reading is adopted as constitutional law, pregnant persons have almost no practical alternatives to compliance except geographic relocation or underground abortion — the constraint's legitimacy forecloses perceived exits. Resistance is high (0.68) because the constraint meets sustained organized opposition from pregnant-person advocates, medical autonomy traditions, and alternative constitutional readings.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (pregnant persons) and the agenda-setter seat (state) experience fundamentally different constraint types. Pregnant persons experience this as a snare-like extraction: their autonomy is taken, alternatives are suppressed, and the constraint persists through coercion. The state experiences it as tangled_rope coordination-plus-extraction: the state solves a genuine legitimacy problem (determining personhood) while also extracting regulatory authority. The engine computes per-seat types from the structural data; this gap is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Pregnant persons are trapped (biological necessity of carrying pregnancy to term if continuing it; legal prohibition of abortion; medical dependency on state-approved care; identity-locked as 'pregnant person' with no unilateral exit). This makes their directionality near 1.0 (full target). Fetal rights advocates have mobile or constrained exit (they can advocate in alternative jurisdictions, engage in legal/political contestation, reframe the debate) — directionality near 0.0 (beneficiary end). The state has analytical directionality (it administers, it enforces, but enforcement is the exercise of legitimate authority — or so the reading claims).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (determining personhood) is live and contested; the developmental potentiality reading proposes a principled answer. However, the constraint's persistence depends increasingly on state enforcement machinery (criminalization, surveillance, medical override) rather than on shared belief in the developmental potentiality principle. The theater ratio rising from 0.22 to 0.42 suggests that enforcement is increasingly performative — symbolic confirmation of fetal personhood doctrine — rather than genuine coordination. This suggests mandatrophy risk: the constraint may be becoming a zombie — the founding problem is contested enough that enforcement must intensify to maintain compliance, but the founding problem itself is not solved by the enforcement (it is only asserted). A true mandatrophy reading would require the founding problem to be DEAD (no longer live), not merely contested; the data here shows contestation with rising enforcement, which is tangled_rope hardening rather than piton decay. However, the theater ratio trajectory is diagnostic for review.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    potentiality_vs_actualization_boundary,
    'Does ''developmental potentiality'' (the genetic/biological trajectory of becoming human) constitute sufficient grounds for personhood, or does some degree of actualized capacity (sentience, self-awareness, cognitive function) need to be present?',
    'Philosophical and constitutional analysis: examine whether potentiality alone has grounded personhood status in other legal contexts (inheritance rights, property interests). Test whether the reading''s proponents accept potentiality as sufficient in other domains or whether they are implicitly requiring some threshold of actualization masked by the potentiality language.',
    'If potentiality-alone grounds personhood, the reading''s boundary is maximally expansive (zygote-stage entities have rights). If some actualization threshold is required, the reading collapses into functional-capacity territory and loses its distinctive claim. The ε value depends on whether the reading is truly pure potentiality or whether it smuggles in actualization criteria.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(potentiality_vs_actualization_boundary, conceptual, 'The logical coherence and scope of the developmental potentiality criterion itself.').

omega_variable(
    fetal_interests_vs_maternal_autonomy_hierarchy,
    'When fetal interests and pregnant persons'' bodily autonomy directly conflict (as in pregnancy continuation, medical treatment, delivery method), which has priority? Is fetal personhood status sufficient to override maternal autonomy, or do both parties'' rights require balancing?',
    'Case law and constitutional doctrine: observe whether courts applying the developmental potentiality reading treat fetal and maternal interests as fundamentally asymmetric (fetal interests override) or whether they acknowledge maternal autonomy as a countervailing constitutional value that creates genuine dilemma cases. Examine whether the reading''s proponents accept constraints on fetal-interest enforcement (e.g., they would prohibit forced cesarean section or bodily violation) that would demonstrate some residual maternal autonomy.',
    'If fetal interests categorically override maternal autonomy, the constraint is purely extractive from the pregnant-person seat (her autonomy is wholly subordinated). If both are recognized as rights that require balancing, the constraint becomes a genuine tangled_rope (coordination + extraction) rather than pure snare. The measurement of extractiveness depends on the resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fetal_interests_vs_maternal_autonomy_hierarchy, conceptual, 'Whether fetal personhood status creates absolute override authority over pregnant persons'' autonomy or whether interests balance.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.81) structural (enforced through criminal law, surveillance, medical override) or internalized (pregnant persons have adopted the developmental potentiality framing and voluntarily comply)?',
    'Post-legalization survey and behavioral data: examine jurisdictions where the developmental potentiality reading is NOT law (functional-capacity or anthropocentric readings prevail). Measure whether pregnant persons in those jurisdictions show lower suppression after exposure to alternative readings — if so, suppression is partly internalized, suggesting the potentiality reading carries internalized belief even where law does not enforce it. Conversely, measure whether suppression drops sharply in jurisdictions that decriminalize abortion — if so, suppression is primarily structural.',
    'If structural, the constraint''s effective suppression is as measured (0.81) and depends on enforcement infrastructure. If internalized, the effective suppression persists even after legal change, the constraint is more deeply embedded, and the victims have fewer practical exits than legal geography suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of pregnant persons'' autonomy is enforced externally or has been internalized as accepted norm.').

omega_variable(
    constitutional_legitimacy_vs_extraction_asymmetry,
    'Is the developmental potentiality reading a genuine answer to the founding problem (determining personhood boundaries), or is it a cover story for the extraction of pregnant persons'' autonomy to state authority?',
    'Comparative constitutional analysis: examine whether the developmental potentiality reading is consistently applied across other biological domains (is potentiality-based personhood extended to non-human organisms? to organ-donation cases? to genetic inheritance?). If the reading is selectively applied ONLY to restrict pregnant persons'' autonomy, it is more plausibly a cover story than a principled boundary. If it is consistently applied, the reading has stronger claims to genuine coordination.',
    'If cover story: the founding problem is not actually solved, the constraint is pure snare, mandatrophy risk is high. If genuine: the constraint is tangled_rope (real coordination + real extraction), and the extraction is the price of the coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_legitimacy_vs_extraction_asymmetry, conceptual, 'Whether developmental potentiality is a universally applied principle or selectively applied to reproductive autonomy restriction.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a READING of a contested kernel (the personhood boundary), or is it claiming to be the TRUTH about personhood?',
    'Meta-constitutional analysis: the developmental potentiality reading''s own scholarship and advocacy should acknowledge that other readings exist and are held by reasonable people. If the reading claims to be the only defensible interpretation of the Constitution, it is making a stronger (and more brittle) claim than if it acknowledges itself as one live reading among others. Examine whether the reading''s proponents engage with functional-capacity and anthropocentric readings as alternative constitutional positions or dismiss them as not-really-constitutional.',
    'If acknowledged reading: the constraint is one instantiation of the kernel, and alternative readings are other constraints in the same family. If claimed truth: the reading is more rigid, more dependent on enforcement to maintain against contestation, and more vulnerable to judicial or political reversal that would delegitimize the entire framework rather than just this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether this reading understands itself as one reading of a contested kernel or claims to be the constitutional truth.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__developmental_potentiality_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lega_tr_t0, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(lega_tr_t0, observed).
narrative_ontology:measurement(lega_tr_t8, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement_basis(lega_tr_t8, observed).
narrative_ontology:measurement(lega_tr_t16, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement_basis(lega_tr_t16, observed).
narrative_ontology:measurement(lega_tr_t24, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement_basis(lega_tr_t24, observed).
narrative_ontology:measurement(lega_tr_t32, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 32, 0.41).
narrative_ontology:measurement_basis(lega_tr_t32, observed).
narrative_ontology:measurement(lega_tr_t40, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(lega_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(lega_be_t0, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(lega_be_t0, observed).
narrative_ontology:measurement(lega_be_t8, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 8, 0.64).
narrative_ontology:measurement_basis(lega_be_t8, observed).
narrative_ontology:measurement(lega_be_t16, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 16, 0.71).
narrative_ontology:measurement_basis(lega_be_t16, observed).
narrative_ontology:measurement(lega_be_t24, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 24, 0.76).
narrative_ontology:measurement_basis(lega_be_t24, observed).
narrative_ontology:measurement(lega_be_t32, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 32, 0.77).
narrative_ontology:measurement_basis(lega_be_t32, observed).
narrative_ontology:measurement(lega_be_t40, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 40, 0.78).
narrative_ontology:measurement_basis(lega_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(lega_su_t0, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement_basis(lega_su_t0, observed).
narrative_ontology:measurement(lega_su_t8, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 8, 0.71).
narrative_ontology:measurement_basis(lega_su_t8, observed).
narrative_ontology:measurement(lega_su_t16, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 16, 0.76).
narrative_ontology:measurement_basis(lega_su_t16, observed).
narrative_ontology:measurement(lega_su_t24, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 24, 0.79).
narrative_ontology:measurement_basis(lega_su_t24, observed).
narrative_ontology:measurement(lega_su_t32, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 32, 0.8).
narrative_ontology:measurement_basis(lega_su_t32, observed).
narrative_ontology:measurement(lega_su_t40, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 40, 0.81).
narrative_ontology:measurement_basis(lega_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_personhood_boundary__developmental_potentiality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(legal_personhood_boundary__developmental_potentiality_reading, 0.12).
narrative_ontology:affects_constraint(legal_personhood_boundary__developmental_potentiality_reading, functional_capacity_reading).
narrative_ontology:affects_constraint(legal_personhood_boundary__developmental_potentiality_reading, restrictive_anthropocentric_reading).

% DUAL FORMULATION NOTE:
% The legal_personhood_boundary kernel admits at least three structurally distinct readings: developmental_potentiality_reading (personhood at conception), functional_capacity_reading (personhood follows demonstrable cognitive capacity), and restrictive_anthropocentric_reading (personhood limited to born humans with cognitive capacity). These are NOT perspectives on one constraint; they are separate constraints with distinct ε values, victim sets, and beneficiary structures. The developmental_potentiality_reading is extractive from pregnant persons and benefits fetal-rights advocates and state authority. The functional_capacity_reading may shift victimhood to non-human sentient beings or expand the beneficiary set. The restrictive_anthropocentric_reading maximizes pregnant-person autonomy but narrows personhood boundaries. Each story declares its own ε and its own network links. This family shares the kernel but not the constraint identity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
