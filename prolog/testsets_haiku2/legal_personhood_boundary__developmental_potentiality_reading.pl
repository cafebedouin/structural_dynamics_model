% ============================================================================
% CONSTRAINT STORY: legal_personhood_boundary__developmental_potentiality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: legal_personhood_boundary__developmental_potentiality_reading
 *   human_readable: Developmental Potentiality Personhood Boundary (Conception Reading)
 *   domain: legal/constitutional/rights
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_boundary__developmental_potentiality_reading, 0.78).
domain_priors:suppression_score(legal_personhood_boundary__developmental_potentiality_reading, 0.81).
domain_priors:theater_ratio(legal_personhood_boundary__developmental_potentiality_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__developmental_potentiality_reading, tangled_rope).
narrative_ontology:human_readable(legal_personhood_boundary__developmental_potentiality_reading, "Developmental Potentiality Personhood Boundary (Conception Reading)").
narrative_ontology:topic_domain(legal_personhood_boundary__developmental_potentiality_reading, "legal/constitutional/rights").

domain_priors:requires_active_enforcement(legal_personhood_boundary__developmental_potentiality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__developmental_potentiality_reading, 'd82cd99c-a79c-4c6c-be85-f61bae2ed4b9').
narrative_ontology:cs_kernel_codification('d82cd99c-a79c-4c6c-be85-f61bae2ed4b9', formalized).
narrative_ontology:cs_authority_grounding('d82cd99c-a79c-4c6c-be85-f61bae2ed4b9', lineage).
narrative_ontology:cs_interpretation_layer_present('d82cd99c-a79c-4c6c-be85-f61bae2ed4b9').
narrative_ontology:cs_reading_relation('d82cd99c-a79c-4c6c-be85-f61bae2ed4b9', legal_personhood_boundary__functional_capacity_reading, forecloses).
narrative_ontology:cs_reading_relation('d82cd99c-a79c-4c6c-be85-f61bae2ed4b9', legal_personhood_boundary__restrictive_anthropocentric_reading, coexists_with).
narrative_ontology:cs_axiom('d82cd99c-a79c-4c6c-be85-f61bae2ed4b9', foundational, potentiality_grounds_personhood).
narrative_ontology:cs_axiom_status(potentiality_grounds_personhood, holdable).
narrative_ontology:cs_axiom_grounding('d82cd99c-a79c-4c6c-be85-f61bae2ed4b9', potentiality_grounds_personhood, deontological).
narrative_ontology:cs_axiom('d82cd99c-a79c-4c6c-be85-f61bae2ed4b9', foundational, fetal_life_protection_priority).
narrative_ontology:cs_axiom_status(fetal_life_protection_priority, holdable).
narrative_ontology:cs_axiom_grounding('d82cd99c-a79c-4c6c-be85-f61bae2ed4b9', fetal_life_protection_priority, deontological).
narrative_ontology:cs_reference_frame('d82cd99c-a79c-4c6c-be85-f61bae2ed4b9', fetal_personhood_from_conception).
narrative_ontology:cs_drift_state('d82cd99c-a79c-4c6c-be85-f61bae2ed4b9', contemporary_post_dobbs_era, gap(codification_collapse, substantial, true)).
narrative_ontology:cs_created_at('d82cd99c-a79c-4c6c-be85-f61bae2ed4b9', '').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__developmental_potentiality_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__developmental_potentiality_reading, anti_abortion_movement).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__developmental_potentiality_reading, fetal_rights_advocates).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__developmental_potentiality_reading, state_reproductive_authority).
narrative_ontology:constraint_victim(legal_personhood_boundary__developmental_potentiality_reading, pregnant_persons).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(legal_personhood_boundary__developmental_potentiality_reading, medical_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Persons capable of pregnancy bear the enforcement cost of the personhood boundary through bodily use claims on behalf of the fetus, transfer of medical decision-making authority to state or fetal rights advocates, and mandatory pregnancy continuation regardless of health risk, rape, or consent. The biological reality of pregnancy is identity-constitutive; exit from the constraint requires jurisdictional exit (traveling to a jurisdiction with a different personhood boundary), which has become increasingly criminalized and surveilled post-Dobbs. Their situation is that they are subject to a rule set by others, about their own bodies, from which they cannot exit without extraordinary cost.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, pregnant_persons, payer,
    moderate, biographical, identity_locked, national).

% Under this reading, a fetus is granted full legal personhood from conception, entitling it to rights to life, bodily integrity, and state protection. The fetus cannot exercise these rights independently; their enforcement occurs through constraint on the pregnant person's decision-making and bodily use. The fetus is classified as a non-agent entity (agent: false) because it cannot act in the world; it is a beneficiary of the constraint's operation but not a party who can negotiate or consent.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, fetus, beneficiary,
    powerless, civilizational, trapped, national).
narrative_ontology:stakeholder_non_agent(legal_personhood_boundary__developmental_potentiality_reading, fetus).

% Sets and enforces the developmental potentiality personhood reading through legislative advocacy, ballot measures, litigation, and institutional capture of reproductive health policy and medical regulation. Collects political power, moral authority, institutional legitimacy, and cultural dominance from successful establishment of personhood from conception. Has high exit options — can shift to new jurisdictions or new policy domains if one jurisdiction liberalizes. Functions as the primary agenda-setting force that maintains the constraint's institutional embedding.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, anti_abortion_movement, agenda_setter,
    organized, civilizational, mobile, national).

% Administers and enforces the personhood boundary through criminal law (criminalization of abortion provision and sometimes abortion seeking), medical regulation (mandatory fetal protection policies, restrictions on standard-of-care pregnancy treatments), forced birth laws, and surveillance of pregnancy outcomes. Collects enforcement authority, institutional legitimacy, and monopoly over the definition of legal personhood from the constraint's operation. Acts as the institutional seat of the constraint's coercive machinery; its analytical exit position reflects the fact that institutional actors can theoretically change policy, though political pressure makes exit costly.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, state_reproductive_authority, agenda_setter,
    institutional, generational, analytical, national).

% Would argue for a different personhood boundary (viability, birth, or functional capacity) and object to the developmental potentiality reading on grounds that it subordinates pregnant persons' bodily autonomy, self-determination, and equality to fetal status claims; that it imposes a metaphysical position as if it were settled fact; and that it treats pregnancy as a status of subordination rather than a choice. Structurally excluded from setting the boundary in jurisdictions enforcing this reading; their presence in the constitutional conversation is acknowledged but overridden by supermajority enforcement. Their constrained exit reflects that they remain present in the jurisdiction even when politically defeated.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, abortion_rights_advocates, excluded,
    organized, civilizational, constrained, national).

% Bear the enforcement burden of the constraint through criminalization of abortion provision, mandatory reporting of pregnancy loss, restriction of standard-of-care medical treatments that might terminate pregnancy (medication that could induce miscarriage is withheld even when medically indicated), legal liability for medical judgments that conflict with fetal personhood claims, and forced participation in fetal protection enforcement. Professional medical autonomy is subordinated to state personhood enforcement authority. Exit is constrained — practitioners can relocate to non-enforcing jurisdictions, but doing so abandons their existing practice and patient population.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, medical_practitioners, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(legal_personhood_boundary__developmental_potentiality_reading, medical_practitioners, observer).

% Adjudicate conflicts between the personhood boundary and other constitutional rights (bodily autonomy, liberty, equal protection, privacy, freedom of movement). In enforcing jurisdictions, courts typically uphold the personhood boundary as a settled constitutional matter (post-Dobbs, U.S. courts returned authority to states to establish personhood boundaries, and many states have adopted the developmental potentiality reading). In non-enforcing jurisdictions, courts may invalidate personhood-from-conception laws on constitutional grounds. Courts' analytical exit position reflects their institutional role: they can theoretically reinterpret the constitution, though political pressure constrains their movement.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% Persons with cognitive disabilities or conditions that impact capacity are structurally implicated by the personhood boundary, though not explicitly named as stakeholders. If personhood requires potentiality but not current capacity, then persons whose capacity is permanently limited or absent face a potential threat to their personhood status — the reading's logic could be extended to question their continued personhood. They are excluded from setting the boundary but would have grounds to object. Their presence in the conversation remains largely unacknowledged, making them an absent voice in the founding problem's corroboration.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, persons_with_disabilities, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legal_personhood_boundary__developmental_potentiality_reading, anti_abortion_movement).
narrative_ontology:fixing_cost_class(legal_personhood_boundary__developmental_potentiality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified, legally-binding definition of personhood status applicable across jurisdictions: removes uncertainty about the moment at which a human entity acquires full rights protection and state enforcement authority. Solves the problem of whether an entity with human genetic code possessing the inherent capacity to develop into a rights-bearing human deserves legal personhood from inception.
% TRANSFER_FUNCTION: Transfers reproductive decision-making authority from pregnant persons (who would have autonomy to terminate pregnancy) to state institutions and anti-abortion movement advocates. Transfers bodily use claims from pregnant persons' exclusive control to the fetus and state enforcement of fetal claims. Transfers enforcement legitimacy and political power to the anti-abortion movement and state institutions; they collect from the constraint's operation.
% ABSENT_VOICES: Pregnant persons have biological centrality to pregnancy but are systematically limited in political voice in jurisdictions enforcing this reading — legislative supermajorities, judicial supermajorities, or constitutional amendment supermajorities are typically required to change personhood law. Abortion rights advocates are structurally excluded from setting the boundary in enforcing jurisdictions. Persons with prior pregnancy loss, miscarriage, or ectopic pregnancy (who would be criminalized for pregnancy loss under some extreme versions of the reading) are not represented in the founding problem narrative. Persons with disabilities who might face threats to their personhood status under extended applications of the potentiality criterion are also absent from the conversation.
% DISAPPEARANCE_RATIONALE: If the developmental potentiality personhood boundary disappeared overnight (replaced by a functional capacity, viability, birth, or consent-based reading), the institutional, legal, and reproductive landscape would reorganize substantially. Reproductive decision-making authority would revert to pregnant persons and medical practitioners. Abortion would become legally permissible in jurisdictions adopting an alternative reading. Medical treatment of pregnancy complications would normalize (medication-induced miscarriage treatment, ectopic pregnancy intervention, etc.). Criminal enforcement machinery targeting pregnant persons and abortion providers would retract. The state's authority over pregnancy outcomes would narrow. This is not a marginal adjustment — it is a fundamental restructuring of who controls reproductive decisions.
% FOUNDING_PROBLEM: The founding problem this reading addresses is: At what point in human development does an entity acquire full legal personhood and the rights that accompany personhood? This is a genuinely important question for law, because personhood status determines which entities have rights, which relationships are legally enforceable, and which state obligations apply. The developmental potentiality reading answers: from conception, because from that moment an entity possesses the inherent capacity to develop into a rights-bearing human being. This answer is offered as a resolution of the philosophical and legal question, grounded in the claim that potentiality itself is morally and legally relevant.
% FOUNDING_PROBLEM_CORROBORATION: Anti-abortion movement leadership and religious authorities attest that the founding problem is live and that the developmental potentiality answer is correct — that an entity with human genetic code possesses inherent moral status from conception and therefore deserves legal protection. Abortion rights advocates, constitutional scholars outside the anti-abortion movement, and medical anthropologists attest that the founding problem is NOT resolved — that the personhood boundary remains fundamentally contestable and that the developmental potentiality reading is one interpretation among several coherent alternatives (functional capacity, birth, or self-identification-based readings are philosophically defensible). Embryologists and developmental biologists note that the constraint rests on a metaphysical claim about potentiality, not on biological fact — biological development is observable, but personhood is a legal/philosophical category, not a biological one. Comparative constitutional law scholars note that multiple democracies have adopted the functional_capacity_reading and have not experienced the collapse of legal order — suggesting the founding problem can be solved in multiple ways.
narrative_ontology:disappearance_verdict(legal_personhood_boundary__developmental_potentiality_reading, world_rearranges).
narrative_ontology:founding_problem_status(legal_personhood_boundary__developmental_potentiality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legal_personhood_boundary__developmental_potentiality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
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


/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    potentiality_vs_capacity_metaphysics,
    'Does ''inherent potentiality to develop into a rights-bearer'' constitute a metaphysically coherent criterion for personhood, or does it project personhood onto a biological state that cannot yet exercise personhood''s defining properties (self-awareness, agency, rationality)?',
    'Philosophical analysis of competing metaphysical frameworks (actualism vs. potentialism); examination of whether potentiality alone grounds rights in other legal domains (e.g., does a person with permanently incapacitating illness retain personhood on potentiality grounds if capacity is permanently unreachable?); cross-cultural analysis of which cultures assign personhood to biological potentiality vs. demonstrable properties.',
    'If potentiality alone is not a coherent criterion, the constraint rests on a metaphysical choice rather than a settled fact, which elevates the status of sibling readings from ''alternative positions'' to ''equally defensible framings.'' This would reframe the personhood question as fundamentally contestable rather than resolved by the developmental potentiality reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(potentiality_vs_capacity_metaphysics, conceptual, 'Whether potentiality is a coherent metaphysical basis for personhood attribution.').

omega_variable(
    fetal_autonomy_vs_bodily_integrity_conflict,
    'When fetal rights claims (right to continued existence) directly conflict with pregnant persons'' bodily autonomy and self-determination, which right takes precedence, and on what grounds?',
    'Comparative constitutional analysis across jurisdictions (how different rights frameworks resolve the conflict); philosophical analysis of bodily autonomy as a foundational right; empirical evidence on forced pregnancy outcomes vs. voluntary continuation (health, psychological, social); testimony from pregnant persons about their experience of the constraint.',
    'If bodily autonomy is privileged as foundational (as many non-enforcing jurisdictions hold), fetal rights claims would need to be subordinated or require consent, which would shift the constraint from tangled_rope toward a different classification where pregnant person autonomy is primary. If fetal existence is absolutely prioritized, the constraint''s extraction character is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fetal_autonomy_vs_bodily_integrity_conflict, preference, 'The irresolvable collision between fetal rights and pregnant persons'' bodily autonomy.').

omega_variable(
    identity_lock_mechanism_in_pregnancy,
    'Is the identity-locking of pregnant persons (cannot refuse pregnancy without jurisdictional exit) a structural feature of the biological constraint (pregnancy is identity-constitutive for persons capable of pregnancy) or a constructed feature of the legal constraint (the state creates exit barriers through criminalization and border control)?',
    'Post-exit psychological and identity trajectory analysis (do persons who exit jurisdictions to obtain abortion report identity reconstruction or persistence?); interview data from pregnant persons about whether identity-fusion is experienced as biological or imposed; comparative analysis of jurisdictions with low vs. high enforcement (do low-enforcement jurisdictions see different identity-locking patterns?).',
    'If identity-locking is constructed rather than biological, it is a mechanism by which the constraint amplifies extraction (suppression is not merely external but internalized through identity fusion). If identity-locking is biological, it explains the constraint''s power but does not reduce the extraction it imposes. Either way, the identity-locked exit classification stands, but the suppression mechanism changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_in_pregnancy, empirical, 'Whether identity-locking in pregnancy is a biological or constructed feature.').

omega_variable(
    fetal_vs_potential_person_distinction,
    'Is the constraint grounded in the claim that a fetus IS a person (ontological claim), or that a fetus WILL BECOME a person and therefore deserves rights anticipatorily (potentiality claim)? These are structurally different claims.',
    'Textual analysis of how the constraint is articulated in law, advocacy, and institutional practice (is the language ''a fetus is a person'' or ''a fetus has the potential to become a person''?); examination of how the reading handles cases where the fetus will not survive to birth or will not develop capacities associated with personhood (anencephaly, etc.); comparison with other legal uses of potentiality (do property law, contract law, etc. attribute current personhood to entities with potentiality?).',
    'If the constraint is grounded in an ontological claim (a fetus IS a person from conception), it makes a strong metaphysical claim that invites empirical challenge. If it is grounded in potentiality (a fetus WILL BECOME a person and deserves anticipatory rights), it is philosophically more defensible but also more clearly distinguishes itself from sibling readings. Clarity on this distinction would affect how the constraint is classified and how sibling readings are positioned.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fetal_vs_potential_person_distinction, conceptual, 'Whether the constraint grounds personhood in ontological fact or potentiality-based entitlement.').

omega_variable(
    reading_relation_foreclosure_vs_coexistence,
    'Does the developmental potentiality reading FORECLOSE the functional_capacity_reading (the two cannot coexist in any single framework), or do they COEXIST as competing positions held by different parties?',
    'Framework analysis: can a single legal system hold that personhood begins at conception (potentiality) AND that personhood requires cognitive capacity? If cognitive capacity is required, does a fetus lack personhood (functional_capacity_reading wins)? If potentiality suffices, does cognitive capacity become irrelevant (developmental_potentiality_reading wins)? Or do both readings deploy different criteria for different purposes (birth personhood and post-birth rights, for instance)?',
    'If foreclosure is real (they cannot coexist), the readings are in genuine logical conflict and one must be abandoned if both are held. If coexistence is the truth (they are held simultaneously in different policy domains or by different parties), the conflict is political rather than logical, and both readings remain live. This affects how the sibling relationship should be classified in cs_structure.reading_relations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_relation_foreclosure_vs_coexistence, conceptual, 'The logical structure of the relationship between potentiality-based and capacity-based personhood readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__developmental_potentiality_reading, 1973, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lega_tr_t1973, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 1973, 0.08).
narrative_ontology:measurement(lega_tr_t1992, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 1992, 0.11).
narrative_ontology:measurement(lega_tr_t2008, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 2008, 0.15).
narrative_ontology:measurement(lega_tr_t2016, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 2016, 0.18).
narrative_ontology:measurement(lega_tr_t2022, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 2022, 0.21).
narrative_ontology:measurement(lega_tr_t2024, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(lega_be_t1973, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 1973, 0.35).
narrative_ontology:measurement(lega_be_t1992, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 1992, 0.52).
narrative_ontology:measurement(lega_be_t2008, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 2008, 0.68).
narrative_ontology:measurement(lega_be_t2016, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 2016, 0.72).
narrative_ontology:measurement(lega_be_t2022, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 2022, 0.76).
narrative_ontology:measurement(lega_be_t2024, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(lega_su_t1973, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 1973, 0.42).
narrative_ontology:measurement(lega_su_t1992, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 1992, 0.58).
narrative_ontology:measurement(lega_su_t2008, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 2008, 0.68).
narrative_ontology:measurement(lega_su_t2016, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 2016, 0.74).
narrative_ontology:measurement(lega_su_t2022, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 2022, 0.79).
narrative_ontology:measurement(lega_su_t2024, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 2024, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_personhood_boundary__developmental_potentiality_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legal_personhood_boundary__developmental_potentiality_reading, 0.12).
narrative_ontology:affects_constraint(legal_personhood_boundary__developmental_potentiality_reading, legal_personhood_boundary__functional_capacity_reading).
narrative_ontology:affects_constraint(legal_personhood_boundary__developmental_potentiality_reading, legal_personhood_boundary__restrictive_anthropocentric_reading).
narrative_ontology:affects_constraint(legal_personhood_boundary__developmental_potentiality_reading, reproductive_autonomy_constraint).
narrative_ontology:affects_constraint(legal_personhood_boundary__developmental_potentiality_reading, fetal_rights_establishment).

% DUAL FORMULATION NOTE:
% The legal personhood boundary is a contested kernel instantiated by three structurally distinct constraint stories: the developmental_potentiality_reading (this file), the functional_capacity_reading, and the restrictive_anthropocentric_reading. Each reading has a different ε (extractiveness relative to different beneficiary/victim structures), different stakeholder configurations, and different classifications. The readings coexist and compete in the same jurisdictions; they are not alternative observables of one constraint but three distinct constraints arising from the same contested kernel. All three must be linked via network.affects_constraints to establish the constraint family. The developmental_potentiality_reading shows rising extractiveness (0.35→0.78) as enforcement machinery strengthens post-Dobbs; the functional_capacity_reading would show different metrics reflecting its different beneficiary structure (persons with cognitive disabilities potentially become beneficiaries rather than targets); the restrictive_anthropocentric_reading would show yet another configuration. None of these differences reflects measurement ambiguity — they reflect genuine structural divergence in what each reading claims and whom it benefits.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legal_personhood_boundary__developmental_potentiality_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
