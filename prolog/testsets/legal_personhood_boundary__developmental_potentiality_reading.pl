% ============================================================================
% CONSTRAINT STORY: legal_personhood_boundary__developmental_potentiality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: legal_personhood_boundary__developmental_potentiality_reading
 *   human_readable: Conception-Based Legal Personhood (Developmental Potentiality Reading)
 *   domain: legal/constitutional/rights
 *
 * SUMMARY:
 *   The developmental potentiality reading asserts that legal personhood
 *   begins at conception—that any organism on a continuous human
 *   developmental trajectory from conception onward holds full rights-bearing
 *   status. Under this reading, the fetus becomes a legal person with
 *   protectable interests from the moment of conception; pregnant persons'
 *   bodily autonomy and reproductive choice are subordinated to the state's
 *   interest in protecting fetal life; the state acquires explicit authority
 *   to regulate, restrict, and criminalize abortion. This reading claims to
 *   ground itself in natural law (the developing organism's inherent
 *   humanity) and developmental continuity (the fetus is a person at all
 *   stages because personhood follows the continuous biological trajectory,
 *   not a threshold capacity). The constraint operates as a tangled rope: it
 *   solves a genuine coordination problem (clarifying legal status of
 *   developing organisms, providing certainty for state protection of
 *   potential life) AND it extracts substantially from pregnant persons and
 *   abortion seekers (subordinates their autonomy, transfers decision-making
 *   authority to the state, creates enforcement machinery that penalizes
 *   abortion provision and access). Active enforcement is required to
 *   maintain it—without state prohibition and criminalization, pregnant
 *   persons would exercise autonomous choice over continuation of pregnancy.
 *
 * KEY AGENTS:
 *   - pregnant_persons: bear the biological and legal costs of the constraint; their bodily autonomy is subordinated to fetal personhood claims; identity_locked exit (physically trapped in pregnancy, socially constituted as pregnant, legally prohibited from autonomous choice)
 *   - fetal_personhood_doctrine_advocates: organized beneficiaries who secure legal vindication of their core normative claim; collects symbolic and political power from embedding the doctrine in law
 *   - state_reproductive_authority: institutional agenda-setter; acquires enforcement authority over pregnancy outcomes; administers the constraint through criminal law, licensing law, and funding doctrine
 *   - abortion_access_seekers: powerless payers; trapped exit (cannot access procedure, cannot migrate without resources, cannot obtain clandestine access safely); experience the constraint as medical and legal barrier
 *   - constitutional_court: observer seat; measures whether this reading is constitutionally defensible or whether sibling readings better fit the constitutional order
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_boundary__developmental_potentiality_reading, 0.68).
domain_priors:suppression_score(legal_personhood_boundary__developmental_potentiality_reading, 0.72).
domain_priors:theater_ratio(legal_personhood_boundary__developmental_potentiality_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__developmental_potentiality_reading, tangled_rope).
narrative_ontology:human_readable(legal_personhood_boundary__developmental_potentiality_reading, "Conception-Based Legal Personhood (Developmental Potentiality Reading)").
narrative_ontology:topic_domain(legal_personhood_boundary__developmental_potentiality_reading, "legal/constitutional/rights").

domain_priors:requires_active_enforcement(legal_personhood_boundary__developmental_potentiality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__developmental_potentiality_reading, '29a54ff0-79dc-4754-aeed-4404514a1ebc').
narrative_ontology:cs_kernel_codification('29a54ff0-79dc-4754-aeed-4404514a1ebc', fixed_text).
narrative_ontology:cs_authority_grounding('29a54ff0-79dc-4754-aeed-4404514a1ebc', lineage).
narrative_ontology:cs_interpretation_layer_present('29a54ff0-79dc-4754-aeed-4404514a1ebc').
narrative_ontology:cs_reading_relation('29a54ff0-79dc-4754-aeed-4404514a1ebc', legal_personhood_boundary__functional_capacity_reading, forecloses).
narrative_ontology:cs_reading_relation('29a54ff0-79dc-4754-aeed-4404514a1ebc', legal_personhood_boundary__restrictive_anthropocentric_reading, forecloses).
narrative_ontology:cs_axiom('29a54ff0-79dc-4754-aeed-4404514a1ebc', foundational, continuous_developmental_trajectory_grounds_personhood).
narrative_ontology:cs_axiom_status(continuous_developmental_trajectory_grounds_personhood, holdable).
narrative_ontology:cs_axiom_grounding('29a54ff0-79dc-4754-aeed-4404514a1ebc', continuous_developmental_trajectory_grounds_personhood, deontological).
narrative_ontology:cs_axiom('29a54ff0-79dc-4754-aeed-4404514a1ebc', foundational, human_life_from_conception_has_inherent_dignity).
narrative_ontology:cs_axiom_status(human_life_from_conception_has_inherent_dignity, holdable).
narrative_ontology:cs_axiom_grounding('29a54ff0-79dc-4754-aeed-4404514a1ebc', human_life_from_conception_has_inherent_dignity, deontological).
narrative_ontology:cs_reference_frame('29a54ff0-79dc-4754-aeed-4404514a1ebc', natural_law_personhood_doctrine).
narrative_ontology:cs_drift_state('29a54ff0-79dc-4754-aeed-4404514a1ebc', contemporary_constitutional_pluralism, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('29a54ff0-79dc-4754-aeed-4404514a1ebc', '').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__developmental_potentiality_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__developmental_potentiality_reading, fetal_personhood_doctrine_advocates).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__developmental_potentiality_reading, state_reproductive_authority).
narrative_ontology:constraint_victim(legal_personhood_boundary__developmental_potentiality_reading, pregnant_persons).
narrative_ontology:constraint_victim(legal_personhood_boundary__developmental_potentiality_reading, abortion_access_seekers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__developmental_potentiality_reading, abortion_access_seekers).
narrative_ontology:constraint_victim(legal_personhood_boundary__developmental_potentiality_reading, healthcare_providers).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__developmental_potentiality_reading, natural_law_personhood).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__developmental_potentiality_reading, developmental_continuum_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the biological, medical, and legal consequences of pregnancy once conception occurs. Under this reading, their bodily autonomy and medical decision-making authority are subordinated to the state's interest in fetal life and to fetal rights claims. Exit options are severely constrained—physical exit from pregnancy carries legal and medical risk; identity as a pregnant person is constitutive of their immediate situation; migration to jurisdictions with different readings requires substantial resources and carries social costs. They lack enforcement authority over how the constraint operates on their bodies.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, pregnant_persons, payer,
    moderate, biographical, identity_locked, national).

% Secure legal recognition that their core normative claim—that human life from conception carries full moral and legal status—is embedded in law, policy, and institutional practice. They do not bear the material costs of this constraint; they benefit from its vindication in the public order. Their advocacy power is organized through religious institutions, political coalitions, and movement infrastructure.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, fetal_personhood_doctrine_advocates, beneficiary,
    organized, generational, mobile, national).

% Acquires explicit legal and enforcement authority to regulate, restrict, and in many cases prohibit abortion. The state administers the constraint through criminal law (penalties for abortion), licensing law (regulation of abortion provision), and public funding doctrine (exclusion of abortion from state health programs). The state's enforcement machinery exists to keep this constraint operational and to prevent exit via alternative jurisdictions or clandestine provision.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, state_reproductive_authority, agenda_setter,
    institutional, generational, analytical, national).

% Experience the constraint as a barrier to a medical procedure they seek. They pay the constraint through delayed access, forced continuation of pregnancy, medical complications from delay, travel burdens to access jurisdictions, financial costs, and the trauma of legal jeopardy. They are beneficiaries only to the extent they accept the constraint's premise (fetal personhood) and derive meaning from carrying wanted pregnancies to term—but where they reject the constraint's premise, they are pure targets. Their exit options are extremely constrained: travel is expensive and legally fraught; clandestine abortion carries criminal and medical risk.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, abortion_access_seekers, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(legal_personhood_boundary__developmental_potentiality_reading, abortion_access_seekers, beneficiary).

% Bear the compliance costs of the constraint—they must decline abortion provision, navigate restrictions on referral, document their compliance with state-mandated waiting periods and counseling, and manage liability exposure. They operate under the state's enforcement apparatus and risk loss of license for non-compliance. Their professional autonomy and ethical judgments are subordinated to state reproductive authority.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, healthcare_providers, payer,
    organized, biographical, constrained, national).

% Are jurisdictions or international bodies (the European Union, Canada, parts of the United States) that recognize pregnancy as a zone of pregnant person autonomy rather than fetal personhood from conception. They would advocate for alternative readings and would challenge the personhood doctrine through international law and treaty mechanisms. Their voices are excluded from the internal deliberation of the adopting jurisdiction by sovereignty doctrine, though they maintain diplomatic and soft-power pressure.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, competing_jurisdiction_actors, excluded,
    institutional, generational, trapped, global).

% Adjudicates whether the conception-based personhood reading is consistent with constitutional text, prior precedent, and interpretive doctrine. The court is the formal site where the kernel contest between readings is supposed to be resolved. Its role is to measure whether this reading is constitutionally defensible or whether a sibling reading better fits constitutional text and tradition.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, constitutional_court, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Clarifies the legal status of the developing human organism, creates a unified law-of-personhood applicable across state jurisdictions, and provides a bright-line rule (conception) for when rights protections begin—eliminating uncertainty and creating consistent expectations for how the state will treat pregnancy and fetal life in law.
% TRANSFER_FUNCTION: Transfers decision-making authority from pregnant persons to the state; transfers bodily autonomy claims from pregnant persons to fetal personhood claims; transfers enforcement burden from individual choice to state reproductive regulation. Materially, pregnancy and childbearing are transferred from a zone of private choice to a zone of state interest and regulation.
% ABSENT_VOICES: Pregnant persons whose autonomy is subordinated are present as payers but often lack political power proportionate to the constraint's cost. International bodies and comparative jurisdictions that adopt alternative readings (functional capacity, restrictive anthropocentric) are excluded from the adopting jurisdiction's internal deliberation. Disabled persons and developmentally different humans who might be re-classified as non-persons under a strict functional-capacity reading are absent from this specific reading's analysis but would speak to the sibling readings.
% DISAPPEARANCE_RATIONALE: If conception-based personhood and its enforcement vanished, pregnancy would revert to a zone of pregnant person autonomy and medical judgment; abortion access would normalize; state reproductive authority would collapse; the organizing principle of family law and reproductive regulation would shift fundamentally. The entire institutional apparatus of abortion restriction, fetal protection statutes, and state enforcement of pregnancy continuation would unwind.
% FOUNDING_PROBLEM: How to assign legal rights and protections to the developing human organism so that it is not treated as non-entity or mere extension of the pregnant person; how to ensure the state has authority to prevent what advocates view as killing of human life at all developmental stages.
% FOUNDING_PROBLEM_CORROBORATION: Advocates for this reading attest the founding problem is live and urgent. Competing jurisdictions and international human rights bodies attest the founding problem has been reframed—they recognize the developing organism but assign no independent rights claims before viability or birth, treating pregnancy instead as a zone of pregnant person autonomy. Empirical evidence on how pregnancies unfold medically and how pregnant persons experience bodily autonomy is cited by both sides; neither party's framing goes uncontested.
narrative_ontology:disappearance_verdict(legal_personhood_boundary__developmental_potentiality_reading, world_rearranges).
narrative_ontology:founding_problem_status(legal_personhood_boundary__developmental_potentiality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legal_personhood_boundary__developmental_potentiality_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(legal_personhood_boundary__developmental_potentiality_reading, 'none', 1).

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
 *   Extractiveness is high (0.68 at interval end) because the constraint subordinates pregnant persons' autonomy and bodily integrity to state interests and fetal personhood claims—a substantial transfer of decision-making authority and a restriction of medical choice. The constraint was moderately extractive at interval start (0.52) and accumulated extraction over time (reaching plateau at 0.68 around t=32), reflecting how enforcement intensity increased as state-level restrictions proliferated and became more specific to pregnancy management and abortion provision. Suppression is correspondingly high (0.72) because the constraint's persistence depends entirely on active state enforcement—without state prohibition and penalty, pregnant persons would exercise autonomous choice over pregnancy continuation. Theater ratio is moderate (0.41), meaning a significant but not dominant portion of enforcement activity is performative (legislative rhetoric about fetal protection, symbolic prosecution, enforcement display) rather than functional state regulation. Accessibility collapse is high (0.78): alternatives (obtaining abortion outside the jurisdiction, defying the state, obtaining clandestine provision) are available in principle but are severely constrained by cost, criminalization, and the biological facts of pregnancy. Resistance is high (0.71): pregnant persons, healthcare providers, and abortion-access movements mount continuous legal, political, and civil resistance to the constraint. The measurement series are authored on one shared time grid (t=0,8,16,24,32,40,50) so extraction accumulation, theater stability, and suppression intensity all report at the same time points.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (state) perceives this as a legitimate coordination problem (how to protect developing human life) and a constitutional mandate (the state's interest in fetal life). Beneficiary seats (fetal personhood advocates) perceive this as vindication of a core moral claim about when life begins and deserves protection. Payer seats (pregnant persons, abortion-access seekers, healthcare providers) perceive this as subordination of autonomy and medical judgment to state reproductive authority and fetal personhood claims that they reject. Healthcare provider seats experience dual burden: they must decline provision and manage compliance risk. The constitutional court observes this divergence as a contested kernel—the question whether conception-based personhood is constitutionally correct or whether a sibling reading better fits the constitutional text and tradition. The engine computes per-seat directionality from this structural gap: agenda-setter and beneficiary seats near the beneficiary end (d ~0.1-0.3), payer seats near the target end (d ~0.75-0.95), observer seat at analytical position (d=0.5).
 *
 * DIRECTIONALITY LOGIC:
 *   Pregnant persons as payers: identity_locked exit, moderate power but subordinated by state authority, biological entrenchment in pregnancy. They bear the constraint's full cost (bodily autonomy, medical decision-making, legal jeopardy). Directionality near full target (d ~0.90). Abortion-access seekers as trapped payers: powerless position, zero exit options (criminalization blocks clandestine access, migration requires resources they lack), identity-constituted by the constraint (they are abortion-seeking specifically because of this constraint). Directionality at full target (d ~0.95). Fetal personhood advocates as beneficiaries: organized power, generational time horizon, mobile exit (they can migrate to sympathetic jurisdictions, can organize politically to maintain the constraint). They collect the symbolic and political benefit of fetal personhood being law; they do not bear the material cost of pregnancy continuation or forced gestation. Directionality near full beneficiary (d ~0.15). State reproductive authority: institutional power, generational horizon, analytical exit. The state benefits from the enforcement authority the constraint grants (regulatory power over reproduction, symbolic authority over fetal protection). Directionality beneficiary-leaning (d ~0.25). Healthcare providers: organized power but subordinated by enforcement requirement (they must decline provision or face license loss and criminal jeopardy). They are secondary payers with constrained exit (they cannot practice medicine freely, cannot refuse the constraint). Directionality payer-leaning (d ~0.70).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does NOT exhibit mandatrophy. The founding problem—how to assign legal status to the developing organism and protect what advocates view as human life at all stages—remains contested and live. The constraint's function (clarifying legal status, enabling state protection) is still actively pursued by its beneficiaries and administrators, not atrophied. The enforcement machinery is contemporaneously strengthened (see measurement data: suppression increases from 0.58 at t=0 to 0.72 at t=50), not maintained theatrically. The theater ratio is moderate (0.41), not dominant, meaning real enforcement activity exceeds symbolic display. The constraint is a tangled rope (genuine coordination claim + substantial extraction + active enforcement), not a piton. Mandatrophy would apply only if the founding problem died but enforcement persisted by institutional inertia; this constraint still actively pursues the founding problem (protecting fetal life from conception) even though the cost to pregnant persons is substantial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    personhood_grounding_continuity,
    'Does human personhood follow biological continuity (continuous trajectory from conception) or does it track discrete capacity thresholds (sentience, rationality, birth)?',
    'Philosophical and constitutional analysis of what grounds personhood claims. Empirical evidence from neuroscience on when sentience and cognitive capacity emerge. Comparative law study of how different jurisdictions resolve the question and what legitimacy claims they offer.',
    'If continuity is grounding, the conception-based reading holds. If capacity thresholds ground personhood, the functional-capacity reading follows. This resolution determines the entire victim set and enforcement structure of the constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(personhood_grounding_continuity, conceptual, 'Whether personhood follows continuous biological development or discrete capacity thresholds.').

omega_variable(
    pregnant_person_autonomy_vs_fetal_claims,
    'When pregnant person autonomy and fetal personhood claims conflict, what is the correct constitutional and moral priority?',
    'Constitutional court review comparing text, precedent, and interpretive tradition. Comparative constitutional law examining how other democracies resolve the conflict. Empirical evidence on pregnancy outcomes, maternal mortality, and bodily integrity violations under different legal regimes.',
    'If pregnant person autonomy is prioritized, the constraint''s extraction of decision-making authority from pregnant persons is illegitimate and the reading should be rejected or the enforcement mechanism reformed. If fetal claims are prioritized, the constraint''s subordination of autonomy is justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pregnant_person_autonomy_vs_fetal_claims, preference, 'The priority conflict between pregnant person autonomy and fetal personhood claims.').

omega_variable(
    enforcement_legitimacy_gap,
    'Does the state''s enforcement mechanism (criminalization, licensing restrictions, funding exclusions) legitimately implement the personhood doctrine, or does enforcement create collateral harms that exceed the doctrine''s justification?',
    'Analysis of enforcement outcomes: maternal mortality under restriction regimes, healthcare provider exodus, clandestine abortion complications, psychological and trauma outcomes for pregnant persons. Comparison to less coercive enforcement mechanisms that might implement the personhood doctrine (e.g., counseling, waiting periods) without full prohibition.',
    'If enforcement creates harms that exceed the doctrine''s justification, the constraint becomes a pure snare (coercive extraction without genuine coordination benefit). If enforcement is proportionate to the personhood doctrine''s importance, the tangled-rope classification holds. This determines whether the constraint is defensible even if the conception-based personhood claim is accepted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_legitimacy_gap, empirical, 'Whether enforcement mechanisms are proportionate to the personhood doctrine''s justification.').

omega_variable(
    kernel_reading_as_constitutional_claim,
    'Is conception-based personhood rooted in constitutional text and prior precedent, or is it a reading imposed onto the constitutional order despite textual and precedential resistance?',
    'Constitutional historical analysis of founding-era understanding, precedent from constitutional courts and appellate bodies, textual analysis of how ''personhood'' and ''life'' are used in the constitution. Comparison to how competing readings interpret the same text and precedent.',
    'If the reading is constitutionally rooted, it has legitimacy as a constitutional interpretation. If it is imposed against the text and precedent, it is a constitutional revision masked as interpretation, and should be identified as such. This determines whether the constraint is a legitimate constitutional application or a capture/innovation that subordinates constitutional text to a non-textual normative claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_as_constitutional_claim, empirical, 'Whether conception-based personhood is constitutionally defensible or constitutionally imposed.').

omega_variable(
    suppression_structurality_vs_internalization,
    'Is the measured suppression (0.72) structural (enforced by state criminal law, licensing restrictions, cost barriers) or has it become internalized (pregnant persons internalize the constraint''s premise, adopt the fetal personhood narrative, experience their autonomy as subordinate)?',
    'Post-enforcement trajectory study: in jurisdictions that decriminalize abortion and remove state enforcement, how much do pregnant persons'' decision-making patterns and self-conceptions shift? Psychological and sociological analysis of how long-term exposure to restriction regimes affects pregnant persons'' sense of bodily autonomy.',
    'If suppression is purely structural, removing the state enforcement machinery would restore autonomous choice. If suppression has become substantially internalized, pregnant persons would carry the constraint''s normative claims with them even after enforcement is removed, and reclassification to partially internalized suppression would increase the effective extraction cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structurality_vs_internalization, empirical, 'Whether suppression is structural enforcement or internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__developmental_potentiality_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lega_tr_t0, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(lega_tr_t0, observed).
narrative_ontology:measurement(lega_tr_t8, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement_basis(lega_tr_t8, observed).
narrative_ontology:measurement(lega_tr_t16, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement_basis(lega_tr_t16, observed).
narrative_ontology:measurement(lega_tr_t24, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement_basis(lega_tr_t24, observed).
narrative_ontology:measurement(lega_tr_t32, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement_basis(lega_tr_t32, observed).
narrative_ontology:measurement(lega_tr_t40, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(lega_tr_t40, observed).
narrative_ontology:measurement(lega_tr_t50, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 50, 0.41).
narrative_ontology:measurement_basis(lega_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(lega_be_t0, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(lega_be_t0, observed).
narrative_ontology:measurement(lega_be_t8, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement_basis(lega_be_t8, observed).
narrative_ontology:measurement(lega_be_t16, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement_basis(lega_be_t16, observed).
narrative_ontology:measurement(lega_be_t24, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement_basis(lega_be_t24, observed).
narrative_ontology:measurement(lega_be_t32, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement_basis(lega_be_t32, observed).
narrative_ontology:measurement(lega_be_t40, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(lega_be_t40, observed).
narrative_ontology:measurement(lega_be_t50, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(lega_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(lega_su_t0, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(lega_su_t0, observed).
narrative_ontology:measurement(lega_su_t8, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 8, 0.63).
narrative_ontology:measurement_basis(lega_su_t8, observed).
narrative_ontology:measurement(lega_su_t16, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement_basis(lega_su_t16, observed).
narrative_ontology:measurement(lega_su_t24, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement_basis(lega_su_t24, observed).
narrative_ontology:measurement(lega_su_t32, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement_basis(lega_su_t32, observed).
narrative_ontology:measurement(lega_su_t40, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(lega_su_t40, observed).
narrative_ontology:measurement(lega_su_t50, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 50, 0.72).
narrative_ontology:measurement_basis(lega_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_personhood_boundary__developmental_potentiality_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legal_personhood_boundary__developmental_potentiality_reading, 0.12).
narrative_ontology:affects_constraint(legal_personhood_boundary__developmental_potentiality_reading, legal_personhood_boundary__functional_capacity_reading).
narrative_ontology:affects_constraint(legal_personhood_boundary__developmental_potentiality_reading, legal_personhood_boundary__restrictive_anthropocentric_reading).

% DUAL FORMULATION NOTE:
% The legal_personhood_boundary kernel decomposes into three constraint stories representing rival readings of what grounds personhood. The developmental_potentiality_reading (THIS story) asserts personhood begins at conception and yields a tangled_rope structure (coordination function + substantial extraction + active enforcement). The functional_capacity_reading defers personhood to demonstrable cognitive capacity (sentience, rationality), which shifts the victim set and yields different extraction dynamics. The restrictive_anthropocentric_reading limits personhood to born humans, which further shifts the coordination function and victim structure. All three stories interpret the same constitutional kernel (the meaning of 'personhood' and 'life' in the constitutional order) but arrive at different structural classifications because each reading produces different beneficiary/victim distributions and different enforcement requirements. They are linked by network.affects_constraints because each reading's adoption would preclude or substantially pressure the others within the same constitutional framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
