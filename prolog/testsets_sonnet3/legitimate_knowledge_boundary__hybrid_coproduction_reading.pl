% ============================================================================
% CONSTRAINT STORY: legitimate_knowledge_boundary__hybrid_coproduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_knowledge_boundary__hybrid_coproduction_reading, []).

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
 *   constraint_id: legitimate_knowledge_boundary__hybrid_coproduction_reading
 *   human_readable: Co-Production Knowledge Legitimacy Standard (Hybrid Reading)
 *   domain: epistemology/science_and_technology_studies/political_theory
 *
 * SUMMARY:
 *   This story instantiates one reading of the legitimate_knowledge_boundary
 *   kernel: the hybrid co-production reading, which holds that neither
 *   methodological rigor alone (credentialed_expertise_reading) nor
 *   experiential/community validation alone (experiential_pluralism_reading)
 *   suffices — legitimate knowledge requires both, integrated through a
 *   formal co-production process. As institutionalized over roughly two
 *   decades, this reading has produced a real coordination gain (catching
 *   both methodological blind spots and community-relevance failures that
 *   either standard alone missed) but has also generated a new
 *   toll-collecting layer: institutes, boundary organizations, and
 *   translational brokers who certify that co-production has 'properly'
 *   occurred, and who capture funding and prestige for administering that
 *   certification. Unaffiliated community members and under-resourced
 *   grassroots groups — the very parties the standard claims to center —
 *   often cannot afford entry into the formal partnership structures the
 *   standard requires, and find their own direct testimony devalued until it
 *   passes through a broker.
 *
 * KEY AGENTS:
 *   - coproduction_institutes: agenda-setting certifiers of the dual-validation standard
 *   - boundary_organizations and translational_research_brokers: intermediary beneficiaries whose function exists because the hybrid bar is mandatory
 *   - credential_only_researchers_excluded_from_partnership_funding and grassroots_groups_lacking_partnership_capacity: structurally disadvantaged payers on opposite sides of the methodology/experience divide
 *   - unaffiliated_community_knowledge_holders: the most powerless payer, whose own lived experience is devalued absent brokered certification
 *   - epistemology_scholars: analytical observers tracing the boundary-work dynamics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.48).
domain_priors:suppression_score(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.42).
domain_priors:theater_ratio(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__hybrid_coproduction_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__hybrid_coproduction_reading, "Co-Production Knowledge Legitimacy Standard (Hybrid Reading)").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__hybrid_coproduction_reading, "epistemology/science_and_technology_studies/political_theory").

domain_priors:requires_active_enforcement(legitimate_knowledge_boundary__hybrid_coproduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__hybrid_coproduction_reading, 'a6c0a487-e49b-4138-bff4-b24845ca4c96').
narrative_ontology:cs_kernel_codification('a6c0a487-e49b-4138-bff4-b24845ca4c96', distributed).
narrative_ontology:cs_authority_grounding('a6c0a487-e49b-4138-bff4-b24845ca4c96', practice).
narrative_ontology:cs_interpretation_layer_present('a6c0a487-e49b-4138-bff4-b24845ca4c96').
narrative_ontology:cs_reading_relation('a6c0a487-e49b-4138-bff4-b24845ca4c96', legitimate_knowledge_boundary__credentialed_expertise_reading, influences).
narrative_ontology:cs_reading_relation('a6c0a487-e49b-4138-bff4-b24845ca4c96', legitimate_knowledge_boundary__experiential_pluralism_reading, influences).
narrative_ontology:cs_axiom('a6c0a487-e49b-4138-bff4-b24845ca4c96', foundational, neither_rigor_nor_experience_alone_suffices).
narrative_ontology:cs_axiom_status(neither_rigor_nor_experience_alone_suffices, holdable).
narrative_ontology:cs_axiom_grounding('a6c0a487-e49b-4138-bff4-b24845ca4c96', neither_rigor_nor_experience_alone_suffices, instrumental).
narrative_ontology:cs_axiom('a6c0a487-e49b-4138-bff4-b24845ca4c96', foundational, coproduction_process_is_the_locus_of_legitimacy).
narrative_ontology:cs_axiom_status(coproduction_process_is_the_locus_of_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('a6c0a487-e49b-4138-bff4-b24845ca4c96', coproduction_process_is_the_locus_of_legitimacy, conventional).
narrative_ontology:cs_reference_frame('a6c0a487-e49b-4138-bff4-b24845ca4c96', post_parachute_research_reform_consensus).
narrative_ontology:cs_drift_state('a6c0a487-e49b-4138-bff4-b24845ca4c96', contemporary_grant_compliance_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a6c0a487-e49b-4138-bff4-b24845ca4c96', '').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__hybrid_coproduction_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, coproduction_institutes).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, boundary_organizations).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, translational_research_brokers).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, unaffiliated_community_knowledge_holders).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, credential_only_researchers_excluded_from_partnership_funding).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, grassroots_groups_lacking_partnership_capacity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, credentialed_researchers_with_partnership_capacity).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, funding_agencies).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, credentialed_researchers_with_partnership_capacity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and administer the dual-validation protocols that certify knowledge as legitimate only when it demonstrates both methodological rigor and documented community/experiential input. They control funding calls, review criteria, and the accreditation of 'co-production' as a method, and they capture prestige and grant revenue from running that certification apparatus.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, coproduction_institutes, agenda_setter,
    institutional, generational, arbitrage, national).

% Sit between researchers and affected communities, translating and packaging both forms of validity into fundable projects. Their livelihood depends on the hybrid standard remaining mandatory; if either pure methodological rigor or pure experiential validation were sufficient on their own, the translation role they sell would not be needed.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, boundary_organizations, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__hybrid_coproduction_reading, boundary_organizations, agenda_setter).

% Individual consultants and firms who specialize in helping academic teams meet the community-validation half of the requirement (facilitating workshops, writing partnership MOUs, running participatory sessions). They earn fees precisely because the dual-validation bar exists.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, translational_research_brokers, beneficiary,
    moderate, biographical, mobile, national).

% Well-resourced academics who can afford the added time, staff, and travel that co-production requires. They gain legitimacy premiums and funding priority for satisfying both standards but also carry real added cost and delay compared to methodology-only review.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, credentialed_researchers_with_partnership_capacity, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__hybrid_coproduction_reading, credentialed_researchers_with_partnership_capacity, payer).

% Rigorous methodologists without existing community relationships or institutional support to build them. They are shut out of funding streams and journals that now gate on co-production evidence, regardless of the quality of their methods, and lack the resources boundary organizations charge for bridging the gap.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, credential_only_researchers_excluded_from_partnership_funding, payer,
    moderate, biographical, constrained, national).

% Community groups without staff time, grant-writing capacity, or existing academic contacts to enter a formal co-production partnership. Their experiential knowledge is disqualified as 'unvalidated' unless it passes through an institutionally sanctioned co-production process they cannot afford to enter.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, grassroots_groups_lacking_partnership_capacity, payer,
    powerless, biographical, trapped, local).

% Individuals holding direct lived experience of the phenomenon under study (illness, environmental harm, poverty) who are not embedded in any recognized partnership structure. Their testimony is treated as anecdote rather than knowledge until a broker or institute formally 'co-produces' it, effectively taxing their own experience with a bureaucratic toll before it counts.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, unaffiliated_community_knowledge_holders, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__hybrid_coproduction_reading, unaffiliated_community_knowledge_holders, excluded).

% Write co-production requirements into grant solicitations, partly in response to genuine past failures of purely top-down research, and partly because it lets them claim procedural legitimacy and community buy-in for controversial funding decisions.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, funding_agencies, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__hybrid_coproduction_reading, funding_agencies, beneficiary).

% The broader populations co-production is meant to represent, most of whom never participate in any formal partnership process at all. The standard is enforced in their name but they have no direct voice in whether the dual-validation bar itself is fair, only in whether specific brokered projects meet it.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, affected_communities_as_collective, excluded,
    powerless, generational, trapped, local).

% Study the co-production standard as a case of boundary-work in science studies, tracing how it emerged, who administers it, and whether it genuinely improves knowledge quality or mainly redistributes gatekeeping authority to a new class of intermediaries.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, epistemology_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Genuinely solves a real problem: purely methodological review had historically produced findings that were technically rigorous but practically useless or actively harmful when applied to communities whose lived context the researchers did not understand, while purely experiential claims lacked mechanisms to adjudicate between competing testimonies or detect confounds. Requiring both forms of validation, brokered through co-production, catches errors either standard alone would miss.
% TRANSFER_FUNCTION: Moves funding, publication credit, and epistemic authority toward institutes and brokers who can certify the co-production process, away from both unaffiliated methodologists (who lack community ties) and unaffiliated community members (whose testimony doesn't count until formally brokered). A toll is extracted at the boundary itself.
% ABSENT_VOICES: Grassroots groups without grant-writing capacity and individual community members outside any recognized partnership never get a vote on whether the dual-validation requirement itself is reasonable — they experience it only as a gate their knowledge must pass through, administered by people who are not them.
% DISAPPEARANCE_RATIONALE: Boundary organizations and coproduction institutes would lose funding and institutional purpose overnight, and journals/funders relying on co-production checklists would need new criteria — a real rearrangement for the certifying layer. But well-resourced credentialed researchers and organized communities with existing ties would likely continue collaborating informally much as before; whether the world 'rearranges' depends heavily on whether you are asking the intermediary layer or the underlying research relationships.
% FOUNDING_PROBLEM: Decades of extractive 'parachute research' and methodologically sound-but-harmful interventions (e.g. in Indigenous health, environmental justice, and international development) where rigorous methods produced findings that ignored or actively damaged the communities studied, alongside a recognition that purely experiential claims could not by themselves resolve competing empirical disputes.
% FOUNDING_PROBLEM_CORROBORATION: Independent historians of science and several Indigenous research ethics boards (external to the coproduction-institute funding stream) corroborate that the original harms were real and the coordination need was genuine. However, some of the same external reviewers, along with grassroots organizers not affiliated with any funded partnership, now argue the standard has been substantially captured by a professional broker class and no longer requires the scale of gatekeeping currently imposed — a status assessment the benefiting institutes themselves dispute.
narrative_ontology:disappearance_verdict(legitimate_knowledge_boundary__hybrid_coproduction_reading, contested).
narrative_ontology:founding_problem_status(legitimate_knowledge_boundary__hybrid_coproduction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_knowledge_boundary__hybrid_coproduction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimate_knowledge_boundary__hybrid_coproduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.48, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_knowledge_boundary__hybrid_coproduction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_knowledge_boundary__hybrid_coproduction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_knowledge_boundary__hybrid_coproduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48 at interval end) rather than severe because a genuine coordination function is present and documented improvements in research relevance and reduced community harm are real, not fabricated cover. Suppression is moderate (0.42): the standard does not forcibly prevent methodology-only or experience-only claims from circulating, but funding, publication, and institutional legitimacy channels increasingly route around anyone who does not clear the dual bar, which functions as soft suppression of alternative validation paths. Theater ratio rises over the interval (0.12 to 0.38) as 'co-production' checklists proliferate in grant applications partly as compliance performance rather than as evidence of substantive integration — a Goodhart drift worth flagging even though it has not yet dominated the metric.
 *
 * PERSPECTIVAL GAP:
 *   From the coproduction_institutes' seat, the standard looks like principled epistemic pluralism correcting historical harms. From the unaffiliated_community_knowledge_holders' seat, the same standard looks like a new credentialing toll on their own lived experience — they must now be 'validated' by an institutional process to be believed at all. The engine should compute these as structurally different experiences of the same arrangement, not reconcile them into one verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Coproduction institutes, boundary organizations, and brokers sit near the beneficiary end: they administer and monetize the standard without bearing its compliance costs. Well-resourced credentialed researchers are dual-positioned — they benefit from the legitimacy premium but also pay real added costs, hence secondary_role payer. The two excluded/payer groups at powerless and moderate power levels bear the costs most acutely and with the least capacity to route around them (trapped and constrained exit respectively), which is why the effective extraction directed at them is amplified relative to the story's base ε.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (extractive, contextually blind research causing real harm) was genuine and is only partially resolved — some communities still face parachute research, so the mandate is not simply obsolete. But the mismatch between founding_problem_status ('contested,' trending toward partially resolved in well-resourced contexts) and the world_rearranges half of the disappearance verdict (the intermediary layer would collapse) is exactly the signal a classification should preserve: this is not pure extraction dressed as coordination, nor is it pure coordination free of rent-seeking. It is a tangled rope — a real coordination function that has grown an extractive certification bureaucracy riding on top of it, which is why claimed_type is tangled_rope rather than rope or snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coproduction_versus_credentialing_boundary,
    'Is the dual-validation requirement of the hybrid reading a genuine epistemic improvement over either sibling reading alone, or is ''co-production'' primarily a new credentialing layer that reproduces the very expertise-gatekeeping the pluralism reading was meant to correct, now doubled?',
    'Comparative studies of research outcomes and community-reported benefit across projects certified via full co-production versus projects using only methodological review or only community-led validation, controlling for topic area and funding level.',
    'If outcomes are equivalent without the co-production certification overhead, the added apparatus (institutes, brokers) is largely extractive rent on top of coordination that could occur informally; if outcomes are substantially better under formal co-production, the apparatus is closer to necessary coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coproduction_versus_credentialing_boundary, empirical, 'Whether the hybrid standard''s certification layer adds real epistemic value beyond what either sibling standard achieves alone.').

omega_variable(
    who_certifies_the_certifiers,
    'Who has standing to judge whether a given co-production process was substantively rigorous and substantively experiential, versus performative on either axis, and can that judgment itself avoid capture by the same institutes that administer the standard?',
    'Track whether independent audit bodies (with no funding relationship to coproduction institutes) exist and are used, versus self-certification by the institutes and brokers who benefit from favorable findings.',
    'Absence of independent audit capacity would support the reading that the standard''s enforcement is largely self-referential and prone to Goodhart drift (rising theater_ratio); presence of credible independent audit would support a more genuinely coordinative reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(who_certifies_the_certifiers, conceptual, 'Whether the standard''s enforcement apparatus can be evaluated independently of the parties who benefit from it.').

omega_variable(
    framing_choice_kernel_versus_reading,
    'Should the analytical unit here be the single kernel (one contested question about what counts as legitimate knowledge) or three genuinely distinct constraints (as authored)? A kernel-level framing would produce one ε averaged or disputed across readings; the reading-level framing produces three stable, comparable ε values.',
    'This has already been resolved by the ε-invariance principle and Rule 1: because the three readings have different beneficiary/victim structures and different ε (credentialed_expertise likely lower suppression/higher accessibility_collapse toward Mountain-like claims of rigor; experiential_pluralism likely lower institutional extraction but higher internal community contestation; hybrid_coproduction as authored here, moderate on both), they are authored as three linked constraints rather than one.',
    'Confirms the decomposition choice; documents for future readers why three files exist instead of one with a measurement parameter.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_choice_kernel_versus_reading, conceptual, 'Documents the kernel-versus-reading decomposition choice per the ε-invariance principle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(legi_tr_t4, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement(legi_tr_t8, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 8, 0.23).
narrative_ontology:measurement(legi_tr_t12, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement(legi_tr_t16, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 16, 0.32).
narrative_ontology:measurement(legi_tr_t20, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(legi_tr_t24, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 24, 0.38).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(legi_be_t4, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 4, 0.28).
narrative_ontology:measurement(legi_be_t8, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 8, 0.34).
narrative_ontology:measurement(legi_be_t12, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 12, 0.39).
narrative_ontology:measurement(legi_be_t16, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 16, 0.43).
narrative_ontology:measurement(legi_be_t20, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 20, 0.46).
narrative_ontology:measurement(legi_be_t24, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 24, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(legi_su_t4, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 4, 0.25).
narrative_ontology:measurement(legi_su_t8, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 8, 0.3).
narrative_ontology:measurement(legi_su_t12, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 12, 0.34).
narrative_ontology:measurement(legi_su_t16, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 16, 0.37).
narrative_ontology:measurement(legi_su_t20, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(legi_su_t24, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 24, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_knowledge_boundary__hybrid_coproduction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.1).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__hybrid_coproduction_reading, credentialed_expertise_reading).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__hybrid_coproduction_reading, experiential_pluralism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the legitimate_knowledge_boundary kernel. credentialed_expertise_reading and experiential_pluralism_reading are separate constraint files with their own ε, beneficiaries, and victims. This hybrid reading is authored as structurally distinct: it declares an active intermediary beneficiary class (coproduction_institutes, boundary_organizations, translational_research_brokers) that neither sibling reading generates, because dual validation requires a certifying layer that pure credentialing or pure experiential validation do not.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
