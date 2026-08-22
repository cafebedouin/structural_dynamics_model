% ============================================================================
% CONSTRAINT STORY: competence_retention_exercise__catastrophe_as_necessary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_retention_exercise__catastrophe_as_necessary, []).

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
 *   constraint_id: competence_retention_exercise__catastrophe_as_necessary
 *   human_readable: Catastrophe-as-Competence-Maintenance Constraint (Catastrophe-Necessary Reading)
 *   domain: safety/organizational_learning
 *
 * SUMMARY:
 *   Organizations in safety-critical domains (aviation, medicine, nuclear,
 *   emergency response) face an organizational learning paradox: operational
 *   competence in crisis management requires experience with irreversible
 *   decisions under extreme time pressure and incomplete information.
 *   High-fidelity simulation can approximate many aspects of crisis
 *   management, but incident-response professionals and safety culture
 *   advocates argue that simulation cannot replicate the visceral,
 *   irreversible stakes that drive genuine learning. The
 *   catastrophe-as-necessary reading asserts that organizations become most
 *   vulnerable during long incident-free periods, when complacency
 *   accumulates invisibly beneath surface-level competence metrics. From this
 *   perspective, real catastrophes function as system resets — forcing
 *   organizational attention, resource commitment, and behavioral change that
 *   simulation-based training cannot achieve. The constraint extracts from
 *   cost-conscious organizations and simulation practitioners by devaluing
 *   their investment in simulation-based training infrastructure, while
 *   benefiting incident-response professionals whose authority and prestige
 *   depend on catastrophe-specific experience. This is a tangled_rope:
 *   genuine coordination function (organizations do need to maintain crisis
 *   readiness) combined with asymmetric extraction (the beneficial
 *   positioning of incident-response professionals and the victimization of
 *   simulation practitioners).
 *
 * KEY AGENTS:
 *   - incident_response_professionals: experienced crews and operators who have survived real crises and claim visceral understanding that simulation cannot convey
 *   - safety_culture_advocates: researchers and regulators who frame catastrophes as organizational learning mechanisms
 *   - simulation_practitioners: training institutions whose commercial viability rests on simulation adequacy
 *   - cost_conscious_organizations: operators who have invested in simulation-based training and now face pressure to devalue that investment
 *   - regulatory_authorities: bodies that set training standards and can enforce certification gatekeeping
 *   - post_incident_investigation_bodies: neutral observers whose analysis shapes professional and public debate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_retention_exercise__catastrophe_as_necessary, 0.68).
domain_priors:suppression_score(competence_retention_exercise__catastrophe_as_necessary, 0.72).
domain_priors:theater_ratio(competence_retention_exercise__catastrophe_as_necessary, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, extractiveness, 0.68).
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_retention_exercise__catastrophe_as_necessary, tangled_rope).
narrative_ontology:human_readable(competence_retention_exercise__catastrophe_as_necessary, "Catastrophe-as-Competence-Maintenance Constraint (Catastrophe-Necessary Reading)").
narrative_ontology:topic_domain(competence_retention_exercise__catastrophe_as_necessary, "safety/organizational_learning").

domain_priors:requires_active_enforcement(competence_retention_exercise__catastrophe_as_necessary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_retention_exercise__catastrophe_as_necessary, '2e3c49ea-94d5-46eb-895b-71909347f99a').
narrative_ontology:cs_kernel_codification('2e3c49ea-94d5-46eb-895b-71909347f99a', distributed).
narrative_ontology:cs_authority_grounding('2e3c49ea-94d5-46eb-895b-71909347f99a', extraction).
narrative_ontology:cs_interpretation_layer_present('2e3c49ea-94d5-46eb-895b-71909347f99a').
narrative_ontology:cs_reading_relation('2e3c49ea-94d5-46eb-895b-71909347f99a', competence_retention_exercise__simulation_as_sufficient, forecloses).
narrative_ontology:cs_reading_relation('2e3c49ea-94d5-46eb-895b-71909347f99a', competence_retention_exercise__near_miss_as_bridge, coexists_with).
narrative_ontology:cs_axiom('2e3c49ea-94d5-46eb-895b-71909347f99a', foundational, irreversibility_irreplaceable_teacher).
narrative_ontology:cs_axiom_status(irreversibility_irreplaceable_teacher, holdable).
narrative_ontology:cs_axiom_grounding('2e3c49ea-94d5-46eb-895b-71909347f99a', irreversibility_irreplaceable_teacher, empirically_contingent).
narrative_ontology:cs_axiom('2e3c49ea-94d5-46eb-895b-71909347f99a', foundational, visceral_stakes_non_simul_able).
narrative_ontology:cs_axiom_status(visceral_stakes_non_simul_able, holdable).
narrative_ontology:cs_axiom_grounding('2e3c49ea-94d5-46eb-895b-71909347f99a', visceral_stakes_non_simul_able, deontological).
narrative_ontology:cs_reference_frame('2e3c49ea-94d5-46eb-895b-71909347f99a', competence_validated_by_catastrophe_survival).
narrative_ontology:cs_drift_state('2e3c49ea-94d5-46eb-895b-71909347f99a', contemporary_high_fidelity_simulation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2e3c49ea-94d5-46eb-895b-71909347f99a', '2026-06-12T09:15:32Z').
narrative_ontology:cs_kernel_id(competence_retention_exercise__catastrophe_as_necessary, competence_retention_exercise).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_retention_exercise__catastrophe_as_necessary, incident_response_professionals).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__catastrophe_as_necessary, safety_culture_advocates).
narrative_ontology:constraint_victim(competence_retention_exercise__catastrophe_as_necessary, simulation_practitioners).
narrative_ontology:constraint_victim(competence_retention_exercise__catastrophe_as_necessary, cost_conscious_organizations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Emergency responders, flight crews, nuclear operators, surgeons whose professional identity and career advancement depend on demonstrated mastery of real-world crisis. They attest that simulation training cannot convey the visceral stakes, irreversibility, and decision-velocity of actual emergencies. Their authority derives from having survived real incidents and seen simulation-only-trained crews fail in the field. Professionally committed to the view that catastrophes are necessary validations of competence.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, incident_response_professionals, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(competence_retention_exercise__catastrophe_as_necessary, incident_response_professionals, agenda_setter).

% Safety researchers, regulatory bodies, and organizational learning scholars who argue that incident-free periods create dangerous complacency; that organizations in their best safety standing are most vulnerable to novel failures; that only real catastrophes generate the organizational attention, resource commitment, and behavioral change required to prevent the next one. Benefit from framing catastrophe as systemic necessity rather than failure.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, safety_culture_advocates, beneficiary,
    organized, generational, constrained, global).

% Flight simulator companies, medical training centers, nuclear control room trainers whose commercial and professional viability rests on the premise that high-fidelity simulation constitutes genuine competence validation. Under the catastrophe-necessary reading, their work is devalued as false confidence-building; they pay in market share, research funding, and professional standing as the constraint frames simulation as inadequate.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, simulation_practitioners, payer,
    moderate, biographical, constrained, global).

% Airlines, hospitals, industrial operators that have invested heavily in simulation-based training as a cost-effective alternative to operational currency. Under this reading, their training infrastructure is dismissed as generating false confidence; the constraint implicitly mandates real-world incident cycles as the true competence validation mechanism, which they cannot cost-effectively provide.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, cost_conscious_organizations, payer,
    powerful, biographical, mobile, regional).

% Aviation authorities, nuclear regulators, medical boards that set training and certification standards. This reading exerts pressure to devalue simulation-only pathways and require operational currency for safety-critical roles, enforced via audit and certification gatekeeping. They administer the constraint through curriculum requirements and recertification rules.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, regulatory_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Accident investigation boards, safety boards, quality assurance teams that conduct root-cause analysis after incidents. They produce testimony and evidence about whether crews trained only in simulation or operators with incident experience performed better or worse when facing novel emergencies. Their findings shape public and professional debate about the adequacy of simulation.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, post_incident_investigation_bodies, observer,
    institutional, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_retention_exercise__catastrophe_as_necessary, incident_response_professionals).
narrative_ontology:fixing_cost_class(competence_retention_exercise__catastrophe_as_necessary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains organizational readiness for catastrophic scenarios by asserting that competence in crisis management decays invisibly during incident-free periods, requiring real-world validation through actual emergencies to prevent the complacency that makes organizations most dangerous when they appear safest. Coordinates distributed teams (crews, operators, regulators, safety cultures) around a shared understanding that simulation alone is insufficient for genuine competence maintenance.
% TRANSFER_FUNCTION: Transfers professional prestige, career advancement pathways, research funding, and regulatory authority FROM simulation practitioners and cost-conscious organizations TO incident-response professionals and safety culture advocates who frame catastrophe-triggered learning as the only legitimate competence validation mechanism. Also transfers organizational attention and resource commitment (toward real-world incident management rather than simulation infrastructure) in the direction favored by incident-response-centered perspectives.
% ABSENT_VOICES: High-reliability organizations with long incident-free records (commercial aviation, modern medicine, nuclear power plants that have operated for decades without catastrophic failures) who would attest that rigorous simulation training DOES work and that their incident-free records prove it. Simulation engineers and training researchers who have documented measurable competence improvements from high-fidelity simulation. Organizations in domains with structurally low catastrophe frequency (aviation) who cannot provide real-incident experience and would argue the constraint is unfeasible. These voices are excluded from professional discourse because incident-response communities' authority derives from rare real-catastrophe experience, which these high-reliability organizations precisely lack.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, organizations would rationally optimize toward demonstrating competence through simulation validation alone, without requiring catastrophe-based recertification or incident-experience mandates. Regulatory standards would accept simulation-only training pathways. Incident-response professionals would lose their unique career-status advantage from catastrophe-specific experience. Simulation training investment would accelerate. However, the disappearance would not eliminate the underlying organizational learning problem (competence decay during incident-free periods); it would simply shift the burden of proof from 'only catastrophes teach competence' to 'simulation training is sufficient.' Organizations might face a different competence crisis — false confidence from untested simulation-only crews — or they might find simulation adequate. The verdict is contested because incident-response professionals and safety advocates genuinely believe the disappearance would create dangerous vulnerabilities, while simulation practitioners and high-reliability organizations believe it would enable rational training optimization.
% FOUNDING_PROBLEM: Organizational competence in crisis management decays invisibly during incident-free periods; operators who pass high-fidelity simulation exercises develop confidence in their decision-making without experiencing the irreversibility, time pressure, incomplete information, and emotional weight of actual crises. During long incident-free periods, this competence-illusion deepens and organizations become maximally vulnerable to novel catastrophes. The founding problem is that simulation training creates false confidence and masks the atrophy of genuine crisis readiness.
% FOUNDING_PROBLEM_CORROBORATION: Incident investigation boards (Tenerife airport disaster, Three Mile Island, Deepwater Horizon, 2009 Air France Flight 447) have documented cases where simulation-trained crews underperformed in novel emergencies, providing direct evidence that simulation-only training left gaps in crisis decision-making. Incident-response professionals and safety researchers attest that these cases validate the founding problem. However, independent aviation safety research documents that incident-free records strongly correlate with rigorous simulation training programs, and cohort studies of simulation-trained crews show they consistently outperform untrained ones in controlled emergency scenarios. The founding problem is attested ONLY by incident-response professionals and post-incident investigators whose authority derives from catastrophe-specific experience; it is contested by high-reliability organizations, simulation researchers, and statistical safety analysis that show incident-free records are evidence that simulation works, not evidence that it fails.
narrative_ontology:disappearance_verdict(competence_retention_exercise__catastrophe_as_necessary, contested).
narrative_ontology:founding_problem_status(competence_retention_exercise__catastrophe_as_necessary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_retention_exercise__catastrophe_as_necessary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_retention_exercise__catastrophe_as_necessary, 'none', 1).
narrative_ontology:epsilon_provenance(competence_retention_exercise__catastrophe_as_necessary, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_retention_exercise__catastrophe_as_necessary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_retention_exercise__catastrophe_as_necessary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_retention_exercise__catastrophe_as_necessary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness measurement (0.68 by 2026) reflects the constraint's asymmetric structure: incident-response professionals and safety advocates benefit from framing catastrophe as necessary, while simulation practitioners and cost-conscious organizations bear the cost of training-infrastructure devaluation. Suppression is high (0.72) because the constraint enforces its narrative through regulatory gatekeeping, curriculum authority, and professional status hierarchies; organizations that want to operate solely on simulation-based training face resistance at certification and insurance levels. Theater is moderately high (0.58) because the constraint's justification — 'only real catastrophes teach genuine competence' — is difficult to falsify: every incident-free period is framed as proof of complacency rather than evidence of training success; every new catastrophe is framed as validation of the reading rather than as evidence of simulation inadequacy. The measurement series tracks the constraint's growth over 56 years: from 1970 (low theater, lower extraction) when simulation training was nascent and incident-response culture dominated, through 2000 (rapid growth in simulation investment and pushback), to 2026 (high theater, extraction, and suppression as simulation has become mainstream but regulatory and professional narrative remains catastrophe-necessary). Accessibility collapse rises because alternatives (fully simulation-based training, demonstrating competence without catastrophe-validation) become increasingly foreclosed by regulatory and professional gatekeeping. Resistance declines as the constraint becomes institutionalized in training standards and professional advancement criteria.
 *
 * PERSPECTIVAL GAP:
 *   From the incident-response professional seat: the catastrophe-necessary reading is an irreducible truth about human learning and organizational dynamics; simulation is a cost-saving measure that creates dangerous overconfidence. From the simulation practitioner seat: the reading is a rhetorical weapon deployed by professions that derive prestige from scarcity of catastrophe-specific experience; it devalues decades of rigorous training innovation and ignores mounting evidence that simulation-trained operators perform reliably. From the regulatory seat: the reading becomes a normative framework that justifies requiring operational currency for recertification, which the engine would compute as a differentiated directionality (beneficiary for incident-response professionals, target for simulation practitioners). The payer seats experience suppression partly through regulatory enforcement and partly through professional status hierarchies that frame simulation-only training as second-class. The constraint's persistence depends on maintaining the narrative that catastrophe is organizationally necessary rather than regrettable; organizations that achieve long incident-free records through rigorous simulation must reframe their success as 'deferred failure' rather than as 'successful prevention.'
 *
 * DIRECTIONALITY LOGIC:
 *   Incident-response professionals (organized, identity-locked exit, generational horizon) are structural beneficiaries: the constraint maintains their professional irreplaceability and prestige; they sit at d ≈ 0.2 (near-beneficiary). Safety advocates (organized, constrained exit) also benefit from framing catastrophe as necessary; they sit at d ≈ 0.25. Simulation practitioners (moderate power, biographical horizon, constrained exit) are targets: their training models are devalued, their investment is framed as insufficient, their career paths are compressed; they sit at d ≈ 0.75. Cost-conscious organizations (powerful, mobile exit options) face enforcement pressure but retain options (they can lobby for regulatory change, shift domains, invest in hybrid approaches); they sit at d ≈ 0.65. Regulatory authorities (institutional power) implement gatekeeping but are not themselves extracted from; they sit near d ≈ 0.5 (symmetric: they coordinate training standards AND enforce suppression of alternatives).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is genuinely live: organizations do face competence decay during incident-free periods, and simulation training can create false confidence if not combined with other maintenance mechanisms. The constraint's tangled_rope classification is justified: there is real coordination function (maintaining crisis readiness, preventing complacency-driven failures) AND asymmetric extraction (devaluing simulation investment, benefiting incident-response professions). The constraint would not be classified as pure rope (coordination only) or pure snare (extraction only) because both are present and both are structural. The beneficiary/victim split is clear and material: incident-response professionals and safety advocates collect career prestige and research funding; simulation practitioners lose market share and professional standing. The enforcement is active: regulatory standards, curriculum gatekeeping, insurance requirements. The mandatrophy test asks whether the founding problem still justifies the extraction structure. The answer is contested: incident-response communities and safety advocates say yes, the problem persists and organizations still become dangerously complacent in incident-free periods; simulation researchers and cost-conscious organizations say the evidence no longer supports the constraint's core premise — modern simulation has become high-fidelity enough that incident-free records correlate with rigorous simulation programs, not with deferred catastrophe. This contest is precisely what the kernel frame captures: the catastrophe_as_necessary reading is one coherent structural position, held by identified beneficiary parties; the sibling readings are other coherent positions held by identified victim parties.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_fidelity_boundary,
    'At what level of simulation fidelity does the distinction between ''real'' and ''rehearsal'' lose empirical meaning? Is there a technical threshold beyond which simulation training produces identical competence outcomes to real incidents?',
    'Longitudinal cohort studies comparing crews trained exclusively in high-fidelity simulation against crews with operational incident experience, measured on novel emergencies in controlled settings; post-incident analysis of crews trained via simulation only vs. operationally currency-based crews in real disasters.',
    'If no such threshold exists and outcomes diverge predictably, the catastrophe-necessary reading holds. If simulations can be made sufficiently faithful that outcomes converge, the reading forecloses and the sibling simulation_as_sufficient reading becomes coherent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_fidelity_boundary, empirical, 'Whether the cognitive/phenomenological gap between simulation and reality is reducible or irreducible.').

omega_variable(
    selection_bias_in_incident_memory,
    'Do incident-response professionals'' claims that ''only catastrophes teach competence'' suffer from survivorship bias — they remember the rare cases where simulation-only training failed, not the far more numerous cases where simulation-trained crews performed flawlessly?',
    'Statistical meta-analysis of all recorded emergency responses in a domain (aviation, medicine, nuclear) comparing failure rates for simulation-only crews vs. operationally current crews, controlling for incident severity. Incident investigation board case-series analysis for frequency of simulation-insufficiency findings.',
    'If simulation-only crews fail at measurably higher rates, the catastrophe-necessary reading is supported. If failure rates are equivalent or lower for simulation-trained crews, the reading loses empirical ground and near_miss_as_bridge or simulation_as_sufficient become more coherent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(selection_bias_in_incident_memory, empirical, 'Whether the catastrophe-necessary reading reflects true competence gaps or incident-reporting artifacts.').

omega_variable(
    identity_fusion_in_incident_response_culture,
    'To what extent is the catastrophe-necessary reading sustained by professional identity fusion among incident-response communities (the unique prestige and irreplaceability of having ''been through it'')? Would the reading persist if incident-response professionals lost career-status advantage from catastrophe-specific experience?',
    'Policy experiments: shift credentialing and advancement standards to treat simulation-validated competence equivalently to incident experience; measure whether the rhetorical endorsement of the catastrophe-necessary reading changes among professionals whose status no longer depends on it. Qualitative research on identity narratives in incident-response communities.',
    'High identity fusion indicates the reading is sustained partly by institutional interests rather than purely empirical grounds; the constraint becomes more tangled_rope (coordination-plus-extraction) than rope (pure coordination). This informs whether regulatory changes would reduce extraction or trigger professional resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_in_incident_response_culture, conceptual, 'Whether professional identity commitments drive the catastrophe-necessary reading beyond empirical warrant.').

omega_variable(
    kernel_contest_committer_frame,
    'This constraint is ONE READING of the contested kernel competence_retention_exercise. How would the catastrophe_as_necessary reading''s structural metrics and beneficiary/victim mapping change if we adopted the sibling reading simulation_as_sufficient as the governing framework?',
    'Author separate constraint stories for each sibling reading: simulation_as_sufficient and near_miss_as_bridge. Compute the ε-invariance decomposition: same referent (the standing organizational arrangement for competence maintenance), different readings → different ε values → different classifications. Network the three stories via affects_constraints.',
    'The kernel contest is irreducible from within any single reading; it is a structural fact about contested legitimacy. The catastrophe-as-necessary reading instantiates one coherent position, which is all a single constraint story can do. The engine will classify this reading''s structural data; sibling readings will classify on their own structural data. The corpus records the contest, not the resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_committer_frame, conceptual, 'Meta-frame: the catastrophe_as_necessary reading is one vertex in a contested kernel; other readings are other constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_retention_exercise__catastrophe_as_necessary, 1970, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t1970, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 1970, 0.25).
narrative_ontology:measurement(comp_tr_t1985, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 1985, 0.32).
narrative_ontology:measurement(comp_tr_t2000, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 2000, 0.41).
narrative_ontology:measurement(comp_tr_t2008, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 2008, 0.5).
narrative_ontology:measurement(comp_tr_t2015, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 2015, 0.55).
narrative_ontology:measurement(comp_tr_t2026, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 2026, 0.58).

% Extraction over time
narrative_ontology:measurement(comp_be_t1970, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 1970, 0.35).
narrative_ontology:measurement(comp_be_t1985, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 1985, 0.42).
narrative_ontology:measurement(comp_be_t2000, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 2000, 0.52).
narrative_ontology:measurement(comp_be_t2008, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 2008, 0.61).
narrative_ontology:measurement(comp_be_t2015, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 2015, 0.65).
narrative_ontology:measurement(comp_be_t2026, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 2026, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t1970, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 1970, 0.48).
narrative_ontology:measurement(comp_su_t1985, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 1985, 0.55).
narrative_ontology:measurement(comp_su_t2000, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 2000, 0.62).
narrative_ontology:measurement(comp_su_t2008, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 2008, 0.68).
narrative_ontology:measurement(comp_su_t2015, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 2015, 0.7).
narrative_ontology:measurement(comp_su_t2026, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 2026, 0.72).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1970, tn=2026
narrative_ontology:measurement(comp_grid_01, competence_retention_exercise__catastrophe_as_necessary, accessibility_collapse(class), 1970, 0.28).
narrative_ontology:measurement(comp_grid_02, competence_retention_exercise__catastrophe_as_necessary, accessibility_collapse(class), 2026, 0.61).
narrative_ontology:measurement(comp_grid_03, competence_retention_exercise__catastrophe_as_necessary, accessibility_collapse(individual), 1970, 0.42).
narrative_ontology:measurement(comp_grid_04, competence_retention_exercise__catastrophe_as_necessary, accessibility_collapse(individual), 2026, 0.58).
narrative_ontology:measurement(comp_grid_05, competence_retention_exercise__catastrophe_as_necessary, accessibility_collapse(organizational), 1970, 0.35).
narrative_ontology:measurement(comp_grid_06, competence_retention_exercise__catastrophe_as_necessary, accessibility_collapse(organizational), 2026, 0.64).
narrative_ontology:measurement(comp_grid_07, competence_retention_exercise__catastrophe_as_necessary, accessibility_collapse(structural), 1970, 0.31).
narrative_ontology:measurement(comp_grid_08, competence_retention_exercise__catastrophe_as_necessary, accessibility_collapse(structural), 2026, 0.65).
narrative_ontology:measurement(comp_grid_09, competence_retention_exercise__catastrophe_as_necessary, resistance(class), 1970, 0.65).
narrative_ontology:measurement(comp_grid_10, competence_retention_exercise__catastrophe_as_necessary, resistance(class), 2026, 0.51).
narrative_ontology:measurement(comp_grid_11, competence_retention_exercise__catastrophe_as_necessary, resistance(individual), 1970, 0.68).
narrative_ontology:measurement(comp_grid_12, competence_retention_exercise__catastrophe_as_necessary, resistance(individual), 2026, 0.48).
narrative_ontology:measurement(comp_grid_13, competence_retention_exercise__catastrophe_as_necessary, resistance(organizational), 1970, 0.72).
narrative_ontology:measurement(comp_grid_14, competence_retention_exercise__catastrophe_as_necessary, resistance(organizational), 2026, 0.42).
narrative_ontology:measurement(comp_grid_15, competence_retention_exercise__catastrophe_as_necessary, resistance(structural), 1970, 0.61).
narrative_ontology:measurement(comp_grid_16, competence_retention_exercise__catastrophe_as_necessary, resistance(structural), 2026, 0.52).
narrative_ontology:measurement(comp_grid_17, competence_retention_exercise__catastrophe_as_necessary, stakes_inflation(class), 1970, 0.41).
narrative_ontology:measurement(comp_grid_18, competence_retention_exercise__catastrophe_as_necessary, stakes_inflation(class), 2026, 0.68).
narrative_ontology:measurement(comp_grid_19, competence_retention_exercise__catastrophe_as_necessary, stakes_inflation(individual), 1970, 0.38).
narrative_ontology:measurement(comp_grid_20, competence_retention_exercise__catastrophe_as_necessary, stakes_inflation(individual), 2026, 0.62).
narrative_ontology:measurement(comp_grid_21, competence_retention_exercise__catastrophe_as_necessary, stakes_inflation(organizational), 1970, 0.45).
narrative_ontology:measurement(comp_grid_22, competence_retention_exercise__catastrophe_as_necessary, stakes_inflation(organizational), 2026, 0.71).
narrative_ontology:measurement(comp_grid_23, competence_retention_exercise__catastrophe_as_necessary, stakes_inflation(structural), 1970, 0.39).
narrative_ontology:measurement(comp_grid_24, competence_retention_exercise__catastrophe_as_necessary, stakes_inflation(structural), 2026, 0.69).
narrative_ontology:measurement(comp_grid_25, competence_retention_exercise__catastrophe_as_necessary, suppression(class), 1970, 0.48).
narrative_ontology:measurement(comp_grid_26, competence_retention_exercise__catastrophe_as_necessary, suppression(class), 2026, 0.72).
narrative_ontology:measurement(comp_grid_27, competence_retention_exercise__catastrophe_as_necessary, suppression(individual), 1970, 0.42).
narrative_ontology:measurement(comp_grid_28, competence_retention_exercise__catastrophe_as_necessary, suppression(individual), 2026, 0.65).
narrative_ontology:measurement(comp_grid_29, competence_retention_exercise__catastrophe_as_necessary, suppression(organizational), 1970, 0.51).
narrative_ontology:measurement(comp_grid_30, competence_retention_exercise__catastrophe_as_necessary, suppression(organizational), 2026, 0.78).
narrative_ontology:measurement(comp_grid_31, competence_retention_exercise__catastrophe_as_necessary, suppression(structural), 1970, 0.44).
narrative_ontology:measurement(comp_grid_32, competence_retention_exercise__catastrophe_as_necessary, suppression(structural), 2026, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_retention_exercise__catastrophe_as_necessary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_retention_exercise__catastrophe_as_necessary, 0.12).
narrative_ontology:affects_constraint(competence_retention_exercise__catastrophe_as_necessary, competence_retention_exercise__simulation_as_sufficient).
narrative_ontology:affects_constraint(competence_retention_exercise__catastrophe_as_necessary, competence_retention_exercise__near_miss_as_bridge).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel competence_retention_exercise. The kernel has three distinct structural instantiations: (1) catastrophe_as_necessary (this story) — real catastrophes are irreplaceable for competence maintenance; (2) simulation_as_sufficient (sibling) — high-fidelity simulation produces equivalent competence without requiring actual catastrophes; (3) near_miss_as_bridge (sibling) — minor incidents and near-misses provide sufficient real-world feedback to validate and update simulator training. The three readings share a referent (organizational arrangements for competence retention in safety-critical domains) but have different ε values, different beneficiary/victim maps, and different classifications. Each story is ε-invariant on its own reading; the kernel contest is irreducible from within any single reading. The corpus models the contest as a network of three linked constraint stories, each authored from the coherent epistemic position of its reading, not as a single story with measurement-relative ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_retention_exercise__catastrophe_as_necessary, powerful, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
