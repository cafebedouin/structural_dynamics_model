% ============================================================================
% CONSTRAINT STORY: legal_personhood_boundary__developmental_potentiality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   human_readable: Developmental Potentiality Reading: Fetal Personhood from Conception
 *   domain: legal/constitutional/rights
 *
 * SUMMARY:
 *   This constraint story models the developmental potentiality reading of
 *   the legal personhood boundary kernel — the claim that personhood and full
 *   rights attach at conception because the human organism from that point is
 *   a unitary life trajectory holder whose developmental potentiality grounds
 *   inherent moral status. The reading instantiates a constraint that legally
 *   enforces fetal personhood: abortion bans, fetal homicide laws, personhood
 *   amendments, and the regulatory apparatus that monitors and criminalizes
 *   pregnancy outcomes. The reading claims this constraint is a mountain
 *   (natural law recognition of inherent rights), but the structural data
 *   reveals active enforcement, identifiable victims (pregnant persons), and
 *   asymmetric extraction — the engine will compute per-seat classifications
 *   from this divergence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_boundary__developmental_potentiality_reading, 0.78).
domain_priors:suppression_score(legal_personhood_boundary__developmental_potentiality_reading, 0.89).
domain_priors:theater_ratio(legal_personhood_boundary__developmental_potentiality_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 0.89).
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__developmental_potentiality_reading, mountain).
narrative_ontology:human_readable(legal_personhood_boundary__developmental_potentiality_reading, "Developmental Potentiality Reading: Fetal Personhood from Conception").
narrative_ontology:topic_domain(legal_personhood_boundary__developmental_potentiality_reading, "legal/constitutional/rights").

domain_priors:requires_active_enforcement(legal_personhood_boundary__developmental_potentiality_reading).
domain_priors:emerges_naturally(legal_personhood_boundary__developmental_potentiality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__developmental_potentiality_reading, '229b8625-606e-4a7f-9fe9-de7566e3601b').
narrative_ontology:cs_kernel_codification('229b8625-606e-4a7f-9fe9-de7566e3601b', formalized).
narrative_ontology:cs_authority_grounding('229b8625-606e-4a7f-9fe9-de7566e3601b', lineage).
narrative_ontology:cs_interpretation_layer_present('229b8625-606e-4a7f-9fe9-de7566e3601b').
narrative_ontology:cs_reading_relation('229b8625-606e-4a7f-9fe9-de7566e3601b', legal_personhood_boundary__restrictive_anthropocentric_reading, coexists_with).
narrative_ontology:cs_reading_relation('229b8625-606e-4a7f-9fe9-de7566e3601b', legal_personhood_boundary__functional_capacity_reading, forecloses).
narrative_ontology:cs_axiom('229b8625-606e-4a7f-9fe9-de7566e3601b', foundational, human_life_begins_at_conception).
narrative_ontology:cs_axiom_status(human_life_begins_at_conception, holdable).
narrative_ontology:cs_axiom_grounding('229b8625-606e-4a7f-9fe9-de7566e3601b', human_life_begins_at_conception, theological).
narrative_ontology:cs_axiom('229b8625-606e-4a7f-9fe9-de7566e3601b', foundational, all_human_life_has_inherent_rights).
narrative_ontology:cs_axiom_status(all_human_life_has_inherent_rights, holdable).
narrative_ontology:cs_axiom_grounding('229b8625-606e-4a7f-9fe9-de7566e3601b', all_human_life_has_inherent_rights, deontological).
narrative_ontology:cs_axiom('229b8625-606e-4a7f-9fe9-de7566e3601b', secondary, developmental_potentiality_grounds_moral_status).
narrative_ontology:cs_axiom_status(developmental_potentiality_grounds_moral_status, holdable).
narrative_ontology:cs_axiom_grounding('229b8625-606e-4a7f-9fe9-de7566e3601b', developmental_potentiality_grounds_moral_status, deontological).
narrative_ontology:cs_reference_frame('229b8625-606e-4a7f-9fe9-de7566e3601b', classical_natural_law_personhood).
narrative_ontology:cs_drift_state('229b8625-606e-4a7f-9fe9-de7566e3601b', post_dobbs_enforcement_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('229b8625-606e-4a7f-9fe9-de7566e3601b', '').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__developmental_potentiality_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__developmental_potentiality_reading, prenatal_human_life).
narrative_ontology:constraint_victim(legal_personhood_boundary__developmental_potentiality_reading, pregnant_persons).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__developmental_potentiality_reading, medical_providers).
narrative_ontology:constraint_victim(legal_personhood_boundary__developmental_potentiality_reading, medical_providers).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__developmental_potentiality_reading, human_life_begins_at_conception).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__developmental_potentiality_reading, all_human_life_has_inherent_rights).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__developmental_potentiality_reading, natural_law_grounds_legal_personhood).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The embryo/fetus from conception onward, recognized as a full rights-bearer under this reading. Receives the full protection of law against termination. Has no capacity for consent, exit, or self-advocacy; its interests are represented by state enforcement and advocacy structures. The constraint's entire coordinate function is framed as protecting this party.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, prenatal_human_life, beneficiary,
    powerless, biographical, trapped, universal).

% Bear the physical, economic, and autonomy costs of gestation and forced birth when the constraint is enforced. Lose the legal capacity to terminate pregnancy; face criminal exposure for seeking abortion; experience medical care restrictions (miscarriage management, ectopic pregnancy treatment) as collateral enforcement. Exit options are geographically constrained (travel to permissive jurisdictions) and economically stratified; some are effectively trapped by poverty, caregiving duties, or legal barriers.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, pregnant_persons, payer,
    moderate, biographical, constrained, national).

% Legislatures and courts that enact and enforce fetal personhood statutes and constitutional amendments. Police, prosecutors, and regulatory bodies that investigate pregnancy outcomes, enforce abortion bans, and regulate medical practice. Acquire expansive authority over reproductive healthcare and pregnancy surveillance. Justify the constraint as protecting constitutional rights of prenatal persons; the enforcement machinery is the mechanism that makes the right operative.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, state_enforcement_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Obstetricians, emergency physicians, and pharmacists who face criminal liability for providing standard reproductive care. Lose professional autonomy; must navigate vague 'life of the mother' exceptions under threat of prosecution. Some benefit from reduced professional conflict (conscience protections align with the constraint), but the dominant experience is constraint and risk. Exit means leaving practice, relocating, or restricting services — costly and professionally damaging.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, medical_providers, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(legal_personhood_boundary__developmental_potentiality_reading, medical_providers, beneficiary).

% Bioethicists, disability rights scholars, reproductive justice organizations, and legal theorists who argue personhood requires demonstrated cognitive capacity (sentience, self-awareness, rationality). Their framework would place personhood later in gestation or at birth, preserving pregnant person autonomy. They are structurally excluded from the constraint's operative logic because the constraint defines them out of the conversation — the fetus's rights are prior to and independent of capacity assessments.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, functional_capacity_advocates, excluded,
    organized, generational, mobile, global).

% Scholars of constitutional law, moral philosophy, and rights theory who analyze the constraint's coherence, its relationship to precedent, and its implications for legal ontology. They do not bear costs or collect benefits from the constraint's operation; they map its structure, track its drift, and evaluate its claims against competing readings of the personhood kernel.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, legal_philosophy_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of extending legal protection to the most vulnerable human life by anchoring personhood in a biologically definite, morally non-arbitrary threshold (conception), thereby avoiding sliding-scale capacity assessments that could exclude other vulnerable populations.
% TRANSFER_FUNCTION: Transfers bodily autonomy, medical decision-making authority, and life-trajectory control from pregnant persons to the fetal rights-holder; transfers enforcement authority and pregnancy surveillance power to the state; transfers professional discretion from medical providers to legal statutes.
% ABSENT_VOICES: Pregnant persons in ban jurisdictions who cannot travel; functional capacity theorists who would ground personhood in sentience rather than species membership; low-income pregnant persons for whom geographic exit is impossible; medical providers who leave practice rather than comply — all would object if present, but the constraint's enforcement architecture and geographic scope structurally exclude their effective participation.
% DISAPPEARANCE_RATIONALE: If fetal personhood from conception vanished overnight, abortion bans would lose their constitutional foundation; pregnant persons would regain legal autonomy over pregnancy decisions; state enforcement apparatus would lose its primary mandate for pregnancy surveillance; medical practice would revert to professional standards rather than criminal statutes; the entire regulatory architecture of post-Dobbs reproductive law would collapse.
% FOUNDING_PROBLEM: The moral and legal status of prenatal human life: whether the human organism from conception possesses inherent rights that the law must recognize and protect, and whether any developmental threshold after conception can serve as a non-arbitrary line for rights-attribution without excluding other vulnerable humans.
% FOUNDING_PROBLEM_CORROBORATION: Religious traditions (Catholic, evangelical Protestant) and natural law theorists attest the problem is live — the unborn child's right to life remains the founding issue. Secular bioethics (Singer, McMahan), reproductive justice movements, and the Roe/Casey precedent lineage (though Dobbs overturned the constitutional holding, the capacity-based reasoning persists in dissent and scholarship) attest the problem is dead or contested — they argue the founding problem was mis-specified, that autonomy and equality demand a different threshold, and that the developmental potentiality reading imports theological anthropology into constitutional law.
narrative_ontology:disappearance_verdict(legal_personhood_boundary__developmental_potentiality_reading, world_rearranges).
narrative_ontology:founding_problem_status(legal_personhood_boundary__developmental_potentiality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legal_personhood_boundary__developmental_potentiality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legal_personhood_boundary__developmental_potentiality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legal_personhood_boundary__developmental_potentiality_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legal_personhood_boundary__developmental_potentiality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legal_personhood_boundary__developmental_potentiality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, ExtMetricName, E),
    domain_priors:suppression_score(legal_personhood_boundary__developmental_potentiality_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(legal_personhood_boundary__developmental_potentiality_reading),
    narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(legal_personhood_boundary__developmental_potentiality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.78) is high because the constraint transfers the full burden of gestation and birth onto pregnant persons without compensation, while the beneficiary (prenatal life) cannot consent or reciprocate. Suppression (0.89) is very high because the constraint's persistence depends on criminal penalties, medical surveillance, and geographic barriers that actively prevent exit. Theater ratio (0.42) reflects that the 'protection of life' framing performs genuine coordination (neonatal care, anti-violence protections for pregnant women) alongside the extraction, but an increasing share of enforcement activity targets pregnancy management rather than independent threats to fetal life. Accessibility collapse (0.72) is high because the constraint defines the moral universe — alternatives (capacity-based personhood) are rendered conceptually incoherent within the reading's framework. Resistance (0.68) is substantial: legal challenges, ballot initiatives, medical non-compliance, and interstate travel networks all oppose the constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the state_enforcement_apparatus seat, the constraint appears as genuine coordination (protecting rights the Constitution demands). From the pregnant_persons seat, it operates as a snare (forced labor, bodily commandeering). From the prenatal_human_life seat (represented by guardians), it is a rope (the only mechanism securing its rights). The engine computes these divergent seat types from the power/exit/role structure — the authored mountain claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Prenatal human life is the structural beneficiary (d ≈ 0.0) — the constraint exists to subsidize its rights-claims. Pregnant persons are full targets (d ≈ 1.0) — they bear the extraction with constrained exit. State enforcement is agenda_setter with arbitrage exit (can modify or repeal the constraint). Medical providers are payers with constrained exit (license dependency, geographic practice). Functional capacity advocates are excluded — their framework is logically foreclosed by the constraint's premises. Observers are analytical with analytical exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (status of prenatal life) remains contested, not dead — but the constraint's enforcement has expanded far beyond the original coordination function (protecting fetal life from third-party violence) into direct commandeering of pregnant persons' bodies. The mandate has atrophied into extraction: the state now polices miscarriage management, IVF, contraception, and interstate travel under the same personhood logic. The constraint persists because the agenda_setter (state enforcement) extracts political legitimacy from maintaining it, while the payers (pregnant persons) are too fragmented and exit-constrained to dismantle it. This is not a piton — the enforcement is active and intensifying, not theatrical inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine natural law recognition (mountain) or a constructed legal regime that benefits identifiable agents (false summit)?',
    'Track whether the constraint''s enforcement machinery expands beyond protecting fetal life from third parties into commandeering pregnant persons'' bodies for state-defined ends. If enforcement targets pregnancy management rather than independent threats, the mountain claim fractures.',
    'If false summit, the engine reclassifies to tangled_rope (coordination + extraction) or snare (pure extraction) via the false_summit_mountain signature. The beneficiary declaration on a claimed mountain triggers FSM evaluation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Natural law vs. constructed constraint ambiguity — the core FSM question for this reading.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal bans, surveillance, criminal penalties) or internalized (moral belief that abortion is wrong, identity fusion with ''mother'' role)?',
    'Post-exit suppression trajectory: if pregnant persons who travel to permissive jurisdictions still experience guilt, surveillance, or legal threat, suppression has internalized components that persist after structural barrier removal.',
    'If internalized, effective suppression is higher than structural measures suggest — the constraint travels with the agent. This would increase χ for the pregnant_persons seat beyond what structural exit_options capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in interpersonal/legal constraint.').

omega_variable(
    fetal_rights_pregnant_autonomy_separability,
    'Can fetal rights protection be structurally separated from pregnant person subordination, or are they logically entailed by the developmental potentiality premise?',
    'Examine whether any legal regime has recognized fetal rights without restricting pregnant person autonomy (e.g., robust social support, voluntary gestational surrogacy frameworks, third-party fetal homicide laws without abortion bans).',
    'If separable, the constraint''s extraction is not necessary to its coordination function — the asymmetric extraction is a policy choice, not a logical entailment. If inseparable, the reading''s core premise structurally requires the subordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fetal_rights_pregnant_autonomy_separability, conceptual, 'Whether the coordination and extraction components are structurally separable.').

omega_variable(
    reading_foreclosure_boundary,
    'Does this reading''s core premise (personhood from conception) logically foreclose the functional_capacity_reading in any single legal framework, or do they merely coexist as competing political positions?',
    'Analyze whether a legal system could simultaneously hold that (a) all human organisms from conception are rights-bearers AND (b) personhood requires demonstrated cognitive capacity. If the predicates are contradictory, foreclosure holds.',
    'If forecloses, the cs_structure.forecloses relation is correct and the engine will compute axiom contradiction. If coexists_with, the readings occupy different institutional niches without logical collision.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_boundary, conceptual, 'Logical foreclosure vs. political coexistence between sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__developmental_potentiality_reading, 1973, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lpb_dpr_tr_t1973, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 1973, 0.18).
narrative_ontology:measurement(lpb_dpr_tr_t1980, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 1980, 0.22).
narrative_ontology:measurement(lpb_dpr_tr_t1992, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 1992, 0.28).
narrative_ontology:measurement(lpb_dpr_tr_t2000, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 2000, 0.31).
narrative_ontology:measurement(lpb_dpr_tr_t2010, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 2010, 0.35).
narrative_ontology:measurement(lpb_dpr_tr_t2018, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 2018, 0.38).
narrative_ontology:measurement(lpb_dpr_tr_t2022, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 2022, 0.4).
narrative_ontology:measurement(lpb_dpr_tr_t2024, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(lpb_dpr_be_t1973, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 1973, 0.35).
narrative_ontology:measurement(lpb_dpr_be_t1980, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 1980, 0.38).
narrative_ontology:measurement(lpb_dpr_be_t1992, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 1992, 0.42).
narrative_ontology:measurement(lpb_dpr_be_t2000, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 2000, 0.45).
narrative_ontology:measurement(lpb_dpr_be_t2010, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 2010, 0.52).
narrative_ontology:measurement(lpb_dpr_be_t2018, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 2018, 0.61).
narrative_ontology:measurement(lpb_dpr_be_t2022, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 2022, 0.73).
narrative_ontology:measurement(lpb_dpr_be_t2024, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(lpb_dpr_su_t1973, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 1973, 0.25).
narrative_ontology:measurement(lpb_dpr_su_t1980, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 1980, 0.35).
narrative_ontology:measurement(lpb_dpr_su_t1992, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 1992, 0.48).
narrative_ontology:measurement(lpb_dpr_su_t2000, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(lpb_dpr_su_t2010, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 2010, 0.62).
narrative_ontology:measurement(lpb_dpr_su_t2018, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 2018, 0.75).
narrative_ontology:measurement(lpb_dpr_su_t2022, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 2022, 0.86).
narrative_ontology:measurement(lpb_dpr_su_t2024, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 2024, 0.89).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_personhood_boundary__developmental_potentiality_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legal_personhood_boundary__developmental_potentiality_reading, 0.08).
narrative_ontology:affects_constraint(legal_personhood_boundary__developmental_potentiality_reading, legal_personhood_boundary__restrictive_anthropocentric_reading).
narrative_ontology:affects_constraint(legal_personhood_boundary__developmental_potentiality_reading, legal_personhood_boundary__functional_capacity_reading).
narrative_ontology:affects_constraint(legal_personhood_boundary__developmental_potentiality_reading, reproductive_autonomy_constraint).
narrative_ontology:affects_constraint(legal_personhood_boundary__developmental_potentiality_reading, medical_conscience_protection_constraint).
narrative_ontology:affects_constraint(legal_personhood_boundary__developmental_potentiality_reading, interstate_travel_surveillance_constraint).

% DUAL FORMULATION NOTE:
% The legal_personhood_boundary kernel decomposes into three readings with distinct ε values: developmental_potentiality (this story, ε≈0.78, claimed mountain, actual tangled_rope/snare), restrictive_anthropocentric (ε≈0.45, claimed rope, actual rope/tangled_rope), functional_capacity (ε≈0.22, claimed rope, actual rope). The developmental reading extracts most because it subordinates an existing rights-holder (pregnant person) to a potential one. The restrictive reading extracts less (only late-term). The functional reading extracts least (capacity threshold excludes early gestation). All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legal_personhood_boundary__developmental_potentiality_reading, moderate, 0.92).
constraint_indexing:directionality_override(legal_personhood_boundary__developmental_potentiality_reading, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
