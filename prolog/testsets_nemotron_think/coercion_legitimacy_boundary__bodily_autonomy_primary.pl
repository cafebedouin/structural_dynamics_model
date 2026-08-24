% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coercion_legitimacy_boundary__bodily_autonomy_primary, []).

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
 *   constraint_id: coercion_legitimacy_boundary__bodily_autonomy_primary
 *   human_readable: Categorical Bodily Autonomy Against Medical Coercion
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint story instantiates the bodily_autonomy_primary reading of
 *   the coercion_legitimacy_boundary kernel. The reading asserts that medical
 *   intervention without consent is categorically impermissible regardless of
 *   collective benefit — a Mountain claim grounded in natural law /
 *   constitutional bodily integrity. The structural data reveals a false
 *   summit: the constraint declares itself a natural law
 *   (emerges_naturally=true) but identifies identifiable beneficiaries
 *   (autonomy claimants) and victims (immunocompromised individuals who bear
 *   epidemiological externalities). The moderate base extractiveness (0.45)
 *   reflects the cost transferred to the immunocompromised; the rising
 *   theater_ratio (0.05→0.42) tracks the growing gap between categorical
 *   doctrine and the practical permission of mandates via emergency powers,
 *   narrow tailoring, and Jacobson-style deference. The declining then rising
 *   suppression_requirement reflects the mid-century triumph of informed
 *   consent norms followed by pandemic-era mandate enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.45).
domain_priors:suppression_score(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.2).
domain_priors:theater_ratio(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, extractiveness, 0.45).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__bodily_autonomy_primary, mountain).
narrative_ontology:human_readable(coercion_legitimacy_boundary__bodily_autonomy_primary, "Categorical Bodily Autonomy Against Medical Coercion").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__bodily_autonomy_primary, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:emerges_naturally(coercion_legitimacy_boundary__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__bodily_autonomy_primary, '7b44d540-3855-4c75-9bb3-ab9a690cc62e').
narrative_ontology:cs_kernel_codification('7b44d540-3855-4c75-9bb3-ab9a690cc62e', fixed_text).
narrative_ontology:cs_authority_grounding('7b44d540-3855-4c75-9bb3-ab9a690cc62e', lineage).
narrative_ontology:cs_interpretation_layer_present('7b44d540-3855-4c75-9bb3-ab9a690cc62e').
narrative_ontology:cs_reading_relation('7b44d540-3855-4c75-9bb3-ab9a690cc62e', coercion_legitimacy_boundary__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('7b44d540-3855-4c75-9bb3-ab9a690cc62e', coercion_legitimacy_boundary__proportionality_reading, forecloses).
narrative_ontology:cs_axiom('7b44d540-3855-4c75-9bb3-ab9a690cc62e', foundational, bodily_integrity_categorically_inviolable).
narrative_ontology:cs_axiom_status(bodily_integrity_categorically_inviolable, holdable).
narrative_ontology:cs_axiom_grounding('7b44d540-3855-4c75-9bb3-ab9a690cc62e', bodily_integrity_categorically_inviolable, deontological).
narrative_ontology:cs_axiom('7b44d540-3855-4c75-9bb3-ab9a690cc62e', foundational, collective_benefit_never_justifies_medical_coercion).
narrative_ontology:cs_axiom_status(collective_benefit_never_justifies_medical_coercion, holdable).
narrative_ontology:cs_axiom_grounding('7b44d540-3855-4c75-9bb3-ab9a690cc62e', collective_benefit_never_justifies_medical_coercion, deontological).
narrative_ontology:cs_reference_frame('7b44d540-3855-4c75-9bb3-ab9a690cc62e', constitutional_bodily_autonomy).
narrative_ontology:cs_drift_state('7b44d540-3855-4c75-9bb3-ab9a690cc62e', contemporary_pandemic_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7b44d540-3855-4c75-9bb3-ab9a690cc62e', '').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__bodily_autonomy_primary, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, bodily_autonomy_claimants).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, vaccine_refusers).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, medical_consent_advocates).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__bodily_autonomy_primary, immunocompromised_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__bodily_autonomy_primary, vaccine_refusers).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__bodily_autonomy_primary, bodily_integrity_as_natural_right).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__bodily_autonomy_primary, informed_consent_as_absolute_precondition).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__bodily_autonomy_primary, state_cannot_override_individual_medical_choice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who refuse specific medical interventions (vaccines, blood transfusions, psychiatric treatment) on grounds of bodily integrity, religious conviction, or personal conscience. They benefit from the categorical prohibition because it legally shields them from state compulsion. Their exit option is mobility — they can relocate to jurisdictions with stronger protections — but most remain and rely on the legal right.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, bodily_autonomy_claimants, beneficiary,
    moderate, biographical, mobile, national).

% Organized groups and individuals who refuse vaccination for themselves or their children. They are primary beneficiaries of the categorical autonomy rule. They also bear costs (payer) when their refusal triggers school exclusion, employment barriers, or social stigma — costs imposed by the *absence* of mandates, not by the autonomy constraint itself. Their exit is constrained by employment, schooling, and community ties.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, vaccine_refusers, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(coercion_legitimacy_boundary__bodily_autonomy_primary, vaccine_refusers, payer).

% Civil liberties organizations, bioethics centers, and legal advocacy groups that litigate and lobby for absolute informed consent. They benefit professionally and ideologically from the categorical framing. They have arbitrage-grade exit: they operate across jurisdictions and forums (courts, legislatures, international bodies).
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, medical_consent_advocates, beneficiary,
    organized, generational, arbitrage, global).

% People with compromised immune systems (transplant recipients, chemotherapy patients, primary immunodeficiencies) who cannot be vaccinated or for whom vaccines are less effective. They bear the epidemiological cost of others' non-vaccination: higher exposure risk, restricted participation in public life, dependence on herd immunity that the categorical rule undermines. They are trapped — they cannot exit their medical vulnerability, and relocation does not eliminate the structural exposure.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, immunocompromised_individuals, payer,
    powerless, biographical, trapped, national).

% High courts that adjudicate bodily integrity claims against state mandates. They set the agenda by defining the scope of the right, the standard of review, and the exceptions (if any). They are the authoritative interpreters of the constitutional kernel. Their exit is analytical — they interpret, they do not bear the policy consequences directly.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, constitutional_courts, agenda_setter,
    institutional, generational, analytical, national).

% Health departments, CDC-equivalents, and ministries of health that would impose mandates during outbreaks. Under the categorical reading, they are structurally excluded from the coercive toolkit — they cannot compel, only persuade. They are constrained by law and political accountability; they cannot simply ignore the courts, but they push for legislative overrides and emergency powers.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, public_health_authorities, excluded,
    institutional, biographical, constrained, national).

% Parliaments and congresses that would pass mandate legislation. They are excluded from exercising what the public_health_primary reading treats as their core police power. They respond by crafting narrow exemptions, incentive structures, or constitutional amendments — constrained by judicial review and electoral politics.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, legislative_bodies, excluded,
    institutional, biographical, constrained, national).

% Academic commentators who analyze the coherence, history, and consequences of the categorical autonomy claim. They neither collect rents nor pay costs from the constraint's operation. Their seat is analytical: they map the conceptual architecture and track drift between doctrine and practice.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, bioethics_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The categorical rule coordinates a society-wide commitment: no person shall be used as a mere means for collective health ends. It solves the coordination problem of trust — individuals engage with medicine voluntarily because they know the state cannot override their consent, preserving the therapeutic relationship and public legitimacy of health systems.
% TRANSFER_FUNCTION: The arrangement transfers epidemiological risk from the non-compliant (who refuse intervention) to the vulnerable (who cannot be protected by their own compliance). It transfers coercive power from the state to the individual. It transfers the burden of outbreak control from mandates to persuasion, incentives, and voluntary uptake.
% ABSENT_VOICES: Future generations who would inherit the precedential framework; children of vaccine refusers who have no voice in their parents' medical decisions; global health equity advocates who see categorical autonomy in wealthy nations as undermining collective pandemic response. They are absent because the constitutional frame centers the current rights-holder, not the future or dependent subject.
% DISAPPEARANCE_RATIONALE: If the categorical prohibition vanished overnight, states would immediately impose mandates for high-consequence diseases (measles, polio, pandemic pathogens). The immunocompromised would gain herd-immunity protection; autonomy claimants would lose legal shield; public health authorities would regain coercive tools; the therapeutic relationship would shift from voluntary to state-mediated. The world of medical decision-making would fundamentally reorganize.
% FOUNDING_PROBLEM: The founding problem is the historical abuse of state medical power: eugenics programs, forced sterilizations, non-consensual experimentation (Tuskegee, Nazi medicine, Cold War radiation tests), and compulsory vaccination campaigns that targeted marginalized groups. The categorical rule was built to make such abuses structurally impossible by removing the state's coercive authority over the body entirely.
% FOUNDING_PROBLEM_CORROBORATION: The historical abuses are documented by the Nuremberg Code (1947), the Belmont Report (1979), and the UNESCO Universal Declaration on Bioethics and Human Rights (2005) — sources outside the beneficiary set. However, public health historians (e.g., Gostin, Bayer, Fairchild) argue the founding problem is substantially addressed by modern procedural safeguards (IRBs, informed consent regulations, judicial review), and that the categorical rule now solves a problem that no longer exists in its original form, while creating new ones. No consensus exists across the beneficiary/excluded divide.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__bodily_autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__bodily_autonomy_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(coercion_legitimacy_boundary__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coercion_legitimacy_boundary__bodily_autonomy_primary_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, ExtMetricName, E),
    domain_priors:suppression_score(coercion_legitimacy_boundary__bodily_autonomy_primary, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(coercion_legitimacy_boundary__bodily_autonomy_primary),
    narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(coercion_legitimacy_boundary__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate because the categorical rule extracts a predictable epidemiological cost from the immunocompromised (they bear higher infection risk, restricted mobility, dependency on others' voluntary choices). Suppression is low because the constraint is a prohibition on state action — it suppresses mandates, not alternatives for the autonomy-holder. Theater_ratio is significant because courts and legislatures routinely invoke categorical language while carving out exceptions (Jacobson v. Massachusetts, COVID emergency orders, school-entry mandates with non-medical exemptions). Accessibility_collapse is high because the categorical frame forecloses proportionality balancing — once the right is absolute, the alternative (mandates) is structurally inaccessible. Resistance is low because few openly oppose bodily autonomy as a principle; resistance appears in the gap between principle and application.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (autonomy claimants) experience the constraint as a genuine Mountain — a natural law protecting them from state violation. The payer seat (immunocompromised) experiences it as a Snare — a rule that extracts their safety for others' autonomy, enforced by courts that deny their vulnerability constitutional weight. The agenda_setter seat (courts) experiences it as a Rope — a coordination mechanism that stabilizes the doctor-patient relationship and public trust. The engine computes these divergences from the structural data; the authored claim (mountain) does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Bodily autonomy claimants and vaccine refusers are beneficiaries (d ≈ 0.15) — the constraint subsidizes their position by legally forbidding coercion. Immunocompromised individuals are payers (d ≈ 0.85) — they bear the extracted epidemiological risk with trapped exit. Constitutional courts are agenda_setters (d ≈ 0.3) — they administer the constraint and gain institutional authority from being its guardian. Public health authorities and legislatures are excluded (d ≈ 0.7) — they are thwarted by the constraint, bearing opportunity costs of forgone mandates. Bioethics scholars are observers (d = 0.5) — analytical seat, symmetric costs/benefits.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (historical medical abuses) is substantially addressed by modern procedural safeguards, yet the categorical rule persists and expands. This is mandatrophy: the mandate (prevent abuse) has atrophied while the constraint (categorical autonomy) remains. The rule now extracts from the immunocompromised without preventing the abuses it was built against — those abuses are already blocked by IRBs, consent regulations, and democratic accountability. The persistence is not inertial (piton) but active: beneficiaries (advocacy groups, courts) actively defend the categorical frame because it secures their institutional role and ideological commitment. The theater_ratio rise confirms performative maintenance of the categorical claim while practice drifts toward proportionality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_right,
    'Is the categorical bodily autonomy right a genuine natural law (mountain) or a constructed legal doctrine that benefits identifiable agents (false summit)?',
    'Cross-jurisdictional comparison: if the right emerges identically in unconnected legal traditions without common origin, natural law claim gains support. If it tracks specific constitutional lineages and advocacy campaigns, constructed claim gains support.',
    'If natural law, the constraint is a genuine Mountain with ε≈0 and the immunocompromised cost is a tragic externality, not extraction. If constructed, FSM reclassifies to tangled_rope — the constraint coordinates trust but extracts from the vulnerable, maintained by active judicial enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_right, conceptual, 'FSM-defining ambiguity: natural law vs. constructed constraint with beneficiaries').

omega_variable(
    immunocompromised_as_victim_or_externality,
    'Are immunocompromised individuals victims of the constraint''s extraction, or are they bearing a background epidemiological risk that the constraint merely fails to mitigate?',
    'Counterfactual modeling: compare infection rates for immunocompromised under categorical autonomy vs. mandate regimes, controlling for vaccine efficacy, coverage, and variant dynamics. If the delta is attributable to the constraint''s prohibition, they are victims of extraction.',
    'If victims, the constraint has asymmetric extraction (beneficiaries + victims) — tangled_rope or snare structure. If externality, the constraint is a pure coordination Mountain with tragic but non-extractive consequences.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(immunocompromised_as_victim_or_externality, empirical, 'Whether the immunocompromised cost structure constitutes extraction under the ε-invariance principle').

omega_variable(
    kernel_reading_foreclosure,
    'Does the bodily_autonomy_primary reading logically foreclose the proportionality_reading within a single commitment framework, or do they coexist as competing but compatible positions?',
    'Analyze constitutional jurisprudence: do any courts hold both ''bodily integrity is absolute'' and ''mandates are permissible if proportional''? If no court can hold both without contradiction, foreclosure holds. If courts switch frameworks case-by-case, coexistence holds.',
    'If forecloses, the kernel has a structural fault line — readings are mutually exclusive frameworks. If coexists_with, the kernel tolerates pluralism and the engine''s cs_reading_conflict detection will not fire.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Structural relationship between this reading and proportionality_reading').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.20) structural (courts blocking mandates) or internalized (public health authorities self-censoring mandate proposals due to constitutional culture)?',
    'Track mandate proposal rates in legislative bodies over time: if proposals decline while judicial doctrine stays constant, internalized suppression is growing. If proposals persist but are struck down, suppression is structural.',
    'If internalized, effective suppression is higher than measured — the constraint''s chilling effect extends beyond judicial rulings into legislative agenda-setting.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the constitutional frame').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__bodily_autonomy_primary, 1905, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clb_bap_tr_t1905, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 1905, 0.05).
narrative_ontology:measurement(clb_bap_tr_t1950, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 1950, 0.12).
narrative_ontology:measurement(clb_bap_tr_t1975, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 1975, 0.25).
narrative_ontology:measurement(clb_bap_tr_t2000, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 2000, 0.33).
narrative_ontology:measurement(clb_bap_tr_t2010, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 2010, 0.37).
narrative_ontology:measurement(clb_bap_tr_t2020, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 2020, 0.4).
narrative_ontology:measurement(clb_bap_tr_t2025, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(clb_bap_be_t1905, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 1905, 0.15).
narrative_ontology:measurement(clb_bap_be_t1950, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 1950, 0.2).
narrative_ontology:measurement(clb_bap_be_t1975, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 1975, 0.28).
narrative_ontology:measurement(clb_bap_be_t2000, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 2000, 0.35).
narrative_ontology:measurement(clb_bap_be_t2010, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 2010, 0.38).
narrative_ontology:measurement(clb_bap_be_t2020, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 2020, 0.43).
narrative_ontology:measurement(clb_bap_be_t2025, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 2025, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(clb_bap_su_t1905, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 1905, 0.65).
narrative_ontology:measurement(clb_bap_su_t1950, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 1950, 0.4).
narrative_ontology:measurement(clb_bap_su_t1975, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 1975, 0.3).
narrative_ontology:measurement(clb_bap_su_t2000, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 2000, 0.22).
narrative_ontology:measurement(clb_bap_su_t2010, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 2010, 0.18).
narrative_ontology:measurement(clb_bap_su_t2020, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 2020, 0.25).
narrative_ontology:measurement(clb_bap_su_t2025, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 2025, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__bodily_autonomy_primary, identity_coordination).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__bodily_autonomy_primary, coercion_legitimacy_boundary__public_health_primary).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__bodily_autonomy_primary, coercion_legitimacy_boundary__proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the coercion_legitimacy_boundary kernel into three structurally distinct readings. The bodily_autonomy_primary reading claims Mountain status (categorical natural law) but exhibits moderate extractiveness from immunocompromised externalities — FSM candidate. The public_health_primary reading claims Scaffold/Tangled Rope (transitional coordination with sunset via herd immunity). The proportionality_reading claims Rope (coordination via calibrated balancing). All three share the kernel but instantiate different constraints with different ε, beneficiaries, and victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(coercion_legitimacy_boundary__bodily_autonomy_primary, institutional, 0.3).
constraint_indexing:directionality_override(coercion_legitimacy_boundary__bodily_autonomy_primary, powerless, 0.85).
constraint_indexing:directionality_override(coercion_legitimacy_boundary__bodily_autonomy_primary, organized, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
