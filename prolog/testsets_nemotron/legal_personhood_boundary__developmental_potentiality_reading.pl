% ============================================================================
% CONSTRAINT STORY: legal_personhood_boundary__developmental_potentiality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-24
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
 *   human_readable: Developmental Potentiality Reading of Legal Personhood Boundary
 *   domain: legal_philosophy/constitutional_law/rights_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the developmental_potentiality_reading
 *   of the contested legal_personhood_boundary kernel. The reading asserts
 *   that personhood and full rights-bearing status attach at conception,
 *   grounding this in the continuous developmental trajectory from zygote to
 *   adult human. The kernel_id is legal_personhood_boundary; sibling readings
 *   are restrictive_anthropocentric_reading (personhood limited to born
 *   humans with cognitive capacity) and functional_capacity_reading
 *   (personhood follows demonstrable cognitive capacity regardless of
 *   species). Under this reading, the fetus enters the victim set from
 *   conception; the pregnant person's bodily autonomy is structurally
 *   subordinated to fetal rights claims; and the state acquires affirmative
 *   enforcement authority over pregnancy outcomes including criminalization
 *   of abortion, mandatory reporting of pregnancy loss, and potential
 *   regulation of IVF and contraception. The arrangement is presented as
 *   protection of the most vulnerable; the metrics describe a constraint that
 *   extracts reproductive autonomy, bodily integrity, and life-course freedom
 *   from pregnant persons through active state enforcement, with
 *   theater_ratio low (the protection function is genuinely believed by
 *   enforcers, not performative).
 *
 * KEY AGENTS:
 *   - pregnant_persons: Primary target (powerless/constrained) — bears extraction of bodily autonomy, life-course freedom, health risk
 *   - fetal_life_claimants: Primary beneficiary (powerless/trapped) — conceptual beneficiaries who cannot self-advocate; the constraint's coordination story centers them
 *   - state_enforcement_authority: Agenda setter (institutional/biographical) — administers and enforces the constraint, gains regulatory authority over reproduction
 *   - pro_natalist_institutions: Beneficiary (organized/generational) — religious, political, and social institutions that gain moral authority and policy influence from the arrangement
 *   - reproductive_autonomy_holders: Victim (moderate/constrained) — broader class including those not currently pregnant whose reproductive futures are constrained
 *   - functional_capacity_advocates: Excluded (powerful/analytical) — would argue personhood requires cognitive capacities absent in early gestation
 *   - restrictive_anthropocentric_advocates: Excluded (powerful/analytical) — would limit personhood to born humans, rejecting both potentiality and cross-species capacity criteria
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_boundary__developmental_potentiality_reading, 0.92).
domain_priors:suppression_score(legal_personhood_boundary__developmental_potentiality_reading, 0.88).
domain_priors:theater_ratio(legal_personhood_boundary__developmental_potentiality_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__developmental_potentiality_reading, snare).
narrative_ontology:human_readable(legal_personhood_boundary__developmental_potentiality_reading, "Developmental Potentiality Reading of Legal Personhood Boundary").
narrative_ontology:topic_domain(legal_personhood_boundary__developmental_potentiality_reading, "legal_philosophy/constitutional_law/rights_theory").

domain_priors:requires_active_enforcement(legal_personhood_boundary__developmental_potentiality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__developmental_potentiality_reading, '954a65ea-0b0e-4435-a16d-889549ba8579').
narrative_ontology:cs_kernel_codification('954a65ea-0b0e-4435-a16d-889549ba8579', fixed_text).
narrative_ontology:cs_authority_grounding('954a65ea-0b0e-4435-a16d-889549ba8579', lineage).
narrative_ontology:cs_interpretation_layer_present('954a65ea-0b0e-4435-a16d-889549ba8579').
narrative_ontology:cs_reading_relation('954a65ea-0b0e-4435-a16d-889549ba8579', legal_personhood_boundary__restrictive_anthropocentric_reading, forecloses).
narrative_ontology:cs_reading_relation('954a65ea-0b0e-4435-a16d-889549ba8579', legal_personhood_boundary__functional_capacity_reading, coexists_with).
narrative_ontology:cs_axiom('954a65ea-0b0e-4435-a16d-889549ba8579', foundational, personhood_at_conception).
narrative_ontology:cs_axiom_status(personhood_at_conception, holdable).
narrative_ontology:cs_axiom_grounding('954a65ea-0b0e-4435-a16d-889549ba8579', personhood_at_conception, deontological).
narrative_ontology:cs_axiom('954a65ea-0b0e-4435-a16d-889549ba8579', foundational, potentiality_grounds_full_moral_status).
narrative_ontology:cs_axiom_status(potentiality_grounds_full_moral_status, holdable).
narrative_ontology:cs_axiom_grounding('954a65ea-0b0e-4435-a16d-889549ba8579', potentiality_grounds_full_moral_status, deontological).
narrative_ontology:cs_axiom('954a65ea-0b0e-4435-a16d-889549ba8579', secondary, pregnant_person_obligation_to_sustain).
narrative_ontology:cs_axiom_status(pregnant_person_obligation_to_sustain, holdable).
narrative_ontology:cs_axiom_grounding('954a65ea-0b0e-4435-a16d-889549ba8579', pregnant_person_obligation_to_sustain, instrumental).
narrative_ontology:cs_reference_frame('954a65ea-0b0e-4435-a16d-889549ba8579', classical_natural_law_personhood).
narrative_ontology:cs_drift_state('954a65ea-0b0e-4435-a16d-889549ba8579', post_dobbs_enforcement_era, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('954a65ea-0b0e-4435-a16d-889549ba8579', '').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__developmental_potentiality_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__developmental_potentiality_reading, fetal_life_claimants).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__developmental_potentiality_reading, state_enforcement_authority).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__developmental_potentiality_reading, pro_natalist_institutions).
narrative_ontology:constraint_victim(legal_personhood_boundary__developmental_potentiality_reading, pregnant_persons).
narrative_ontology:constraint_victim(legal_personhood_boundary__developmental_potentiality_reading, fetal_viability_threshold_gestators).
narrative_ontology:constraint_victim(legal_personhood_boundary__developmental_potentiality_reading, reproductive_autonomy_holders).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__developmental_potentiality_reading, human_life_begins_at_conception).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__developmental_potentiality_reading, potentiality_grounds_moral_status).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__developmental_potentiality_reading, state_duty_to_protect_prenatal_life).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the full physical, economic, and autonomy costs of the constraint. Forced to continue pregnancies against their will; face health risks including mortality; lose educational, career, and life-course opportunities; subject to surveillance and potential criminalization for pregnancy outcomes. Exit options exist (interstate travel, medication abortion, self-management) but are legally risky, economically burdensome, and increasingly restricted.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, pregnant_persons, payer,
    powerless, biographical, constrained, national).

% Conceptual beneficiaries — zygotes, embryos, and fetuses claimed as rights-bearers from conception. Cannot self-advocate, consent, or experience benefit. Their 'benefit' is continued existence, claimed by advocates. The constraint's entire coordination story centers them, but they have no voice and no exit — they are the object of the constraint, not a participant.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, fetal_life_claimants, beneficiary,
    powerless, generational, trapped, universal).

% Administers and enforces the personhood boundary through criminal law, health regulation, and administrative rulemaking. Gains expansive regulatory authority over reproduction: abortion bans, IVF restrictions, contraception limits, mandatory reporting, fetal personhood statutes. Can shift enforcement intensity across administrations. Exit is arbitrage-grade — the state can reinterpret, narrow, or expand the constraint through legislation, regulation, and judicial appointment.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, state_enforcement_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Religious denominations, political movements, crisis pregnancy center networks, and legal advocacy organizations that gain moral authority, policy influence, donor base, and institutional power from the constraint. They authored the intellectual framework, litigated the cases, and built the enforcement infrastructure. Exit is mobile — they can shift focus to other issues if the political winds change, but their identity is fused with this constraint.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, pro_natalist_institutions, beneficiary,
    organized, generational, mobile, global).

% Broader class including people who may become pregnant, partners, families, and communities whose reproductive futures are constrained. Face reduced access to reproductive healthcare, contraception, IVF, and miscarriage management. Economic burden falls disproportionately on low-income, rural, and minority populations. Exit is constrained — geographic mobility helps but federal action could nationalize restrictions.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, reproductive_autonomy_holders, payer,
    moderate, biographical, constrained, national).

% Philosophers, bioethicists, neuroscientists, and legal scholars who argue personhood requires demonstrable cognitive capacities (sentience, self-awareness, rationality). Their framework would place personhood at viability (22-24 weeks) or birth, not conception. They are excluded from the enforcement framework — their reading is not recognized in potentiality-based jurisdictions. They operate in academic and international human rights fora.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, functional_capacity_advocates, excluded,
    powerful, civilizational, analytical, universal).

% Legal theorists and jurists who limit personhood to born humans with cognitive capacity, rejecting both potentiality arguments and cross-species capacity criteria. Their reading would permit early abortion but restrict late-term abortion and infanticide. They are excluded from both the potentiality-based enforcement framework and the capacity-based animal rights framework. They occupy a narrowing middle ground in constitutional law.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, restrictive_anthropocentric_advocates, excluded,
    powerful, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Protects a class of human life (prenatal) that cannot self-advocate from being terminated by the autonomous choices of the pregnant person. Solves the coordination problem of who speaks for the voiceless by assigning the state as guardian and the pregnant person as obligated sustainer.
% TRANSFER_FUNCTION: Moves bodily autonomy, life-course freedom, health risk, and economic opportunity from pregnant persons to the fetal life claim and state enforcement authority. The pregnant person's body, labor, and future are conscripted to sustain the fetal life trajectory; the state gains regulatory authority over reproduction; pro-natalist institutions gain moral and political capital.
% ABSENT_VOICES: Pregnant persons who would seek abortion but cannot access it — geographically trapped, economically immobilized, legally threatened, or medically silenced. Also absent: the fetal life claimants themselves (who cannot speak); functional_capacity and restrictive_anthropocentric advocates (excluded from the enforcement framework); medical professionals who would provide care but face criminal penalties. They are in restrictive jurisdictions, in poverty, in fear of prosecution, or in professional disciplines that punish dissent.
% DISAPPEARANCE_RATIONALE: If this constraint vanished overnight, abortion access would be restored in all jurisdictions, IVF and contraception regulation would revert to medical standards, pregnant persons would regain bodily autonomy and life-course freedom, state enforcement authority over reproduction would contract dramatically, and pro-natalist institutions would lose their central policy anchor. The reproductive governance regime built since 2022 would collapse; a new equilibrium would form around functional capacity or restrictive anthropocentric readings.
% FOUNDING_PROBLEM: The founding problem was the vulnerability of prenatal human life to being treated as disposable property or medical waste — the absence of legal recognition for the continuity of human development from conception. The developmental potentiality reading was built to solve this by extending personhood and rights-bearing status to the earliest stage of human life.
% FOUNDING_PROBLEM_CORROBORATION: The pro-natalist institutions and state enforcement authority attest the problem remains live and the solution is necessary. Functional capacity advocates, reproductive rights organizations, medical associations, and international human rights bodies attest the founding problem has been captured: the vulnerability of prenatal life is real but the solution (full personhood from conception) creates a greater rights violation by subsuming the pregnant person. Corroboration from outside the beneficiary set (medical associations, human rights bodies) supports the captured reading.
narrative_ontology:disappearance_verdict(legal_personhood_boundary__developmental_potentiality_reading, world_rearranges).
narrative_ontology:founding_problem_status(legal_personhood_boundary__developmental_potentiality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legal_personhood_boundary__developmental_potentiality_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(legal_personhood_boundary__developmental_potentiality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legal_personhood_boundary__developmental_potentiality_reading, 0.92, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.92) is near-maximum because the constraint transfers the entire reproductive life-course autonomy of pregnant persons to fetal claims and state enforcement, with no reciprocity — the fetus cannot consent, reciprocate, or bear costs. Suppression (0.88) is structural: criminal penalties for abortion providers, mandatory waiting periods, geographic clinic closures, interstate travel restrictions, surveillance of pregnancy outcomes, and potential criminalization of pregnant persons themselves. Theater ratio (0.12) is low because enforcers genuinely believe they are protecting rights-bearing human life; the coordination story is not a cover. Accessibility collapse (0.78) is high but not total: medication abortion, interstate travel, and self-managed abortion create partial exits, though these are increasingly targeted. Resistance (0.65) is substantial: legal challenges, ballot initiatives, medical refusal networks, and cross-border provision persist despite severe suppression. The claimed_type is snare: the coordination function (protecting non-self-advocating life) is real to its believers, but the cost asymmetry is extreme and enforced, with identifiable victims (pregnant persons) and no mechanism for them to exit the extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the state_enforcement_authority seat, this is genuine coordination: the state protects a class that cannot protect itself. From the pregnant_person seat, it is extraction: their body is conscripted for a claim they may reject, with criminal penalties for resistance. From the fetal_life_claimant seat (conceptual), the constraint is the only thing preventing their destruction — but they have no voice. The engine computes per-seat types from structural data; this divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Pregnant persons are full targets (d → 1.0): they bear the physical, economic, and autonomy costs with constrained exit (geographic, legal, economic barriers). Fetal life claimants are conceptual beneficiaries (d → 0.0) but cannot experience benefit — the benefit is claimed on their behalf by advocates. State enforcement authority sits near d=0.15 (beneficiary of expanded regulatory power) but also bears enforcement costs. Pro-natalist institutions are beneficiaries (d ≈ 0.1) gaining moral authority. Functional_capacity and restrictive_anthropocentric advocates are excluded (d not computed) — they would challenge the constraint's premise but are structurally locked out of the enforcement framework. The directionality derivation chain: beneficiary/victim declarations + power levels + exit options → d. No overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting vulnerable human life from being treated as disposable) remains live but has been captured by a reading that subordinates the autonomy of another rights-bearing class (pregnant persons) to a potentiality claim that did not exist in 1973. The arrangement now extracts from pregnant persons to sustain a moral and institutional order that benefits state enforcement authority and pro-natalist institutions. Mandatrophy is not resolved — the constraint's mandate has expanded beyond its founding function into comprehensive reproductive governance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of a contested kernel rather than a freestanding constraint?',
    'Committee-level analysis identifying the kernel_id (legal_personhood_boundary) and confirming sibling readings (restrictive_anthropocentric_reading, functional_capacity_reading) instantiate distinct constraints with different ε values and victim sets.',
    'If confirmed, this reading must be analyzed as a kernel reading with its own ε, not as a universal claim about personhood. The ε=0.92 applies to THIS reading''s instantiation, not to the kernel label itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Kernel vs. freestanding constraint identity').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.88) structural (legal bans, criminal penalties, geographic barriers) or internalized (moral internalization, identity fusion with fetal-protective role, epistemic closure from exclusionary communities)?',
    'Post-dobbs trajectory analysis: if suppression persists or intensifies after structural barriers are partially removed (e.g., medication abortion access via telemedicine, interstate travel), reclassify as partially internalized. Track pregnant persons who exit restrictive jurisdictions but report continued psychological surveillance.',
    'If substantially internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after geographic exit. This would amplify χ for identity_locked pregnant persons beyond what structural d-derivation captures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in post-Dobbs enforcement regime').

omega_variable(
    extraction_vs_protection_boundary,
    'Does the high extractiveness (0.92) reflect genuine extraction from pregnant persons, or the coordination cost of protecting a vulnerable class (fetal life claimants) that cannot self-advocate?',
    'Compare resource flows: measure state expenditure on prenatal protection vs. opportunity costs imposed on pregnant persons (lost wages, health risks, forced parenting). If protection expenditure << imposed costs, the gap is extraction. If protection expenditure ≈ imposed costs, the coordination function may be genuine.',
    'If protection expenditure is a small fraction of imposed costs, the snare classification is reinforced. If they are comparable, a tangled_rope classification becomes plausible — genuine coordination (protecting non-self-advocating life) with asymmetric cost allocation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_vs_protection_boundary, empirical, 'Whether the arrangement''s cost asymmetry is extractive overhead or necessary coordination cost for non-self-advocating beneficiaries').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__developmental_potentiality_reading, 1973, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lpbdpr_tr_t1973, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 1973, 0.08).
narrative_ontology:measurement(lpbdpr_tr_t1992, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 1992, 0.1).
narrative_ontology:measurement(lpbdpr_tr_t2010, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(lpbdpr_tr_t2018, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 2018, 0.11).
narrative_ontology:measurement(lpbdpr_tr_t2022, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 2022, 0.12).
narrative_ontology:measurement(lpbdpr_tr_t2024, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 2024, 0.12).

% Extraction over time
narrative_ontology:measurement(lpbdpr_be_t1973, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 1973, 0.35).
narrative_ontology:measurement(lpbdpr_be_t1992, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 1992, 0.42).
narrative_ontology:measurement(lpbdpr_be_t2010, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(lpbdpr_be_t2018, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 2018, 0.71).
narrative_ontology:measurement(lpbdpr_be_t2022, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 2022, 0.89).
narrative_ontology:measurement(lpbdpr_be_t2024, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 2024, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(lpbdpr_su_t1973, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 1973, 0.45).
narrative_ontology:measurement(lpbdpr_su_t1992, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 1992, 0.55).
narrative_ontology:measurement(lpbdpr_su_t2010, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(lpbdpr_su_t2018, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 2018, 0.78).
narrative_ontology:measurement(lpbdpr_su_t2022, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 2022, 0.85).
narrative_ontology:measurement(lpbdpr_su_t2024, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 2024, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_personhood_boundary__developmental_potentiality_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legal_personhood_boundary__developmental_potentiality_reading, 0.08).
narrative_ontology:affects_constraint(legal_personhood_boundary__developmental_potentiality_reading, abortion_criminalization_enforcement).
narrative_ontology:affects_constraint(legal_personhood_boundary__developmental_potentiality_reading, ivf_embryo_disposal_regulation).
narrative_ontology:affects_constraint(legal_personhood_boundary__developmental_potentiality_reading, contraception_access_restriction).
narrative_ontology:affects_constraint(legal_personhood_boundary__developmental_potentiality_reading, maternal_fetal_conflict_law).
narrative_ontology:affects_constraint(legal_personhood_boundary__developmental_potentiality_reading, pregnancy_surveillance_infrastructure).

% DUAL FORMULATION NOTE:
% Kernel decomposition: legal_personhood_boundary kernel splits into three readings with distinct ε and victim sets. This reading (developmental_potentiality) has ε=0.92, fetal victim set from conception. restrictive_anthropocentric_reading has lower ε (~0.3) as it only affects late-term abortion access. functional_capacity_reading has ε≈0.1 as it primarily affects animal rights and AI personhood debates, not human reproductive governance. All three linked via network.affects_constraints in their respective files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
