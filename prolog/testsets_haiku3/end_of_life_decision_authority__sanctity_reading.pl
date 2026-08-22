% ============================================================================
% CONSTRAINT STORY: end_of_life_decision_authority__sanctity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_decision_authority__sanctity_reading, []).

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
 *   constraint_id: end_of_life_decision_authority__sanctity_reading
 *   human_readable: Sanctity of Life Reading: End-of-Life Decision Authority
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   The sanctity-of-life reading grounds end-of-life decision authority in
 *   the claim that human life possesses intrinsic value independent of
 *   individual will or circumstance. Under this reading, intentional
 *   life-ending violates that value and is therefore impermissible, even when
 *   a competent individual requests it. The constraint operates by: (1)
 *   reserving decision authority to institutional gatekeepers (physicians,
 *   ethics committees, legal authorities) rather than placing it with the
 *   patient; (2) reconstructing the physician's role as
 *   life-preservation-only, foreclosing participation in death-hastening; (3)
 *   externalizing the cost of terminal suffering onto patients and families
 *   (palliative care is offered but active euthanasia is refused); (4)
 *   protecting vulnerable individuals from coercion by making euthanasia
 *   legally unavailable, but at the cost of constraining autonomous choice
 *   for competent, non-vulnerable patients. This reading competes with the
 *   autonomy reading (individuals possess sovereign authority) and the
 *   vulnerability-protection reading (authority should be distributed to
 *   prevent both denial and coercion). The sanctity reading is CLAIMED as a
 *   tangled rope because it serves a genuine coordination function
 *   (protecting vulnerable individuals from coercion) while also extracting
 *   from terminal suffering patients and from patients who would choose death
 *   but are foreclosed by the reading's operation. The constraint's
 *   persistence depends on active enforcement: legal prohibition of assisted
 *   dying, professional licensing rules that discipline physicians who
 *   participate, institutional policies that elevate palliative care as the
 *   alternative, and messaging that frames the prohibition as protection
 *   rather than restriction.
 *
 * KEY AGENTS:
 *   - institutional_medical_gatekeepers (physicians, hospital ethics committees, licensing boards) — agenda-setters, enforce the life-preservation norm, control what information patients receive about end-of-life options
 *   - sanctity_doctrine_adherents (religious institutions, pro-life advocacy organizations, certain philosophical traditions) — beneficiaries, vindicate the intrinsic-value claim through institutional policy alignment
 *   - pressured_vulnerable_patients (economically precarious, relationally isolated, institutionally powerless patients in healthcare systems with bed rationing or high treatment costs) — victims, structurally at risk if euthanasia becomes available because cost/pressure logic could override autonomous choice
 *   - terminal_suffering_patients (competent, non-vulnerable individuals with irreversible terminal diagnoses who experience unbearable suffering and would choose assisted death) — victims, structurally constrained by the reading's foreclosure of death as an option, bear the cost of life-preservation norm regardless of their preference
 *   - analytical_observer (medical ethicists, policy researchers, human rights advocates) — observers, measure whether the constraint's protective function justifies its extraction from terminal patients
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_decision_authority__sanctity_reading, 0.68).
domain_priors:suppression_score(end_of_life_decision_authority__sanctity_reading, 0.71).
domain_priors:theater_ratio(end_of_life_decision_authority__sanctity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_decision_authority__sanctity_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_decision_authority__sanctity_reading, "Sanctity of Life Reading: End-of-Life Decision Authority").
narrative_ontology:topic_domain(end_of_life_decision_authority__sanctity_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_decision_authority__sanctity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_decision_authority__sanctity_reading, '7e37c29e-c6fe-406a-b220-0040d012197d').
narrative_ontology:cs_kernel_codification('7e37c29e-c6fe-406a-b220-0040d012197d', fixed_text).
narrative_ontology:cs_authority_grounding('7e37c29e-c6fe-406a-b220-0040d012197d', lineage).
narrative_ontology:cs_interpretation_layer_present('7e37c29e-c6fe-406a-b220-0040d012197d').
narrative_ontology:cs_reading_relation('7e37c29e-c6fe-406a-b220-0040d012197d', end_of_life_decision_authority__autonomy_reading, coexists_with).
narrative_ontology:cs_reading_relation('7e37c29e-c6fe-406a-b220-0040d012197d', end_of_life_decision_authority__vulnerability_protection_reading, coexists_with).
narrative_ontology:cs_axiom('7e37c29e-c6fe-406a-b220-0040d012197d', foundational, human_life_intrinsic_sacred_value).
narrative_ontology:cs_axiom_status(human_life_intrinsic_sacred_value, holdable).
narrative_ontology:cs_axiom_grounding('7e37c29e-c6fe-406a-b220-0040d012197d', human_life_intrinsic_sacred_value, deontological).
narrative_ontology:cs_axiom('7e37c29e-c6fe-406a-b220-0040d012197d', foundational, intentional_death_violates_sacred_value).
narrative_ontology:cs_axiom_status(intentional_death_violates_sacred_value, holdable).
narrative_ontology:cs_axiom_grounding('7e37c29e-c6fe-406a-b220-0040d012197d', intentional_death_violates_sacred_value, deontological).
narrative_ontology:cs_reference_frame('7e37c29e-c6fe-406a-b220-0040d012197d', life_preservation_imperative).
narrative_ontology:cs_drift_state('7e37c29e-c6fe-406a-b220-0040d012197d', contemporary_palliative_care_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7e37c29e-c6fe-406a-b220-0040d012197d', '').
narrative_ontology:cs_kernel_id(end_of_life_decision_authority__sanctity_reading, end_of_life_decision_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, institutional_medical_gatekeepers).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, sanctity_doctrine_adherents).
narrative_ontology:constraint_victim(end_of_life_decision_authority__sanctity_reading, pressured_vulnerable_patients).
narrative_ontology:constraint_victim(end_of_life_decision_authority__sanctity_reading, terminal_suffering_patients).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, pressured_vulnerable_patients).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Physicians, hospital ethics committees, medical licensing boards, and national health authorities that enforce the life-preservation norm. They control what information patients receive about end-of-life options, enforce professional discipline against physicians who participate in assisted dying, design institutional policies that elevate palliative care as the only acceptable alternative, and adjudicate edge cases. They benefit from the constraint by having clear role definition (healer, not death-facilitator) and alignment with religious/philosophical authority structures that validate their professional identity.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, institutional_medical_gatekeepers, agenda_setter,
    institutional, generational, arbitrage, national).

% Religious institutions, pro-life advocacy organizations, philosophical traditions grounded in natural law or theological frameworks that view human life as intrinsically sacred. They benefit by having their normative reading embedded in law, medical practice, and institutional policy. The constraint vindicates their worldview and gives it legal force without requiring them to convince every individual of its truth — the law does the work for them.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, sanctity_doctrine_adherents, beneficiary,
    institutional, generational, analytical, national).

% Competent individuals with irreversible terminal diagnoses (advanced cancer, ALS, end-stage dementia, etc.) who experience unbearable suffering and would choose assisted death to end that suffering. They are barred by the constraint from that choice. They can refuse treatment (passive exit), but active hastening of death is prohibited. Their suffering is externalized — the constraint leaves them to die slowly while palliative care is offered as the sole alternative. They cannot exit the constraint because their dying condition is identity-locked; death will come, but only on the institution's timeline. The constraint extracts from them by foreclosing their autonomous choice while leaving the suffering in place.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, terminal_suffering_patients, payer,
    moderate, biographical, identity_locked, national).

% Economically precarious, socially isolated, or institutionally powerless patients who might face coercion from family members (do not burden us), healthcare systems (you are taking up a needed bed; consider a quicker exit), or internalized shame (I am a burden; I should not continue). Under the sanctity reading's reading, they are protected by the blanket prohibition on euthanasia — euthanasia is not an option anyone can pressure them toward. However, they also bear costs: they cannot access euthanasia even if they genuinely choose it; the constraint assumes they cannot be trusted with autonomous choice and must be protected from themselves. The constraint overrides their agency for their own good.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, pressured_vulnerable_patients, beneficiary,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_decision_authority__sanctity_reading, pressured_vulnerable_patients, payer).

% A subset of physicians who view end-of-life assistance as consistent with healing (ending suffering is a form of care) and who would provide it if permitted and requested. They are excluded from participation by professional licensing rules, legal prohibition, and institutional policy. They are kept out not by lack of expertise or legitimacy within the broader medical community (some jurisdictions and peer communities recognize their perspective), but by the sanctity reading's reconstruction of the physician role as life-preservation-only. Their voice is suppressed by professional discipline.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, physicians_facilitating_death, excluded,
    powerful, biographical, constrained, national).

% A diverse group that includes (1) ethicists who defend the sanctity reading on vulnerability grounds — the blanket prohibition protects the pressured-vulnerable from coercion, (2) ethicists who criticize the reading as overly restrictive and propose narrower alternatives (mandatory consultation, cooling-off periods, social support), and (3) human-rights advocates who argue that terminal patients' right to bodily autonomy and relief from suffering should outweigh the vulnerability-protection function. They analyze the constraint's operation and corroborate or contest the founding problem's status.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, medical_ethicists_vulnerability_advocates, observer,
    organized, generational, analytical, national).

% National parliaments and state legislatures that enact laws prohibiting assisted dying. They set the constraint's formal legal structure and can modify it through new legislation. They are observers in that they respond to public pressure and expert testimony; they are also partial agenda-setters in that they can choose which reading to enshrine in law or maintain neutrality and permit regional variation.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, legislative_bodies, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_decision_authority__sanctity_reading, legislative_bodies, observer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_decision_authority__sanctity_reading, institutional_medical_gatekeepers).
narrative_ontology:fixing_cost_class(end_of_life_decision_authority__sanctity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Protects economically precarious and relationally isolated patients from coercion toward death by making euthanasia legally unavailable — by removing the option entirely, the constraint removes the coercive pressure that family members or healthcare systems under financial stress could exert. Also coordinates physicians around a unified role definition (life-preservation) that simplifies professional identity and institutional practice, reducing the cognitive and moral burden on individual clinicians to make case-by-case judgments about hastening death.
% TRANSFER_FUNCTION: Moves decision authority FROM individuals (patient autonomy) TO institutional gatekeepers (physicians, ethics committees, legal authorities). Also moves suffering FROM healthcare systems and families (who might bear social/financial costs of caring for a dying person) TO dying patients (who bear the costs by continuing to live and suffer). Moves moral authority FROM individuals (who might invoke secular autonomy grounds) TO doctrine-adherents and institutional frameworks (who invoke sacred-life doctrine).
% ABSENT_VOICES: Patients in jurisdictions or communities where the sanctity reading does not dominate are largely absent from this constraint's decision-making (e.g., patients in Netherlands, Belgium, Switzerland where different readings are legally instantiated). Secular bioethicists who defend patient autonomy on grounds other than vulnerability are present in medical ethics literature but excluded from institutional policy-making in many jurisdictions. Some religious traditions (e.g., Jewish Talmudic reasoning on pikuach nefesh — saving life overrides other commandments, but ending suffering can be consistent with respecting life) have nuanced positions that are not represented in the simplified doctrine used to enforce this reading.
% DISAPPEARANCE_RATIONALE: If the sanctity reading's legal enforcement disappeared overnight, institutional policy would shift rapidly. Physicians would regain discretion; patients would regain choice; institutional gatekeepers would lose their unified role definition and would need to develop case-by-case reasoning frameworks or defer to patient preference. Vulnerable patients would face increased coercion risk (from family and healthcare systems) unless social safety nets improved. Healthcare systems would face new resource-allocation questions (how to support patients choosing death?). The arrangement's disappearance would not return the world to a natural state — it would activate a different reading of the same kernel (autonomy or vulnerability-protection), which would have its own institutional consequences.
% FOUNDING_PROBLEM: Economically precarious and relationally isolated patients face coercion from family members (financial burden narratives), healthcare systems under bed-rationing or cost pressure (you are taking needed resources), or internalized shame (I should not burden others). This coercion can push patients toward requesting death not from autonomous choice but from external pressure. The founding problem is: how can we protect vulnerable patients from this coercion? The sanctity reading's answer: make euthanasia legally unavailable so no one can use the threat/promise of death as a coercive tool.
% FOUNDING_PROBLEM_CORROBORATION: Institutional medical gatekeepers and sanctity doctrine adherents attest the founding problem is live and active. Some medical ethicists and human-rights advocates outside the benefiting parties corroborate vulnerability-driven coercion as a real risk. However, other medical ethicists and autonomy advocates argue: (1) the founding problem has been overstated and is not as severe as the blanket prohibition assumes, (2) narrower interventions (mandatory consultation, social support, mandatory waiting periods) could address the founding problem without foreclosing autonomous death, (3) the constraint persists not because the founding problem demands it but because institutional gatekeepers and doctrine adherents benefit from maintaining it. The corroboration is partial — some outside voices support the founding problem's importance, others contest whether the founding problem's severity justifies the constraint's current scope.
narrative_ontology:disappearance_verdict(end_of_life_decision_authority__sanctity_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_decision_authority__sanctity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_decision_authority__sanctity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(end_of_life_decision_authority__sanctity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_decision_authority__sanctity_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_decision_authority__sanctity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_decision_authority__sanctity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(end_of_life_decision_authority__sanctity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and rising (0.42 to 0.68 over the interval) because: (1) the reading imposes a single normative position on all patients regardless of individual circumstance, (2) the cost of that imposition falls asymmetrically on terminal patients and the economically vulnerable, and (3) institutional gatekeepers do not bear the cost of the constraint — they benefit from clarity of role and alignment with doctrine. Suppression is also high and rising (0.58 to 0.71) because the constraint's persistence depends on active enforcement: legal prohibition, professional discipline, messaging campaigns that frame the prohibition as compassionate protection. Theater ratio is moderate (0.28 to 0.42) because the constraint does perform a genuine protective function — vulnerable individuals are genuinely at risk from coercion — but a growing share of enforcement activity serves institutional legitimacy-maintenance rather than actual protection (as palliative care capacity increases and socioeconomic inequality persists, the need to blanket-prohibit euthanasia to protect the vulnerable diminishes, but the prohibition remains). The rising theater ratio suggests the constraint's protective function is being maintained theatrically rather than responding to actual vulnerability.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional gatekeeper seat: the constraint is coordination (protects vulnerable patients from coercion by patients' families or healthcare systems under financial pressure; clarity of role for physicians; alignment of medicine with intrinsic-value doctrine). From the terminal suffering patient seat: the same structure is extraction (my autonomous choice is foreclosed; my suffering is externalized; I bear the cost of protecting someone else's vulnerability). From the pressured-vulnerable patient seat: the constraint is genuine protection (euthanasia availability under current socioeconomic conditions creates coercive pressure to choose death; I am protected by the prohibition). The engine computes these divergent readings from directionality: institutional gatekeepers sit at powerful/analytical with arbitrage-grade exit options (they can exit into private practice, administrative roles, or other healthcare systems), so their effective extraction from this constraint is near-zero or negative (they collect authority and role clarity). Terminal suffering patients sit at various power levels but all face trapped or identity-locked exit (they cannot leave the constraint's domain; their identity as a dying person in pain is identity-locked to the system). Pressured-vulnerable patients sit at powerless/organized with constrained exit, making them legitimate beneficiaries of the constraint's protective function.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional medical gatekeepers: powerful/analytical, arbitrage exit → directionality near 0.0 (full beneficiary). They set the rules, control implementation, align with doctrine authority, suffer minimal cost. Terminal suffering patients: moderate-to-powerful/biographical, identity-locked exit (cannot leave dying condition) → directionality near 1.0 (full target). Their autonomy is foreclosed; their suffering is externalized; they cannot exit the constraint. Pressured-vulnerable patients: powerless/biographical, trapped exit → directionality near 0.5 (symmetric/protected). They benefit from the constraint's protection but also bear its costs (no choice, constrained agency). The asymmetry is the structure: institutional gatekeepers experience the constraint as empowering; terminal patients experience it as constraining; vulnerable patients experience it as protecting them from themselves. No override needed — the derived directionality captures the structural divergence.
 *
 * MANDATROPHY ANALYSIS:
 *   The sanctity reading avoids the snare classification despite high extraction because: (1) it articulates a genuine coordination problem (protecting vulnerable patients from coercion), (2) beneficiaries exist who genuinely benefit (pressured-vulnerable patients, institutional gatekeepers, doctrine adherents), and (3) the constraint's persistence is defended by reference to a live founding problem (vulnerability to euthanasia coercion). However, mandatrophy risks are present: (a) the founding problem's scope has narrowed as palliative care capacity increases and as socioeconomic inequality persists but becomes more visible, making the blanket prohibition less necessary for actual protection and more theatrical; (b) as the constraint persists and terminal patients age into the system, awareness of the constraint's extraction cost rises, increasing pressure on institutional gatekeepers to justify ongoing prohibition; (c) the constraint's vulnerability-protection function could be achieved through narrower interventions (mandatory consultation, cooling-off periods, social support for pressured patients) while restoring autonomy for terminal patients — the refusal to adopt narrower alternatives shifts the constraint toward pure extraction. The theater ratio's rise from 0.28 to 0.42 signals the beginning of this drift: the constraint's protective machinery is increasingly performative rather than responsive to actual vulnerability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sanctity_vs_autonomy_foreclosure,
    'Does the sanctity reading''s core premise (life has intrinsic value independent of individual will) logically foreclose the autonomy reading''s core premise (individuals possess sovereign authority over their own death), or do they represent incommensurable frameworks?',
    'Philosophical analysis of whether ''intrinsic value transcends will'' and ''will determines value'' are logical contradictories (foreclosing) or merely incompatible policy orientations held by different parties (coexisting). Test: can a single legal or institutional framework coherently hold both premises without contradiction, or does adopting one require rejecting the other?',
    'If foreclosing: one reading will eventually displace the other at the institutional level, producing a unitary policy. If coexisting: the constraint family represents an irreducible pluralism, and policy outcomes will reflect power struggles between parties holding each reading, not logical resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sanctity_vs_autonomy_foreclosure, conceptual, 'Whether sanctity and autonomy readings logically foreclose each other or represent coexistent frameworks.').

omega_variable(
    suffering_externalization_mechanism,
    'Is the sanctity reading''s externalization of terminal patient suffering a structural feature of the reading itself, or a contingent policy outcome dependent on institutional implementation?',
    'Comparative institutional analysis: do jurisdictions embracing the sanctity reading uniformly externalize suffering, or do some implement aggressive palliative care that reduces but does not eliminate the structural cost-shift to patients? If suffering externalization is contingent, the reading is not entailed to extract from terminal patients.',
    'If structural: terminal suffering patients are necessarily in the victim set regardless of palliative investment; the constraint extracts from them by design. If contingent: terminal suffering patients are victims only in jurisdictions that fail to invest in palliative alternatives; the constraint''s extractiveness varies by implementation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suffering_externalization_mechanism, empirical, 'Whether terminal patient suffering is a necessary extraction or a contingent policy failure.').

omega_variable(
    pressured_vulnerable_definition,
    'What specific constellation of pressures and vulnerabilities makes a patient ''pressured-vulnerable'' such that euthanasia availability predicts harm? Is it economic coercion (healthcare costs), relational coercion (family pressure), institutional coercion (bed rationing), identity-bound shame, or some combination?',
    'Epidemiological analysis of euthanasia uptake patterns in jurisdictions with availability: do uptake rates correlate with economic precarity, relational isolation, institutional bed pressure, or a mix? Post-legalization trajectory analysis: do uptake rates among economically disadvantaged populations rise or stabilize?',
    'The specific mechanism determines which agents are in the victim set and whether the sanctity reading extracts from them by design (structural coercion of the vulnerable) or by institutional failure (inadequate social safety nets that could be fixed without abandoning the reading). Victim identification depends on this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pressured_vulnerable_definition, empirical, 'Which constellation of pressures constitutes the ''pressured-vulnerable'' victim set.').

omega_variable(
    intrinsic_value_grounding,
    'What grounds the sanctity reading''s claim that human life possesses intrinsic value independent of individual will? Is the grounding theological (divine source), deontological (rights-based), or a different epistemic framework?',
    'Textual and institutional analysis of how sanctity advocates defend the intrinsic-value claim: appeal to religious authority (theological), appeal to universal human rights or dignity doctrines (deontological), appeal to natural law (philosophical). The grounding type determines which communities'' corroboration would authenticate the founding problem.',
    'If theological: the reading''s authority rests on religious authority lineage; policy contestation may be irreducible across secular/religious boundaries. If deontological: secular philosophical argument about rights could shift the reading''s legitimacy. If natural-law: empirical challenge to the natural-law premise could undermine the reading''s foundation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intrinsic_value_grounding, conceptual, 'Theological, deontological, or philosophical grounding of intrinsic-value claim.').

omega_variable(
    physician_healer_role_reconstruction,
    'Does the sanctity reading require that physician role be reconstructed as life-preservation-only (healer cannot intentionally end life under any circumstances), or is that reconstruction a contingent institutional choice?',
    'Historical and comparative analysis: did the medical community adopt the healer-only role BECAUSE of the sanctity reading, or did the sanctity reading later claim alignment with an existing professional norm? Did physician opposition to assisted dying exist before the sanctity doctrine formalization? Do contemporary sanctity advocates argue the healer-role is entailed by the reading, or a separate professional boundary?',
    'If entailed: the reading''s institutional expression requires physician role change; moving the reading into law means restructuring medical practice. If contingent: the reading could coexist with physicians trained to support patient autonomy in end-of-life decisions; role reconstruction is a separate policy choice, not a reading-entailed consequence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physician_healer_role_reconstruction, conceptual, 'Whether physician healer-only role is entailed by sanctity reading or contingently adopted.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_decision_authority__sanctity_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_decision_authority__sanctity_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(end__tr_t5, end_of_life_decision_authority__sanctity_reading, theater_ratio, 5, 0.31).
narrative_ontology:measurement(end__tr_t10, end_of_life_decision_authority__sanctity_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(end__tr_t15, end_of_life_decision_authority__sanctity_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement(end__tr_t20, end_of_life_decision_authority__sanctity_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(end__tr_t25, end_of_life_decision_authority__sanctity_reading, theater_ratio, 25, 0.42).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(end__be_t5, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(end__be_t10, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(end__be_t15, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 15, 0.61).
narrative_ontology:measurement(end__be_t20, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(end__be_t25, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(end__su_t5, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(end__su_t10, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(end__su_t15, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(end__su_t20, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(end__su_t25, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 25, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_decision_authority__sanctity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(end_of_life_decision_authority__sanctity_reading, 0.12).
narrative_ontology:affects_constraint(end_of_life_decision_authority__sanctity_reading, end_of_life_decision_authority__autonomy_reading).
narrative_ontology:affects_constraint(end_of_life_decision_authority__sanctity_reading, end_of_life_decision_authority__vulnerability_protection_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of the end-of-life decision authority constraint family (three readings of a contested kernel). The sanctity reading grounds decision authority in intrinsic human value; the autonomy reading grounds it in individual will; the vulnerability-protection reading grounds it in distributed checkpoints. Each reading has a different ε, beneficiary set, victim set, and enforcement mechanism. They are NOT alternative observables of a single constraint (ε-invariance principle, OQ-26) — they are genuinely different constraints arising from different readings of a contested kernel. Network links enable the engine to trace how each reading's evolution affects the others' legitimacy and enforcement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(end_of_life_decision_authority__sanctity_reading, powerless, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
