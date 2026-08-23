% ============================================================================
% CONSTRAINT STORY: refugee_convention_text__procedural_integrity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_refugee_convention_text__procedural_integrity_reading, []).

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
 *   constraint_id: refugee_convention_text__procedural_integrity_reading
 *   human_readable: Refugee Convention Procedural Integrity Reading
 *   domain: international_law/migration_governance/human_rights
 *
 * SUMMARY:
 *   This constraint story instantiates the procedural integrity reading of
 *   the 1951 Refugee Convention (as amended by the 1967 Protocol). The
 *   reading holds that the Convention's core obligation is a fair,
 *   individualized procedure for determining refugee status — the 'process
 *   integrity' is non-negotiable, while the substantive protection threshold
 *   (who qualifies as a refugee) is flexible and subject to sovereign
 *   interpretation within the treaty's terms. Offshore processing is
 *   permissible only if full procedural guarantees travel with the transfer.
 *   States may narrow definitions (e.g., restrictive 'particular social
 *   group' interpretations) but cannot eliminate substantive review. The
 *   constraint coordinates a global floor for procedural fairness while
 *   extracting compliance costs from states; it is a tangled rope because it
 *   simultaneously solves a genuine coordination problem (preventing
 *   arbitrary refoulement) and asymmetrically extracts resources from states
 *   parties, requiring active enforcement through UNHCR supervision, treaty
 *   body monitoring, and domestic/international litigation.
 *
 * KEY AGENTS:
 *   - states_parties: Primary agenda_setter and payer (institutional/constrained) — implement procedure, bear costs
 *   - asylum_seekers: Primary beneficiary and payer (powerless/trapped) — receive procedural guarantee, bear burden of proof
 *   - unhcr: Observer (institutional/analytical) — supervises compliance, no extraction
 *   - human_rights_ngos: Beneficiary (organized/mobile) — litigate for procedural compliance
 *   - restrictive_sovereign_states: Payer and agenda_setter (powerful/constrained) — minimize substantive grants within procedural floor
 *   - expansive_humanitarian_advocates: Excluded (organized/mobile) — want substantive guarantees this reading does not provide
 *   - offshore_processing_states: Payer (powerful/constrained) — bear costs of ensuring procedural guarantees travel with transfer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__procedural_integrity_reading, 0.35).
domain_priors:suppression_score(refugee_convention_text__procedural_integrity_reading, 0.45).
domain_priors:theater_ratio(refugee_convention_text__procedural_integrity_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__procedural_integrity_reading, tangled_rope).
narrative_ontology:human_readable(refugee_convention_text__procedural_integrity_reading, "Refugee Convention Procedural Integrity Reading").
narrative_ontology:topic_domain(refugee_convention_text__procedural_integrity_reading, "international_law/migration_governance/human_rights").

domain_priors:requires_active_enforcement(refugee_convention_text__procedural_integrity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__procedural_integrity_reading, '71437303-2cd6-465a-9192-46033afbfa96').
narrative_ontology:cs_kernel_codification('71437303-2cd6-465a-9192-46033afbfa96', formalized).
narrative_ontology:cs_authority_grounding('71437303-2cd6-465a-9192-46033afbfa96', lineage).
narrative_ontology:cs_interpretation_layer_present('71437303-2cd6-465a-9192-46033afbfa96').
narrative_ontology:cs_reading_relation('71437303-2cd6-465a-9192-46033afbfa96', refugee_convention_text__restrictive_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('71437303-2cd6-465a-9192-46033afbfa96', refugee_convention_text__expansive_humanitarian_reading, coexists_with).
narrative_ontology:cs_axiom('71437303-2cd6-465a-9192-46033afbfa96', foundational, procedure_prior_to_substance).
narrative_ontology:cs_axiom_status(procedure_prior_to_substance, holdable).
narrative_ontology:cs_axiom_grounding('71437303-2cd6-465a-9192-46033afbfa96', procedure_prior_to_substance, deontological).
narrative_ontology:cs_axiom('71437303-2cd6-465a-9192-46033afbfa96', foundational, individualized_assessment_non_derogable).
narrative_ontology:cs_axiom_status(individualized_assessment_non_derogable, holdable).
narrative_ontology:cs_axiom_grounding('71437303-2cd6-465a-9192-46033afbfa96', individualized_assessment_non_derogable, conventional).
narrative_ontology:cs_axiom('71437303-2cd6-465a-9192-46033afbfa96', secondary, offshore_processing_permissible_with_guarantees).
narrative_ontology:cs_axiom_status(offshore_processing_permissible_with_guarantees, holdable).
narrative_ontology:cs_axiom_grounding('71437303-2cd6-465a-9192-46033afbfa96', offshore_processing_permissible_with_guarantees, instrumental).
narrative_ontology:cs_reference_frame('71437303-2cd6-465a-9192-46033afbfa96', treaty_procedural_floor_1951).
narrative_ontology:cs_drift_state('71437303-2cd6-465a-9192-46033afbfa96', contemporary_externalization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('71437303-2cd6-465a-9192-46033afbfa96', '').
narrative_ontology:cs_kernel_id(refugee_convention_text__procedural_integrity_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__procedural_integrity_reading, asylum_seekers).
narrative_ontology:constraint_beneficiary(refugee_convention_text__procedural_integrity_reading, human_rights_ngos).
narrative_ontology:constraint_beneficiary(refugee_convention_text__procedural_integrity_reading, unhcr).
narrative_ontology:constraint_victim(refugee_convention_text__procedural_integrity_reading, states_parties).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(refugee_convention_text__procedural_integrity_reading, asylum_seekers).
narrative_ontology:constraint_victim(refugee_convention_text__procedural_integrity_reading, restrictive_sovereign_states).
narrative_ontology:constraint_victim(refugee_convention_text__procedural_integrity_reading, offshore_processing_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Implement and administer refugee status determination procedures domestically. Bear the administrative, judicial, and social costs of fair individualized assessment (legal aid, interpreters, appeals bodies, detention alternatives). Can narrow substantive protection thresholds (e.g., restrictive 'particular social group' interpretations) but cannot eliminate substantive review without violating treaty obligations. Exit from the constraint means withdrawing from the Convention — politically costly and reputationally damaging.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, states_parties, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__procedural_integrity_reading, states_parties, payer).

% Receive the procedural guarantee of fair individualized assessment before removal. Protection outcome is not guaranteed — only the process is. Bear the burden of proof in establishing 'well-founded fear' through individualized evidence, often without documentation, in a foreign language, while detained or destitute. Exit options are near-zero: return means persecution; onward movement is blocked by border regimes; staying means navigating a complex adversarial process.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, asylum_seekers, beneficiary,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__procedural_integrity_reading, asylum_seekers, payer).

% Supervisory mandate under Article 35 to monitor Convention compliance. Issues guidelines, intervenes in courts, conducts RSD in states without national systems. Does not enforce — relies on moral authority and state cooperation. Collects no rents; its institutional survival depends on the Convention's relevance. Sees the full structural picture: where procedure is performative, where it is genuine, and where it is weaponized.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, unhcr, observer,
    institutional, generational, analytical, global).

% Litigate for procedural compliance, document violations, provide legal representation. Benefit institutionally from the procedural framework — it gives them enforceable standards to invoke. Funding and advocacy momentum depend on the constraint's vitality. Can shift focus to other human rights mechanisms if this constraint degrades (mobile exit), but lose the specific leverage of Convention-based claims.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, human_rights_ngos, beneficiary,
    organized, biographical, mobile, global).

% States that interpret the Convention narrowly (e.g., limiting 'particular social group' to immutable characteristics, requiring state-perpetrated persecution). Bear procedural costs they would prefer to avoid — they implement minimal compliance (accelerated procedures, limited appeals, detention-heavy systems) while pushing substantive thresholds down. Their agenda-setting power lets them shape domestic RSD to minimize grants, but they cannot exit the procedural requirement without treaty withdrawal.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, restrictive_sovereign_states, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__procedural_integrity_reading, restrictive_sovereign_states, agenda_setter).

% Advocates who read the Convention as requiring broad substantive protection (gender-based claims, generalized violence, climate displacement, non-state actor persecution). Their reading is structurally excluded from this constraint's logic — this reading treats outcome as secondary to procedure. They would object that procedural integrity without substantive scope is a hollow shell. They operate in the same forums but their interpretive frame is not the one this constraint instantiates.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, expansive_humanitarian_advocates, excluded,
    organized, biographical, mobile, global).

% States that transfer asylum seekers to third countries for RSD (e.g., Australia-Nauru/PNG, UK-Rwanda, EU-Turkey). Bear the financial cost of offshore arrangements and the procedural obligation to ensure the third country provides fair individualized assessment. The constraint permits offshore processing ONLY if full procedural guarantees travel with the transfer — a condition that creates ongoing litigation and monitoring costs. Exit means ending offshore processing, which is politically difficult once established.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, offshore_processing_states, payer,
    powerful, biographical, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a predictable, legally binding framework for individualized refugee status determination that prevents arbitrary rejection, ensures non-refoulement is operationally meaningful, and creates a common procedural language across 149 states parties — solving the coordination problem of how to distinguish refugees from other migrants without leaving it to unilateral sovereign discretion.
% TRANSFER_FUNCTION: Moves the cost of fair procedure (legal infrastructure, trained adjudicators, appeals, detention alternatives, monitoring) from asylum seekers — who would otherwise bear the cost of arbitrary rejection and refoulement — to states parties. Moves protection outcomes from guaranteed substantive rights to procedurally contingent results: a fair process may still yield a negative decision, and the Convention does not guarantee a positive outcome.
% ABSENT_VOICES: Expansive humanitarian advocates who demand substantive protection guarantees (gender, LGBTQ+, clan, climate, generalized violence) are excluded from this reading's logic — their frame treats outcome as primary. Would-be asylum seekers intercepted at sea or in transit zones never reach the procedural safeguard; their voices are absent by geography. Stateless persons not meeting the Convention's refugee definition fall outside the constraint entirely. States not party to the Convention (e.g., India, Malaysia, Gulf states) operate without this procedural obligation — their asylum seekers have no access to it.
% DISAPPEARANCE_RATIONALE: If the procedural integrity requirement vanished overnight, states could reject claims summarily without individualized assessment, accelerated procedures would become the norm, appeals would be eliminated, and non-refoulement would become an unenforceable aspiration. The global asylum system would reorganize around sovereign discretion with no procedural floor — refoulement would become routine, and the distinction between refugee protection and migration control would collapse.
% FOUNDING_PROBLEM: Post-WWII displacement crisis revealed that states could arbitrarily reject refugees and return them to persecution. The Convention was built to solve this by requiring individualized assessment before removal — a procedural floor that prevents summary refoulement while preserving sovereign control over the substantive definition of who qualifies.
% FOUNDING_PROBLEM_CORROBORATION: UNHCR annual Global Trends reports document persistent displacement (117M+ forcibly displaced in 2024). ECtHR and Inter-American Court jurisprudence consistently affirms procedural safeguards as Convention requirements (e.g., M.S.S. v. Belgium & Greece, Hirsi Jamaa v. Italy). State practice surveys by the Migration Policy Institute and academic studies (Goodwin-Gill, Hathaway) confirm procedural compliance remains contested and incomplete — corroboration comes from outside the beneficiary set (courts, monitoring bodies, critical scholars).
narrative_ontology:disappearance_verdict(refugee_convention_text__procedural_integrity_reading, world_rearranges).
narrative_ontology:founding_problem_status(refugee_convention_text__procedural_integrity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__procedural_integrity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(refugee_convention_text__procedural_integrity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(refugee_convention_text__procedural_integrity_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(refugee_convention_text__procedural_integrity_reading_tests).
:- end_tests(refugee_convention_text__procedural_integrity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.35) because the constraint imposes real but bounded costs on states (administrative, judicial, social) while giving them substantive flexibility — the extraction is the procedural floor, not the outcome. Suppression (0.45) reflects that states resisting procedural obligations face treaty-body pressure, litigation, and reputational costs, but exit (withdrawal) remains legally possible. Theater ratio (0.35) captures the gap between procedural compliance on paper and substantive fairness in practice — accelerated procedures, limited legal aid, detention-as-deterrence, and offshore arrangements that mimic process without its substance. Accessibility collapse (0.55) is moderate: arbitrary rejection is suppressed but not eliminated; alternatives (summary removal, non-entrée policies) persist at the margins. Resistance (0.55) reflects sustained state pushback: restrictive interpretations, externalization, deterrence policies. The measurement series shows extraction and theater rising from the 1980s onward (asylum surge, restrictive turn, externalization), peaking around 2015, then slightly moderating as some courts push back.
 *
 * PERSPECTIVAL GAP:
 *   From the states_parties seat (agenda_setter/payer), the constraint is a genuine coordination mechanism they built and maintain — it provides a manageable framework for migration governance. From the asylum_seekers seat (beneficiary/payer, powerless/trapped), the same structure delivers a procedural lifeline that is often performative — the individualized assessment exists on paper but is undermined by evidentiary barriers, accelerated timelines, and credibility assessments that function as substantive gatekeeping. From the restrictive_sovereign_states seat (payer/agenda_setter, powerful/constrained), the constraint is an extraction mechanism they comply with minimally — they pay the procedural cost but hollow out the substance. The engine computes these divergent seat classifications from the structural data; the authored claim (tangled_rope) reflects the coordination-extraction hybrid visible from the analytical seat.
 *
 * DIRECTIONALITY LOGIC:
 *   States_parties are both agenda_setter (they administer the procedure) and payer (they bear its costs) — directionality is near-symmetric (d ~0.5) because they designed the constraint and can shape its implementation, but cannot exit without treaty withdrawal. Asylum_seekers are beneficiary (receive procedural guarantee) and payer (bear burden of proof in an adversarial process) — but their exit is trapped (return = persecution), so directionality is pulled toward target (d ~0.7) despite beneficiary status. Unhcr is observer (analytical) — d = 0.5 by definition. Human_rights_ngos are beneficiary (organized/mobile) — they gain advocacy leverage, d ~0.2. Restrictive_sovereign_states are payer (constrained) — they bear costs they'd avoid, d ~0.65. Expansive_humanitarian_advocates are excluded — their reading is not instantiated here. Offshore_processing_states are payer (constrained) — they bear offshore costs plus monitoring obligations, d ~0.6.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (arbitrary refoulement) remains live — displacement is at record highs, and states still attempt summary rejection. The constraint has not atrophied into a piton because states actively contest its scope (substantive thresholds, offshore processing, accelerated procedures) and civil society actively enforces it (litigation, monitoring). The coordination function (preventing arbitrary rejection) is genuine and contested; the extraction (procedural costs on states) is real and resisted. This is not a degraded institution maintained theatrically — it is a live battlefield where the procedural floor is the terrain. Mandatrophy is not resolved; the constraint's mandate is actively disputed, not forgotten.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the procedural_integrity_reading a distinct constraint from its sibling readings, or a strategic framing of the same constraint?',
    'Compare the three readings'' victim sets, beneficiary sets, and enforcement mechanisms. If victim/beneficiary structures differ structurally (not just rhetorically), they are distinct constraints per ε-invariance.',
    'If distinct, each reading gets its own ε, type, and classification. If not, the kernel is a single constraint with observer-dependent classification — violating ε-invariance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel''s three readings instantiate three constraints or one constraint with three observer positions.').

omega_variable(
    procedural_substantive_boundary,
    'Where does the procedural floor end and substantive discretion begin? At what point does a ''procedural'' restriction become a substantive denial?',
    'Court jurisprudence on accelerated procedures, credibility assessment standards, burden-of-proof allocation, and offshore processing standards. Track where courts draw the line between permissible procedural design and impermissible substantive exclusion.',
    'If the boundary is unstable, the constraint''s ε and type drift over time. A collapsing procedural floor into substantive restriction would increase extractiveness on asylum seekers and shift classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_substantive_boundary, empirical, 'The structural boundary between procedural integrity (this reading''s claim) and substantive restriction (what restrictive states implement).').

omega_variable(
    offshore_processing_permissibility,
    'Does the procedural integrity reading genuinely permit offshore processing with full guarantees, or is offshore processing structurally incompatible with fair individualized assessment?',
    'Empirical assessment of offshore RSD systems (Nauru, PNG, Rwanda, Turkey): do they deliver individualized assessment, legal representation, independent appeals, and non-refoulement protection equivalent to domestic systems? UNHCR monitoring reports and court rulings on specific arrangements.',
    'If offshore processing cannot deliver procedural integrity, this reading''s claim is internally contradictory — it forecloses the very arrangement it claims to permit. Would shift classification toward snare for offshore-processing states.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(offshore_processing_permissibility, empirical, 'Whether the procedural integrity reading''s offshore-processing permission is practically realizable or a theoretical loophole.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers, interdiction, detention) or internalized (asylum seekers'' belief that they cannot win, lawyer complicity in accelerated procedures, NGO adaptation to restrictive frameworks)?',
    'Post-exit suppression trajectory: track asylum seekers who reach fair procedures (e.g., via litigation, relocation, policy change) — if suppression persists (psychological barriers, distrust, adapted behavior), reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit. Affects classification stability over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the asylum procedure context.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__procedural_integrity_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rcpir_tr_t0, refugee_convention_text__procedural_integrity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(rcpir_tr_t10, refugee_convention_text__procedural_integrity_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(rcpir_tr_t16, refugee_convention_text__procedural_integrity_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(rcpir_tr_t25, refugee_convention_text__procedural_integrity_reading, theater_ratio, 25, 0.25).
narrative_ontology:measurement(rcpir_tr_t35, refugee_convention_text__procedural_integrity_reading, theater_ratio, 35, 0.3).
narrative_ontology:measurement(rcpir_tr_t45, refugee_convention_text__procedural_integrity_reading, theater_ratio, 45, 0.35).
narrative_ontology:measurement(rcpir_tr_t55, refugee_convention_text__procedural_integrity_reading, theater_ratio, 55, 0.4).
narrative_ontology:measurement(rcpir_tr_t64, refugee_convention_text__procedural_integrity_reading, theater_ratio, 64, 0.38).
narrative_ontology:measurement(rcpir_tr_t70, refugee_convention_text__procedural_integrity_reading, theater_ratio, 70, 0.35).

% Extraction over time
narrative_ontology:measurement(rcpir_be_t0, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(rcpir_be_t10, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(rcpir_be_t16, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 16, 0.3).
narrative_ontology:measurement(rcpir_be_t25, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 25, 0.32).
narrative_ontology:measurement(rcpir_be_t35, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 35, 0.35).
narrative_ontology:measurement(rcpir_be_t45, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 45, 0.37).
narrative_ontology:measurement(rcpir_be_t55, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 55, 0.38).
narrative_ontology:measurement(rcpir_be_t64, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 64, 0.36).
narrative_ontology:measurement(rcpir_be_t70, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 70, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(rcpir_su_t0, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(rcpir_su_t10, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 10, 0.32).
narrative_ontology:measurement(rcpir_su_t16, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 16, 0.35).
narrative_ontology:measurement(rcpir_su_t25, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 25, 0.4).
narrative_ontology:measurement(rcpir_su_t35, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 35, 0.45).
narrative_ontology:measurement(rcpir_su_t45, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 45, 0.5).
narrative_ontology:measurement(rcpir_su_t55, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 55, 0.55).
narrative_ontology:measurement(rcpir_su_t64, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 64, 0.52).
narrative_ontology:measurement(rcpir_su_t70, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 70, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__procedural_integrity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(refugee_convention_text__procedural_integrity_reading, 0.12).
narrative_ontology:affects_constraint(refugee_convention_text__procedural_integrity_reading, refugee_convention_text__restrictive_sovereignty_reading).
narrative_ontology:affects_constraint(refugee_convention_text__procedural_integrity_reading, refugee_convention_text__expansive_humanitarian_reading).
narrative_ontology:affects_constraint(refugee_convention_text__procedural_integrity_reading, non_refoulement_customary_law).
narrative_ontology:affects_constraint(refugee_convention_text__procedural_integrity_reading, eu_asylum_acquis_procedural_standards).
narrative_ontology:affects_constraint(refugee_convention_text__procedural_integrity_reading, australia_offshore_processing_regime).
narrative_ontology:affects_constraint(refugee_convention_text__procedural_integrity_reading, uk_rwanda_asylum_partnership).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the refugee_convention_text kernel. The restrictive_sovereignty_reading narrows substantive scope while accepting the procedural floor; the expansive_humanitarian_reading broadens substantive scope while demanding the procedural floor; this reading treats the procedural floor as the Convention's core and the substantive scope as flexible. All three share the same treaty text but instantiate different constraints with different ε, victim sets, and enforcement logics. The procedural_integrity_reading sits upstream: its procedural requirements are the platform on which the other two readings contest substantive outcomes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(refugee_convention_text__procedural_integrity_reading, institutional, 0.48).
constraint_indexing:directionality_override(refugee_convention_text__procedural_integrity_reading, powerless, 0.7).
constraint_indexing:directionality_override(refugee_convention_text__procedural_integrity_reading, powerful, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
