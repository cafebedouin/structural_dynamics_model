% ============================================================================
% CONSTRAINT STORY: refugee_convention_text__procedural_integrity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: refugee_convention_text__procedural_integrity_reading
 *   human_readable: 1951 Refugee Convention as Procedural Integrity Safeguard
 *   domain: international_law/migration_governance/human_rights
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the 1951 Refugee Convention —
 *   the procedural integrity reading. The Convention itself is the kernel: a
 *   text whose binding force has been disputed since ratification. Different
 *   states and courts interpret the same Convention to support three
 *   incompatible positions: (1) the expansive humanitarian reading (the
 *   Convention mandates broad material protection; procedure is instrumental
 *   to outcome); (2) the procedural integrity reading (the Convention
 *   mandates fair individualized assessment; procedure is the protection
 *   itself, outcome is secondary); (3) the restrictive sovereignty reading
 *   (the Convention is a minimum floor; states retain maximum discretion over
 *   both gate and process). This constraint models the procedural integrity
 *   reading's instantiation: what structure and classification follow from
 *   treating procedure as non-negotiable and outcome as flexible. The victim
 *   set is shaped by procedural access: those admitted to fair procedure are
 *   beneficiaries; those excluded or offshore-processed are payers. The
 *   constraint's stability depends on states committing to procedure
 *   integrity while maintaining gate discretion — a tangled rope:
 *   coordination on procedure (genuine benefit), extraction via gate
 *   (differential cost).
 *
 * KEY AGENTS:
 *   - asylum_seekers_with_access: procedural beneficiary (immediate, trapped, national) — collects from the constraint's protection of procedure
 *   - asylum_seekers_without_procedural_access: payer (immediate, trapped, local) — excluded from the procedure itself; bears extraction
 *   - offshore_processing_populations: payer (immediate, trapped, regional) — admitted to assessment but denied full procedural framework declared non-negotiable by this reading
 *   - state_asylum_adjudicators: agenda_setter (generational, mobile, national) — operates the procedure and chooses which populations enter it
 *   - international_bodies: observer (generational, analytical, universal) — interprets what procedural adequacy requires and produces jurisprudence that constrains state discretion over process
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__procedural_integrity_reading, 0.42).
domain_priors:suppression_score(refugee_convention_text__procedural_integrity_reading, 0.38).
domain_priors:theater_ratio(refugee_convention_text__procedural_integrity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__procedural_integrity_reading, tangled_rope).
narrative_ontology:human_readable(refugee_convention_text__procedural_integrity_reading, "1951 Refugee Convention as Procedural Integrity Safeguard").
narrative_ontology:topic_domain(refugee_convention_text__procedural_integrity_reading, "international_law/migration_governance/human_rights").

domain_priors:requires_active_enforcement(refugee_convention_text__procedural_integrity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__procedural_integrity_reading, '000aba96-83bd-4efd-91cf-bd80c2189e5f').
narrative_ontology:cs_kernel_codification('000aba96-83bd-4efd-91cf-bd80c2189e5f', fixed_text).
narrative_ontology:cs_authority_grounding('000aba96-83bd-4efd-91cf-bd80c2189e5f', lineage).
narrative_ontology:cs_interpretation_layer_present('000aba96-83bd-4efd-91cf-bd80c2189e5f').
narrative_ontology:cs_reading_relation('000aba96-83bd-4efd-91cf-bd80c2189e5f', refugee_convention_text__restrictive_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('000aba96-83bd-4efd-91cf-bd80c2189e5f', refugee_convention_text__expansive_humanitarian_reading, coexists_with).
narrative_ontology:cs_axiom('000aba96-83bd-4efd-91cf-bd80c2189e5f', foundational, procedure_integrity_non_negotiable).
narrative_ontology:cs_axiom_status(procedure_integrity_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('000aba96-83bd-4efd-91cf-bd80c2189e5f', procedure_integrity_non_negotiable, deontological).
narrative_ontology:cs_axiom('000aba96-83bd-4efd-91cf-bd80c2189e5f', foundational, outcome_secondary_to_process).
narrative_ontology:cs_axiom_status(outcome_secondary_to_process, holdable).
narrative_ontology:cs_axiom_grounding('000aba96-83bd-4efd-91cf-bd80c2189e5f', outcome_secondary_to_process, deontological).
narrative_ontology:cs_reference_frame('000aba96-83bd-4efd-91cf-bd80c2189e5f', fair_individualized_assessment_as_protection).
narrative_ontology:cs_drift_state('000aba96-83bd-4efd-91cf-bd80c2189e5f', contemporary_offshore_processing_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('000aba96-83bd-4efd-91cf-bd80c2189e5f', '').
narrative_ontology:cs_kernel_id(refugee_convention_text__procedural_integrity_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__procedural_integrity_reading, asylum_seekers_with_access).
narrative_ontology:constraint_victim(refugee_convention_text__procedural_integrity_reading, asylum_seekers_without_procedural_access).
narrative_ontology:constraint_victim(refugee_convention_text__procedural_integrity_reading, offshore_processing_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who reach territorial asylum systems with functioning procedural review mechanisms. They benefit from fair individualized assessment, independent adjudication, and legal representation (where available). The procedure itself is their protection; they collect from the constraint's operation because it mandates their hearing and mandates the hearing be fair. Outcome uncertainty is high (they may still be denied), but the procedural guarantee is real and valuable to them.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, asylum_seekers_with_access, beneficiary,
    powerless, immediate, trapped, national).

% Asylum seekers processed in offshore facilities (detention centers in third countries, offshore platforms) or in third-country arrangements without full procedural guarantees as defined by this reading. They undergo assessment but lack access to independent adjudication, full legal counsel, or meaningful appeal within a framework recognized by international bodies as procedurally adequate. Under this reading, they extract the cost because states truncate the very procedure the reading declares non-negotiable.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, offshore_processing_populations, payer,
    powerless, immediate, trapped, regional).

% Individuals intercepted at borders, summarily deterred, or subjected to non-refoulement decisions (return prevention) without procedural review at all. They bear extraction because the constraint — as this reading defines it — mandates fair individualized assessment, but they are never admitted to that assessment. The constraint protects only those the state admits to the procedure; those excluded extract the cost (extended detention, deprivation of asylum access) without collecting the benefit (never heard).
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, asylum_seekers_without_procedural_access, payer,
    powerless, immediate, trapped, local).

% National governments operating asylum determination systems: judges, review boards, immigration officers, appeal bodies. They enforce the constraint by maintaining procedural machinery (hearings, evidence consideration, legal representation, appeals) and by choosing which populations enter the system. This reading gives them discretion over who accesses the procedure (gate control) but removes discretion over how the procedure operates for those admitted: they must afford fair assessment or be in violation of the reading's non-negotiable procedure requirement. They set the gate; they cannot set the outcome for those admitted.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, state_asylum_adjudicators, agenda_setter,
    institutional, generational, mobile, national).

% UNHCR, UN Human Rights Committee, treaty monitoring bodies, regional human rights courts (European Court of Human Rights, Inter-American Court), and international advocacy organizations. They do not operate asylum systems directly but interpret the Convention, produce jurisprudence on procedural adequacy, issue advisory opinions, and monitor state compliance. They observe the constraint's operation and influence what 'fair individualized assessment' means in practice, thereby constraining state discretion over procedure.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, international_bodies, observer,
    institutional, generational, analytical, universal).

% States that explicitly reject this reading's procedural framing and prefer to interpret the Convention to maximize sovereign discretion over both gate (who accesses asylum systems) and process (what assessment those admitted undergo). They are excluded from this constraint's parties — they do not accept its core premise that procedure integrity is non-negotiable. Their voices would argue for summit procedure (minimal, expedited review) and maximum gate control; they would dispute that the Convention mandates the procedural framework this reading asserts.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, restrictive_states_interpreting_convention, excluded,
    institutional, generational, mobile, national).

% International and domestic NGOs arguing that the Convention mandates material protection and that fair procedure without substantive refugee status is meaningless theater. They are excluded from this constraint's parties because they contest its core premise: that procedure is the protection. They would argue outcome is primary, procedure is instrumental, and fair procedures that deny most claims fail the Convention's humanitarian mandate.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, humanitarian_advocacy_organizations, excluded,
    organized, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(refugee_convention_text__procedural_integrity_reading, state_asylum_adjudicators).
narrative_ontology:fixing_cost_class(refugee_convention_text__procedural_integrity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a transnational procedural standard for assessing refugee status claims: a uniform framework for fair individualized evaluation that states coordinate on to make determination decisions predictable and legally defensible across jurisdictions. Solves the collective-action problem of asylum seekers whose persecution may be transnational and of states' incentives to externalize refugee burdens — coordination on procedure makes the burden distribution predictable.
% TRANSFER_FUNCTION: Moves access to procedural review from universal (all asylum seekers everywhere) to gated (only those the state admits to its system). The procedure itself is the transfer mechanism: those admitted collect access to fair assessment; those excluded or offshore-processed pay the cost (deprivation of the procedure). States collect discretionary gate power in exchange for procedural commitment to those admitted.
% ABSENT_VOICES: Restrictive sovereignty advocates who would argue states should retain discretion over procedure design and that summary determinations are permissible under the Convention. Expansive humanitarian advocates who would argue the Convention mandates material protection regardless of procedure and that procedure is insufficient without substantive refugee status outcomes. Neither is a voice in this reading; both would contest whether procedure or outcome is the Convention's core mandate.
% DISAPPEARANCE_RATIONALE: If this reading vanished (states abandoned commitment to fair individualized assessment for those admitted to asylum systems), the international asylum regime would reorganize around unilateral state determination without procedural constraint. Claimants would have no recourse to independent adjudication; states would have sole discretion over gate, process, and outcome. Entire populations currently protected by procedural safeguards (right to hearing, legal representation, appeal) would lose those protections. The protection floor would collapse to bare non-refoulement (do not return to persecution) without procedural guarantee of fair assessment of whether persecution is real.
% FOUNDING_PROBLEM: Post-WWII refugee crisis required a mechanism to distinguish those fleeing persecution (entitled to asylum) from those fleeing other forms of hardship. No single state could adjudicate fairly under pressure from its own security and resource interests; persecution is often transnational and requires cross-border evaluation. A shared procedural standard was needed to make adjudication defensible across states and protect claimants from wholly arbitrary outcomes based on state convenience or animus.
% FOUNDING_PROBLEM_CORROBORATION: International bodies (UNHCR, treaty monitoring bodies) attest the procedural fairness problem remains live: asylum seekers continue to face summary determinations, inadequate legal representation, and appeals without meaningful independent review in many jurisdictions. Restrictive states and border-security officials attest the foundational problem is solved — they argue the Convention's procedural requirements have been met and the remaining tension is between humanitarian desire and state capacity. Expansive humanitarian advocates attest the foundational problem is not solved procedurally; they argue the problem was always about material protection, and fair procedure that denies most claims fails to solve it. The divergent attestations reflect the reading contest itself; consensus does not exist on whether this founding problem is live, dead, or redefined.
narrative_ontology:disappearance_verdict(refugee_convention_text__procedural_integrity_reading, world_rearranges).
narrative_ontology:founding_problem_status(refugee_convention_text__procedural_integrity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__procedural_integrity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(refugee_convention_text__procedural_integrity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(refugee_convention_text__procedural_integrity_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate (0.42 at interval end) because the constraint protects those it admits (genuine coordination benefit) but defines protection narrowly as procedure, not outcome — states can deny most claims fairly. Suppression is moderate-low (0.38) because the constraint is actively defended by adjudicators and international bodies, but faces resistance from restrictive states and humanitarian advocates who contest whether procedure alone is sufficient protection. Theater ratio rises over time (0.08→0.28) because states increasingly maintain procedural machinery performatively: procedures exist (to satisfy the reading) but operate at accelerated pace with reduced legal representation and fewer substantive review options — the function persists but the form becomes theater. Accessibility collapse is moderate (0.62): once claimants understand they may undergo fair procedure and still be denied (outcome is not guaranteed), alternatives to asylum seeking emerge (irregular residence, family sponsorship, economic migration) — but procedural access itself remains theoretically available to all who reach the territory. Resistance is high (0.71) because the procedural reading faces consistent challenge from restrictive states (wanting more gate discretion) and humanitarian advocates (arguing procedure without protection is meaningless). The measurement series runs on one shared time grid from 1951 (Convention ratification) to 2026 (present); early measurements are projected (1951 reflects founding intent; institutional drift emerges post-1980).
 *
 * PERSPECTIVAL GAP:
 *   The state agenda_setter and the asylum seeker payers experience this constraint radically differently. From the state's position, the constraint is protective-looking: it establishes fair procedures for those admitted (coordination benefit) and gives states discretion over gate and outcome (extractive benefit). From the offshore-processed and excluded populations' positions, the constraint is a structure of differential access: those in territory get procedure; those outside do not. The engine should compute the state as experiencing low extraction (beneficiary position via gate discretion) and the powerless claimants as experiencing higher extraction (targets via gate exclusion). An additional perspectival gap opens between procedural adjudicators (who may genuinely value fair procedure) and institutional administrators (who may experience procedure as performative obligation), affecting theater_ratio interpretation.
 *
 * DIRECTIONALITY LOGIC:
 *   Asylum seekers with access sit at d≈0.25 (low extraction, genuine coordination benefit from fair procedure). Offshore-processed and gate-excluded populations sit at d≈0.85 (high extraction, denied the procedure declared non-negotiable). States sit at d≈0.35 (moderate target, constrained on process for those admitted but retaining gate discretion). The directionality derives from beneficiary/victim declarations: those admitted to procedure are beneficiaries (collect protection); those excluded or offshore-processed are victims (extract the cost via gate exclusion or procedure truncation). No override is needed; the structural derivation captures the reading's dynamics.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy conflation by precisely defining what 'protection' means under this reading: access to fair individualized assessment. The reading does NOT mandate material refugee status or admission; it mandates procedure integrity. This distinction is crucial because it allows states to claim fidelity to the constraint while denying most claims, provided procedures are fair. The mandate (fair procedure for those admitted) has not outlived its function (procedures remain contested and are actively maintained), so mandatrophy has not occurred. However, if theater_ratio continues rising and procedures become increasingly performative (form without substance), mandatrophy may emerge: the mandate's function (fair assessment) would have atrophied while the form (procedures that exist) persists. The measurement trajectory is flagging this risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    procedural_sufficiency_without_outcome,
    'Is a fair procedural framework that denies most claims genuinely protective, or is procedural integrity a cover story for material exclusion when outcome asymmetry is high?',
    'Empirical tracking of asylum approval rates over time and across jurisdictions: if rates remain consistently low despite procedural improvements, the framework may function more as legitimation theater than protection. If rates correlate with claimant quality improvements (refugee crises abate) rather than procedure quality, procedure is correlate, not cause.',
    'If procedure is theater covering material exclusion, the constraint reclassifies from tangled_rope (coordination + extraction) to snare (extraction with procedural cover). If procedure integrity correlates with substantive protection outcomes, it remains tangled_rope (genuine coordination benefit for those admitted, extraction via gate for those excluded).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_sufficiency_without_outcome, empirical, 'Whether procedural access translates to material protection or legitimates denial.').

omega_variable(
    offshore_processing_procedural_equivalence,
    'Can offshore processing arrangements deliver procedurally equivalent review to territorial asylum systems, or is the separation from territory itself an irreducible procedural impediment?',
    'Comparative audit of offshore processing centers (legal representation availability, appeal mechanisms, access to witness testimony, independent adjudication) against territorial systems. If equivalence is achievable, offshore arrangements respecting this reading would not extract. If separation entails structural procedural deficit, offshore processing is categorically extractive under this reading.',
    'If procedurally equivalent, states could satisfy this reading without admitting claimants to territory — the procedure is the protection, location is secondary. If territorial presence is structurally required for fair assessment, offshore processing systematically violates this reading and affects the victim set size.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(offshore_processing_procedural_equivalence, conceptual, 'Whether procedural integrity is location-independent or inherently territorial.').

omega_variable(
    reading_vs_restrictive_sovereignty_foreclosure,
    'Is the procedural integrity reading logically foreclosed by the restrictive sovereignty reading, or do they represent two coherent but incompatible framings of the same kernel?',
    'Trace the textual warrant each reading cites from the Convention. The procedural reading emphasizes ''fair and efficient procedures'' (1951 text); the sovereignty reading emphasizes ''each contracting state shall determine'' (operative discretion language). If both wordings are present and irreducible, the readings coexist; if one reading''s warrant directly contradicts the other''s core premise (procedure is non-negotiable vs. discretion is total), foreclosure obtains.',
    'If coexist: this constraint and the restrictive_sovereignty_reading are two live positions in an ongoing dispute; neither rules out the other logically. If foreclosed: this reading asserts a logical priority that would place sovereignty-maximalist interpretations in systematic violation of the kernel''s binding content. Classification consequence: forecloses vs. coexists_with determines how the engine models sibling relationships.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_restrictive_sovereignty_foreclosure, conceptual, 'Whether procedure-integrity and sovereignty-discretion framings are logically incompatible or merely in political tension.').

omega_variable(
    identity_locked_procedural_doctrine,
    'Are asylum adjudicators identity-locked to the procedural integrity reading through professional training and institutional inertia, or do they actively choose it?',
    'Interview asylum judges and review officers across jurisdictions: Do they articulate a commitment to procedure as intrinsically valuable, or do they describe following rules they were trained to follow? Do they experience pressure to reach predetermined outcomes? If institutional machinery is maintained performatively (procedures exist but are shortcuts), the adjudicators are identity-locked; if procedures are defended as necessary, they hold the reading.',
    'If identity-locked: suppression and theater_ratio measurements underestimate the true extraction because the constraint persists not through conviction but through institutional inertia. The constraint resembles piton more than tangled_rope, even if procedures are formally fair. If actively chosen: the adjudicators are a genuine beneficiary cohort (they protect the integrity they value) and suppression measures reflect real resistance, not theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_procedural_doctrine, empirical, 'Whether procedural commitment reflects institutional identity-lock or active normative choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__procedural_integrity_reading, 1951, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refu_tr_t1951, refugee_convention_text__procedural_integrity_reading, theater_ratio, 1951, 0.08).
narrative_ontology:measurement_basis(refu_tr_t1951, projected).
narrative_ontology:measurement(refu_tr_t1980, refugee_convention_text__procedural_integrity_reading, theater_ratio, 1980, 0.12).
narrative_ontology:measurement_basis(refu_tr_t1980, observed).
narrative_ontology:measurement(refu_tr_t2000, refugee_convention_text__procedural_integrity_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement_basis(refu_tr_t2000, observed).
narrative_ontology:measurement(refu_tr_t2010, refugee_convention_text__procedural_integrity_reading, theater_ratio, 2010, 0.24).
narrative_ontology:measurement_basis(refu_tr_t2010, observed).
narrative_ontology:measurement(refu_tr_t2020, refugee_convention_text__procedural_integrity_reading, theater_ratio, 2020, 0.27).
narrative_ontology:measurement_basis(refu_tr_t2020, observed).
narrative_ontology:measurement(refu_tr_t2026, refugee_convention_text__procedural_integrity_reading, theater_ratio, 2026, 0.28).
narrative_ontology:measurement_basis(refu_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(refu_be_t1951, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 1951, 0.28).
narrative_ontology:measurement_basis(refu_be_t1951, projected).
narrative_ontology:measurement(refu_be_t1980, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 1980, 0.35).
narrative_ontology:measurement_basis(refu_be_t1980, observed).
narrative_ontology:measurement(refu_be_t2000, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 2000, 0.39).
narrative_ontology:measurement_basis(refu_be_t2000, observed).
narrative_ontology:measurement(refu_be_t2010, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 2010, 0.41).
narrative_ontology:measurement_basis(refu_be_t2010, observed).
narrative_ontology:measurement(refu_be_t2020, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 2020, 0.42).
narrative_ontology:measurement_basis(refu_be_t2020, observed).
narrative_ontology:measurement(refu_be_t2026, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 2026, 0.42).
narrative_ontology:measurement_basis(refu_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(refu_su_t1951, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 1951, 0.22).
narrative_ontology:measurement_basis(refu_su_t1951, projected).
narrative_ontology:measurement(refu_su_t1980, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 1980, 0.28).
narrative_ontology:measurement_basis(refu_su_t1980, observed).
narrative_ontology:measurement(refu_su_t2000, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 2000, 0.33).
narrative_ontology:measurement_basis(refu_su_t2000, observed).
narrative_ontology:measurement(refu_su_t2010, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 2010, 0.36).
narrative_ontology:measurement_basis(refu_su_t2010, observed).
narrative_ontology:measurement(refu_su_t2020, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 2020, 0.38).
narrative_ontology:measurement_basis(refu_su_t2020, observed).
narrative_ontology:measurement(refu_su_t2026, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 2026, 0.38).
narrative_ontology:measurement_basis(refu_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__procedural_integrity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(refugee_convention_text__procedural_integrity_reading, 0.12).
narrative_ontology:affects_constraint(refugee_convention_text__procedural_integrity_reading, refugee_convention_text__restrictive_sovereignty_reading).
narrative_ontology:affects_constraint(refugee_convention_text__procedural_integrity_reading, refugee_convention_text__expansive_humanitarian_reading).
narrative_ontology:affects_constraint(refugee_convention_text__procedural_integrity_reading, asylum_determination_infrastructure).
narrative_ontology:affects_constraint(refugee_convention_text__procedural_integrity_reading, offshore_processing_legal_framework).

% DUAL FORMULATION NOTE:
% This constraint is part of a three-reading family instantiating the contested 1951 Refugee Convention kernel. All three readings reference the same text but instantiate different constraints (different ε, different victim sets, different beneficiary structures) through different interpretive frames. The procedural integrity reading (this file) treats procedure as the protection and outcome as secondary; the restrictive sovereignty reading treats state discretion as paramount; the expansive humanitarian reading treats material protection as the referent. They are linked as network neighbors because contests about which reading governs the Convention's force create structural pressure on all three interpretations simultaneously.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
