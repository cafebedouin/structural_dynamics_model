% ============================================================================
% CONSTRAINT STORY: constitutional_interpretive_authority__coordinate_construction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coordinate_construction_reading, []).

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
 *   constraint_id: constitutional_interpretive_authority__coordinate_construction_reading
 *   human_readable: Dispersed Constitutional Interpretive Authority (Coordinate Construction Reading)
 *   domain: constitutional law/political theory/jurisprudence
 *
 * SUMMARY:
 *   The constraint under authorship is the coordinate construction reading of
 *   constitutional interpretive authority: no branch holds final power to fix
 *   the constitution's meaning, and constitutional content emerges from
 *   continuing dialogue among court, legislature, and executive, with
 *   disputes settled through political mechanisms (amendment, appointment,
 *   budget control, jurisdiction) rather than singular adjudication. The
 *   arrangement solves a real collective-action problem — preventing capture
 *   of constitutional meaning by any single institution — while imposing real
 *   asymmetric costs: litigants wait indefinitely for answers that may arrive
 *   twice with opposite signs, and future electorates inherit interpreters
 *   chosen by coalitions they could not vote in or against. Claim and metrics
 *   are authored independently: I claim tangled_rope because I believe the
 *   structure genuinely coordinates (multi-forum anti-capture dispersion)
 *   while genuinely extracting (certainty from finality-seekers, interpretive
 *   lock-in from the unborn), and I authored the metric values as my best
 *   descriptive estimates of how the arrangement actually operates, without
 *   tuning either to the other or to a predicted engine output.
 *
 * KEY AGENTS:
 *   - - apex_constitutional_court: Co-agenda-setter (institutional/constrained) — issues rulings that bind only while the other branches fund, enforce, and obey them
 *   - - national_legislature: Co-agenda-setter (institutional/immediate) — controls amendment, jurisdiction, budget, and confirmation; answers rulings it dislikes with countermeasures rather than obedience
 *   - - executive_branch: Co-agenda-setter (institutional/biographical) — converts appointment power into the most durable interpretive leverage in the system
 *   - - civil_rights_organizations: Primary beneficiary (organized/constrained) — exploits forum pluralism; bears the cost of running parallel campaigns across venues
 *   - - subnational_governments: Secondary beneficiary and incidental payer (organized/generational) — litigates from dispersion while absorbing the costs of programs whose legal basis keeps shifting
 *   - - ordinary_litigants_seeking_finality: Primary target (moderate/trapped) — waits on contested questions they cannot route around or resolve themselves
 *   - - future_electorates: Structural target (powerless/trapped) — inherits interpreters and settlements fixed before they could participate
 *   - - territorial_residents_without_full_representation: Excluded voice (powerless/trapped) — governed by every settlement, seated in none of the dialogues
 *   - - constitutional_scholars: Analytical observer (analytical/analytical) — maps the structure and supplies the vocabulary all sides argue in
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_interpretive_authority__coordinate_construction_reading, 0.46).
domain_priors:suppression_score(constitutional_interpretive_authority__coordinate_construction_reading, 0.39).
domain_priors:theater_ratio(constitutional_interpretive_authority__coordinate_construction_reading, 0.24).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__coordinate_construction_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 0.39).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 0.24).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__coordinate_construction_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__coordinate_construction_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_interpretive_authority__coordinate_construction_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_interpretive_authority__coordinate_construction_reading, "Dispersed Constitutional Interpretive Authority (Coordinate Construction Reading)").
narrative_ontology:topic_domain(constitutional_interpretive_authority__coordinate_construction_reading, "constitutional law/political theory/jurisprudence").

domain_priors:requires_active_enforcement(constitutional_interpretive_authority__coordinate_construction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_interpretive_authority__coordinate_construction_reading, 'a1930335-76c0-45ea-91d0-01ff41741df1').
narrative_ontology:cs_kernel_codification('a1930335-76c0-45ea-91d0-01ff41741df1', fixed_text).
narrative_ontology:cs_authority_grounding('a1930335-76c0-45ea-91d0-01ff41741df1', distributed).
narrative_ontology:cs_reading_relation('a1930335-76c0-45ea-91d0-01ff41741df1', constitutional_interpretive_authority__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('a1930335-76c0-45ea-91d0-01ff41741df1', constitutional_interpretive_authority__parliamentary_supremacy_reading, forecloses).
narrative_ontology:cs_axiom('a1930335-76c0-45ea-91d0-01ff41741df1', foundational, no_branch_holds_final_interpretive_authority).
narrative_ontology:cs_axiom_status(no_branch_holds_final_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('a1930335-76c0-45ea-91d0-01ff41741df1', no_branch_holds_final_interpretive_authority, conventional).
narrative_ontology:cs_axiom('a1930335-76c0-45ea-91d0-01ff41741df1', foundational, meaning_emerges_from_inter_branch_contestation).
narrative_ontology:cs_axiom_status(meaning_emerges_from_inter_branch_contestation, holdable).
narrative_ontology:cs_axiom_grounding('a1930335-76c0-45ea-91d0-01ff41741df1', meaning_emerges_from_inter_branch_contestation, instrumental).
narrative_ontology:cs_reference_frame('a1930335-76c0-45ea-91d0-01ff41741df1', departmental_coexistence_baseline).
narrative_ontology:cs_drift_state('a1930335-76c0-45ea-91d0-01ff41741df1', contemporary_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a1930335-76c0-45ea-91d0-01ff41741df1', '').
narrative_ontology:cs_kernel_id(constitutional_interpretive_authority__coordinate_construction_reading, constitutional_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__coordinate_construction_reading, apex_constitutional_court).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__coordinate_construction_reading, national_legislature).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__coordinate_construction_reading, executive_branch).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__coordinate_construction_reading, civil_rights_organizations).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__coordinate_construction_reading, subnational_governments).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__coordinate_construction_reading, ordinary_litigants_seeking_finality).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__coordinate_construction_reading, future_electorates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__coordinate_construction_reading, subnational_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decides constitutional questions brought before it and publishes reasoned rulings that other institutions treat as weighty precedent. Its orders take effect only when the legislature funds them, the executive enforces them, and subordinate officials comply; each dependency is a point where the other branches can push back. Members serve long terms and know their successors will inherit whatever accommodation the branches reach today.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, apex_constitutional_court, agenda_setter,
    institutional, generational, constrained, national).

% Writes statutes, controls the amendment procedure, sets court budgets and jurisdiction, and confirms or blocks appointments to the bench. When it dislikes a ruling it can answer with new legislation, jurisdictional limits, or appointment obstruction rather than simple obedience. Its members face short electoral cycles, so its moves tend to be tactical and reversible.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, national_legislature, agenda_setter,
    institutional, immediate, constrained, national).

% Chooses whom to nominate to the bench, decides which rulings to enforce vigorously or slowly, and directs the government's own litigation positions. A single term of nominations can shift constitutional doctrine for decades, which makes the appointment channel the most durable prize available in inter-branch contests.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, executive_branch, agenda_setter,
    institutional, biographical, constrained, national).

% Pursue their goals through whichever institution is currently receptive — litigation when the bench is favorable, legislation and mobilization when it is not. Forum pluralism lowers the cost of losing in any one venue, but sustaining parallel campaigns across several venues requires resources that smaller groups lack.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, civil_rights_organizations, beneficiary,
    organized, biographical, constrained, national).

% Sue the national government, assert their own readings of divided powers, and exploit the openings that inter-branch disagreement creates. They also absorb the costs of national programs whose legal basis shifts with each new accommodation, since their obligations persist while the justification for them is renegotiated overhead.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, subnational_governments, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__coordinate_construction_reading, subnational_governments, payer).

% Bring cases whose outcomes turn on constitutional questions the branches have left open. Their lives and transactions wait on answers that arrive late, arrive qualified, and sometimes arrive twice with opposite signs. They cannot leave the legal system while their case is pending and cannot choose which branch settles their question.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, ordinary_litigants_seeking_finality, payer,
    moderate, immediate, trapped, local).

% Will live under constitutional meanings shaped by nominees selected by today's coalitions and entrenched through long tenure. They cast no vote in the contests that determine their interpreters and have no way to revisit the settlements reached before they could participate.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, future_electorates, payer,
    powerless, generational, trapped, national).

% Are governed by the resulting interpretations — in criminal law, taxation, and civil status — while holding little or no vote in any of the branches doing the interpreting and limited standing to bring their own challenges. Every seat in the dialogue is occupied by someone else.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, territorial_residents_without_full_representation, excluded,
    powerless, biographical, trapped, regional).

% Map how authority moves among the branches, compare jurisdictions, and supply the vocabularies in which the branches justify their moves. They collect no direct proceeds and bear no direct burdens; every side cites their analyses.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_interpretive_authority__coordinate_construction_reading, executive_branch).
narrative_ontology:fixing_cost_class(constitutional_interpretive_authority__coordinate_construction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents any single institution from fixing constitutional meaning unilaterally; routes disputes between branches into shared mechanisms — amendment, appointment, budget, jurisdiction — where each branch's leverage checks the others, and preserves multiple access points so no political interest depends on a single forum.
% TRANSFER_FUNCTION: Moves interpretive leverage among the branches with each political alignment; moves the costs of unresolved questions onto litigants awaiting outcomes and onto future electorates whose interpreters are nominated by present coalitions; concentrates appointment leverage in chief executives and confirming senators.
% ABSENT_VOICES: Territorial and disenfranchised populations are governed by the resulting interpretations but hold no vote in any branch and limited standing to sue; ordinary litigants whose cases become doctrinal vehicles never consented to that role. Both sit outside every forum the arrangement maintains.
% DISAPPEARANCE_RATIONALE: If dispersion vanished overnight, whichever branch moved first would consolidate finality — rulings would become self-executing or statutes unreviewable — appointment politics would lose its central prize, interest groups would reroute strategy toward the surviving forum, and amendment campaigns would reorganize around the new locus of constitutional meaning.
% FOUNDING_PROBLEM: The founding-era problem of concentrated interpretive power: whichever institution could fix the constitution's meaning would hold a master key over all the others. The designers distributed the power to interpret alongside the powers to legislate, execute, and adjudicate, betting that mutual dependence would hold the meaning open.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians document the anti-consolidation design intent from ratification debates written before branch interests had crystallized; comparative scholars observe newly democratized states repeatedly adopting dispersed models after experiencing authoritarian concentration; and the proponents of both sibling readings concede the anti-capture rationale while disputing its institutional form. Corroboration exists outside the beneficiary set.
narrative_ontology:disappearance_verdict(constitutional_interpretive_authority__coordinate_construction_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_interpretive_authority__coordinate_construction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_interpretive_authority__coordinate_construction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_interpretive_authority__coordinate_construction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_interpretive_authority__coordinate_construction_reading, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_interpretive_authority__coordinate_construction_reading_tests).
:- end_tests(constitutional_interpretive_authority__coordinate_construction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.46 at interval end) because the arrangement's transfers are real but cyclical rather than continuous: certainty is drained from finality-seekers during consolidation crises and partially repaid in equilibria, and the appointment channel continuously converts present electoral wins into future interpretive control. Suppression (0.39) is authored as a raw structural property, unscaled by power or scope — the coercive machinery here is intra-elite mutual deterrence (defiance met with budget cuts, curbing bills met with compliance strikes), not popular coercion, and the sibling readings remain legally expressible alternatives. Theater ratio (0.24) is low in equilibrium and spikes during crises, when institutional position-taking is performed for public audiences rather than aimed at opponents. Accessibility collapse is low (0.30): understanding this arrangement does not close off its rivals — a participant persuaded of dispersion can still campaign for judicial or parliamentary supremacy through the same institutions, which is unusual openness for a governing constraint. Resistance is high (0.60): every branch periodically attempts escape toward finality, and scholars and reformers attack the arrangement from both directions. The measurement series run on one shared time grid (all three metrics at all seven points) and document a full cycle: consolidation attempt (T5-T10) -> crisis peak -> rebalancing (T15) -> equilibrium (T20) -> second attempt on a different axis (T25) -> partial accommodation (T30). The oscillation is plausibly partly an extraction mechanism — each crisis re-prices interpretive leverage and intermittently reinforces every branch's dependence on the others — which is why the endogeneity omega is attached.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the branch seats should compute differently. From the ordinary litigant's position the arrangement is experienced as endless deferral — a structure that consumes their case as fuel for other institutions' contest and returns no answer; from a branch's position the same structure is preserved leverage — each branch retains interpretive power it would entirely lose under either sibling reading. Future electorates occupy the extreme: maximal extraction with zero participation. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   The three branches are declared beneficiaries because each retains, under dispersion, interpretive power it would forfeit under judicial or parliamentary supremacy — relative to both sibling arrangements they are net gainers. But the pure beneficiary declaration understates their burden: each branch is compliance-dependent (court needs funding and enforcement; legislature needs its statutes to survive review; executive needs its nominees confirmed), absorbs the labor of perpetual contestation, and periodically loses rounds outright. Hence the directionality override for the institutional power atom, lifting d from the near-floor value the beneficiary declarations would derive to 0.30 — beneficiary-leaning but materially burdened. Civil rights organizations derive low d from their beneficiary position (forum pluralism subsidizes them). Subnational governments carry a dual position — beneficiary through litigation opportunity, payer through shifting program obligations — and the derivation should place them between the pure beneficiaries and the targets. Ordinary litigants and future electorates derive high d near the full-target end; future electorates sit at the extreme because they combine victim status with total absence of exit or voice.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing concentrated interpretive power — remains live under every reading of the kernel, including the siblings', so there is no mandate outliving its function and no mandatrophy to resolve; the status=live x verdict=world_rearranges pairing is internally consistent and should not trip the zombie flag. The classification discipline matters here in both directions: labeling this arrangement a snare would erase the genuine coordination function (multi-forum access genuinely protects interests that single-forum consolidation would expose), while labeling it a rope would erase the asymmetric costs borne by finality-seekers and future electorates — costs that are not coordination overhead but transfers convertible into branch leverage. Tangled rope holds both facts without letting either cancel the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_indexicality_of_authority_location,
    'This constraint is one reading of the kernel constitutional_interpretive_authority (the coordinate construction reading). What changes structurally if a sibling reading — judicial_supremacy_reading or parliamentary_supremacy_reading — is adopted instead?',
    'Adoption of a sibling reading consolidates final interpretive authority in one branch: the multi-forum structure collapses to a single agenda-setting seat, appointment politics loses its central prize, and the victim set contracts (litigants gain finality) while the displaced branches become the new targets.',
    'Under either sibling, this story''s beneficiary set inverts (two branches lose the leverage they currently retain), the coordination function narrows to single-adjudicator consistency, and the classification migrates toward whichever consolidated-authority profile the adopted sibling instantiates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_indexicality_of_authority_location, conceptual, 'Committer-frame omega: this story instantiates one reading of a contested kernel; sibling readings are separate constraints with different epsilon, victim sets, and enforcement profiles.').

omega_variable(
    de_facto_consolidation_trajectory,
    'Is the coordinate arrangement stable, or does administrative practice converge on de facto judicial supremacy regardless of the official dispersed doctrine?',
    'Longitudinal compliance tracking: measure how often legislatures fund and obey rulings they publicly opposed, and how often executives enforce judgments against their own policy preferences, across successive political alignments.',
    'If convergence is real, this reading describes a fading arrangement drifting toward its judicial-supremacy sibling; the drift_state''s practice_drift vector deepens and the extraction profile migrates toward the consolidated-authority constraint''s.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(de_facto_consolidation_trajectory, empirical, 'Whether dispersion survives in practice or has already consolidated informally in one branch.').

omega_variable(
    crisis_cycle_endogeneity,
    'Are the recurring consolidation crises (open defiance, court-curbing, appointment warfare) endogenous to the dispersed arrangement itself, or exogenous products of external political polarization?',
    'Comparative analysis across jurisdictions that hold dispersion constant while varying polarization levels; if crisis frequency tracks polarization rather than dispersion, the cycle is imported.',
    'If endogenous, the oscillation is itself an extraction mechanism (each crisis re-prices interpretive leverage and intermittently reinforces branch dependence) and epsilon should be read at crisis-adjusted levels; if exogenous, the arrangement is a transmission belt rather than a generator.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crisis_cycle_endogeneity, empirical, 'Whether the measured cyclical dynamics are produced by the constraint or merely pass through it.').

omega_variable(
    appointment_channel_capture_depth,
    'How much of the arrangement''s effective extraction flows through the appointment channel, which converts present electoral victories into generational interpretive control?',
    'Quantitative study of the divergence between appointing coalitions'' expected doctrine and appointees'' subsequent voting behavior, plus counterfactual modeling of term-limited or lotteried selection.',
    'High capture depth means the arrangement functions as a delayed-consolidation machine for whichever coalition holds the executive, raising effective extraction on future_electorates well above the base measure; low depth supports the genuine-dispersion framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appointment_channel_capture_depth, empirical, 'Depth of the appointment channel as the mechanism converting dispersion into durable capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_interpretive_authority__coordinate_construction_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coordinate_construction_tr_t0, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(coordinate_construction_tr_t5, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 5, 0.26).
narrative_ontology:measurement(coordinate_construction_tr_t10, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 10, 0.34).
narrative_ontology:measurement(coordinate_construction_tr_t15, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 15, 0.28).
narrative_ontology:measurement(coordinate_construction_tr_t20, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(coordinate_construction_tr_t25, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 25, 0.3).
narrative_ontology:measurement(coordinate_construction_tr_t30, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 30, 0.24).

% Extraction over time
narrative_ontology:measurement(coordinate_construction_be_t0, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(coordinate_construction_be_t5, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(coordinate_construction_be_t10, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(coordinate_construction_be_t15, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 15, 0.47).
narrative_ontology:measurement(coordinate_construction_be_t20, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(coordinate_construction_be_t25, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 25, 0.52).
narrative_ontology:measurement(coordinate_construction_be_t30, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 30, 0.46).

% Suppression requirement over time
narrative_ontology:measurement(coordinate_construction_su_t0, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(coordinate_construction_su_t5, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 5, 0.44).
narrative_ontology:measurement(coordinate_construction_su_t10, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(coordinate_construction_su_t15, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 15, 0.4).
narrative_ontology:measurement(coordinate_construction_su_t20, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 20, 0.34).
narrative_ontology:measurement(coordinate_construction_su_t25, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 25, 0.46).
narrative_ontology:measurement(coordinate_construction_su_t30, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 30, 0.39).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_interpretive_authority__coordinate_construction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__coordinate_construction_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__coordinate_construction_reading, parliamentary_supremacy_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial question 'who interprets the constitution?' covers three structurally distinct authority arrangements — coordinate construction (this file), judicial supremacy, and parliamentary supremacy. Each has its own epsilon, victim set, and enforcement profile; forcing them into one story would make epsilon observer-dependent. The coordinate reading is the historical baseline from which the supremacy readings emerge as consolidation claims, and each supremacy reading cites documented failures of dispersion as evidence for consolidation — hence the edges run from this story to both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_interpretive_authority__coordinate_construction_reading, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
