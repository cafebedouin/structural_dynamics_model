% ============================================================================
% CONSTRAINT STORY: state_killing_legitimacy__retributive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_legitimacy__retributive_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: state_killing_legitimacy__retributive_reading
 *   human_readable: Capital Punishment as Proportional Desert (Retributive Reading)
 *   domain: criminal_justice/political_philosophy/legal_theory
 *
 * SUMMARY:
 *   This story instantiates the retributive reading of the contested
 *   state_killing_legitimacy kernel: the claim that a murderer forfeits their
 *   right to life through proportional desert (lex talionis), such that
 *   execution restores rather than violates the moral order. This is one of
 *   three structurally distinct constraints sharing a single kernel — the
 *   deterrence_reading (execution justified as rational
 *   future-harm-prevention signal) and the abolition_reading (state killing
 *   categorically violates dignity regardless of desert or utility) are
 *   separate constraints with their own ε, their own beneficiary/victim
 *   structure, and their own type. This file does not describe or average
 *   over those readings; it authors only the retributive claim, clean and
 *   ε-invariant, per Rule 1 of the committer frame.
 *
 * KEY AGENTS:
 *   - state_execution_authority: agenda_setter (institutional/analytical) — administers and enforces the forfeiture doctrine
 *   - condemned_offenders: primary payer (powerless/trapped) — bears the sanction the doctrine claims is deserved
 *   - wrongfully_convicted_death_row_prisoners: payer whose case breaks the doctrine's own premise (powerless/trapped)
 *   - victims_families_seeking_desert: beneficiary (moderate/constrained) — the reading's felt constituency
 *   - moral_order_restoration: non-agent beneficiary — the abstract good the reading is organized around
 *   - prosecutors_and_retributive_advocates: beneficiary/agenda_setter (organized/mobile) — professional stake in doctrine persistence
 *   - abolitionist_and_deterrence_advocates: excluded — hold rival premises but do not set sentencing doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__retributive_reading, 0.72).
domain_priors:suppression_score(state_killing_legitimacy__retributive_reading, 0.68).
domain_priors:theater_ratio(state_killing_legitimacy__retributive_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__retributive_reading, tangled_rope).
narrative_ontology:human_readable(state_killing_legitimacy__retributive_reading, "Capital Punishment as Proportional Desert (Retributive Reading)").
narrative_ontology:topic_domain(state_killing_legitimacy__retributive_reading, "criminal_justice/political_philosophy/legal_theory").

domain_priors:requires_active_enforcement(state_killing_legitimacy__retributive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__retributive_reading, '002ec00f-a475-4efc-9d9f-f62eab249522').
narrative_ontology:cs_kernel_codification('002ec00f-a475-4efc-9d9f-f62eab249522', distributed).
narrative_ontology:cs_authority_grounding('002ec00f-a475-4efc-9d9f-f62eab249522', lineage).
narrative_ontology:cs_interpretation_layer_present('002ec00f-a475-4efc-9d9f-f62eab249522').
narrative_ontology:cs_reading_relation('002ec00f-a475-4efc-9d9f-f62eab249522', state_killing_legitimacy__deterrence_reading, coexists_with).
narrative_ontology:cs_reading_relation('002ec00f-a475-4efc-9d9f-f62eab249522', state_killing_legitimacy__abolition_reading, forecloses).
narrative_ontology:cs_axiom('002ec00f-a475-4efc-9d9f-f62eab249522', foundational, culpable_act_forfeits_life_right).
narrative_ontology:cs_axiom_status(culpable_act_forfeits_life_right, holdable).
narrative_ontology:cs_axiom_grounding('002ec00f-a475-4efc-9d9f-f62eab249522', culpable_act_forfeits_life_right, deontological).
narrative_ontology:cs_axiom('002ec00f-a475-4efc-9d9f-f62eab249522', secondary, proportionality_requires_accurate_desert_determination).
narrative_ontology:cs_axiom_status(proportionality_requires_accurate_desert_determination, holdable).
narrative_ontology:cs_axiom_grounding('002ec00f-a475-4efc-9d9f-f62eab249522', proportionality_requires_accurate_desert_determination, empirically_contingent).
narrative_ontology:cs_reference_frame('002ec00f-a475-4efc-9d9f-f62eab249522', lex_talionis_proportional_forfeiture).
narrative_ontology:cs_drift_state('002ec00f-a475-4efc-9d9f-f62eab249522', post_dna_exoneration_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('002ec00f-a475-4efc-9d9f-f62eab249522', '').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__retributive_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__retributive_reading, moral_order_restoration).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__retributive_reading, victims_families_seeking_desert).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__retributive_reading, retributive_justice_system_legitimacy).
narrative_ontology:constraint_victim(state_killing_legitimacy__retributive_reading, condemned_offenders).
narrative_ontology:constraint_victim(state_killing_legitimacy__retributive_reading, wrongfully_convicted_death_row_prisoners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__retributive_reading, prosecutors_and_retributive_advocates).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__retributive_reading, proportional_desert_doctrine).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__retributive_reading, moral_forfeiture_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers capital sentencing and carries out executions under statutory authority. Justifies the practice as restoring moral balance disrupted by the murder — the offender's own act, by this reading, forfeits the right to life. Controls charging decisions, appeals processes, and clemency review, and thereby controls whether and when the forfeiture is enforced.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, state_execution_authority, agenda_setter,
    institutional, generational, analytical, national).

% Have been convicted of murder and sentenced to death. Under this reading their culpable act is what generates the forfeiture — the constraint treats their claim to continued life as morally extinguished by their own conduct. They can appeal, seek clemency, or await exoneration, but cannot exit the sentence once affirmed; the reading treats this narrowing of options as deserved rather than imposed.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, condemned_offenders, payer,
    powerless, biographical, trapped, national).

% Have not committed the underlying act but are classified identically to guilty offenders by the same forfeiture logic once convicted. The retributive reading's legitimacy depends entirely on actual desert; for this group the desert premise is false, and they bear the full weight of the sanction the reading claims is proportionate.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, wrongfully_convicted_death_row_prisoners, payer,
    powerless, biographical, trapped, national).

% Have lost a family member to murder and, under this reading, are owed a proportionate moral reckoning — the offender's execution answers the wrong done to them. Some describe the execution as providing closure; the reading treats this as vindication of desert rather than as therapeutic outcome, and their felt need for reckoning is cited to justify the sentence regardless of whether execution in fact provides psychological resolution.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, victims_families_seeking_desert, beneficiary,
    moderate, biographical, constrained, national).

% The abstract moral-order rebalancing the retributive reading claims execution accomplishes — the notion that proportional punishment restores an equilibrium disturbed by the crime. Named for completeness as the non-agent good the reading is organized around; it collects no rents itself but grounds the legitimacy claim under which the state and the case for desert operate.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, moral_order_restoration, beneficiary,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(state_killing_legitimacy__retributive_reading, moral_order_restoration).

% Build careers and institutional standing on securing and defending capital sentences, framing successful prosecution as delivering deserved justice. Their professional incentives align with the forfeiture doctrine's persistence; they can exit into other legal practice, but the doctrine's continuation sustains a distinct professional track (capital litigation, capital appellate defense-adjacent specialization) that would not exist without it.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, prosecutors_and_retributive_advocates, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(state_killing_legitimacy__retributive_reading, prosecutors_and_retributive_advocates, agenda_setter).

% Hold that state killing is either categorically impermissible regardless of desert (abolition) or justified only by forward-looking prevention rather than backward-looking desert (deterrence). Within a legal system operating under the retributive reading, their competing premises are litigated but the desert-forfeiture logic remains the operative legitimacy claim in sentencing statutes and appellate doctrine that explicitly invoke proportionality and desert.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, abolitionist_and_deterrence_advocates, excluded,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_legitimacy__retributive_reading, diffuse).
narrative_ontology:fixing_cost_class(state_killing_legitimacy__retributive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, publicly legible standard for when the state may permissibly kill a citizen: proportional forfeiture tied to the gravity and moral culpability of the underlying act, rather than ad hoc or purely discretionary killing.
% TRANSFER_FUNCTION: Moves the offender's continued existence — the ultimate stake — from the offender to the state's execution authority, justified as restoring a moral balance the murder disturbed; families of victims receive symbolic and sometimes psychological vindication without bearing the sanction's execution cost.
% ABSENT_VOICES: Deterrence advocates would object that desert alone, absent any showing of prevented future harm, cannot justify killing; abolitionists would object that no proportionality calculus can license the state taking a life at all. Both groups participate in the broader legal and political debate but are structurally excluded from the sentencing doctrine itself, which is written and applied on retributive-proportionality grounds.
% DISAPPEARANCE_RATIONALE: If the desert-forfeiture doctrine were removed from capital sentencing law overnight, existing death sentences would lose their primary stated justification, appellate courts would need a new legitimating theory (deterrence, incapacitation, or abolition) to sustain or overturn pending executions, and capital litigation practice built around proportionality argument would need to reorganize entirely around a different doctrinal axis.
% FOUNDING_PROBLEM: Pre-modern and early modern legal systems needed a principled, non-arbitrary limit on state violence in response to homicide — a rule that punishment should track the gravity of the offense rather than the sovereign's unconstrained will, replacing blood feud and disproportionate collective retaliation.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and retributivist philosophers (outside the direct beneficiary set of prosecutors and victims'-rights organizations) attest the proportionality-limiting function was genuinely operative in curbing arbitrary sovereign violence historically. Wrongful-conviction exoneration data, compiled by innocence-project researchers and post-conviction DNA testing programs external to prosecutorial institutions, corroborates that the desert premise fails in a measurable, non-trivial fraction of capital cases — undercutting the claim that the doctrine currently operates as advertised rather than as inherited sentencing architecture.
narrative_ontology:disappearance_verdict(state_killing_legitimacy__retributive_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_legitimacy__retributive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_legitimacy__retributive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_killing_legitimacy__retributive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_legitimacy__retributive_reading, 0.72, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_legitimacy__retributive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_legitimacy__retributive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_legitimacy__retributive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.72 at interval end) because the retributive reading licenses the state's most severe and irreversible sanction on a desert claim that cannot be corrected after execution — any error in the desert determination (wrongful conviction) converts what the reading calls proportionate justice into pure, unrecoverable extraction from an innocent party. Suppression is substantial (0.68) because the doctrine requires active appellate and executive machinery to sustain executions against constitutional challenge, clemency petitions, and abolitionist advocacy. Theater ratio rises over the interval (0.20 to 0.40) reflecting a documented pattern: as exoneration evidence accumulates and DNA-based post-conviction review expands, capital sentencing increasingly persists through procedural ritual (extended appeals, clemency boards that rarely grant relief) rather than through confidence that desert has been correctly established in each case. All three tracked metrics are authored on the single shared time grid (0-40) per the alignment rule.
 *
 * PERSPECTIVAL GAP:
 *   From the state execution authority's seat, the constraint is coordination: a principled, proportionality-bounded limit on state violence, an improvement over arbitrary retaliation. From the condemned offender's seat — and especially the wrongfully convicted offender's seat — the same structure is the state's most severe extractive act, irreversible and, for the innocent, entirely unjustified by the reading's own desert criterion. The engine computes this divergence from the structural power/exit data; the retributive claim itself does not resolve it.
 *
 * DIRECTIONALITY LOGIC:
 *   The state execution authority and prosecutors are structural beneficiaries/agenda-setters: they administer, enforce, and derive institutional and professional legitimacy from the doctrine's operation (d near the beneficiary end). Condemned offenders sit at the target end of directionality by the reading's own logic — but the reading's legitimacy is entirely conditional on the desert premise being true. Wrongfully convicted prisoners are the structurally decisive case: they are classified identically to guilty offenders by the forfeiture logic, yet the desert premise the entire reading depends on is false for them, making their directionality that of a pure, undiluted target with no offsetting justification available even within the reading's own terms. Victims' families are beneficiaries of the reading's symbolic function without bearing its enforcement costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — curbing arbitrary, disproportionate sovereign violence in response to homicide — is genuinely contested as live or dead: proportionality principles have been substantially absorbed into ordinary (non-capital) sentencing law, which raises the question of whether capital punishment specifically still serves a function that graduated incarceration does not. Where the desert premise is correct, the reading resists mandatrophy by tying the sanction tightly to actual culpability. Where wrongful conviction is later established, the doctrine's mandate has been retroactively falsified for that case even as the institutional machinery (courts, prosecutors, statutes) continues operating as though desert had been correctly determined — the classic signature of mandatrophy: mandate outliving verified function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    desert_accuracy_dependency,
    'Does the retributive reading''s legitimacy claim survive at the rate wrongful capital convictions actually occur, or does empirical error-rate data undermine the desert premise the entire reading depends on?',
    'Compile post-conviction DNA exoneration rates and documented wrongful-execution cases across capital jurisdictions; compare against the reading''s implicit claim that desert determinations are reliable enough to justify an irreversible sanction.',
    'If wrongful conviction rates are non-trivial and not fully correctable pre-execution, the retributive reading''s own internal logic (forfeiture requires actual desert) is violated by its practice, converting a claimed proportionate-justice mechanism into a documented extraction mechanism for a known subset of cases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(desert_accuracy_dependency, empirical, 'Whether the doctrine''s desert premise holds at observed real-world accuracy rates.').

omega_variable(
    moral_forfeiture_vs_constructed_legitimacy,
    'Is proportional forfeiture a genuine feature of moral desert independent of institutional interest, or is it a constructed legitimating doctrine that happens to also serve prosecutorial and state institutional interests in retaining capital sentencing authority?',
    'Comparative jurisprudence: examine whether jurisdictions that abolish capital punishment while retaining strong retributive sentencing philosophy elsewhere show doctrinal instability in the forfeiture claim, versus whether the claim is treated as freestanding moral truth independent of institutional stakes.',
    'If constructed, the beneficiary structure (prosecutors, state authority) suggests partial regulatory-capture-like dynamics in doctrine maintenance rather than pure moral philosophy; if genuine, the high ε reflects a real cost of enacting a correct moral principle rather than extraction dressed as principle.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(moral_forfeiture_vs_constructed_legitimacy, conceptual, 'Whether desert-forfeiture is free-standing moral fact or institutionally interested doctrine.').

omega_variable(
    reading_choice_location_of_disagreement,
    'Where exactly do the three kernel readings disagree — is it about whether desert exists at all, whether desert (if it exists) is sufficient justification absent deterrent effect, or whether any justification could license state killing?',
    'This is inherently a conceptual/framing question resolved by which premise a given legal or moral framework treats as foundational; it is not resolvable by additional empirical data alone.',
    'Clarifies that the retributive reading and deterrence reading could both be held by a single retentionist framework (desert AND deterrence as independent justifications), while the abolition reading is structurally incompatible with either — this shapes which reading_relations below are coexists_with versus forecloses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_choice_location_of_disagreement, conceptual, 'Locating the precise premise-level disagreement among sibling kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__retributive_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_legitimacy__retributive_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(stat_tr_t8, state_killing_legitimacy__retributive_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement(stat_tr_t16, state_killing_legitimacy__retributive_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement(stat_tr_t24, state_killing_legitimacy__retributive_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(stat_tr_t32, state_killing_legitimacy__retributive_reading, theater_ratio, 32, 0.38).
narrative_ontology:measurement(stat_tr_t40, state_killing_legitimacy__retributive_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_legitimacy__retributive_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(stat_be_t8, state_killing_legitimacy__retributive_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(stat_be_t16, state_killing_legitimacy__retributive_reading, base_extractiveness, 16, 0.64).
narrative_ontology:measurement(stat_be_t24, state_killing_legitimacy__retributive_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement(stat_be_t32, state_killing_legitimacy__retributive_reading, base_extractiveness, 32, 0.7).
narrative_ontology:measurement(stat_be_t40, state_killing_legitimacy__retributive_reading, base_extractiveness, 40, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_legitimacy__retributive_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(stat_su_t8, state_killing_legitimacy__retributive_reading, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(stat_su_t16, state_killing_legitimacy__retributive_reading, suppression_requirement, 16, 0.6).
narrative_ontology:measurement(stat_su_t24, state_killing_legitimacy__retributive_reading, suppression_requirement, 24, 0.63).
narrative_ontology:measurement(stat_su_t32, state_killing_legitimacy__retributive_reading, suppression_requirement, 32, 0.66).
narrative_ontology:measurement(stat_su_t40, state_killing_legitimacy__retributive_reading, suppression_requirement, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_legitimacy__retributive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_legitimacy__retributive_reading, state_killing_legitimacy__deterrence_reading).
narrative_ontology:affects_constraint(state_killing_legitimacy__retributive_reading, state_killing_legitimacy__abolition_reading).

% DUAL FORMULATION NOTE:
% This file is one of three constraints decomposing the natural-language concept 'legitimacy of capital punishment' per the ε-invariance principle. The retributive_reading authors a backward-looking desert-based legitimacy claim (offender enters the victim set as the one who forfeited, high ε from desert logic applied to an irreversible sanction); the deterrence_reading authors a forward-looking prevention claim with different beneficiary/victim structure (future potential victims as beneficiaries, indifference to desert accuracy); the abolition_reading treats the entire practice as illegitimate regardless of desert or deterrence, making its own ε near-total for the offender and negative for no one, since it endorses no execution at all. These are linked via affects_constraints, not merged, because their ε values, beneficiary structures, and classifications genuinely differ.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
