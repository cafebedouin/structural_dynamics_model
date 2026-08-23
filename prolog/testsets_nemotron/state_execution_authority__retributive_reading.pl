% ============================================================================
% CONSTRAINT STORY: state_execution_authority__retributive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_execution_authority__retributive_reading, []).

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
 *   constraint_id: state_execution_authority__retributive_reading
 *   human_readable: State Execution Authority — Retributive Reading
 *   domain: criminal_justice/constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint story instantiates the RETRIBUTIVE READING of the
 *   contested kernel 'state_execution_authority.' The kernel is the state's
 *   claimed authority to execute; the retributive reading grounds that
 *   authority in moral desert and proportional punishment for heinous crimes.
 *   The reading declares that execution restores moral balance — a claim that
 *   cannot be satisfied by imprisonment (high ε from non-substitutability).
 *   Victims' families are the primary moral beneficiaries; the executed
 *   offender is the legitimate cost; wrongful execution is a tragic error
 *   rate that does not invalidate the framework. The sibling readings are the
 *   deterrence reading (execution prevents future murders) and the abolition
 *   reading (execution is categorically impermissible). This story models
 *   only the retributive reading's structural claims — its ε, its
 *   beneficiary/victim structure, its type — as a clean ε-invariant
 *   constraint per Rule 1.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__retributive_reading, 0.68).
domain_priors:suppression_score(state_execution_authority__retributive_reading, 0.72).
domain_priors:theater_ratio(state_execution_authority__retributive_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__retributive_reading, tangled_rope).
narrative_ontology:human_readable(state_execution_authority__retributive_reading, "State Execution Authority — Retributive Reading").
narrative_ontology:topic_domain(state_execution_authority__retributive_reading, "criminal_justice/constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(state_execution_authority__retributive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__retributive_reading, '0e4ca920-3c2f-4bba-95b2-8111506adef7').
narrative_ontology:cs_kernel_codification('0e4ca920-3c2f-4bba-95b2-8111506adef7', formalized).
narrative_ontology:cs_authority_grounding('0e4ca920-3c2f-4bba-95b2-8111506adef7', lineage).
narrative_ontology:cs_interpretation_layer_present('0e4ca920-3c2f-4bba-95b2-8111506adef7').
narrative_ontology:cs_reading_relation('0e4ca920-3c2f-4bba-95b2-8111506adef7', state_execution_authority__deterrence_reading, coexists_with).
narrative_ontology:cs_reading_relation('0e4ca920-3c2f-4bba-95b2-8111506adef7', state_execution_authority__abolition_reading, forecloses).
narrative_ontology:cs_axiom('0e4ca920-3c2f-4bba-95b2-8111506adef7', foundational, proportional_punishment_requires_death_for_heinous_crimes).
narrative_ontology:cs_axiom_status(proportional_punishment_requires_death_for_heinous_crimes, holdable).
narrative_ontology:cs_axiom_grounding('0e4ca920-3c2f-4bba-95b2-8111506adef7', proportional_punishment_requires_death_for_heinous_crimes, deontological).
narrative_ontology:cs_axiom('0e4ca920-3c2f-4bba-95b2-8111506adef7', secondary, wrongful_execution_is_tragic_error_not_structural_defeater).
narrative_ontology:cs_axiom_status(wrongful_execution_is_tragic_error_not_structural_defeater, holdable).
narrative_ontology:cs_axiom_grounding('0e4ca920-3c2f-4bba-95b2-8111506adef7', wrongful_execution_is_tragic_error_not_structural_defeater, instrumental).
narrative_ontology:cs_reference_frame('0e4ca920-3c2f-4bba-95b2-8111506adef7', classical_retributive_authority).
narrative_ontology:cs_drift_state('0e4ca920-3c2f-4bba-95b2-8111506adef7', contemporary_constitutional_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0e4ca920-3c2f-4bba-95b2-8111506adef7', '').
narrative_ontology:cs_kernel_id(state_execution_authority__retributive_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_execution_authority__retributive_reading, victims_families).
narrative_ontology:constraint_beneficiary(state_execution_authority__retributive_reading, retributive_legal_tradition).
narrative_ontology:constraint_beneficiary(state_execution_authority__retributive_reading, capital_prosecutors).
narrative_ontology:constraint_victim(state_execution_authority__retributive_reading, executed_offenders).
narrative_ontology:constraint_victim(state_execution_authority__retributive_reading, wrongfully_convicted_death_row).
narrative_ontology:constraint_victim(state_execution_authority__retributive_reading, execution_staff).
narrative_ontology:constraint_vindicates(state_execution_authority__retributive_reading, proportional_punishment_doctrine).
narrative_ontology:constraint_vindicates(state_execution_authority__retributive_reading, moral_desert_theory).
narrative_ontology:constraint_vindicates(state_execution_authority__retributive_reading, state_sovereignty_over_life_death).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Families of murder victims who experience the execution as moral closure — the state's imposition of proportionate punishment restores a balance they experience as shattered. Their role in the process is largely testimonial (victim impact statements) and symbolic; they do not administer the constraint but their moral claim is the constraint's primary public justification. Exit from this identity is psychologically costly: to reject the retributive frame after having embraced it feels like betraying the loved one's memory.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, victims_families, beneficiary,
    moderate, biographical, identity_locked, national).

% The body of constitutional doctrine, statutory law, and judicial precedent that authorizes and regulates capital punishment as a proportionate response to aggravated murder. This tradition sets the agenda: it defines which crimes are death-eligible, what procedural safeguards apply, and how proportionality is assessed. It benefits by maintaining its interpretive authority over the kernel of state killing power. Its exit options are analytical — it can be studied, critiqued, and reformed from within, but abandoning the retributive frame would dissolve its distinctive claim to legitimacy.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, retributive_legal_tradition, agenda_setter,
    institutional, generational, analytical, national).

% Prosecutors who seek death sentences as a professional tool and a moral mission. They gain career advancement, institutional leverage, and the satisfaction of 'speaking for the dead.' Their exit is constrained: they are embedded in a professional culture where capital prosecution is a marker of seriousness; moving to a non-capital jurisdiction or role means losing status and the specific professional identity built on death-penalty work.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, capital_prosecutors, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(state_execution_authority__retributive_reading, capital_prosecutors, agenda_setter).

% Individuals sentenced to death and executed under this reading's authority. They bear the ultimate cost — their lives — as the 'legitimate cost' of moral restoration. No exit exists once the process reaches execution; the constraint's operation literally eliminates the agent. The reading treats this as proportionate, not extractive: the offender's moral desert justifies the taking. Wrongful conviction is acknowledged as a tragic error rate, not a structural defeater.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, executed_offenders, payer,
    powerless, immediate, trapped, local).

% The subset of death-row inmates who are factually innocent of the capital crime. They bear the constraint's error rate as a concentrated cost — execution of an innocent person is the 'tragic error' the reading accepts as the price of maintaining the system. Their structural position is trapped: procedural barriers (exhaustion, procedural default, actual innocence gateway standards) make exit from the death sentence nearly impossible even when evidence of innocence emerges.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, wrongfully_convicted_death_row, payer,
    powerless, immediate, trapped, local).

% Corrections officers, medical personnel, and witnesses who carry out executions. They bear psychological costs (moral injury, PTSD, secondary trauma) as the human agents of the state's killing. Their exit is constrained: the work is assigned within institutional roles; refusing participation risks career consequences and social stigma within the correctional culture. The reading treats their burden as a necessary implementation cost, not a structural objection.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, execution_staff, payer,
    moderate, biographical, constrained, local).

% Organizations and legal actors who argue state execution is categorically impermissible. They are structurally excluded from the retributive reading's framework — their premise (categorical impermissibility) is treated as external criticism, not an internal constraint. They have arbitrage-grade exit: they operate in a different normative framework (international human rights law, state-level abolition campaigns) and can shift venues without losing coherence.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, abolitionist_advocates, excluded,
    organized, generational, arbitrage, national).

% Empirical researchers studying whether capital punishment deters homicide. They observe the constraint's operation but do not participate in its moral economy. Their analytical exit is total — they can change their methodological commitments without personal cost. The retributive reading treats deterrence evidence as irrelevant to its core claim (moral desert, not social utility), so the researchers' findings create no internal pressure on the reading.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, deterrence_researchers, observer,
    analytical, generational, analytical, global).

% UN treaty bodies, regional courts, and NGOs that treat capital punishment as a human rights violation. They are excluded from the domestic retributive framework — their authority is not recognized within the reading's sovereignty claim. They have arbitrage-grade exit: they operate across multiple legal orders and can apply external pressure (diplomatic, reputational) without depending on the reading's internal logic.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, international_human_rights_bodies, excluded,
    institutional, generational, arbitrage, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, authoritative, state-administered mechanism for imposing proportionate punishment on the worst offenders, replacing private vengeance and preventing cycles of retaliatory violence. The coordination claim: the state's monopoly on proportionate killing stabilizes the moral order by channeling retributive impulses through law.
% TRANSFER_FUNCTION: Transfers the power over life and death from the offender (who took a life) to the state (which takes the offender's life in measured, proceduralized form), with moral satisfaction flowing to victims' families and the retributive tradition as the 'return' on the transfer. The executed offender's life is the cost; moral balance restored is the benefit.
% ABSENT_VOICES: The executed offenders themselves (silenced by the constraint), the wrongfully convicted who cannot testify after execution, and abolitionist frameworks that deny the moral permissibility of state killing under any circumstances. International human rights bodies are also absent — their categorical opposition is treated as external interference, not internal debate.
% DISAPPEARANCE_RATIONALE: If the retributive execution authority vanished overnight, the legal architecture of death eligibility, proportionality review, and execution protocols would collapse. Victims' families would lose the state-sanctioned closure mechanism. Prosecutors would lose a charging lever. The moral vocabulary of 'proportionality for heinous crimes' would lose its institutional anchor. The world would rearrange toward either abolition (life without parole as maximum) or a deterrence-only framework — both are structurally different arrangements.
% FOUNDING_PROBLEM: The founding problem was the failure of private vengeance and blood feud to stabilize social order after heinous crimes, and the perceived moral inadequacy of mere imprisonment for crimes that 'shock the conscience.' The state claimed a monopoly on proportionate killing to replace cycles of retaliation with a single, lawful, procedurally guarded act of moral restoration.
% FOUNDING_PROBLEM_CORROBORATION: The retributive legal tradition (judicial opinions, statutory histories) attests the problem is live — heinous crimes still occur and still demand proportionate response. Abolitionist legal scholars and international human rights bodies (outside the beneficiary set) attest the founding problem is substantially solved by modern penology (life without parole, restorative justice) and that the arrangement persists as moral theater. The U.S. Supreme Court's own jurisprudence (e.g., Gregg v. Georgia, 1976; Baze v. Rees, 2008; Glossip v. Gross, 2015) simultaneously affirms the retributive justification and documents the growing procedural fragility — a contested corroboration from within the tradition itself.
narrative_ontology:disappearance_verdict(state_execution_authority__retributive_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_execution_authority__retributive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__retributive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(state_execution_authority__retributive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_execution_authority__retributive_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_execution_authority__retributive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_execution_authority__retributive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_execution_authority__retributive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claimed type is tangled_rope because the constraint has a genuine coordination function (channeling retributive impulses through law to prevent private vengeance cycles) AND asymmetric extraction (the executed offender and wrongfully convicted bear concentrated, irreversible costs while the retributive tradition and prosecutors capture institutional authority and moral capital). Active enforcement is required: the elaborate procedural machinery (capital trials, appeals, clemency, execution protocols) is the enforcement apparatus that holds the coordination-extraction hybrid together. The metrics reflect the reading's own lights: ε=0.68 because the moral-restoration requirement cannot be satisfied by any substitute (imprisonment is structurally inadequate for 'heinous' crimes in this frame); suppression=0.72 because the constraint actively suppresses abolitionist alternatives (legislative repeal, judicial invalidation, moratoria) and the wrongfully convicted's claims of innocence; theater=0.41 because a growing share of the elaborate procedural apparatus performs the appearance of due process while the substantive moral claim (proportionality) becomes harder to operationalize (e.g., evolving standards of decency, method-of-execution litigation). The measurement series shows rising extraction and theater over the post-Gregg era (1976–2024), consistent with a coordination function that is increasingly overwhelmed by its own enforcement machinery.
 *
 * PERSPECTIVAL GAP:
 *   The retributive tradition and victims' families experience the constraint as genuine coordination (moral order restored through law). The executed offender and wrongfully convicted experience it as pure extraction (their lives taken for a moral balance they do not share). Execution staff experience it as a burdensome duty imposed by institutional role. The engine computes these divergent seat classifications from the power/exit/role structure — the retributive reading's claim does not adjudicate the divergence; the structural data does.
 *
 * DIRECTIONALITY LOGIC:
 *   Victims' families are identity_locked beneficiaries: their moral claim is fused to the retributive frame — rejecting execution after having sought it feels like betrayal. The retributive legal tradition is an institutional agenda_setter with analytical exit (it can be reformed from within but not abandoned without dissolving its distinctive authority). Capital prosecutors are organized beneficiaries with constrained exit (professional identity tied to capital work). Executed offenders are powerless, trapped payers — the constraint literally eliminates them. Wrongfully convicted are powerless, trapped payers bearing the error rate. Execution staff are moderate, constrained payers bearing moral injury. Abolitionist advocates and international bodies are excluded with arbitrage exit — they operate in different normative frameworks. Deterrence researchers are analytical observers. The engine computes per-seat directionality from these structural declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (private vengeance cycles, moral inadequacy of imprisonment for heinous crimes) is contested: abolitionists and international bodies argue modern penology solves it without killing; the retributive tradition argues it does not. The constraint persists with rising theater and extraction despite the contested founding problem — a signal of mandatrophy where the original justification has attenuated but the enforcement machinery has grown. The retributive reading treats wrongful execution as a tragic error rate, not a structural defeater, which is itself a mandatrophic move: it reclassifies a systematic cost (innocent people executed) as an acceptable operational parameter to preserve the framework.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    retributive_vs_deterrence_coexistence,
    'Does the retributive reading logically foreclose the deterrence reading, or do they coexist as distinct justifications that can be held simultaneously by different parties within the same legal framework?',
    'Analyze whether any single legal framework (e.g., a state''s capital statute) can simultaneously ground its authority in moral desert AND in deterrence without contradiction. Examine judicial opinions that cite both rationales.',
    'If they foreclose, the kernel has a genuine structural split — a state must choose one grounding. If they coexist, the kernel supports multiple simultaneous readings, and the retributive reading''s extraction profile may be amplified by deterrence rhetoric that serves as cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retributive_vs_deterrence_coexistence, conceptual, 'Structural relationship between retributive and deterrence readings of the same kernel.').

omega_variable(
    moral_restoration_non_substitutability,
    'Is the retributive reading''s claim that imprisonment cannot satisfy moral restoration for heinous crimes a genuine structural feature of the constraint, or a rhetorical move that could be abandoned without dissolving the reading?',
    'Compare jurisdictions that abolished capital punishment but retained ''heinous crime'' categories with life-without-parole maxima. Assess whether victims'' families in those jurisdictions report moral closure without execution. Analyze whether the retributive tradition''s own texts treat proportionality as inherently lethal for certain crimes.',
    'If non-substitutability is structural, ε is intrinsically high and the tangled_rope classification is stable. If it is rhetorical, the reading could mutate toward a lower-extraction scaffold (life without parole as proportionate) without losing its core claim — the current high ε would be contingent, not structural.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(moral_restoration_non_substitutability, conceptual, 'Whether the high extractiveness from moral-restoration non-substitutability is structural or rhetorical.').

omega_variable(
    wrongful_execution_error_rate_threshold,
    'At what error rate (wrongful executions per total executions) would the retributive reading''s ''tragic error'' framing collapse into a structural defeater?',
    'Track exoneration rates, actual innocence claims, and judicial/legislative responses. Identify whether there is a threshold (empirical or normative) where the error rate becomes incompatible with the moral-restoration claim — i.e., where executing innocents undermines the very moral balance the constraint claims to restore.',
    'If a threshold exists and is approached, the constraint''s suppression profile would spike (the reading would need to actively suppress error-rate evidence) or the reading would fracture. If no threshold exists (error rate is always ''tragic'' but never structural), the reading has an infinite absorption capacity for its own failures — a hallmark of extractive inertia.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(wrongful_execution_error_rate_threshold, empirical, 'Whether the retributive reading has a structural error-rate tolerance limit.').

omega_variable(
    committer_frame_reading_relations,
    'What is the structural relationship from the retributive reading to each sibling reading of the state_execution_authority kernel?',
    'Authored in cs_structure.reading_relations per Rule 4: forecloses/coexists_with/influences for each sibling. This omega records the committer-frame metadata that the schema does not capture in standard fields.',
    'If retributive forecloses abolition, the kernel has a genuine logical split. If they coexist, the kernel supports pluralism. If retributive influences deterrence, there is a downstream structural pressure (e.g., retributive proportionality requirements constrain deterrence-based sentencing).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_reading_relations, conceptual, 'Committee-frame structural relationships to sibling readings (deterrence_reading, abolition_reading).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__retributive_reading, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(state_execution_authority__retributive_reading_tr_t1976, state_execution_authority__retributive_reading, theater_ratio, 1976, 0.18).
narrative_ontology:measurement(state_execution_authority__retributive_reading_tr_t1985, state_execution_authority__retributive_reading, theater_ratio, 1985, 0.22).
narrative_ontology:measurement(state_execution_authority__retributive_reading_tr_t1994, state_execution_authority__retributive_reading, theater_ratio, 1994, 0.28).
narrative_ontology:measurement(state_execution_authority__retributive_reading_tr_t2000, state_execution_authority__retributive_reading, theater_ratio, 2000, 0.32).
narrative_ontology:measurement(state_execution_authority__retributive_reading_tr_t2008, state_execution_authority__retributive_reading, theater_ratio, 2008, 0.36).
narrative_ontology:measurement(state_execution_authority__retributive_reading_tr_t2016, state_execution_authority__retributive_reading, theater_ratio, 2016, 0.39).
narrative_ontology:measurement(state_execution_authority__retributive_reading_tr_t2024, state_execution_authority__retributive_reading, theater_ratio, 2024, 0.41).

% Extraction over time
narrative_ontology:measurement(state_execution_authority__retributive_reading_be_t1976, state_execution_authority__retributive_reading, base_extractiveness, 1976, 0.42).
narrative_ontology:measurement(state_execution_authority__retributive_reading_be_t1985, state_execution_authority__retributive_reading, base_extractiveness, 1985, 0.48).
narrative_ontology:measurement(state_execution_authority__retributive_reading_be_t1994, state_execution_authority__retributive_reading, base_extractiveness, 1994, 0.55).
narrative_ontology:measurement(state_execution_authority__retributive_reading_be_t2000, state_execution_authority__retributive_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(state_execution_authority__retributive_reading_be_t2008, state_execution_authority__retributive_reading, base_extractiveness, 2008, 0.62).
narrative_ontology:measurement(state_execution_authority__retributive_reading_be_t2016, state_execution_authority__retributive_reading, base_extractiveness, 2016, 0.65).
narrative_ontology:measurement(state_execution_authority__retributive_reading_be_t2024, state_execution_authority__retributive_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(state_execution_authority__retributive_reading_su_t1976, state_execution_authority__retributive_reading, suppression_requirement, 1976, 0.55).
narrative_ontology:measurement(state_execution_authority__retributive_reading_su_t1985, state_execution_authority__retributive_reading, suppression_requirement, 1985, 0.6).
narrative_ontology:measurement(state_execution_authority__retributive_reading_su_t1994, state_execution_authority__retributive_reading, suppression_requirement, 1994, 0.65).
narrative_ontology:measurement(state_execution_authority__retributive_reading_su_t2000, state_execution_authority__retributive_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(state_execution_authority__retributive_reading_su_t2008, state_execution_authority__retributive_reading, suppression_requirement, 2008, 0.7).
narrative_ontology:measurement(state_execution_authority__retributive_reading_su_t2016, state_execution_authority__retributive_reading, suppression_requirement, 2016, 0.71).
narrative_ontology:measurement(state_execution_authority__retributive_reading_su_t2024, state_execution_authority__retributive_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_execution_authority__retributive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_execution_authority__retributive_reading, state_execution_authority__deterrence_reading).
narrative_ontology:affects_constraint(state_execution_authority__retributive_reading, state_execution_authority__abolition_reading).

% DUAL FORMULATION NOTE:
% This story is one of three in the state_execution_authority constraint family. The retributive reading claims moral restoration as the coordination function (high ε from non-substitutability). The deterrence reading claims crime prevention (ε depends on empirical deterrence effect). The abolition reading claims zero legitimate authority (ε=0 for the arrangement, high ε for the prohibition). The three readings share the kernel (state killing authority) but instantiate different constraints with different ε, different beneficiary/victim structures, and different types. They are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_execution_authority__retributive_reading, institutional, 0.15).
constraint_indexing:directionality_override(state_execution_authority__retributive_reading, powerless, 0.98).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
