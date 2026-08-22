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
 *   human_readable: Retributive Justification for Capital Punishment (Lex Talionis Reading)
 *   domain: criminal_justice/political_philosophy/legal_theory
 *
 * SUMMARY:
 *   This story instantiates the RETRIBUTIVE reading of the contested
 *   state_killing_legitimacy kernel: the claim that a murderer forfeits their
 *   right to life through proportional moral desert (lex talionis), such that
 *   execution restores a moral balance rather than merely deterring future
 *   crime or violating an inviolable dignity claim. This reading treats the
 *   offender as having entered the victim/payer set through their own act —
 *   moral forfeiture is the mechanism, not incapacitation or signaling. The
 *   moral order of the polity and the retributive legal tradition are named
 *   beneficiaries because the doctrine's legitimacy is explicitly that a
 *   moral balance is restored, which is a real (if contested) claim distinct
 *   from deterrence's causal claim about future crime prevention. This is a
 *   SEPARATE constraint from deterrence_reading and abolition_reading — those
 *   readings have different ε, different beneficiary/victim structures, and
 *   are NOT folded into this file's classification. See kernel_context and
 *   omegas for the committer structure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__retributive_reading, 0.72).
domain_priors:suppression_score(state_killing_legitimacy__retributive_reading, 0.68).
domain_priors:theater_ratio(state_killing_legitimacy__retributive_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__retributive_reading, tangled_rope).
narrative_ontology:human_readable(state_killing_legitimacy__retributive_reading, "Retributive Justification for Capital Punishment (Lex Talionis Reading)").
narrative_ontology:topic_domain(state_killing_legitimacy__retributive_reading, "criminal_justice/political_philosophy/legal_theory").

domain_priors:requires_active_enforcement(state_killing_legitimacy__retributive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__retributive_reading, '64728a3a-8d79-4629-9947-23ebf0579efc').
narrative_ontology:cs_kernel_codification('64728a3a-8d79-4629-9947-23ebf0579efc', distributed).
narrative_ontology:cs_authority_grounding('64728a3a-8d79-4629-9947-23ebf0579efc', lineage).
narrative_ontology:cs_interpretation_layer_present('64728a3a-8d79-4629-9947-23ebf0579efc').
narrative_ontology:cs_reading_relation('64728a3a-8d79-4629-9947-23ebf0579efc', state_killing_legitimacy__abolition_reading, forecloses).
narrative_ontology:cs_reading_relation('64728a3a-8d79-4629-9947-23ebf0579efc', state_killing_legitimacy__deterrence_reading, coexists_with).
narrative_ontology:cs_axiom('64728a3a-8d79-4629-9947-23ebf0579efc', foundational, life_right_forfeitable_by_desert).
narrative_ontology:cs_axiom_status(life_right_forfeitable_by_desert, holdable).
narrative_ontology:cs_axiom_grounding('64728a3a-8d79-4629-9947-23ebf0579efc', life_right_forfeitable_by_desert, deontological).
narrative_ontology:cs_axiom('64728a3a-8d79-4629-9947-23ebf0579efc', secondary, proportionality_bounds_permissible_punishment).
narrative_ontology:cs_axiom_status(proportionality_bounds_permissible_punishment, holdable).
narrative_ontology:cs_axiom_grounding('64728a3a-8d79-4629-9947-23ebf0579efc', proportionality_bounds_permissible_punishment, deontological).
narrative_ontology:cs_reference_frame('64728a3a-8d79-4629-9947-23ebf0579efc', classical_proportional_desert_doctrine).
narrative_ontology:cs_drift_state('64728a3a-8d79-4629-9947-23ebf0579efc', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('64728a3a-8d79-4629-9947-23ebf0579efc', '').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__retributive_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__retributive_reading, moral_order_of_the_polity).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__retributive_reading, victims_families_seeking_vindication).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__retributive_reading, retributive_legal_tradition).
narrative_ontology:constraint_victim(state_killing_legitimacy__retributive_reading, convicted_murderers).
narrative_ontology:constraint_victim(state_killing_legitimacy__retributive_reading, wrongfully_convicted_death_row_inmates).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__retributive_reading, proportional_desert_doctrine).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__retributive_reading, moral_forfeiture_of_life_right).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sentenced to death under a doctrine holding that by taking a life, they have forfeited their own right to live. They have no exit from the sentence except appeals within the same system that condemned them; their moral status as 'deserving' is asserted by the state and cannot be contested from outside the framework once conviction is final.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, convicted_murderers, payer,
    powerless, immediate, trapped, national).

% Executed or awaiting execution under the same desert doctrine despite factual innocence. The retributive framework's legitimacy depends on accurate desert-attribution; wrongful convictions expose that the forfeiture claim is applied by a fallible process, but the executed cannot be un-executed once the doctrine's presumption of accurate desert has been acted upon.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, wrongfully_convicted_death_row_inmates, payer,
    powerless, immediate, trapped, national).

% The abstract moral-order construct that the retributive reading holds is restored or vindicated when proportional punishment is exacted. It collects no rents directly but is invoked as the beneficiary whose balance is repaired by execution; it is not an actor and takes no action, but the doctrine's legitimacy depends on this restoration claim being real.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, moral_order_of_the_polity, beneficiary,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(state_killing_legitimacy__retributive_reading, moral_order_of_the_polity).

% Receive symbolic and sometimes psychological vindication from the state's proportional response to the murder of their kin. They participate in the process through victim-impact testimony and parole/clemency objections, but do not control the sentencing outcome; some report closure, others report the execution does not resolve grief, meaning the benefit is contested even within this group.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, victims_families_seeking_vindication, beneficiary,
    moderate, biographical, constrained, local).

% The doctrinal lineage of proportional desert theory (lex talionis, Kantian retributivism) that supplies the legitimating framework courts and legislatures draw on. It is vindicated each time a capital sentence is upheld on desert grounds; it is not an actor but a body of doctrine that agenda-setting institutions invoke.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, retributive_legal_tradition, beneficiary,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(state_killing_legitimacy__retributive_reading, retributive_legal_tradition, agenda_setter).
narrative_ontology:stakeholder_non_agent(state_killing_legitimacy__retributive_reading, retributive_legal_tradition).

% Charge, try, and sentence under statutes authorizing capital punishment on desert grounds. They administer the forfeiture doctrine, decide when it applies, and can decline to seek death even where eligible; they bear no personal cost from the sentence's execution and their institutional legitimacy is partly built on successfully invoking the desert framework.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, prosecutors_and_capital_courts, agenda_setter,
    institutional, generational, arbitrage, national).

% Argue that no proportional-desert calculation can license the state taking a life, regardless of the offender's culpability. Their objection to the forfeiture premise itself is treated by capital courts as a policy preference outside the doctrinal question of whether desert has been proportionally established, so their core challenge to the kernel is structurally excluded from the retributive framework's own deliberation.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, abolitionist_advocates, excluded,
    organized, generational, mobile, national).

% Analyze whether moral forfeiture is a coherent basis for lethal punishment, comparing it against deterrence and dignity-based frameworks. They publish critiques and defenses but do not control sentencing; their analysis can shift doctrine over generations but not adjudicate individual cases.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, moral_philosophers_and_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_legitimacy__retributive_reading, diffuse).
narrative_ontology:fixing_cost_class(state_killing_legitimacy__retributive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, publicly legible moral accounting: when someone takes a life, the retributive framework specifies what is proportionally owed, giving courts, victims, and the public a common standard for what 'justice done' means in the gravest cases, rather than leaving the response to private vengeance or ad hoc political pressure.
% TRANSFER_FUNCTION: Moves the offender's life from the offender to the satisfaction of a claimed moral balance held by the polity and, derivatively, the victim's survivors — the state extracts the forfeited life as the payment the desert doctrine says is proportionally owed.
% ABSENT_VOICES: Abolitionist advocates who reject the forfeiture premise itself are not permitted to contest the underlying legitimacy of desert-based killing within the sentencing proceeding itself — courts applying retributive doctrine treat that as a legislative-policy question, not a trial question, so the deepest objection to the kernel structurally cannot be raised where the sentence is actually decided.
% DISAPPEARANCE_RATIONALE: If the retributive justification disappeared overnight, capital sentencing statutes grounded in desert language would lose their doctrinal foundation; courts would have to re-ground capital punishment in deterrence or incapacitation rationales (each independently contested) or abolish it; existing death sentences imposed on forfeiture reasoning would face immediate appellate challenge, and victims' families currently told the sentence vindicates a moral balance would lose that framing entirely.
% FOUNDING_PROBLEM: Pre-state societies and early legal codes needed a way to bound private vengeance (blood feuds, unlimited retaliation) by fixing punishment to the proportional severity of the offense — lex talionis emerged as a limiting principle, not merely a maximal one, replacing potentially unbounded retaliatory violence with a capped, publicly administered equivalent.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and anthropologists outside the capital-punishment apparatus attest that lex talionis originally functioned as a ceiling on private vengeance in stateless or weak-state contexts — a genealogy independent of modern retentionist advocacy. But comparative-law scholars and international human rights bodies (also outside the benefiting parties) attest that in developed state systems with functioning monopolies on violence, the original vengeance-limiting problem is largely solved by ordinary incarceration, and that capital punishment's persistence in retributive terms now serves symbolic/political functions rather than the founding proportionality-limiting function; no source affiliated with prosecutorial or retentionist advocacy groups was treated as sufficient corroboration on its own.
narrative_ontology:disappearance_verdict(state_killing_legitimacy__retributive_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_legitimacy__retributive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_legitimacy__retributive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.72) because the retributive reading, taken on its own terms, still extracts the offender's life as payment for a claimed moral debt — even a sound desert theory is a transfer, and the possibility of wrongful conviction means the transfer sometimes falls on those who owe nothing. Suppression (0.68) reflects that the doctrine forecloses the offender's own objection to the forfeiture premise within capital proceedings; appeals can contest facts and procedure but rarely the moral-forfeiture theory itself. Resistance is high (0.75) because abolitionist and human-rights movements actively contest the premise at every level. Accessibility collapse is moderate (0.4) rather than near-total because alternative sentencing frameworks (life imprisonment, restorative justice) remain legally and politically available in most jurisdictions — the retributive premise has not foreclosed all alternatives, only foreclosed them within its own doctrinal proceedings.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (prosecutors/courts) and the beneficiary seats (moral order, victims' families, doctrinal tradition), this reads as coordination: a shared, bounded, legally legible standard for the gravest wrong, replacing private vengeance or arbitrary state violence. From the payer seat (convicted murderers, and especially the wrongfully convicted), the same structure is extraction backed by lethal enforcement, with no meaningful exit and no capacity to contest the forfeiture premise itself within the proceeding that applies it. The engine computes both seats from the same structural data; this story does not adjudicate which seat is 'correct' — that adjudication is exactly what the sibling readings and the omega variables are for.
 *
 * DIRECTIONALITY LOGIC:
 *   Convicted murderers and especially wrongfully convicted inmates sit at the full-target end of directionality: trapped exit, no capacity to contest the forfeiture premise once convicted, bearing the sentence directly. The moral order of the polity and retributive legal tradition are non-agent beneficiaries — abstractions vindicated by the doctrine's operation, collecting no literal rents but structurally central to why the doctrine persists. Victims' families are real beneficiaries but with contested and partial benefit (some report vindication, others do not), which is why their power is only moderate and exit constrained rather than arbitrage. Prosecutors and courts are agenda-setters with institutional power and effectively arbitrage exit (they administer the doctrine but bear none of its costs and can decline to invoke it).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem interview surfaces a genealogy gap: lex talionis originally functioned to CAP private vengeance in contexts without a state monopoly on violence. In modern states with functioning incarceration systems, that founding problem is largely solved by imprisonment alone — the founding_problem_status is marked contested precisely because retentionist advocates treat the desert-satisfaction function as still live, while comparative-law and human-rights corroboration (outside the benefiting parties) suggests the vengeance-capping function has been superseded, leaving symbolic/political persistence rather than functional necessity. This divergence between status and disappearance_verdict (world_rearranges) is the mismatch the engine is built to flag — it does not resolve the mandatrophy question here, but the story data documents both sides honestly rather than either accepting the retentionist genealogy uncritically or dismissing it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    forfeiture_premise_coherence,
    'Is ''moral forfeiture of the right to life through desert'' a coherent, defensible normative claim, or is it a post-hoc rationalization for state violence that would be classified as extraction under any other framing?',
    'Sustained philosophical analysis of whether desert-based forfeiture survives standard objections (e.g., that rights cannot be forfeited by wrongdoing without independent argument for why desert specifically licenses killing rather than lesser proportional responses); comparative analysis of jurisdictions that have abandoned capital punishment without abandoning proportional-desert reasoning elsewhere in sentencing.',
    'If the forfeiture premise is incoherent independent of any empirical question, this reading collapses toward the abolition reading''s classification (near-total extraction, no genuine coordination function). If coherent, the tangled_rope classification (genuine but extraction-riding coordination) holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(forfeiture_premise_coherence, conceptual, 'Whether moral forfeiture is a coherent normative basis distinguishable from rationalized extraction.').

omega_variable(
    wrongful_conviction_rate_effect,
    'At what wrongful-conviction rate does the retributive framework''s legitimacy claim (that desert is being accurately and proportionally administered) become empirically false regardless of the underlying moral theory''s coherence?',
    'DNA-exoneration and capital-case reversal rate studies; comparison of pre- and post-DNA-testing era wrongful conviction discovery rates in capital cases specifically.',
    'A high enough wrongful-execution rate undermines the practical application of the retributive doctrine even if the underlying desert theory is sound in principle — this would not change the theoretical classification but would sharply raise ε for the applied practice as distinct from the pure doctrine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wrongful_conviction_rate_effect, empirical, 'Whether error rates in capital sentencing undermine the doctrine''s claimed accuracy of desert attribution.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the ''obvious'' framing of this constraint (the sentencing statute and court doctrine) the right unit, or is the less obvious framing — the philosophical desert-theory tradition that legitimates the statute and that the statute depends on for its claimed moral force — the actual kernel under contest?',
    'Track whether reform or repeal efforts target the statutory language (suggesting the statute is the kernel) or target the underlying desert-theory justification in briefs and public argument (suggesting the philosophical tradition is the kernel); a persistent decoupling would indicate two nested kernels rather than one.',
    'If the philosophical tradition is the real kernel, this story''s cs_structure authority_grounding (lineage, per the retributive doctrinal tradition) is correct; if the statute itself is the operative kernel independent of its philosophical justification, authority_grounding would shift toward practice or extraction (courts applying settled doctrine regardless of its philosophical defensibility), which would change the reading_relations analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the doctrinal tradition or the enacted statute is the operative kernel this reading is a reading of.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__retributive_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_legitimacy__retributive_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(stat_tr_t8, state_killing_legitimacy__retributive_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(stat_tr_t16, state_killing_legitimacy__retributive_reading, theater_ratio, 16, 0.21).
narrative_ontology:measurement(stat_tr_t24, state_killing_legitimacy__retributive_reading, theater_ratio, 24, 0.25).
narrative_ontology:measurement(stat_tr_t32, state_killing_legitimacy__retributive_reading, theater_ratio, 32, 0.28).
narrative_ontology:measurement(stat_tr_t40, state_killing_legitimacy__retributive_reading, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_legitimacy__retributive_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(stat_be_t8, state_killing_legitimacy__retributive_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(stat_be_t16, state_killing_legitimacy__retributive_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement(stat_be_t24, state_killing_legitimacy__retributive_reading, base_extractiveness, 24, 0.67).
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
% This story is one of three sibling constraints decomposing the natural-language concept 'justification for capital punishment' under the ε-invariance principle: retributive_reading (this file — desert-based forfeiture, tangled_rope), deterrence_reading (consequentialist crime-prevention claim, its own ε keyed to deterrence evidence), and abolition_reading (categorical dignity violation, near-pure snare from the offender's seat). Each reading has a distinct beneficiary/victim structure and a distinct ε because each reading is answering a structurally different legitimacy question, not measuring the same question differently. They are linked here via affects_constraints rather than merged because measuring 'capital punishment' by desert-satisfaction yields a different ε than measuring it by deterrence-efficacy or by dignity-violation — three constraints, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
