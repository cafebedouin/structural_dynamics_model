% ============================================================================
% CONSTRAINT STORY: deferred_consent_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deferred_consent_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: deferred_consent_reading
 *   human_readable: Deferred-Consent Reading of Authorial Legitimacy (Accession-Gravity Model)
 *   domain: constitutional_design/political_theory/sovereignty_architecture
 *
 * SUMMARY:
 *   This story instantiates the deferred-consent reading of the
 *   authorial_legitimacy_kernel: a charter's legitimacy is not fixed at
 *   drafting but is wholly contingent and prospective, activated one party at
 *   a time through accession under an Article VII 'gravity model.'
 *   Pre-window, the draft is a legal nullity — a private artifact with no
 *   claim on anyone. Legitimacy talk directed at the drafting process itself
 *   is treated as a category error, since the only legitimating act is
 *   accession. Crucially, this reading treats §7.1's bootstrap problem (the
 *   charter cannot derive authority from the law it replaces nor from prior
 *   sovereign constituent authority) as still fully open — the
 *   deferred-consent design does not solve the bootstrap paradox, it merely
 *   relocates and re-times it to each individual accession event, making the
 *   paradox more legible rather than resolved. This is a scaffold: the
 *   deferred-consent structure is explicitly transitional, meant to carry the
 *   document from drafting through a defined accession window (declared
 *   sunset) until enough parties have acceded that the document's legitimacy
 *   is no longer meaningfully contingent — the coordination story is the
 *   transition itself, not a permanent steady state.
 *
 * KEY AGENTS:
 *   - drafting_committee: sets the accession mechanism, bears no legal exposure pre-accession — organized/mobile
 *   - acceding_founding_parties: convert draft into binding obligation one at a time, benefit from early-mover gravitational advantage — powerful/constrained post-accession
 *   - non_acceding_holdout_parties: lose bargaining leverage as gravity accumulates, though no formal claim yet exists against them — moderate/mobile
 *   - pre_accession_third_party_claimants: bound by proxy through others' accession, no independent voice — powerless/trapped
 *   - successor_courts_and_interpreters: must decide retroactively whether drafting-stage conduct has any interpretive weight — institutional/analytical
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferred_consent_reading, 0.28).
domain_priors:suppression_score(deferred_consent_reading, 0.15).
domain_priors:theater_ratio(deferred_consent_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferred_consent_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(deferred_consent_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(deferred_consent_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferred_consent_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(deferred_consent_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferred_consent_reading, scaffold).
narrative_ontology:human_readable(deferred_consent_reading, "Deferred-Consent Reading of Authorial Legitimacy (Accession-Gravity Model)").
narrative_ontology:topic_domain(deferred_consent_reading, "constitutional_design/political_theory/sovereignty_architecture").

narrative_ontology:has_sunset_clause(deferred_consent_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferred_consent_reading, '8eb79af3-f3f1-4a83-8699-87b2fe8d7e4c').
narrative_ontology:cs_kernel_codification('8eb79af3-f3f1-4a83-8699-87b2fe8d7e4c', formalized).
narrative_ontology:cs_authority_grounding('8eb79af3-f3f1-4a83-8699-87b2fe8d7e4c', practice).
narrative_ontology:cs_interpretation_layer_present('8eb79af3-f3f1-4a83-8699-87b2fe8d7e4c').
narrative_ontology:cs_reading_relation('8eb79af3-f3f1-4a83-8699-87b2fe8d7e4c', authorial_legitimacy_kernel__benign_dictator_reading, coexists_with).
narrative_ontology:cs_reading_relation('8eb79af3-f3f1-4a83-8699-87b2fe8d7e4c', authorial_legitimacy_kernel__bootstrap_incoherence_reading, influences).
narrative_ontology:cs_axiom('8eb79af3-f3f1-4a83-8699-87b2fe8d7e4c', foundational, accession_as_sole_legitimating_act).
narrative_ontology:cs_axiom_status(accession_as_sole_legitimating_act, holdable).
narrative_ontology:cs_axiom_grounding('8eb79af3-f3f1-4a83-8699-87b2fe8d7e4c', accession_as_sole_legitimating_act, conventional).
narrative_ontology:cs_axiom('8eb79af3-f3f1-4a83-8699-87b2fe8d7e4c', foundational, drafting_process_legal_nullity_doctrine).
narrative_ontology:cs_axiom_status(drafting_process_legal_nullity_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('8eb79af3-f3f1-4a83-8699-87b2fe8d7e4c', drafting_process_legal_nullity_doctrine, conventional).
narrative_ontology:cs_axiom('8eb79af3-f3f1-4a83-8699-87b2fe8d7e4c', secondary, section_7_1_remains_formally_open).
narrative_ontology:cs_axiom_status(section_7_1_remains_formally_open, holdable).
narrative_ontology:cs_axiom_grounding('8eb79af3-f3f1-4a83-8699-87b2fe8d7e4c', section_7_1_remains_formally_open, empirically_contingent).
narrative_ontology:cs_reference_frame('8eb79af3-f3f1-4a83-8699-87b2fe8d7e4c', accession_as_exclusive_legitimating_act).
narrative_ontology:cs_drift_state('8eb79af3-f3f1-4a83-8699-87b2fe8d7e4c', post_accession_window_closure, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8eb79af3-f3f1-4a83-8699-87b2fe8d7e4c', '').
narrative_ontology:cs_kernel_id(deferred_consent_reading, authorial_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferred_consent_reading, drafting_committee).
narrative_ontology:constraint_beneficiary(deferred_consent_reading, acceding_founding_parties).
narrative_ontology:constraint_victim(deferred_consent_reading, non_acceding_holdout_parties).
narrative_ontology:constraint_victim(deferred_consent_reading, pre_accession_third_party_claimants).
narrative_ontology:constraint_vindicates(deferred_consent_reading, accession_as_sole_legitimating_act).
narrative_ontology:constraint_vindicates(deferred_consent_reading, drafting_process_legal_nullity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes the charter text and the Article VII accession mechanism itself, including the design of the gravity model that determines how legitimacy accrues once parties begin acceding. Before any accession, the committee's product is a private draft with no claim on anyone — the committee bears no legal exposure for the drafting act itself, only reputational and design-quality exposure. Can revise the text freely until the first accession locks terms for existing accedents.
narrative_ontology:constraint_stakeholder(deferred_consent_reading, drafting_committee, agenda_setter,
    organized, generational, mobile, national).

% Each party that accedes converts the private draft into binding obligation for itself, one accession at a time. Early accedents benefit from setting the gravitational pull that later parties respond to — their accession is what gives the document any legal weight at all. Once acceded, they are bound by terms they helped make binding for others, but exit before accession was costless and remains so for anyone who has not yet joined.
narrative_ontology:constraint_stakeholder(deferred_consent_reading, acceding_founding_parties, beneficiary,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(deferred_consent_reading, acceding_founding_parties, agenda_setter).

% Parties who have not yet acceded face a document with increasing gravitational pull as more parties join — the accession-gravity model means their bargaining position erodes with each new accedent even though the document has, by this reading's own logic, no formal claim on them until they sign. They pay in diminished future leverage, not present legal cost, which is precisely what makes the erosion hard to contest: there is no legitimacy claim yet to challenge.
narrative_ontology:constraint_stakeholder(deferred_consent_reading, non_acceding_holdout_parties, payer,
    moderate, biographical, mobile, national).

% Populations or entities whose interests the charter purports to eventually govern (successor-state residents, minority factions, future office-holders) have no seat at drafting and no accession right of their own — their fate is bundled into whichever party accedes on their behalf. Under this reading they cannot even claim the drafting process wronged them, since the drafting process is declared legally inert; their only recourse is to contest the accession act of the party that bound them, long after the fact.
narrative_ontology:constraint_stakeholder(deferred_consent_reading, pre_accession_third_party_claimants, excluded,
    powerless, immediate, trapped, national).

% Later tribunals must decide whether to treat the drafting record as interpretively relevant at all, given this reading's claim that legitimacy talk about drafting is a category error. They observe the accession pattern — who joined, in what order, under what gravitational pressure — as the only legally cognizable history, and must adjudicate disputes about parties bound before they had genuine bargaining power.
narrative_ontology:constraint_stakeholder(deferred_consent_reading, successor_courts_and_interpreters, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(deferred_consent_reading, acceding_founding_parties).
narrative_ontology:fixing_cost_class(deferred_consent_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the bootstrap problem of a founding document by relocating the moment of legal birth entirely to individual, sequential acts of accession rather than to the drafting act — this lets a document exist and circulate in draft form without anyone needing to resolve, at drafting time, the question of what authorizes the drafters.
% TRANSFER_FUNCTION: Moves bargaining leverage from later-acceding or non-acceding parties to earlier-acceding parties and to the drafting committee that shaped the accession mechanism itself; each accession increases the gravitational pull on remaining holdouts, transferring negotiating power away from them without any formal act directed at them.
% ABSENT_VOICES: Pre-accession third-party claimants and populations bound by proxy through a party's eventual accession have no voice in either drafting or in their own binding-in; non-acceding holdouts are present but structurally weakened by every accession that precedes their decision.
% DISAPPEARANCE_RATIONALE: If this reading disappeared, the drafting committee and early accedents would lose their strongest defense against bootstrap-illegitimacy challenges (the claim that drafting carries zero legal weight); non-acceding holdouts might gain leverage since the accession-gravity dynamic tying their bargaining erosion to others' accession would need a different justification; successor courts would need a new theory to decide what, if anything, drafting-stage conduct means retroactively. Whether the world 'rearranges' depends on which sibling reading fills the vacuum — bootstrap_incoherence would destabilize far more than benign_dictator would.
% FOUNDING_PROBLEM: How can a founding charter acquire legal legitimacy when, by its own Article VII (§7.1), it cannot derive authority from the law it replaces nor from any prior sovereign constituent authority (the SCA) — the classic bootstrap paradox of constitutional founding.
% FOUNDING_PROBLEM_CORROBORATION: The drafting committee and early acceding parties attest that the deferred-consent design fully resolves the founding problem by making accession, not drafting, the legitimating act. Independent legal scholars associated with the bootstrap_incoherence_reading dispute this from outside the benefiting parties, arguing the anti-domination procedure only relocates the paradox to each accession event without dissolving it — §7.1 remains formally unresolved, merely rendered less visible.
narrative_ontology:disappearance_verdict(deferred_consent_reading, contested).
narrative_ontology:founding_problem_status(deferred_consent_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferred_consent_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(deferred_consent_reading, 'none', 1).
narrative_ontology:epsilon_provenance(deferred_consent_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deferred_consent_reading_tests).
:- end_tests(deferred_consent_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is modest but rising (0.12 to 0.28) because the deferred-consent reading's primary cost is not direct transfer but leverage erosion imposed on holdouts as gravitational pull accumulates — a soft, structural cost rather than an enforced one. Suppression is low (0.15) because, by this reading's own logic, nothing formally binds a non-acceding party; the pressure is persuasive/structural, not coercive. Theater ratio is low and slowly rising (0.10 to 0.20) reflecting that most of the activity is genuine accession-tracking rather than performance, though some theatrical invocation of 'voluntary accession' grows as gravitational pressure on holdouts becomes harder to distinguish from de facto coercion. Accessibility collapse is moderate (0.35): holdouts genuinely retain the option not to accede, but that option degrades in value over time. Resistance is moderate-high (0.55) because holdout parties and third-party claimants have genuine grounds to contest the framing, particularly the claim that pre-accession leverage erosion is not itself a form of extraction.
 *
 * PERSPECTIVAL GAP:
 *   The drafting committee and early accedents experience this as clean, low-friction coordination — a design solution to a hard bootstrap problem. Non-acceding holdouts and third-party claimants experience the same structure as slow-motion coercion dressed in the language of voluntary accession: the gravity model imposes real costs on them without ever formally claiming authority over them, which is exactly what makes the cost hard to name or contest. The engine should compute these seats differently from the same structural facts — that divergence is the point of this reading, not a defect in it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (drafting_committee, acceding_founding_parties) sit near the low end of directionality: the committee bears no exposure pre-accession, and early accedents capture disproportionate influence over terms precisely because their accession comes first. Victims (non_acceding_holdout_parties, pre_accession_third_party_claimants) sit toward the target end: holdouts lose leverage through no formal act against them, and third-party claimants are bound by proxy with zero independent voice. The asymmetry is structural, not enforced — which is exactly why this reading claims low suppression even while extraction rises over time.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists mandatrophy mislabeling in a specific way: by insisting the drafting process itself is legally inert, it prevents anyone from later claiming the *drafters* over-extended a mandate they never formally held — there was no mandate to over-extend, only a design that others chose to activate. But this same move risks masking a different mandatrophy: the accession-gravity mechanism itself could persist past its useful transitional function (hence the declared sunset), continuing to erode holdout leverage long after enough parties have acceded that the bootstrap problem is, for practical purposes, moot. The scaffold classification depends on the sunset actually terminating gravitational pressure once accession reaches a threshold — if it does not, this reading degrades toward a tangled_rope or worse.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    accession_sufficiency_ambiguity,
    'Is a single act of accession by one party genuinely sufficient to legitimate the charter''s application to that party, or does the underlying §7.1 bootstrap problem (no derivation from the replaced law nor from prior sovereign authority) contaminate every subsequent accession regardless of how many parties join?',
    'Track whether successor courts, when adjudicating disputes from accedents, ever revisit the foundational legitimacy question or treat it as settled by the fact of accession alone. If courts consistently refuse to re-open §7.1 once accession has occurred, the deferred-consent reading is functioning as claimed; if courts periodically re-open it, the bootstrap problem was never actually relocated, only deferred rhetorically.',
    'If accession is genuinely sufficient, this reading is a stable scaffold that resolves into ordinary constitutional legitimacy once the accession window closes. If accession is not sufficient — if the paradox persists through every accession — this reading collapses toward the bootstrap_incoherence_reading and the scaffold classification becomes a disguised permanent extraction (favoring early accedents who benefited from a legitimacy claim that was never actually established).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accession_sufficiency_ambiguity, conceptual, 'Whether serial accession actually dissolves or merely relocates the founding bootstrap paradox.').

omega_variable(
    gravity_model_coercion_threshold,
    'At what point does the accumulating gravitational pull on non-acceding holdouts become functionally indistinguishable from coercion, even though no formal legal claim is asserted against them?',
    'Compare holdout parties'' actual bargaining outcomes across the accession window against a counterfactual where accession order was randomized rather than sequential-and-cumulative; a sharp divergence would indicate the gravity model itself, not merely voluntary accession choices, is doing coercive work.',
    'If the threshold is low (gravity becomes coercive early), this reading''s low suppression score is descriptively wrong and the constraint is closer to a tangled_rope than a scaffold. If the threshold is high (gravity remains genuinely persuasive throughout), the scaffold/low-suppression framing holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gravity_model_coercion_threshold, empirical, 'Whether structural leverage erosion under the gravity model crosses into de facto coercion.').

omega_variable(
    third_party_binding_by_proxy,
    'Can this reading coherently claim the drafting process carries zero legal weight while simultaneously permitting acceding parties to bind third-party populations who had no accession right of their own?',
    'Examine whether any accedent''s binding of proxied populations has been challenged specifically on the grounds that the underlying charter''s legitimacy chain (drafting -> accession -> proxy-binding) breaks down at the proxy step even if it holds at the accession step.',
    'If proxy-binding survives legal challenge unmodified, the deferred-consent reading effectively re-imports drafting-stage legitimacy questions through the back door of accession, undermining its own central claim. If proxy-binding is successfully challenged or limited, the reading''s internal consistency is preserved at the cost of narrowing what accession can actually accomplish.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(third_party_binding_by_proxy, conceptual, 'Whether binding-by-proxy is compatible with a reading that denies drafting any legitimating force.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferred_consent_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defe_tr_t0, deferred_consent_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(defe_tr_t4, deferred_consent_reading, theater_ratio, 4, 0.12).
narrative_ontology:measurement(defe_tr_t8, deferred_consent_reading, theater_ratio, 8, 0.14).
narrative_ontology:measurement(defe_tr_t12, deferred_consent_reading, theater_ratio, 12, 0.16).
narrative_ontology:measurement(defe_tr_t16, deferred_consent_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement(defe_tr_t20, deferred_consent_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(defe_tr_t24, deferred_consent_reading, theater_ratio, 24, 0.2).

% Extraction over time
narrative_ontology:measurement(defe_be_t0, deferred_consent_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(defe_be_t4, deferred_consent_reading, base_extractiveness, 4, 0.16).
narrative_ontology:measurement(defe_be_t8, deferred_consent_reading, base_extractiveness, 8, 0.2).
narrative_ontology:measurement(defe_be_t12, deferred_consent_reading, base_extractiveness, 12, 0.22).
narrative_ontology:measurement(defe_be_t16, deferred_consent_reading, base_extractiveness, 16, 0.25).
narrative_ontology:measurement(defe_be_t20, deferred_consent_reading, base_extractiveness, 20, 0.27).
narrative_ontology:measurement(defe_be_t24, deferred_consent_reading, base_extractiveness, 24, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(deferred_consent_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferred_consent_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(deferred_consent_reading, benign_dictator_reading).
narrative_ontology:affects_constraint(deferred_consent_reading, bootstrap_incoherence_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the authorial_legitimacy_kernel governing Article VII accession. benign_dictator_reading locates legitimacy in drafters' demonstrated restraint/competence at drafting time (drafting itself carries legal weight). bootstrap_incoherence_reading holds that no procedural device, including deferred accession, escapes the §7.1 paradox (nothing carries legitimating weight, ever). deferred_consent_reading (this story) takes the middle position: drafting carries zero weight (agreeing with bootstrap_incoherence) but accession is sufficient and serial legitimation is real (disagreeing with bootstrap_incoherence's global negation). Each reading yields a different beneficiary/victim structure and a different classification — this story's ε (0.28, rising) reflects the leverage-erosion cost imposed on holdouts, distinct from the drafting-stage ε that benign_dictator_reading would carry (concentrated in drafters' discretion) and distinct from the totalizing illegitimacy bootstrap_incoherence_reading would assign to the entire structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
