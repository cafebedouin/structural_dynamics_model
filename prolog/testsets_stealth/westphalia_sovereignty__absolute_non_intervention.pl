% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__absolute_non_intervention
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalia_sovereignty__absolute_non_intervention, []).

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
 *   constraint_id: westphalia_sovereignty__absolute_non_intervention
 *   human_readable: Westphalian Sovereignty — Categorical Non-Intervention Reading
 *   domain: international_law/political_theory/state_systems
 *
 * SUMMARY:
 *   This story instantiates the absolute_non_intervention reading of the
 *   westphalia_sovereignty kernel: sovereignty as categorical territorial
 *   inviolability, under which external interference in domestic affairs is
 *   illegitimate per se, regardless of the interfering state's motive or the
 *   target state's internal conduct. The standing arrangement under contest —
 *   the referent for every metric below — is the categorical non-intervention
 *   regime as it has actually operated from the Peace of Westphalia through
 *   the UN Charter settlement to the post-2005 responsibility-to-provide era.
 *   The reading's own lights assess that arrangement: it is defended by this
 *   reading as the load-bearing wall of interstate peace, and the metrics are
 *   authored from the structural record of what the wall shelters as well as
 *   what it supports. The claim/metric independence rule applies in full:
 *   claimed_type is tangled_rope because the arrangement possesses both a
 *   genuine, broadly distributed coordination function (conquest prevention,
 *   border stability, small-state survival) and an asymmetric extraction
 *   channel (impunity rents flowing to governments with predatory internal
 *   conduct, paid for by populations sealed inside the shield). The engine
 *   computes per-seat classifications from the structural data; nothing here
 *   reconciles the claim to the metrics or to any predicted verdict.
 *
 * KEY AGENTS:
 *   - territorial_state_elites: Primary beneficiary (institutional/identity_locked) — hold the domestic prerogatives the categorical bar secures; their political self-concept is fused with inviolability
 *   - predatory_regime_leaderships: Concentrated beneficiary (organized/trapped) — governments whose internal conduct makes the shield's value rise with the severity of their conduct
 *   - vulnerable_small_states: Distributed beneficiary (moderate/constrained) — receive the conquest-prevention side of the bargain
 *   - populations_under_authoritarian_control: Excluded payer (powerless/trapped) — bear the abandonment cost; enter international forums only through the votes of the governments afflicting them
 *   - minorities_facing_mass_atrocities: Payer and excluded voice (powerless/trapped) — lose the last external remedy when the categorical bar holds
 *   - permanent_five_custodians: Agenda setter and beneficiary (institutional/arbitrage) — police the rule for others while reserving the exception machinery behind a unanimous veto
 *   - r2p_advocacy_coalition: Excluded challenger (organized/constrained) — presses the conduct-forfeiture framing; voiced in debate, contained in outcomes
 *   - icj_and_legal_doctrine: Analytical observer (institutional/analytical) — supplies the doctrinal scaffolding the categorical reading cites
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__absolute_non_intervention, 0.66).
domain_priors:suppression_score(westphalia_sovereignty__absolute_non_intervention, 0.62).
domain_priors:theater_ratio(westphalia_sovereignty__absolute_non_intervention, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, extractiveness, 0.66).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__absolute_non_intervention, tangled_rope).
narrative_ontology:human_readable(westphalia_sovereignty__absolute_non_intervention, "Westphalian Sovereignty — Categorical Non-Intervention Reading").
narrative_ontology:topic_domain(westphalia_sovereignty__absolute_non_intervention, "international_law/political_theory/state_systems").

domain_priors:requires_active_enforcement(westphalia_sovereignty__absolute_non_intervention).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__absolute_non_intervention, '82409d39-abcb-40d4-bf5b-f0c202ea2bd7').
narrative_ontology:cs_kernel_codification('82409d39-abcb-40d4-bf5b-f0c202ea2bd7', fixed_text).
narrative_ontology:cs_authority_grounding('82409d39-abcb-40d4-bf5b-f0c202ea2bd7', lineage).
narrative_ontology:cs_interpretation_layer_present('82409d39-abcb-40d4-bf5b-f0c202ea2bd7').
narrative_ontology:cs_reading_relation('82409d39-abcb-40d4-bf5b-f0c202ea2bd7', westphalia_sovereignty__conditional_responsibility, forecloses).
narrative_ontology:cs_reading_relation('82409d39-abcb-40d4-bf5b-f0c202ea2bd7', westphalia_sovereignty__graded_sovereignty, forecloses).
narrative_ontology:cs_axiom('82409d39-abcb-40d4-bf5b-f0c202ea2bd7', foundational, non_interference_per_se_illegitimate).
narrative_ontology:cs_axiom_status(non_interference_per_se_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('82409d39-abcb-40d4-bf5b-f0c202ea2bd7', non_interference_per_se_illegitimate, conventional).
narrative_ontology:cs_axiom('82409d39-abcb-40d4-bf5b-f0c202ea2bd7', secondary, exception_mechanisms_invite_predatory_abuse).
narrative_ontology:cs_axiom_status(exception_mechanisms_invite_predatory_abuse, holdable).
narrative_ontology:cs_axiom_grounding('82409d39-abcb-40d4-bf5b-f0c202ea2bd7', exception_mechanisms_invite_predatory_abuse, empirically_contingent).
narrative_ontology:cs_reference_frame('82409d39-abcb-40d4-bf5b-f0c202ea2bd7', absolute_territorial_inviolability_baseline).
narrative_ontology:cs_drift_state('82409d39-abcb-40d4-bf5b-f0c202ea2bd7', post_r2p_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('82409d39-abcb-40d4-bf5b-f0c202ea2bd7', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__absolute_non_intervention, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, territorial_state_elites).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, predatory_regime_leaderships).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, vulnerable_small_states).
narrative_ontology:constraint_victim(westphalia_sovereignty__absolute_non_intervention, populations_under_authoritarian_control).
narrative_ontology:constraint_victim(westphalia_sovereignty__absolute_non_intervention, minorities_facing_mass_atrocities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, permanent_five_custodians).
narrative_ontology:constraint_victim(westphalia_sovereignty__absolute_non_intervention, territorial_state_elites).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Govern recognized territories and wield the domestic prerogatives the non-intervention rule secures. The rule converts their internal authority into something no outside actor may formally challenge; in exchange they forswear intervention elsewhere. Their political identity is bound up with the inviolability claim — accepting external review of internal conduct would redefine what they understand themselves to be, so the option is not seriously entertained from where they stand.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, territorial_state_elites, beneficiary,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__absolute_non_intervention, territorial_state_elites, payer).

% Govern through repression, mass detention, or atrocity. The categorical rule is load-bearing for them: it is the only standing barrier between their internal conduct and external consequence. Liberalizing would invite the scrutiny they depend on the rule to prevent, so the shield's value to them rises with the severity of their conduct, and they are its most energetic defenders in international forums.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, predatory_regime_leaderships, beneficiary,
    organized, biographical, trapped, regional).

% Small or militarily weak states whose survival has historically depended on the conquest ban that travels alongside non-interference. They receive the shield's protection and contribute little to its enforcement; their principal exposure is to the great-power exception machinery rather than to the rule itself.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, vulnerable_small_states, beneficiary,
    moderate, generational, constrained, national).

% Live under governments the rule insulates. When the state preys on them, the same guarantee that secures their borders seals them inside: no external petition, no protective entry, no hearing. They appear in international forums only through the votes and voices of the governments afflicting them, and flight — the individual exit — leads to camps and statelessness rather than out from under the arrangement.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, populations_under_authoritarian_control, excluded,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__absolute_non_intervention, populations_under_authoritarian_control, payer).

% Face expulsion or extermination during internal collapses. The categorical bar removes the last external remedy short of unanimity among the custodial powers; their appeals for protection arrive in international venues as requests to violate the rule rather than as claims upon it, and they have no procedural standing of their own anywhere in the system.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, minorities_facing_mass_atrocities, payer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__absolute_non_intervention, minorities_facing_mass_atrocities, excluded).

% Hold the pen over the Charter's exception machinery. They police the categorical rule for everyone else while reserving enforcement discretion for cases they unanimously accept, and each carries a veto that converts the rule's exceptions into a private good. They wrote the text they administer and can act through it when aligned, which places their exit from the rule's constraints in a category no other seat occupies.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, permanent_five_custodians, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__absolute_non_intervention, permanent_five_custodians, beneficiary).

% A coalition of states, non-governmental organizations, and jurists pressing the conduct-forfeiture framing of territorial authority. They secured consensus language in 2005 but implementation runs through the same custodial veto the categorical rule empowers; their position is voiced in every General Assembly debate and contained in every operational decision.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, r2p_advocacy_coalition, excluded,
    organized, generational, constrained, global).

% Adjudicates boundaries, treaty disputes, and the legality of uses of force. Its case law treats internal conduct as jurisdictionally irrelevant except where specific treaties provide otherwise, and its opinions and the academic doctrine around them supply the scaffolding the categorical reading cites when defending itself.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, icj_and_legal_doctrine, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalia_sovereignty__absolute_non_intervention, predatory_regime_leaderships).
narrative_ontology:fixing_cost_class(westphalia_sovereignty__absolute_non_intervention, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the interstate intervention-spiral problem: by converting each state's internal settlement into something no outsider may contest, mutual non-interference breaks the cycle in which every state's enemies find pretexts inside its borders. Borders stabilize, conquest recedes as an instrument, and small states survive — one renunciation, consumed by all.
% TRANSFER_FUNCTION: Moves immunity from external accountability to state elites — concentrated on governments whose internal conduct is worst — paid for by at-risk populations who lose access to external protection, and by all states in the form of surrendered intervention options they might otherwise have priced case by case.
% ABSENT_VOICES: The governed, as distinct from governments, have no seat: populations under authoritarian control and minorities facing atrocities enter the system only through the votes of the states afflicting them. The r2p_advocacy_coalition speaks partially for them and is contained by the custodial veto; no procedural mechanism exists by which the sealed-in populations could object in their own names.
% DISAPPEARANCE_RATIONALE: If the categorical bar vanished overnight, the society of states would reorganize around case-by-case intervention justification: predatory governments would lose their shield immediately, at-risk populations would acquire a live (if imperfect) external remedy, great powers would gain discretionary license they currently lack, and every state's border-security expectation would need renegotiation — the conquest ban's credibility, built on the same foundation, would wobble with it.
% FOUNDING_PROBLEM: The confessional intervention spirals of the seventeenth century: when rulers' legitimacy was tied to transnational religious allegiance, every internal settlement became a foreign policy grievance, and intervention in neighbors' domestic affairs was a duty rather than a crime. The arrangement was built to make each ruler's internal settlement immune by mutual renunciation.
% FOUNDING_PROBLEM_CORROBORATION: Historical scholarship on the Westphalian settlement attests the founding problem and its resolution in its original confessional form — corroboration from outside the benefiting parties that the original problem is dead. Custodial diplomacy attests a generalized security version remains live: that any weakening of the categorical bar invites pretextual intervention by rivals. At-risk-population advocates and responsibility-to-provide scholarship dispute that any surviving version justifies the categorical form rather than a conditioned one. No neutral arbiter exists between these attestations; the disagreement is itself the finding.
narrative_ontology:disappearance_verdict(westphalia_sovereignty__absolute_non_intervention, world_rearranges).
narrative_ontology:founding_problem_status(westphalia_sovereignty__absolute_non_intervention, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__absolute_non_intervention, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(westphalia_sovereignty__absolute_non_intervention, 'none', 1).
narrative_ontology:epsilon_provenance(westphalia_sovereignty__absolute_non_intervention, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalia_sovereignty__absolute_non_intervention_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalia_sovereignty__absolute_non_intervention, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalia_sovereignty__absolute_non_intervention_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.66) is substantial but bounded: the arrangement transfers immunity-from-accountability to state elites, concentrated on predatory governments, and is paid for by at-risk populations who lose access to external protection — but the same structure delivers a real, broadly consumed good (the effective end of territorial conquest). Suppression (0.62) measures the current height of the normative barrier: per se illegitimacy is the reading's defining feature, leaving no balancing test through which an intervention motive could register, so alternatives are delegitimized in advance rather than weighed. Theater ratio (0.48) reflects the growing share of maintenance activity that is selective invocation — states demanding inviolability against rivals while breaching it when convenient — against the still-real core of reciprocal restraint. Accessibility collapse (0.58): the categorical form collapses most intervention alternatives by stipulation, but the conditional and graded siblings keep live alternatives in the discourse, and Kosovo-, Libya-, and R2P-era practice shows the collapse is partial. Resistance (0.60): institutionalized opposition exists (the 2005 consensus language, the atrocity-prevention agenda, humanitarian-access litigation), unusual for a norm of this age and a marker of its contested status. The measurement series run on one shared time grid (1648, 1815, 1945, 1970, 1999, 2005, 2011, 2025) with every tracked metric authored at every point. Extractiveness rises monotonically as human-rights expectations raise the opportunity cost of abandonment and decolonization multiplies the number of shielded elites. The suppression_requirement series is authored because this story specifically tracks enforcement-capacity change: thin reciprocal restraint (1648), an era when the Concert openly qualified sovereignty (1815, the series' trough), the Charter codification hump (1945-1970, the peak of active enforcement), then decay under Kosovo, the 2005 consensus language, and Libya-era backlash. The end-state series value (0.45) sits below the scalar suppression (0.62) deliberately: doctrinal rigidity has outlasted enforcement capacity, and that gap — a barrier held increasingly by inherited text and rhetoric rather than by working machinery — is itself the drift signal the series exists to surface.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute divergent types from identical structure. From the state seats the reading recognizes, the arrangement is close to pure coordination: every state buys border security with a renunciation it can afford, and the P5 additionally collect exception rents. From the population seats the reading refuses to seat, the same structure operates as enforced abandonment — maximal burden, zero exit, no hearing. The refusal is not an oversight; it is the reading's architecture, encoded here as the excluded role carried by populations_under_authoritarian_control and minorities_facing_mass_atrocities. Coalition prospects for the powerless seats are structurally blocked: their would-be members are isolated inside the very jurisdictions the shield seals, and their external champions (the r2p_advocacy_coalition) hold organized but constrained position — voiced in General Assembly debate, contained by the custodial veto whenever implementation is proposed. The permanent_five_custodians seat is genuinely dual-positioned and should compute ambivalently: administrator of the bar, beneficiary of the shield, and the only seat with arbitrage-grade exit (they wrote the exceptions they police).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. territorial_state_elites sit near the beneficiary end: the bar subsidizes their domestic authority, and identity-lock amplifies their attachment (exit would require redefining what they are). predatory_regime_leaderships sit nearest the full-beneficiary pole with trapped exit — the shield's value to them rises monotonically with the severity of their internal conduct, making them the concentrated capturers of the arrangement's rents. vulnerable_small_states are genuine but passive beneficiaries: they consume the conquest ban without administering it. populations_under_authoritarian_control and minorities_facing_mass_atrocities sit at the full-target end: they pay the arrangement's costs (foregone rescue, foregone external pressure) with trapped exit and no independent voice. permanent_five_custodians derive near-beneficiary directionality from their secondary beneficiary role, moderated by their administrative burdens. No directionality overrides are authored: the beneficiary/victim declarations plus exit options already produce the correct relationships, and the one ambiguity worth flagging — whether the reading's state-only subject matter should exclude the population seats from the computation entirely — is routed to the subject_of_extraction_ambiguity omega rather than papered over with an override.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim guards against mislabeling in both directions. Calling this a snare would erase the real coordination achievement — the near-disappearance of territorial conquest, the survival of small states, the stabilization of borders — which is exactly the cover-story risk inverted: here the coordination function is genuine and the extraction rides on it, rather than the coordination story being cover. Calling it a rope would erase the identifiable victims and the concentrated impunity rents that the same structure delivers. The R5 genealogy interview bears on obsolescence without resolving it: the founding problem (confessional intervention spirals among states with irreconcilable internal settlements) is historically resolved in its original form, but a generalized security version is sincerely pressed by the custodians, so founding_problem_status is authored contested rather than dead — which also withholds the dead-plus-world_rearranges mismatch flag until the dispute resolves. The theater series is the early-warning instrument: if selective invocation continues rising while enforcement capacity continues decaying, the structure drifts toward inertial performance — a norm maintained by habit and rhetoric, administered by seats that no longer bear its costs — and the measurement grid is positioned to catch that transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading (absolute_non_intervention) of the westphalia_sovereignty kernel; what would the sibling readings (conditional_responsibility, graded_sovereignty) change structurally if adopted?',
    'Comparative structural analysis across the three readings'' files: victim sets, intervention thresholds, and beneficiary classes. Conditional_responsibility moves at-risk populations into the protected set and introduces conduct-forfeiture; graded_sovereignty replaces the binary shield with capacity-calibrated legitimacy.',
    'Adopting either sibling dissolves the categorical bar that generates this reading''s impunity rents and its abandoned-population victim set; the classification of the family''s downstream intervention-authority constraints shifts accordingly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: this story instantiates one reading of a contested kernel; sibling readings are separate constraints with different victim sets.').

omega_variable(
    subject_of_extraction_ambiguity,
    'Does the categorical reading''s own subject-matter restriction (only states count as parties) mean the arrangement registers as near-pure coordination from every seat the reading recognizes, with the extraction visible only from the population seats the reading refuses to seat?',
    'Seat-level computation admitting versus excluding non-state subjects: run the classification with state seats only, then with population seats added, and compare.',
    'If only state seats count, the arrangement computes close to a rope (broad reciprocal benefit, minimal residual cost); if population seats count, the same structure computes as substantially extractive for the trapped seats. The divergence is not noise — it is the reading''s defining architectural choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subject_of_extraction_ambiguity, conceptual, 'Whether the constraint''s measured character depends on which subjects the reading admits to the analysis.').

omega_variable(
    separability_of_shield_and_impunity,
    'Is the conquest-prevention benefit of categorical non-intervention structurally separable from the impunity it grants predatory governments, or does any conduct-forfeiture exception mechanically reintroduce pretextual intervention?',
    'Natural experiment from post-2005 practice: compare rates of genuinely protection-motivated and pretextually-framed military interventions before and after the responsibility-to-provide consensus language, controlling for great-power involvement.',
    'If separable, the impunity component is removable overhead and the conditional sibling dominates; if inseparable, part of the measured burden on at-risk populations is the price of the conquest ban that also protects them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(separability_of_shield_and_impunity, empirical, 'Whether the coordination benefit and the impunity rent can be unbundled by institutional design.').

omega_variable(
    selective_invocation_theater_share,
    'What share of contemporary maintenance activity is sincere commitment to the categorical rule versus great-power convenience — invoking inviolability against rivals while breaching it when convenient?',
    'Code invocation patterns in Security Council debate and diplomatic protest by intervener identity and alignment: sincere-commitment maintenance predicts symmetric invocation; convenience maintenance predicts alignment-correlated invocation.',
    'A high convenience share raises the effective theater ratio above the authored 0.48 and pushes the structure toward inertial performance; a low share supports the genuine-coordination component of the tangled-rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_invocation_theater_share, empirical, 'Decomposing maintenance activity into sincere coordination defense versus selective rhetorical deployment.').

omega_variable(
    enforcement_rehardening_trajectory,
    'Will the enforcement machinery of the categorical bar continue decaying from its 1970 peak, or re-harden under great-power competition that rewards inviolability rhetoric?',
    'Track Security Council authorization practice, customary-law commentary, and state protest behavior over the next two decades for renewed codification or further attrition.',
    'Re-hardening restores the suppression series'' upward arc and stabilizes the tangled-rope configuration; continued decay with sustained rhetoric drives the structure toward inertial performance maintained by habit rather than capacity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_rehardening_trajectory, empirical, 'Future direction of the enforcement-capacity trajectory the measurement series documents.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__absolute_non_intervention, 1648, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(westphalia_ani_tr_t1648, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 1648, 0.08).
narrative_ontology:measurement(westphalia_ani_tr_t1815, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 1815, 0.12).
narrative_ontology:measurement(westphalia_ani_tr_t1945, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 1945, 0.22).
narrative_ontology:measurement(westphalia_ani_tr_t1970, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 1970, 0.3).
narrative_ontology:measurement(westphalia_ani_tr_t1999, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 1999, 0.4).
narrative_ontology:measurement(westphalia_ani_tr_t2005, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 2005, 0.44).
narrative_ontology:measurement(westphalia_ani_tr_t2011, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 2011, 0.46).
narrative_ontology:measurement(westphalia_ani_tr_t2025, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 2025, 0.48).

% Extraction over time
narrative_ontology:measurement(westphalia_ani_be_t1648, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 1648, 0.34).
narrative_ontology:measurement(westphalia_ani_be_t1815, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 1815, 0.38).
narrative_ontology:measurement(westphalia_ani_be_t1945, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 1945, 0.48).
narrative_ontology:measurement(westphalia_ani_be_t1970, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 1970, 0.56).
narrative_ontology:measurement(westphalia_ani_be_t1999, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 1999, 0.61).
narrative_ontology:measurement(westphalia_ani_be_t2005, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 2005, 0.63).
narrative_ontology:measurement(westphalia_ani_be_t2011, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 2011, 0.65).
narrative_ontology:measurement(westphalia_ani_be_t2025, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 2025, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(westphalia_ani_su_t1648, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 1648, 0.25).
narrative_ontology:measurement(westphalia_ani_su_t1815, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 1815, 0.2).
narrative_ontology:measurement(westphalia_ani_su_t1945, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 1945, 0.6).
narrative_ontology:measurement(westphalia_ani_su_t1970, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 1970, 0.65).
narrative_ontology:measurement(westphalia_ani_su_t1999, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 1999, 0.55).
narrative_ontology:measurement(westphalia_ani_su_t2005, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 2005, 0.5).
narrative_ontology:measurement(westphalia_ani_su_t2011, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 2011, 0.47).
narrative_ontology:measurement(westphalia_ani_su_t2025, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 2025, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__absolute_non_intervention, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalia_sovereignty__absolute_non_intervention, westphalia_sovereignty__conditional_responsibility).
narrative_ontology:affects_constraint(westphalia_sovereignty__absolute_non_intervention, westphalia_sovereignty__graded_sovereignty).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'sovereignty' covers three structurally distinct claims with different victim sets, intervention thresholds, and beneficiary classes. This story (absolute_non_intervention) is the historical baseline reading from which the other two depart; it is upstream in influence (its Charter codification is the text both siblings amend or reinterpret) and downstream in contest (both siblings exist as responses to its abandonment costs). Each family member carries its own stable epsilon; the edges here propagate contamination and legitimacy shifts across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
