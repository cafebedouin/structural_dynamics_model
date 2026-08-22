% ============================================================================
% CONSTRAINT STORY: article_17_complementarity__national_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-22
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_17_complementarity__national_primacy_reading, []).

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
 *   constraint_id: article_17_complementarity__national_primacy_reading
 *   human_readable: Article 17 Complementarity — National Primacy Reading
 *   domain: international_law/criminal_justice/state_sovereignty
 *
 * SUMMARY:
 *   The national_primacy_reading of Article 17 complementarity treats
 *   national courts as presumptively adequate to prosecute international
 *   crimes, placing the burden on the ICC Prosecutor to demonstrate that a
 *   state is 'unwilling or unable genuinely' to proceed. This reading
 *   structures complementarity as a sovereignty-protection mechanism: it
 *   coordinates deference to national systems while extracting accountability
 *   from victims in states where proceedings exist but are structurally weak.
 *   The constraint operates as a tangled rope — it solves a genuine
 *   coordination problem (preventing parallel proceedings, respecting
 *   sovereign judicial functions) while simultaneously creating a high
 *   inadmissibility threshold that excludes victims in weak-but-genuine
 *   systems. Beneficiaries are national judiciaries (which retain primacy)
 *   and sovereignty-maximizing states (which avoid ICC scrutiny). Victims are
 *   those in states with ongoing but ineffective proceedings, and civil
 *   society actors in states conducting sham proceedings that nevertheless
 *   meet the formal 'genuine' threshold. The constraint requires active
 *   enforcement through the ICC's admissibility challenge mechanism and state
 *   cooperation obligations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_17_complementarity__national_primacy_reading, 0.38).
domain_priors:suppression_score(article_17_complementarity__national_primacy_reading, 0.42).
domain_priors:theater_ratio(article_17_complementarity__national_primacy_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_17_complementarity__national_primacy_reading, tangled_rope).
narrative_ontology:human_readable(article_17_complementarity__national_primacy_reading, "Article 17 Complementarity — National Primacy Reading").
narrative_ontology:topic_domain(article_17_complementarity__national_primacy_reading, "international_law/criminal_justice/state_sovereignty").

domain_priors:requires_active_enforcement(article_17_complementarity__national_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_17_complementarity__national_primacy_reading, '6b4e6145-dd4d-408f-b189-a633ddc27e20').
narrative_ontology:cs_kernel_codification('6b4e6145-dd4d-408f-b189-a633ddc27e20', formalized).
narrative_ontology:cs_authority_grounding('6b4e6145-dd4d-408f-b189-a633ddc27e20', lineage).
narrative_ontology:cs_interpretation_layer_present('6b4e6145-dd4d-408f-b189-a633ddc27e20').
narrative_ontology:cs_reading_relation('6b4e6145-dd4d-408f-b189-a633ddc27e20', article_17_complementarity__international_oversight_reading, coexists_with).
narrative_ontology:cs_axiom('6b4e6145-dd4d-408f-b189-a633ddc27e20', foundational, national_courts_presumptively_adequate).
narrative_ontology:cs_axiom_status(national_courts_presumptively_adequate, holdable).
narrative_ontology:cs_axiom_grounding('6b4e6145-dd4d-408f-b189-a633ddc27e20', national_courts_presumptively_adequate, conventional).
narrative_ontology:cs_axiom('6b4e6145-dd4d-408f-b189-a633ddc27e20', foundational, icc_burden_to_demonstrate_inadmissibility).
narrative_ontology:cs_axiom_status(icc_burden_to_demonstrate_inadmissibility, holdable).
narrative_ontology:cs_axiom_grounding('6b4e6145-dd4d-408f-b189-a633ddc27e20', icc_burden_to_demonstrate_inadmissibility, conventional).
narrative_ontology:cs_reference_frame('6b4e6145-dd4d-408f-b189-a633ddc27e20', rome_statute_original_negotiated_balance).
narrative_ontology:cs_drift_state('6b4e6145-dd4d-408f-b189-a633ddc27e20', post_lubanga_katanga_gaddafi_afghanistan_jurisprudence, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6b4e6145-dd4d-408f-b189-a633ddc27e20', '').
narrative_ontology:cs_kernel_id(article_17_complementarity__national_primacy_reading, article_17_complementarity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, national_judiciaries).
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, sovereignty_maximizing_states).
narrative_ontology:constraint_victim(article_17_complementarity__national_primacy_reading, victims_in_weak_judicial_systems).
narrative_ontology:constraint_victim(article_17_complementarity__national_primacy_reading, civil_society_actors_in_sham_proceeding_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(article_17_complementarity__national_primacy_reading, icc_prosecutor).
narrative_ontology:constraint_vindicates(article_17_complementarity__national_primacy_reading, state_sovereignty_primacy).
narrative_ontology:constraint_vindicates(article_17_complementarity__national_primacy_reading, national_judicial_autonomy).
narrative_ontology:constraint_vindicates(article_17_complementarity__national_primacy_reading, complementarity_as_sovereignty_shield).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain primary jurisdiction over international crimes committed on their territory or by their nationals. The complementarity principle protects their adjudicative authority from ICC encroachment unless they are proven unwilling or unable. They can voluntarily refer situations to the ICC (arbitrage exit) but structurally benefit from the presumption of adequacy.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, national_judiciaries, beneficiary,
    institutional, generational, arbitrage, national).

% Use the high inadmissibility threshold to shield officials and allies from ICC prosecution. They benefit from the coordination function (non-interference) and the extraction function (accountability avoidance). Their exit is arbitrage — they can accept ICC jurisdiction ad hoc but treat complementarity as a default shield.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, sovereignty_maximizing_states, beneficiary,
    institutional, generational, arbitrage, national).

% Bears the burden of proving inadmissibility — must demonstrate that national proceedings are not genuine, or that the state is unwilling or unable. The structural presumption of adequacy makes this burden high. The Prosecutor pays reputational and resource costs for failed admissibility challenges. Exit is constrained by the Rome Statute mandate.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, icc_prosecutor, agenda_setter,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(article_17_complementarity__national_primacy_reading, icc_prosecutor, payer).

% Adjudicate admissibility challenges. Their jurisprudence sets the operational 'sham vs. genuine' threshold. They are constrained by the statutory language ('unwilling or unable genuinely') and the presumption of adequacy, but their interpretations structurally determine the constraint's effective extraction.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, pre_trial_chambers, agenda_setter,
    institutional, biographical, constrained, global).

% Experience national proceedings that are formally ongoing but structurally incapable of delivering accountability (lack of resources, witness protection, judicial independence). The ICC is inadmissible because proceedings exist; national courts cannot deliver justice. They are trapped — no effective domestic remedy, no international access.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, victims_in_weak_judicial_systems, payer,
    powerless, biographical, trapped, national).

% Document and challenge sham proceedings that nevertheless meet the formal 'genuine' threshold (e.g., show trials with procedural regularity but predetermined outcomes). They bear the cost of the constraint's false negatives. Their exit is constrained — they can petition the ICC but face high evidentiary bars to overcome the presumption.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, civil_society_actors_in_sham_proceeding_states, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(article_17_complementarity__national_primacy_reading, civil_society_actors_in_sham_proceeding_states, excluded).

% Monitor complementarity application, advocate for broader 'unwilling or unable' interpretation, submit amicus briefs on admissibility. They experience the constraint's extraction but lack structural power to alter the threshold. Their role is analytical — documenting the gap between the coordination claim and the extraction reality.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, international_ngos_victim_advocates, observer,
    organized, generational, analytical, global).

% The ICC's legislative and oversight body. Can amend the Rules of Procedure and Evidence to adjust admissibility standards, but political dynamics (sovereignty-maximizing states form a blocking coalition) make amendment unlikely. They observe the constraint's operation but are structurally constrained from reforming it.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, assembly_of_states_parties, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents parallel international and national proceedings for the same conduct; respects the primary sovereign right and duty of states to prosecute international crimes; allocates adjudicative authority between national systems and the ICC based on genuine national capacity and will.
% TRANSFER_FUNCTION: Transfers accountability for international crimes from the ICC (which would prosecute) to national systems (which may be weak or compromised), benefiting state sovereignty interests at the cost of victims in systems that cannot deliver justice.
% ABSENT_VOICES: Victims in non-States Parties situations (e.g., Myanmar, Syria) where the ICC has no jurisdiction at all — they are excluded from both the national and international tracks. Also absent: future generations who bear the systemic cost of normalized impunity for structural judicial weakness.
% DISAPPEARANCE_RATIONALE: If the national primacy reading vanished overnight, the ICC would become a court of concurrent rather than complementary jurisdiction. States with weak systems would lose the inadmissibility shield; the Prosecutor could open investigations without proving 'unwilling or unable.' The accountability landscape would shift toward the international_oversight_reading's operational logic — more situations reach the ICC, but state cooperation would likely collapse further.
% FOUNDING_PROBLEM: The Rome Statute drafters needed to reconcile universal jurisdiction for core crimes with state sovereignty — specifically, to prevent the ICC from becoming a court of first resort that displaces national systems, while ensuring accountability when national systems fail.
% FOUNDING_PROBLEM_CORROBORATION: The sovereignty-protection framing is corroborated by the negotiating history (delegations from major powers insisted on a high admissibility threshold). The accountability-gap framing is corroborated by ICC ASP-commissioned independent expert reviews (2020, 2023), victim participation surveys, and civil society shadow reports documenting the 'weak-but-genuine' exclusion zone. No single corroborating source resolves the contestation — the founding problem itself is structurally ambiguous.
narrative_ontology:disappearance_verdict(article_17_complementarity__national_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_17_complementarity__national_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_17_complementarity__national_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(article_17_complementarity__national_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_17_complementarity__national_primacy_reading, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_17_complementarity__national_primacy_reading_tests).
:- end_tests(article_17_complementarity__national_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) reflects that the constraint extracts accountability from a defined victim class (those in weak-but-genuine systems) to benefit national sovereignty interests. The extraction is not maximal because the constraint genuinely coordinates — it prevents forum shopping and respects functional national systems. Suppression (0.42) is moderate: the constraint suppresses ICC jurisdiction through the inadmissibility bar, but states can still voluntarily refer situations or accept jurisdiction. Theater ratio (0.18) is low: the coordination function (preventing duplicative proceedings, respecting sovereign adjudication) is real and operational, not merely performative. Accessibility collapse (0.35) is moderate: alternatives exist (state referrals, UNSC referrals, voluntary acceptance) but are politically constrained. Resistance (0.28) is low-moderate: states generally accept complementarity as a principle; contestation focuses on the 'unwilling or unable' threshold, not the principle itself.
 *
 * PERSPECTIVAL GAP:
 *   From the national_judiciaries seat, the constraint appears as genuine coordination — a rope that respects sovereign equality and prevents international overreach. From the victims_in_weak_judicial_systems seat, the same structure operates as extraction — a tangled rope where the coordination story (genuine proceedings) covers accountability denial. The engine computes this divergence from the beneficiary/victim declarations and exit options; the claimed_type (tangled_rope) reflects the structural reality that both perceptions are simultaneously true.
 *
 * DIRECTIONALITY LOGIC:
 *   National judiciaries and sovereignty-maximizing states are structural beneficiaries (d ~ 0.15-0.25): they receive the primary coordination benefit (primacy retention) and the extraction benefit (ICC exclusion). Their exit options are arbitrage (they can opt into ICC jurisdiction via referral or acceptance but rarely do). Victims in weak judicial systems and civil society actors in sham-proceeding states are structural targets (d ~ 0.75-0.85): they bear the accountability extraction with constrained exit (trapped in national systems that cannot deliver justice, ICC access blocked by inadmissibility). The ICC Prosecutor and Pre-Trial Chambers are agenda-setters (d ~ 0.45): they administer the constraint but are structurally pressured toward deference by the presumption of adequacy. International NGOs and victim advocates are observers/excluded (d ~ 0.6-0.7): they experience the constraint's extraction but lack structural power to alter the threshold.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing ICC overreach while ensuring accountability for core crimes — remains contested (founding_problem_status: contested). The national_primacy_reading claims the problem is live (sovereign overreach is a real risk); the international_oversight_reading claims the problem has shifted (impunity via weak proceedings is the dominant risk). Corroboration from outside beneficiaries (ICC ASP reports, independent expert reviews, victim surveys) supports the shifted-problem reading. The constraint does not resolve mandatrophy — it institutionalizes the sovereignty-protection framing while the accountability gap widens, making it a candidat for future reclassification if extraction accumulation continues.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'Is complementarity a single coherent principle or a contested kernel producing structurally distinct constraints?',
    'Analyze whether the national_primacy_reading and international_oversight_reading instantiate constraints with different ε values, beneficiary/victim sets, and enforcement logics — which would confirm they are separate constraints under the ε-invariance principle.',
    'If confirmed as separate constraints, each must be modeled independently with its own classification; the ''complementarity'' label would be identified as a false-summit conflation similar to the BGS case.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Whether the national_primacy_reading and international_oversight_reading are readings of a single kernel producing distinct constraints').

omega_variable(
    sham_proceeding_threshold,
    'Where is the structural boundary between ''genuine but weak'' proceedings (which trigger inadmissibility under this reading) and ''sham'' proceedings (which do not)?',
    'Empirical analysis of ICC Pre-Trial Chamber admissibility decisions — coding the factual predicates that distinguish ''unwilling'' from ''unable'' and ''genuine'' from ''sham'' across the Lubanga, Katanga, Gaddafi, Al Bashir, and Afghanistan decisions.',
    'If the boundary is operationally indeterminate, the constraint''s suppression is higher than measured (prosecutorial discretion becomes a de facto gate). If the boundary is strict and transparent, the constraint''s coordination function is more credible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sham_proceeding_threshold, empirical, 'Operational determinacy of the ''sham vs. genuine'' threshold that governs ICC admissibility').

omega_variable(
    victim_exclusion_scope,
    'How many victims of serious crimes fall into the ''weak-but-genuine'' gap — states investigating but lacking capacity to prosecute effectively — and are therefore excluded from ICC reach under this reading?',
    'Quantitative mapping of situations where national proceedings exist but are structurally incapable of delivering accountability (e.g., CAR II, Georgia, Ukraine pre-2022), compared against ICC jurisdictional reach.',
    'If the gap is large, the constraint operates as a substantial victim-exclusion mechanism despite its coordination framing; if small, the coordination function dominates.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_exclusion_scope, empirical, 'Scale of victim exclusion created by the ''genuine proceedings'' inadmissibility bar').

omega_variable(
    enforcement_asymmetry,
    'Does the constraint''s enforcement burden fall asymmetrically — requiring state cooperation for ICC action while imposing no reciprocal obligation on states to accept ICC jurisdiction?',
    'Structural analysis of the Rome Statute''s cooperation regime (Part 9) versus the complementarity regime (Article 17): who must act, who may refuse, and what are the consequences.',
    'Asymmetric enforcement would confirm the constraint as a sovereignty-protection mechanism with a coordination veneer; symmetric enforcement would support the rope interpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_asymmetry, conceptual, 'Whether the enforcement structure is reciprocal or one-directional').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_17_complementarity__national_primacy_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_17_complementarity__national_primacy_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(arti_tr_t5, article_17_complementarity__national_primacy_reading, theater_ratio, 5, 0.14).
narrative_ontology:measurement(arti_tr_t10, article_17_complementarity__national_primacy_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(arti_tr_t15, article_17_complementarity__national_primacy_reading, theater_ratio, 15, 0.16).
narrative_ontology:measurement(arti_tr_t20, article_17_complementarity__national_primacy_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement(arti_tr_t25, article_17_complementarity__national_primacy_reading, theater_ratio, 25, 0.18).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_17_complementarity__national_primacy_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(arti_be_t5, article_17_complementarity__national_primacy_reading, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(arti_be_t10, article_17_complementarity__national_primacy_reading, base_extractiveness, 10, 0.31).
narrative_ontology:measurement(arti_be_t15, article_17_complementarity__national_primacy_reading, base_extractiveness, 15, 0.34).
narrative_ontology:measurement(arti_be_t20, article_17_complementarity__national_primacy_reading, base_extractiveness, 20, 0.36).
narrative_ontology:measurement(arti_be_t25, article_17_complementarity__national_primacy_reading, base_extractiveness, 25, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_17_complementarity__national_primacy_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(arti_su_t5, article_17_complementarity__national_primacy_reading, suppression_requirement, 5, 0.38).
narrative_ontology:measurement(arti_su_t10, article_17_complementarity__national_primacy_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(arti_su_t15, article_17_complementarity__national_primacy_reading, suppression_requirement, 15, 0.41).
narrative_ontology:measurement(arti_su_t20, article_17_complementarity__national_primacy_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(arti_su_t25, article_17_complementarity__national_primacy_reading, suppression_requirement, 25, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_17_complementarity__national_primacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_17_complementarity__national_primacy_reading, 0.12).
narrative_ontology:affects_constraint(article_17_complementarity__national_primacy_reading, article_17_complementarity__international_oversight_reading).
narrative_ontology:affects_constraint(article_17_complementarity__national_primacy_reading, rome_statute_article_53_prosecutorial_discretion).
narrative_ontology:affects_constraint(article_17_complementarity__national_primacy_reading, state_cooperation_obligations_part_9).

% DUAL FORMULATION NOTE:
% This constraint and international_oversight_reading form a constraint family decomposing the 'complementarity' label. This reading (national_primacy) has ε=0.38, beneficiaries=national_judiciaries/sovereignty_maximizing_states, victims=victims_in_weak_judicial_systems. The sibling reading (international_oversight) has ε≈0.65, beneficiaries=victims_of_core_crimes/civil_society, victims=state_officials_evading_accountability. They are not the same constraint measured differently — they are structurally distinct claims linked by the kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_17_complementarity__national_primacy_reading, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
