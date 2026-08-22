% ============================================================================
% CONSTRAINT STORY: constitutional_interpretive_authority__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_interpretive_authority__judicial_supremacy_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: constitutional_interpretive_authority__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy Reading of Constitutional Interpretive Authority
 *   domain: political/jurisprudential
 *
 * SUMMARY:
 *   In constitutional orders that adopt this reading, an apex court holds the
 *   last word on constitutional meaning, and legislative acts that conflict
 *   with the court's interpretation are void. The arrangement presents itself
 *   as rights guardianship — protecting fundamental rights and constitutional
 *   limits from transient majorities — while simultaneously concentrating
 *   interpretive authority in an unelected body whose members gain agenda
 *   control, prestige, and policy influence from the arrangement's
 *   continuation. This story instantiates ONE reading of the
 *   constitutional_interpretive_authority kernel; the sibling readings are
 *   separate constraints, not averaged into this file. KEY AGENTS (by
 *   structural relationship): - apex_court_judges: Agenda-setting beneficiary
 *   (institutional/identity_locked) — holds the final word and collects
 *   authority, prestige, and agenda control from its exercise -
 *   elected_legislature: Primary target (institutional/constrained) — its
 *   enactments are subject to nullification; amendment path runs through the
 *   court's own shadow - individual_rights_holders: Secondary beneficiary
 *   (powerless/constrained) — receives the rights protection the arrangement
 *   provides, accessed through costly litigation -
 *   democratic_majority_voters: Mixed-position target (organized/constrained)
 *   — enacted preferences are the thing nullified, yet the same population is
 *   shielded by the rights enforcement - legislative_override_proponents:
 *   Excluded challenger (powerful/trapped) — seeks to reallocate final
 *   authority; holds no seat in the process -
 *   comparative_constitutional_scholars: Analytical observer
 *   (analytical/analytical) — sees the full comparative structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_interpretive_authority__judicial_supremacy_reading, 0.44).
domain_priors:suppression_score(constitutional_interpretive_authority__judicial_supremacy_reading, 0.52).
domain_priors:theater_ratio(constitutional_interpretive_authority__judicial_supremacy_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, extractiveness, 0.44).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_interpretive_authority__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_interpretive_authority__judicial_supremacy_reading, "Judicial Supremacy Reading of Constitutional Interpretive Authority").
narrative_ontology:topic_domain(constitutional_interpretive_authority__judicial_supremacy_reading, "political/jurisprudential").

domain_priors:requires_active_enforcement(constitutional_interpretive_authority__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_interpretive_authority__judicial_supremacy_reading, '51872630-6c9a-47b1-bd79-916fbf56da28').
narrative_ontology:cs_kernel_codification('51872630-6c9a-47b1-bd79-916fbf56da28', fixed_text).
narrative_ontology:cs_authority_grounding('51872630-6c9a-47b1-bd79-916fbf56da28', expertise).
narrative_ontology:cs_interpretation_layer_present('51872630-6c9a-47b1-bd79-916fbf56da28').
narrative_ontology:cs_reading_relation('51872630-6c9a-47b1-bd79-916fbf56da28', constitutional_interpretive_authority__parliamentary_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('51872630-6c9a-47b1-bd79-916fbf56da28', constitutional_interpretive_authority__coordinate_construction_reading, forecloses).
narrative_ontology:cs_axiom('51872630-6c9a-47b1-bd79-916fbf56da28', foundational, fundamental_rights_require_insulated_guardian).
narrative_ontology:cs_axiom_status(fundamental_rights_require_insulated_guardian, holdable).
narrative_ontology:cs_axiom_grounding('51872630-6c9a-47b1-bd79-916fbf56da28', fundamental_rights_require_insulated_guardian, deontological).
narrative_ontology:cs_axiom('51872630-6c9a-47b1-bd79-916fbf56da28', foundational, constitutional_interpretation_is_law_not_will).
narrative_ontology:cs_axiom_status(constitutional_interpretation_is_law_not_will, holdable).
narrative_ontology:cs_axiom_grounding('51872630-6c9a-47b1-bd79-916fbf56da28', constitutional_interpretation_is_law_not_will, conventional).
narrative_ontology:cs_reference_frame('51872630-6c9a-47b1-bd79-916fbf56da28', final_judicial_interpretation_of_supreme_law).
narrative_ontology:cs_drift_state('51872630-6c9a-47b1-bd79-916fbf56da28', contemporary_polarization_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('51872630-6c9a-47b1-bd79-916fbf56da28', '').
narrative_ontology:cs_kernel_id(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__judicial_supremacy_reading, apex_court_judges).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__judicial_supremacy_reading, individual_rights_holders).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, elected_legislature).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, democratic_majority_voters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__judicial_supremacy_reading, democratic_majority_voters).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__judicial_supremacy_reading, countermajoritarian_guardianship_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Life-tenured justices who decide which constitutional disputes reach decision, write the operative statements of constitutional meaning, and void statutes that conflict with their reading. Institutional prestige, agenda control over the nation's deepest policy questions, and historical legacy flow to them from holding the final word. Exit means retirement from the bench; the guardianship role is constitutive of their professional identity, and no external market exists for the authority they hold.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, apex_court_judges, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__judicial_supremacy_reading, apex_court_judges, beneficiary).

% Drafts and enacts statutes under the standing possibility that any act may be voided by the court's reading of the constitution. Bears the nullification of its products and the planning uncertainty of governing under review. Its remedy — constitutional amendment — requires supermajorities and, in practice, forms of assent the court itself shapes; it cannot opt out of being reviewed.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, elected_legislature, payer,
    institutional, biographical, constrained, national).

% Rely on the court's nullification power as the working shield against abridgment of speech, liberty, and equal treatment by majoritarian bodies. Access runs through litigation — costly, slow, and dependent on the court's discretionary docket — so protection is real but unevenly reachable.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, individual_rights_holders, beneficiary,
    powerless, biographical, constrained, national).

% Elect the legislature whose acts the court may void; when a favored statute falls, the policy decision moves from their coalition to the bench. The same population is covered by the rights protections the arrangement enforces, so each voter stands on both sides of the exchange depending on which act is at issue.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, democratic_majority_voters, payer,
    organized, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__judicial_supremacy_reading, democratic_majority_voters, beneficiary).

% Legislators and theorists who press for override mechanisms, jurisdiction limits, or escalation rules that would let elected bodies reclaim the last word on constitutional questions. They hold no seat in the interpretive process they seek to reform; their proposals die in the very institutions the arrangement empowers, and their remedy lies inside a system whose gatekeepers decline to admit it.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, legislative_override_proponents, excluded,
    powerful, biographical, trapped, national).

% Study how different constitutional orders allocate final interpretive authority — judicial supremacy, parliamentary sovereignty, dialogic hybrids — and document the trade-offs each produces in rights protection, legitimacy, and stability. They neither collect nor pay under this arrangement; they observe and publish.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_interpretive_authority__judicial_supremacy_reading, apex_court_judges).
narrative_ontology:fixing_cost_class(constitutional_interpretive_authority__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single authoritative resolver of disputes over constitutional meaning: conflicts among branches, levels of government, and citizens are settled by one forum, producing stable, predictable limits on governmental power and a uniform guarantee of fundamental rights across jurisdictions.
% TRANSFER_FUNCTION: Moves interpretive authority over the constitution — and with it veto power over enacted legislation — from elected legislatures and their constituents to unelected judges; each nullified act transfers the decision over a policy question from the electoral coalition that enacted it to the court that strikes it.
% ABSENT_VOICES: Parliamentary-supremacy and coordinate-construction theorists stand outside the courtroom frame — their preferred allocations are not represented in the proceeding that exercises final authority. Legislative sponsors of struck statutes appear only as losing litigants; ordinary citizens encounter the arrangement through appointment politics and compliance, with no seat in the interpretive process itself.
% DISAPPEARANCE_RATIONALE: If final judicial interpretive authority vanished overnight, constitutional politics would reorganize around whichever alternative allocation the polity adopted — legislative self-interpretation, inter-branch bargaining, or a specialized council — rights enforcement would migrate to political channels, and the large body of settled precedent governing legislation would become open questions.
% FOUNDING_PROBLEM: After ratification, someone had to decide disputes over the constitution's meaning: whether federal or state power prevailed, whether enacted laws exceeded granted powers, and how fundamental-rights limits bound majoritarian institutions. The arrangement was built to settle those disputes through a body insulated from the electoral pressures that produced the contested acts.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: two centuries of legislative testimony and court-curbing debate take the persistence of constitutional disagreement for granted while disputing the judicial monopoly on resolving it; comparative constitutional scholarship documents that the underlying problem — binding precommitment requires an interpreter — recurs in every constitutional order regardless of which reading it adopts. No serious participant, including the arrangement's sharpest critics, claims the founding problem is dead; the contest is over the allocator, not the problem.
narrative_ontology:disappearance_verdict(constitutional_interpretive_authority__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_interpretive_authority__judicial_supremacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_interpretive_authority__judicial_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_interpretive_authority__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_interpretive_authority__judicial_supremacy_reading, 0.44, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_interpretive_authority__judicial_supremacy_reading_tests).
:- end_tests(constitutional_interpretive_authority__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored mid-range (0.44 at interval end) because the reading grants the guardianship function genuine weight — rights protection and stable constitutional meaning are real outputs — while conceding that every nullification transfers decision power from an electoral coalition to a small unelected body, and that the judiciary's institutional stake in its own finality is never priced into the arrangement. Suppression (0.52) reflects a compliance regime built less on overt coercion than on jurisdictional control, precedent accumulation, and official socialization into the finality norm. Theater ratio (0.32) captures the share of guardianship rhetoric that functions as institutional self-defense rather than rights protection — visible in opinion language that presents preference as principle. Accessibility collapse (0.58): once the reading is entrenched, rival allocations are foreclosed inside the system though demonstrably alive in other orders. Resistance (0.48): recurring court-curbing proposals, jurisdiction-limit bills, defiance episodes, and appointment warfare. The measurement series run on one shared eight-point grid spanning the interval; the mild non-monotonicity in both series tracks the activism-retrenchment cycle (early restraint, Lochner-era aggression, mid-century retreat, late-century expansion, partial pullback, recent renewal). The cycle is driven by appointment turnover interacting with case mix, not engineered as intermittent reinforcement; end-state values were taken during a consolidation phase following the most recent expansion wave.
 *
 * PERSPECTIVAL GAP:
 *   From the bench, the arrangement is the rule of law itself: neutral exposition of supreme law, with nullification as duty rather than choice. From the legislature's seat, the same arrangement is a standing veto held by an unaccountable body over everything the electorate enacted. From the rights-holder's seat it is the only reliable shield available. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   The declarations map onto the structure directly: apex_court_judges hold the pen and the docket — full beneficiary side; individual_rights_holders receive the protection the arrangement provides — beneficiary side. elected_legislature bears nullification with no exit (amendment runs through supermajorities and ultimately assent the court shapes) — near-full target. democratic_majority_voters are declared victims because their enacted preferences are precisely what gets nullified, but they are simultaneously the population the rights shield protects; the derivation from the victim declaration alone would read them as near-full targets, so an override sets their directionality to 0.62 — mixed, target-leaning. No override is applied at the institutional power atom despite its containing both the bench (beneficiary) and the legislature (target), because each seat's directionality derives correctly from its own opposed declarations; a single atom-level override would corrupt one of them.
 *
 * MANDATROPHY ANALYSIS:
 *   Reading the arrangement as pure coordination (neutral arbitration everyone needs) erases the transfer function — the standing veto and the judiciary's self-interested stake in its own finality. Reading it as pure extraction (unelected lawyers vetoing democracy) erases the coordination function — without a final interpreter, constitutional disputes have no terminus and rights guarantees fragment by jurisdiction. The tangled-rope classification keeps both halves visible: genuine coordination worth paying for, asymmetrically positioned seats, and enforcement that must stay active because the losing side never consents to lose. The founding problem — constitutional disagreement needs a settler — remains live, so no mandatrophy declaration is authored; the arrangement's characteristic risk is not obsolescence but drift, guardianship shading into preference, which the omega variables carry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_locus_contestation,
    'This story instantiates the judicial_supremacy_reading of the constitutional_interpretive_authority kernel; how would the parliamentary_supremacy or coordinate_construction readings restructure the beneficiary and victim sets?',
    'Comparative institutional analysis: jurisdictions that adopted each reading (parliamentary sovereignty orders, judicial-supremacy orders, dialogic hybrid systems) reveal divergent beneficiary/victim structures; adoption events such as rights-statute enactments and override-clause adoptions act as natural experiments on the allocation question.',
    'Under the parliamentary reading the judiciary exits the beneficiary set and the legislature becomes agenda-setter; under the coordinate reading no seat holds final authority and positional advantage diffuses across branches. This story''s classification is invariant — siblings are separate constraints — but cross-reading comparison changes which seats bear costs and which collect.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_locus_contestation, conceptual, 'Committer structure: one of three mutually exclusive readings of the final-authority kernel; sibling readings relocate the entire beneficiary/victim structure.').

omega_variable(
    guardianship_vs_self_dealing,
    'Is the judiciary''s exercise of final interpretive authority primarily rights-protective guardianship or institutionally self-serving aggrandizement?',
    'Outcome audit against counterfactual legislative resolution of the same disputes; coding of opinions for rights-protective versus institution-aggrandizing rationales; comparison of invalidation rates when judicial power itself is the stake.',
    'Predominantly self-serving operation would push the arrangement toward the pure-extraction end; predominantly protective operation supports the coordination-dominant reading and strengthens the guardianship justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(guardianship_vs_self_dealing, empirical, 'Whether the guardianship rationale tracks outcomes or covers institutional self-interest.').

omega_variable(
    insulation_independence_tradeoff,
    'Does life-tenured insulation produce the independence the guardianship function requires, or the unaccountability that lets interpretive authority drift toward personal and partisan preference?',
    'Cross-jurisdiction comparison of selection and tenure designs against rights-protection outcomes and ideological voting coherence on benches.',
    'If insulation yields capture-by-preference, the coordination function degrades and effective extraction rises; if it yields genuine independence, the design justification strengthens and the floor cost of the arrangement is vindicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(insulation_independence_tradeoff, empirical, 'Whether the insulating design delivers guardianship quality or enables drift toward preference.').

omega_variable(
    compliance_internalization_ambiguity,
    'Is compliance with judicial nullification maintained by structural enforcement capacity or by internalized rule-of-law norms among officials and the bar?',
    'Post-crisis compliance trajectories: episodes of open defiance and delayed implementation reveal how much compliance depends on active coercive capacity versus socialized acceptance of judicial finality.',
    'If internalized, the arrangement persists even as formal enforcement capacity decays — the suppression measure understates its durability; if structural, enforcement erosion predicts rapid unraveling of the finality norm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_internalization_ambiguity, empirical, 'Structural versus internalized maintenance of compliance with nullification rulings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_interpretive_authority__judicial_supremacy_reading, 0, 210).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(cons_tr_t0, observed).
narrative_ontology:measurement(cons_tr_t30, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement_basis(cons_tr_t30, observed).
narrative_ontology:measurement(cons_tr_t60, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 60, 0.16).
narrative_ontology:measurement_basis(cons_tr_t60, observed).
narrative_ontology:measurement(cons_tr_t90, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 90, 0.24).
narrative_ontology:measurement_basis(cons_tr_t90, observed).
narrative_ontology:measurement(cons_tr_t120, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 120, 0.26).
narrative_ontology:measurement_basis(cons_tr_t120, observed).
narrative_ontology:measurement(cons_tr_t150, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 150, 0.33).
narrative_ontology:measurement_basis(cons_tr_t150, observed).
narrative_ontology:measurement(cons_tr_t180, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 180, 0.31).
narrative_ontology:measurement_basis(cons_tr_t180, observed).
narrative_ontology:measurement(cons_tr_t210, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 210, 0.32).
narrative_ontology:measurement_basis(cons_tr_t210, observed).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 0, 0.14).
narrative_ontology:measurement_basis(cons_be_t0, observed).
narrative_ontology:measurement(cons_be_t30, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 30, 0.17).
narrative_ontology:measurement_basis(cons_be_t30, observed).
narrative_ontology:measurement(cons_be_t60, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 60, 0.21).
narrative_ontology:measurement_basis(cons_be_t60, observed).
narrative_ontology:measurement(cons_be_t90, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 90, 0.24).
narrative_ontology:measurement_basis(cons_be_t90, observed).
narrative_ontology:measurement(cons_be_t120, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 120, 0.34).
narrative_ontology:measurement_basis(cons_be_t120, observed).
narrative_ontology:measurement(cons_be_t150, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 150, 0.45).
narrative_ontology:measurement_basis(cons_be_t150, observed).
narrative_ontology:measurement(cons_be_t180, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 180, 0.43).
narrative_ontology:measurement_basis(cons_be_t180, observed).
narrative_ontology:measurement(cons_be_t210, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 210, 0.44).
narrative_ontology:measurement_basis(cons_be_t210, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(constitutional_interpretive_authority__judicial_supremacy_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_interpretive_authority__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__judicial_supremacy_reading, parliamentary_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__judicial_supremacy_reading, coordinate_construction_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'who interprets the constitution' decomposes into three structurally distinct constraints — one per reading of the final-authority kernel. Each member carries its own epsilon, beneficiary/victim structure, and classification; this file is the judicial-supremacy member. The linkage pattern differs from empirical families: here each reading cites the same founding materials (the ratification debates, the canonical nullification opinion, the judicial-power clause) as evidence for itself, so the family links record shared-kernel kinship and mutual foreclosure rather than evidential dependency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_interpretive_authority__judicial_supremacy_reading, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
