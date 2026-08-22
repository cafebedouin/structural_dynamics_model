% ============================================================================
% CONSTRAINT STORY: constitutional_text_authority__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text_authority__positivist_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: constitutional_text_authority__positivist_reading
 *   human_readable: Constitutional Validity via Formal Pedigree (Positivist Reading)
 *   domain: legal/constitutional/jurisprudential
 *
 * SUMMARY:
 *   This story instantiates the positivist reading of the
 *   constitutional_text_authority kernel: constitutional validity is a
 *   function of formal enactment procedure and institutional pedigree, not of
 *   moral content. A rule that traces cleanly to a valid enactment procedure
 *   is law regardless of whether it is just; a morally compelling rule with
 *   no such pedigree is not law, whatever its merits. The reading solves a
 *   genuine coordination problem — determinate identification of valid law
 *   without adjudicating contested moral frameworks case by case — but the
 *   same procedural gate that supplies determinacy also insulates pedigrees
 *   produced under exclusionary conditions from the moral scrutiny that would
 *   otherwise attach to them. This is why the reading is authored as
 *   tangled_rope rather than a clean rope: real coordination function, real
 *   asymmetric cost borne by those excluded from the founding procedure or
 *   whose only recourse is a moral argument the framework declares irrelevant
 *   to validity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text_authority__positivist_reading, 0.42).
domain_priors:suppression_score(constitutional_text_authority__positivist_reading, 0.48).
domain_priors:theater_ratio(constitutional_text_authority__positivist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text_authority__positivist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text_authority__positivist_reading, "Constitutional Validity via Formal Pedigree (Positivist Reading)").
narrative_ontology:topic_domain(constitutional_text_authority__positivist_reading, "legal/constitutional/jurisprudential").

domain_priors:requires_active_enforcement(constitutional_text_authority__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text_authority__positivist_reading, '47bbf69a-afba-416d-baf6-e0026389ba29').
narrative_ontology:cs_kernel_codification('47bbf69a-afba-416d-baf6-e0026389ba29', formalized).
narrative_ontology:cs_authority_grounding('47bbf69a-afba-416d-baf6-e0026389ba29', practice).
narrative_ontology:cs_interpretation_layer_present('47bbf69a-afba-416d-baf6-e0026389ba29').
narrative_ontology:cs_reading_relation('47bbf69a-afba-416d-baf6-e0026389ba29', constitutional_text_authority__originalist_reading, influences).
narrative_ontology:cs_reading_relation('47bbf69a-afba-416d-baf6-e0026389ba29', constitutional_text_authority__living_constitutionalist_reading, coexists_with).
narrative_ontology:cs_axiom('47bbf69a-afba-416d-baf6-e0026389ba29', foundational, law_morality_separability_thesis).
narrative_ontology:cs_axiom_status(law_morality_separability_thesis, holdable).
narrative_ontology:cs_axiom_grounding('47bbf69a-afba-416d-baf6-e0026389ba29', law_morality_separability_thesis, conventional).
narrative_ontology:cs_axiom('47bbf69a-afba-416d-baf6-e0026389ba29', foundational, validity_is_pedigree_not_content).
narrative_ontology:cs_axiom_status(validity_is_pedigree_not_content, holdable).
narrative_ontology:cs_axiom_grounding('47bbf69a-afba-416d-baf6-e0026389ba29', validity_is_pedigree_not_content, conventional).
narrative_ontology:cs_axiom('47bbf69a-afba-416d-baf6-e0026389ba29', secondary, moral_defect_does_not_void_procedural_validity).
narrative_ontology:cs_axiom_status(moral_defect_does_not_void_procedural_validity, holdable).
narrative_ontology:cs_axiom_grounding('47bbf69a-afba-416d-baf6-e0026389ba29', moral_defect_does_not_void_procedural_validity, instrumental).
narrative_ontology:cs_reference_frame('47bbf69a-afba-416d-baf6-e0026389ba29', rule_of_recognition_practice).
narrative_ontology:cs_drift_state('47bbf69a-afba-416d-baf6-e0026389ba29', contemporary_transitional_justice_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('47bbf69a-afba-416d-baf6-e0026389ba29', '').
narrative_ontology:cs_kernel_id(constitutional_text_authority__positivist_reading, constitutional_text_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, sitting_judiciary).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, enacting_legislative_majorities).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, constitutional_law_faculty).
narrative_ontology:constraint_victim(constitutional_text_authority__positivist_reading, claimants_with_unenacted_moral_claims).
narrative_ontology:constraint_victim(constitutional_text_authority__positivist_reading, historically_disenfranchised_groups_excluded_from_ratification).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, constitutional_amendment_drafters).
narrative_ontology:constraint_vindicates(constitutional_text_authority__positivist_reading, law_morality_separability_thesis).
narrative_ontology:constraint_vindicates(constitutional_text_authority__positivist_reading, rule_of_recognition_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Applies the rule of recognition to determine which enacted rules count as valid law, refusing to import moral content as an independent validity condition. Controls which formal-pedigree arguments succeed in court, and thereby controls its own institutional workload and legitimacy narrative — a court that need not adjudicate morality adjudicates less contestably.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, sitting_judiciary, agenda_setter,
    institutional, generational, arbitrage, national).

% Whatever a qualified majority enacts through the prescribed procedure becomes valid constitutional law regardless of its moral content, so long as the pedigree is clean. This lets legislative coalitions lock in preferred arrangements without needing to win a separate moral argument, and insulates their enactments from later moral relitigation.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, enacting_legislative_majorities, beneficiary,
    organized, biographical, mobile, national).

% Builds careers and doctrine on the analytic tractability of pedigree-based validity — a clean separability thesis is easier to teach, litigate, and publish around than open moral adjudication. Benefits from the discipline's self-conception as scientific rather than evaluative.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, constitutional_law_faculty, beneficiary,
    institutional, generational, arbitrage, national).

% Bring claims resting on moral argument — that a law is unjust, that a practice violates dignity — and are told the argument is simply not cognizable as a validity question under this framework, however forceful morally. Their only route is the enactment process itself, which requires organized political power they may lack.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, claimants_with_unenacted_moral_claims, payer,
    powerless, biographical, trapped, national).

% Were absent or excluded from the enacting procedures that produced the founding pedigree (denied suffrage, denied standing, denied a seat at ratifying conventions). Under this reading, that absence does not undermine the validity of what was enacted — the pedigree is clean by the framework's own lights even though the enacting body was not representative of them.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, historically_disenfranchised_groups_excluded_from_ratification, payer,
    powerless, civilizational, trapped, national).

% Would argue that a law's moral defensibility bears on its legal validity, not merely its wisdom — that gross injustice can void pedigree. This reading structurally excludes that argument from the validity question itself, relegating it to a separate conversation about whether to obey or reform, not whether the law IS law.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, moral_philosophers_and_natural_law_theorists, excluded,
    moderate, civilizational, analytical, national).

% Operate within the procedural rules for altering the constitutional text itself. They both study how validity is determined and benefit from a stable, predictable procedural bar that (once met) cannot be reopened on moral grounds.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, constitutional_amendment_drafters, observer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text_authority__positivist_reading, constitutional_amendment_drafters, beneficiary).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a determinate, procedure-based test for what counts as valid constitutional law, so courts, legislators, and citizens can identify the law without first resolving contested moral disputes case by case.
% TRANSFER_FUNCTION: Moves interpretive authority away from open moral argument and toward institutions that control or can access the formal enactment record — legislatures, ratifying bodies, and the courts that certify pedigree — at the expense of parties whose claims are moral rather than procedural.
% ABSENT_VOICES: Natural law theorists and claimants whose grievance is that an enacted rule is unjust rather than improperly enacted are structurally outside the validity conversation; historically excluded groups who had no voice in the founding enactment are treated as bound by a pedigree they never consented to.
% DISAPPEARANCE_RATIONALE: If the positivist separability thesis were abandoned overnight, courts would have to treat moral defensibility as a live validity question rather than a separate matter of obedience or reform — reopening settled enactments to moral challenge, destabilizing the predictability the framework currently supplies, and shifting real power from procedural gatekeepers to whoever can win the moral argument.
% FOUNDING_PROBLEM: To give courts and citizens a determinate way to identify valid law that does not collapse into open-ended moral dispute, especially amid religious and ideological pluralism where no single moral framework commands universal assent.
% FOUNDING_PROBLEM_CORROBORATION: Legal positivists and sitting courts attest the determinacy problem remains live in pluralist societies. Critical legal scholars, some natural-law theorists, and scholars of transitional/post-colonial constitutionalism — outside the beneficiary set of judges and enacted majorities — argue the separability thesis has become a shield that immunizes unjust pedigrees (e.g., constitutions ratified under exclusionary franchises) from the very moral scrutiny the founding problem was meant to make tractable, not eliminate.
narrative_ontology:disappearance_verdict(constitutional_text_authority__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text_authority__positivist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text_authority__positivist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_text_authority__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text_authority__positivist_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text_authority__positivist_reading_tests).
:- end_tests(constitutional_text_authority__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) — the reading does not manufacture rents so much as foreclose a category of argument, which imposes real costs on specific groups (excluded ratification-era populations, moral claimants) without functioning as an accumulating extraction mechanism the way a rent-seeking arrangement would. Suppression is moderate (0.48): the framework does not physically coerce anyone, but it does structurally bar moral arguments from being heard as validity arguments, which is a real form of argumentative foreclosure enforced by every court that adopts the rule of recognition. Accessibility collapse (0.6) reflects that once a court commits to positivism, the moral-validity door is close to fully shut within that framework; resistance (0.55) reflects the live, organized opposition from natural-law theorists, critical legal scholars, and litigants for whom this is not academic.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting judiciary's seat, this looks like principled restraint — courts declining to legislate morality from the bench. From the seat of a claimant whose only argument is that an enacted rule is unjust, the identical structure looks like an arbitrary refusal to hear the argument that matters. The engine computes this divergence from the declared power/exit structure; the positivist reading itself denies that the divergence has any bearing on validity, which is exactly the structural feature this story is describing.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary and enacting majorities sit near the beneficiary end: the judiciary gains a tractable, self-limiting adjudicative role, and enacting majorities gain the ability to lock in enactments against later moral relitigation. Claimants with moral-only grievances and groups excluded from the founding procedure sit near the target end: the same procedural gate that gives everyone determinacy is precisely what forecloses their argument. Constitutional law faculty are a softer beneficiary — their professional and disciplinary stake is real but indirect.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — determinacy amid moral pluralism — remains partly live (societies remain morally plural) but is contested as to whether the positivist solution still tracks it, or has become a vehicle for insulating historically exclusionary pedigrees from scrutiny they would otherwise attract. Tangled Rope avoids two mislabeling errors: calling this a pure Snare would ignore the genuine coordination value of determinate validity criteria in a pluralist polity; calling it a pure Rope would ignore that the same gate structurally silences a real, non-frivolous category of claimant. Declaring both a coordination function and named victims, with active enforcement by courts and legislatures, keeps both halves of the structure visible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    positivism_originalism_practical_indistinguishability,
    'When positivist validity criteria are satisfied by an originalist-era enactment, is there any practical case where the positivist and originalist readings would diverge in outcome, or does the distinction only surface at the level of justificatory theory (procedure-as-such vs. historical-meaning-as-such)?',
    'Identify constitutional disputes where an enactment is procedurally valid by positivist criteria but where originalist historical-meaning analysis would reach a different substantive result (e.g., cases where later amendments changed procedural pedigree without changing original historical meaning, or vice versa) — divergent case outcomes would demonstrate the theories are not merely two labels for one practice.',
    'If no such divergence case exists, the positivist and originalist readings are extensionally identical in this legal system and the kernel contest is purely about justificatory grounding, not practical validity outcomes — which would count against authoring them as fully separate constraints for THIS jurisdiction, though the framework still requires separate stories per the ε-invariance principle since their grounding claims differ.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(positivism_originalism_practical_indistinguishability, conceptual, 'Whether positivism and originalism are practically distinguishable or only theoretically distinguishable in outcome.').

omega_variable(
    exclusionary_pedigree_validity_bootstrapping,
    'Does a constitutional pedigree remain fully valid under positivist criteria when the enacting body itself was constituted through procedures that excluded large populations (e.g., restricted suffrage at ratification), or does the exclusion taint the pedigree''s own procedural validity?',
    'Examine whether the positivist rule of recognition, as actually practiced by courts, treats the enacting body''s own composition as a validity condition (i.e., does the framework ask ''was the enacting procedure itself properly constituted'' all the way down) or treats it as settled once any recognized enactment procedure produced the text.',
    'If the rule of recognition bottoms out without ever questioning the enacting body''s inclusiveness, the positivist reading is more purely procedural (and more exposed to the extraction reading this story authors); if it does interrogate enacting-body legitimacy at some level, positivism has an internal resource for addressing exclusionary pedigrees that this story''s ε may be underweighting.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exclusionary_pedigree_validity_bootstrapping, conceptual, 'Whether positivist validity criteria are recursively procedural or bottom out in an unexamined founding act.').

omega_variable(
    separability_thesis_as_cover_or_genuine_constraint,
    'Is the law/morality separability thesis, as actually invoked by courts, a genuine analytic constraint that sometimes produces outcomes the invoking court dislikes morally, or is it selectively invoked only when it produces outcomes the court already favors on other grounds?',
    'Empirical survey of cases where courts explicitly invoked separability to reach a result they characterized as morally troubling but legally compelled, versus cases where separability language appears alongside outcomes that also happen to be morally convenient to the deciding institution.',
    'A high rate of separability invoked against the court''s own moral preference would support reading positivism as a genuine, self-binding constraint (lower effective extractiveness); a low rate would support the story''s tangled_rope framing that separability functions partly as post-hoc cover for outcomes already preferred on other institutional grounds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(separability_thesis_as_cover_or_genuine_constraint, empirical, 'Whether the separability thesis operates as a genuine self-binding constraint or as selective justificatory cover.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text_authority__positivist_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text_authority__positivist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cons_tr_t10, constitutional_text_authority__positivist_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement(cons_tr_t20, constitutional_text_authority__positivist_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(cons_tr_t30, constitutional_text_authority__positivist_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(cons_tr_t40, constitutional_text_authority__positivist_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement(cons_tr_t50, constitutional_text_authority__positivist_reading, theater_ratio, 50, 0.26).
narrative_ontology:measurement(cons_tr_t60, constitutional_text_authority__positivist_reading, theater_ratio, 60, 0.28).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text_authority__positivist_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(cons_be_t10, constitutional_text_authority__positivist_reading, base_extractiveness, 10, 0.33).
narrative_ontology:measurement(cons_be_t20, constitutional_text_authority__positivist_reading, base_extractiveness, 20, 0.36).
narrative_ontology:measurement(cons_be_t30, constitutional_text_authority__positivist_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(cons_be_t40, constitutional_text_authority__positivist_reading, base_extractiveness, 40, 0.4).
narrative_ontology:measurement(cons_be_t50, constitutional_text_authority__positivist_reading, base_extractiveness, 50, 0.41).
narrative_ontology:measurement(cons_be_t60, constitutional_text_authority__positivist_reading, base_extractiveness, 60, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text_authority__positivist_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(cons_su_t10, constitutional_text_authority__positivist_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(cons_su_t20, constitutional_text_authority__positivist_reading, suppression_requirement, 20, 0.44).
narrative_ontology:measurement(cons_su_t30, constitutional_text_authority__positivist_reading, suppression_requirement, 30, 0.45).
narrative_ontology:measurement(cons_su_t40, constitutional_text_authority__positivist_reading, suppression_requirement, 40, 0.46).
narrative_ontology:measurement(cons_su_t50, constitutional_text_authority__positivist_reading, suppression_requirement, 50, 0.47).
narrative_ontology:measurement(cons_su_t60, constitutional_text_authority__positivist_reading, suppression_requirement, 60, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(constitutional_text_authority__positivist_reading, originalist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__positivist_reading, living_constitutionalist_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language 'constitutional authority' concept per the ε-invariance principle: positivist_reading (this file — validity from procedure/pedigree, ε=0.42, tangled_rope), originalist_reading (validity from fixed historical public meaning), and living_constitutionalist_reading (validity partly from evolving moral/social consensus). The three share a text and an institutional apparatus but diverge on WHAT makes that apparatus authoritative, producing different victim sets (positivism's victims are moral claimants and exclusionary-pedigree-bound groups; living constitutionalism's victims, by contrast, are typically those who lose when contemporary moral consensus overrides settled textual expectations). Positivism converges heavily with originalism on textual fidelity and procedural constraint on change, which is modeled as an 'influences' edge rather than 'coexists_with' or 'forecloses' — positivism's procedural framework creates the legitimacy conditions originalism's historical-meaning inquiry operates within, without originalism being logically foreclosed by it. Positivism and living constitutionalism, by contrast, hold genuinely incompatible premises about whether moral content bears on validity, but both remain live positions held by different judicial coalitions — hence coexists_with rather than forecloses.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
