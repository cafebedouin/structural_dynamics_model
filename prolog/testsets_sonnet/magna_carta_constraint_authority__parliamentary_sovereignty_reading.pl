% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__parliamentary_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_constraint_authority__parliamentary_sovereignty_reading, []).

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
 *   constraint_id: magna_carta_constraint_authority__parliamentary_sovereignty_reading
 *   human_readable: Magna Carta Restraint Absorbed Into Parliamentary Sovereignty
 *   domain: constitutional_history/legal_philosophy/political_theory
 *
 * SUMMARY:
 *   This story instantiates the parliamentary_sovereignty_reading of the
 *   magna_carta_constraint_authority kernel: the claim that Magna Carta's
 *   restraints on arbitrary power survive today only because Parliament has,
 *   at successive junctures (Petition of Right 1628, Bill of Rights 1689,
 *   Habeas Corpus Acts, Human Rights Act 1998), absorbed and restated them as
 *   ordinary statute — and that Parliament, having inherited this constraint
 *   authority, retains full power to revise or repeal any of it. This is a
 *   distinct constraint from the living_constitutionalism_reading (which
 *   holds the restraint binds through judicial precedent independent of
 *   legislative will) and the feudal_obsolescence_reading (which holds no
 *   binding restraint survives at all). Each reading has its own
 *   beneficiary/victim structure and its own epsilon; they are not measured
 *   on a shared scale here. Under this reading, extraction is moderate: the
 *   coordination function (routing restraint through a legitimated, updatable
 *   legislature) is genuine, but the same mechanism structurally exposes
 *   minorities and future generations to the possibility that today's
 *   protections are tomorrow's repealed clause.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.48).
domain_priors:suppression_score(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.42).
domain_priors:theater_ratio(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_constraint_authority__parliamentary_sovereignty_reading, "Magna Carta Restraint Absorbed Into Parliamentary Sovereignty").
narrative_ontology:topic_domain(magna_carta_constraint_authority__parliamentary_sovereignty_reading, "constitutional_history/legal_philosophy/political_theory").

domain_priors:requires_active_enforcement(magna_carta_constraint_authority__parliamentary_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 'c0df3020-76ec-4780-ab45-b7712e7fa4fe').
narrative_ontology:cs_kernel_codification('c0df3020-76ec-4780-ab45-b7712e7fa4fe', fixed_text).
narrative_ontology:cs_authority_grounding('c0df3020-76ec-4780-ab45-b7712e7fa4fe', extraction).
narrative_ontology:cs_interpretation_layer_present('c0df3020-76ec-4780-ab45-b7712e7fa4fe').
narrative_ontology:cs_reading_relation('c0df3020-76ec-4780-ab45-b7712e7fa4fe', magna_carta_constraint_authority__living_constitutionalism_reading, forecloses).
narrative_ontology:cs_reading_relation('c0df3020-76ec-4780-ab45-b7712e7fa4fe', magna_carta_constraint_authority__feudal_obsolescence_reading, coexists_with).
narrative_ontology:cs_axiom('c0df3020-76ec-4780-ab45-b7712e7fa4fe', foundational, parliament_holds_unlimited_revisionary_authority).
narrative_ontology:cs_axiom_status(parliament_holds_unlimited_revisionary_authority, holdable).
narrative_ontology:cs_axiom_grounding('c0df3020-76ec-4780-ab45-b7712e7fa4fe', parliament_holds_unlimited_revisionary_authority, conventional).
narrative_ontology:cs_axiom('c0df3020-76ec-4780-ab45-b7712e7fa4fe', secondary, no_judicial_strike_down_of_primary_legislation).
narrative_ontology:cs_axiom_status(no_judicial_strike_down_of_primary_legislation, holdable).
narrative_ontology:cs_axiom_grounding('c0df3020-76ec-4780-ab45-b7712e7fa4fe', no_judicial_strike_down_of_primary_legislation, conventional).
narrative_ontology:cs_reference_frame('c0df3020-76ec-4780-ab45-b7712e7fa4fe', diceyan_parliamentary_supremacy).
narrative_ontology:cs_drift_state('c0df3020-76ec-4780-ab45-b7712e7fa4fe', post_human_rights_act_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c0df3020-76ec-4780-ab45-b7712e7fa4fe', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__parliamentary_sovereignty_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliamentary_majority).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__parliamentary_sovereignty_reading, executive_government_of_the_day).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__parliamentary_sovereignty_reading, enfranchised_electorate).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, unrepresented_minorities).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, judicially_disfavored_litigants).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, future_generations_bound_by_repealable_rights).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, executive_government_of_the_day).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, judiciary).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliamentary_supremacy_doctrine).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__parliamentary_sovereignty_reading, rule_of_law_through_statute).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the legal power to amend, codify, or repeal any provision historically traced to Magna Carta (habeas corpus protections, due process guarantees, restraints on arbitrary detention) through ordinary statute. Controls which charter-derived restraints survive and in what form, subject only to political cost, not legal barrier.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliamentary_majority, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliamentary_majority, beneficiary).

% Operates within restraints inherited from the charter tradition but can seek their narrowing through legislation it controls via its parliamentary majority. Benefits from the flexibility to redefine restraint boundaries (emergency powers, detention without trial, surveillance statutes) whenever it commands the votes.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, executive_government_of_the_day, beneficiary,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_constraint_authority__parliamentary_sovereignty_reading, executive_government_of_the_day, payer).

% Receives the benefit of due-process norms descending from the charter as codified in statute, and can in principle alter the content of those protections through electoral pressure on Parliament. Its influence is real but mediated and slow, exercised only at the ballot box and through organized political pressure.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, enfranchised_electorate, beneficiary,
    organized, generational, constrained, national).

% Groups whose interests do not command majoritarian legislative attention (non-citizens, detainees, unpopular defendants, minority religious or ethnic communities) find that the charter's restraint tradition offers them protection only insofar as a current parliamentary majority chooses to preserve it in statute. Exit from the jurisdiction is often impossible; recourse to an entrenched, unrepealable right does not exist.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, unrepresented_minorities, payer,
    powerless, biographical, trapped, national).

% Individuals invoking due-process or liberty protections in court find those protections are only as strong as the current statutory text; a hostile Parliament can narrow or abolish the relevant statute prospectively or, in some systems, retroactively, leaving the litigant without the ultimate backstop a genuinely entrenched constitutional right would provide.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, judicially_disfavored_litigants, payer,
    powerless, immediate, trapped, national).

% Have no voice in the present legislative bargains that determine what charter-derived restraint survives for them; whatever protection exists today is contingent on ongoing parliamentary forbearance, not on an irrevocable settlement, and they cannot object to that arrangement now.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, future_generations_bound_by_repealable_rights, excluded,
    powerless, civilizational, trapped, national).

% Interprets and applies charter-descended protections as embodied in statute and common law, but under this reading holds no power to strike down an Act of Parliament for violating those protections. Can issue declarations of incompatibility or interpretive rulings but must ultimately defer to Parliament's revisionary authority.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, judiciary, observer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_constraint_authority__parliamentary_sovereignty_reading, judiciary, payer).

% Study how successive statutes (Petition of Right, Bill of Rights 1689, Habeas Corpus Acts, Human Rights Act) have absorbed, restated, and sometimes narrowed the charter's original restraints, documenting the historical process by which baronial privilege became parliamentary doctrine.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a durable, legitimated mechanism for constraining arbitrary executive action (unlawful detention, seizure of property, denial of due process) by routing the restraint through a representative legislature that can update its content as circumstances change, rather than freezing 13th-century baronial terms as unrevisable law.
% TRANSFER_FUNCTION: Moves the power to define the content and scope of the restraint from the Crown (originally) and from any entrenched judicial or customary authority (under sibling readings) to the parliamentary majority of the day; correspondingly, it moves the security of that restraint away from those who lack majoritarian political power and toward those the current majority chooses to protect.
% ABSENT_VOICES: Unrepresented minorities, non-citizens, and future generations have no seat in the legislative process that determines whether a given charter-descended protection is retained, narrowed, or repealed; they would argue for entrenchment beyond ordinary statute but are not structurally present in the parliamentary bargaining that sets the restraint's actual content.
% DISAPPEARANCE_RATIONALE: If parliamentary sovereignty over charter-descended restraints were replaced overnight by judicial entrenchment (the living_constitutionalism_reading) or by the position that no binding restraint survives at all (the feudal_obsolescence_reading), the entire mechanism by which due-process protections are created, narrowed, and enforced in this jurisdiction would change: courts would gain or lose the power to override statute, and the electorate's leverage over rights content would be replaced by judicial or purely historical determination.
% FOUNDING_PROBLEM: The founding problem this reading solves is reconciling a medieval charter of baronial privilege with a modern doctrine of legislative supremacy: how can restraints on arbitrary rule persist and evolve without vesting a permanent veto in unelected judges or in a fossilized 13th-century text.
% FOUNDING_PROBLEM_CORROBORATION: Parliamentary constitutional scholars and successive governments attest the problem remains live and is well-solved by statutory supremacy (citing the Bill of Rights 1689, Habeas Corpus Acts, and the Human Rights Act as evidence of ongoing adaptive absorption). Civil liberties organizations and comparative constitutional scholars operating outside government attest that the same arrangement leaves protections structurally contingent and has permitted retrenchment (control orders, indefinite detention provisions, surveillance statutes) precisely because no provision is beyond ordinary repeal; this corroboration comes from outside the parliamentary majority that benefits from retaining revisionary power.
narrative_ontology:disappearance_verdict(magna_carta_constraint_authority__parliamentary_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_constraint_authority__parliamentary_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__parliamentary_sovereignty_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.48, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_constraint_authority__parliamentary_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_constraint_authority__parliamentary_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_constraint_authority__parliamentary_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.48 — moderate, not low — because the tangled_rope structure genuinely coordinates (a legislature that can update restraint content prevents ossification) while genuinely extracting (whoever lacks majoritarian power bears the risk that their protection is withdrawn without their consent). Suppression sits at 0.42: enforcement of the parliamentary-supremacy doctrine against competing claims (judicial review claims, entrenchment claims) requires active judicial and political defense, visible in doctrines like the enrolled bill rule and judicial deference under Diceyan orthodoxy. Theater ratio rises from 0.15 to 0.40 over the interval as the doctrine's application increasingly involves declaratory and symbolic gestures (declarations of incompatibility that carry no binding force) alongside its substantive function. Accessibility collapse is moderate (0.5): once parliamentary supremacy is understood, alternative framings (entrenched judicial review, written constitutional supremacy) remain conceptually available and are actively argued by comparative constitutionalists, so collapse is partial, not total. Resistance is moderate (0.45): civil liberties advocates, some judges, and comparative scholars actively contest the sufficiency of statutory-only protection.
 *
 * PERSPECTIVAL GAP:
 *   From the parliamentary majority's seat, this is coordination: a flexible, democratically legitimate mechanism superior to rule by unelected judges or a frozen medieval text. From the seat of an unrepresented minority facing a hostile statute, the same structure is extraction: a promise of restraint that evaporates exactly when it is most needed, because the body empowered to preserve it is also the body with an interest in narrowing it. The engine's per-seat computation should reflect this divergence without either seat's view being privileged as the story's official verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Parliamentary majorities and the executive of the day sit near the beneficiary end: they control the content of the restraint and can adjust it to political advantage. The enfranchised electorate sits closer to symmetric — it benefits from statutory rights but exercises only diffuse, periodic influence over their content. Unrepresented minorities, judicially disfavored litigants, and future generations sit at the target end: they bear the risk of the restraint's revisability without commensurate voice in revising it, and their exit options are trapped (they cannot leave the jurisdiction or appeal to an entrenched right beyond the statute).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (constraining arbitrary rule without either judicial supremacy or textual ossification) remains genuinely live in the sense that arbitrary executive action remains a live risk (emergency powers, detention statutes, surveillance legislation), so this is not a pure mandatrophy case of a dead problem sustaining a live institution. But the corroboration is contested: the parliamentary majority's own account of continued adequacy is not corroborated by independent civil-liberties assessment, which documents concrete retrenchment episodes. This divergence is precisely why founding_problem_status is authored as contested rather than live or dead outright.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_magna_carta,
    'Is the parliamentary_sovereignty_reading the structurally correct account of where Magna Carta''s constraint authority now resides, or do the living_constitutionalism_reading (binding through judicial precedent) or feudal_obsolescence_reading (no binding authority survives) better describe the actual operative constraint in a given jurisdiction and era?',
    'Comparative constitutional analysis across jurisdictions descending from the common-law tradition: jurisdictions with written, judicially enforceable constitutions (entrenched bills of rights, constitutional courts with strike-down power) instantiate the living_constitutionalism_reading; jurisdictions retaining pure parliamentary supremacy (Diceyan orthodoxy, no judicial strike-down power) instantiate this reading; purely historical/academic treatments with no operative legal claim instantiate the feudal_obsolescence_reading. The correct reading is jurisdiction- and era-specific, not universal.',
    'If the living_constitutionalism_reading is structurally operative in a given system (e.g., post-1998 Human Rights Act interpretive obligations creeping toward de facto entrenchment, or written constitutions modeled on Magna Carta with judicial review), this story''s tangled_rope classification and victim set would not apply there — the restraint would instead be classified with courts as agenda_setters rather than Parliament, and the victim set would shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_magna_carta, conceptual, 'Which kernel reading structurally governs a given common-law jurisdiction at a given time is an empirical-cum-conceptual question, not resolved by this story alone.').

omega_variable(
    statutory_entrenchment_practical_effect,
    'Does the theoretical unlimited revisability of parliamentary statute (Parliament can repeal any charter-descended protection by simple majority) translate into practical vulnerability, or do political and constitutional conventions provide de facto protection approaching entrenchment?',
    'Empirical survey of actual repeal or narrowing episodes affecting charter-descended protections (habeas corpus suspensions, detention-without-trial statutes, surveillance legislation) versus episodes where political cost prevented such narrowing despite legal power to do so.',
    'If de facto convention consistently prevents narrowing regardless of legal power, effective extraction is lower than the theoretical vulnerability suggests, moving this constraint closer to a rope. If narrowing episodes are frequent and target politically weak groups, the tangled_rope/moderate-extraction reading is confirmed or the classification should move toward snare for the affected victim groups specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(statutory_entrenchment_practical_effect, empirical, 'Whether theoretical parliamentary revisability functions as real risk or is checked by unwritten convention.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 1215, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1215, 0.15).
narrative_ontology:measurement(magn_tr_t1689, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1689, 0.2).
narrative_ontology:measurement(magn_tr_t1832, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1832, 0.25).
narrative_ontology:measurement(magn_tr_t1928, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1928, 0.3).
narrative_ontology:measurement(magn_tr_t1998, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1998, 0.35).
narrative_ontology:measurement(magn_tr_t2025, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1215, 0.2).
narrative_ontology:measurement(magn_be_t1689, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1689, 0.25).
narrative_ontology:measurement(magn_be_t1832, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1832, 0.3).
narrative_ontology:measurement(magn_be_t1928, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1928, 0.35).
narrative_ontology:measurement(magn_be_t1998, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1998, 0.4).
narrative_ontology:measurement(magn_be_t2025, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 2025, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1215, 0.35).
narrative_ontology:measurement(magn_su_t1689, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1689, 0.32).
narrative_ontology:measurement(magn_su_t1832, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1832, 0.3).
narrative_ontology:measurement(magn_su_t1928, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1928, 0.34).
narrative_ontology:measurement(magn_su_t1998, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1998, 0.38).
narrative_ontology:measurement(magn_su_t2025, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 2025, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__parliamentary_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__parliamentary_sovereignty_reading, magna_carta_constraint_authority__living_constitutionalism_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__parliamentary_sovereignty_reading, magna_carta_constraint_authority__feudal_obsolescence_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language claim 'Magna Carta's authority survives today' per the epsilon-invariance principle. Each reading has a distinct beneficiary/victim structure, a distinct claimed_type, and a distinct epsilon: parliamentary_sovereignty_reading (this story, tangled_rope, moderate extraction concentrated on minorities and future generations), living_constitutionalism_reading (expected rope or mountain-adjacent, lower extraction, protection framed as judicially guaranteed), feudal_obsolescence_reading (expected mountain or piton, near-zero extraction, framed as historically inert). The three are linked via affects_constraints rather than merged, because measuring 'the same' claim under different observables (statutory text vs. judicial precedent vs. historical inertness) yields different epsilon values — the signature of three distinct constraints, not one constraint under three lenses.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
