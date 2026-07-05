% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__guided_nationalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_july_charter_sovereign_legitimacy__guided_nationalism_reading, []).

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
 *   constraint_id: july_charter_sovereign_legitimacy__guided_nationalism_reading
 *   human_readable: July Charter — Islamic-Nationalist Sovereign Legitimacy Reading
 *   domain: constitutional_law/political_transitions/post_revolutionary_state_building
 *
 * SUMMARY:
 *   In the aftermath of a revolutionary collapse of the prior secular
 *   authoritarian order, a constitutional convention produced a founding
 *   charter. This story instantiates ONE reading of that charter's contested
 *   kernel — the reading under which the charter establishes an
 *   Islamic-nationalist framework, grounding sovereign legitimacy in
 *   religious identity rather than in secular popular sovereignty or military
 *   guardianship. Under this reading, religious law and norms acquire
 *   constitutional status, secular institutions (judiciary, civil society,
 *   family law) are constrained by a new conformity-review layer, and the
 *   victim set concentrates on secular civil society, religious minorities,
 *   and women's rights advocates. The sibling readings —
 *   secular_democratic_reading (secular institutions with military
 *   subordination to civilian authority) and military_custodian_reading
 *   (military as permanent institutional guardian) — are NOT this constraint;
 *   they are separate stories with their own ε, stakeholders, and
 *   classification, linked here only via network edges and this reading's
 *   kernel_context note.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.71).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.78).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__guided_nationalism_reading, tangled_rope).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__guided_nationalism_reading, "July Charter — Islamic-Nationalist Sovereign Legitimacy Reading").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__guided_nationalism_reading, "constitutional_law/political_transitions/post_revolutionary_state_building").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__guided_nationalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__guided_nationalism_reading, '35ef08c3-97a3-4bd5-804d-3e9ecce7cd19').
narrative_ontology:cs_kernel_codification('35ef08c3-97a3-4bd5-804d-3e9ecce7cd19', formalized).
narrative_ontology:cs_authority_grounding('35ef08c3-97a3-4bd5-804d-3e9ecce7cd19', lineage).
narrative_ontology:cs_interpretation_layer_present('35ef08c3-97a3-4bd5-804d-3e9ecce7cd19').
narrative_ontology:cs_reading_relation('35ef08c3-97a3-4bd5-804d-3e9ecce7cd19', july_charter_sovereign_legitimacy__secular_democratic_reading, forecloses).
narrative_ontology:cs_reading_relation('35ef08c3-97a3-4bd5-804d-3e9ecce7cd19', july_charter_sovereign_legitimacy__military_custodian_reading, coexists_with).
narrative_ontology:cs_axiom('35ef08c3-97a3-4bd5-804d-3e9ecce7cd19', foundational, religious_identity_as_sole_sovereign_ground).
narrative_ontology:cs_axiom_status(religious_identity_as_sole_sovereign_ground, holdable).
narrative_ontology:cs_axiom_grounding('35ef08c3-97a3-4bd5-804d-3e9ecce7cd19', religious_identity_as_sole_sovereign_ground, theological).
narrative_ontology:cs_axiom('35ef08c3-97a3-4bd5-804d-3e9ecce7cd19', secondary, clerical_conformity_review_supersedes_secular_judicial_finality).
narrative_ontology:cs_axiom_status(clerical_conformity_review_supersedes_secular_judicial_finality, holdable).
narrative_ontology:cs_axiom_grounding('35ef08c3-97a3-4bd5-804d-3e9ecce7cd19', clerical_conformity_review_supersedes_secular_judicial_finality, conventional).
narrative_ontology:cs_reference_frame('35ef08c3-97a3-4bd5-804d-3e9ecce7cd19', revolutionary_founding_moment_religious_sovereignty).
narrative_ontology:cs_drift_state('35ef08c3-97a3-4bd5-804d-3e9ecce7cd19', post_ratification_institutional_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('35ef08c3-97a3-4bd5-804d-3e9ecce7cd19', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__guided_nationalism_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_nationalist_political_bloc).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, clerical_authority_institutions).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, charter_drafting_committee).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, secular_civil_society_organizations).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_minority_communities).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, women_rights_advocacy_groups).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, independent_judiciary).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Secured constitutional entrenchment of religious identity as the ground of sovereign legitimacy during the charter drafting process following the prior regime's collapse. Controls seats on the drafting committee and the parliamentary majority needed to ratify the text. Frames the arrangement as restoring the nation's authentic character after decades of secular authoritarian rule; in practice gains a durable veto over any future legislation framed as contrary to religious identity.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_nationalist_political_bloc, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_nationalist_political_bloc, agenda_setter).

% Gain constitutionally recognized interpretive authority over legislation's conformity with religious norms — a new institutional gatekeeping role that did not exist under the prior secular constitution. Collects legitimacy, funding, and appointive power from the new arrangement; has no exit need since the charter is the source of its enhanced status.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, clerical_authority_institutions, beneficiary,
    institutional, civilizational, arbitrage, national).

% Wrote and ratified the founding text in the aftermath of the revolutionary rupture, choosing religious-national identity over the secular-democratic and military-custodian alternatives that were live options during drafting. Enforces the settlement through the new constitutional court and through political pressure on any body that proposes revision.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, charter_drafting_committee, agenda_setter,
    institutional, generational, arbitrage, national).

% Lose constitutional protection for secular associational life, education, and advocacy that existed under the prior framework. Operating space for organizing, publishing, and litigating against religiously-grounded restrictions narrows sharply; leaving the country is the only full exit, and most lack the resources to relocate their institutional base.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, secular_civil_society_organizations, payer,
    moderate, biographical, constrained, national).

% Find their civic standing redefined relative to the newly sovereign religious-national identity; personal status law, public office eligibility, and protections against discrimination now sit downstream of a framework that treats one religious identity as the source of state legitimacy. Emigration is the primary exit and is costly, risky, and unavailable to many.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_minority_communities, payer,
    powerless, biographical, trapped, national).

% Face constitutional subordination of gender-equality guarantees to religious-normative interpretation on family law, inheritance, and personal status matters. Continue operating but under a legal ceiling that did not exist in the same form under the prior charter; challenging the ceiling now requires contesting the sovereignty clause itself, not just statute.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, women_rights_advocacy_groups, payer,
    moderate, biographical, constrained, national).

% Judicial review authority is now subordinated to conformity review against religious-national identity, administered by a new or reconstituted body with clerical input. Judges who resist this hierarchy risk removal or bypass; the institution retains formal existence but with narrowed independent interpretive scope.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, independent_judiciary, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__guided_nationalism_reading, independent_judiciary, excluded).

% Document the charter's departure from the secular-democratic and military-custodian alternatives that were also on the table during the drafting period, and track downstream effects on minority protections and civil liberties for comparative constitutional analysis and treaty-compliance reporting.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, international_constitutional_observers, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unifying national identity narrative after a revolutionary rupture, offering a legitimacy ground perceived by its architects as more culturally authentic and mobilizationally stable than either secular constitutionalism or open military rule — solving the founding coalition's need for a durable, popularly resonant sovereignty claim.
% TRANSFER_FUNCTION: Moves interpretive and legislative veto power from secular civil institutions, religious minorities, and gender-equality advocates to the religious-nationalist political bloc and clerical authority bodies; converts contested cultural authority into entrenched constitutional standing that persists independent of ordinary electoral outcomes.
% ABSENT_VOICES: Secular constitutional drafters and minority religious community representatives who participated in the broader post-revolutionary constitutional convention but whose alternative frameworks were not selected; religious minorities and secular women's rights advocates were consulted in hearings but were structurally outvoted in the final drafting committee.
% DISAPPEARANCE_RATIONALE: If the sovereign-legitimacy clause were struck, the constitutional basis for clerical conformity review would dissolve, personal-status law would revert to contestable ordinary legislation rather than constitutionally shielded doctrine, and secular civil society and minority communities would regain standing to challenge restrictions as ordinary statute rather than as constitutional order — a substantial institutional rearrangement, not a cosmetic one.
% FOUNDING_PROBLEM: The prior regime's collapse left a legitimacy vacuum: no agreed sovereign ground for the new state, competing claims from secular reformists, the military, and religious-nationalist coalitions, and an urgent need to stabilize governance before renewed conflict or foreign intervention.
% FOUNDING_PROBLEM_CORROBORATION: The religious-nationalist bloc and clerical institutions attest the legitimacy vacuum required exactly this religious-national grounding and that it remains necessary against ongoing secular and international pressure. Independent comparative constitutional scholars and international human-rights monitoring bodies — outside the beneficiary set — attest the legitimacy vacuum could have been resolved by either sibling framework (secular-democratic or military-custodian) and that the religious-national choice reflects the drafting committee's composition rather than a functional necessity unique to that framework.
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__guided_nationalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__guided_nationalism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__guided_nationalism_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__guided_nationalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(july_charter_sovereign_legitimacy__guided_nationalism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__guided_nationalism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(july_charter_sovereign_legitimacy__guided_nationalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.48 to 0.71 across the interval as the clerical conformity-review apparatus matures from a nominal constitutional clause into an operating institutional veto over legislation — the same pattern of enforcement infrastructure hardening reflected in the suppression_requirement series (0.55 to 0.78). Theater ratio is moderate and rising (0.12 to 0.32): the founding-narrative function (national unity, authentic identity) is genuinely operative early on, but an increasing share of enforcement activity over time defends the sovereignty clause's institutional entrenchment itself rather than performing any unification function that could not have been achieved by the sibling readings. Accessibility collapse (0.62) reflects that alternatives (the secular-democratic framework was a live option during drafting) have been substantially foreclosed for ordinary political actors, though not with the totality of a genuine natural-law mountain. Resistance (0.58) reflects active, organized contestation from secular civil society, minority communities, and international observers — this is not an uncontested settlement.
 *
 * PERSPECTIVAL GAP:
 *   From the religious-nationalist bloc's seat, the charter looks like coordination: a genuinely needed sovereignty settlement after chaos, chosen through a legitimate (if contested) drafting process. From secular civil society's and religious minorities' seats, the identical clause structure operates as enforced subordination — their pre-charter civic standing was constitutionally stronger, and the mechanism securing the bloc's position is the same mechanism narrowing theirs. The engine computes this divergence from the structural beneficiary/victim/enforcement data; the claimed_type does not resolve it in advance.
 *
 * DIRECTIONALITY LOGIC:
 *   The religious-nationalist bloc, clerical institutions, and the drafting committee sit near the full-beneficiary end: they authored the sovereignty ground, hold arbitrage-grade institutional exit (their standing is generative, not dependent on the charter surviving unchanged), and collect the interpretive veto directly. Secular civil society, religious minorities, women's rights advocates, and the independent judiciary sit toward the full-target end: they bear the transfer (narrowed civic standing, subordinated legal protections, curtailed independent review) and their exit options range from constrained (organizations that can adapt but at real cost) to trapped (minority communities without emigration capacity). The independent judiciary's dual role (payer + excluded) reflects that it retains formal institutional existence while losing substantive interpretive independence — a captured rather than destroyed institution.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a post-revolutionary legitimacy vacuum requiring SOME sovereign ground to stabilize governance — was genuinely live at founding. Under this reading, the question is whether the religious-national answer to that vacuum remains necessary once the immediate stabilization crisis passed, or whether it has calcified into a permanent extraction structure (clerical veto power, minority subordination) that outlived the acute crisis it was built to resolve. The founding_problem_status is authored as contested precisely because the beneficiary bloc and outside comparative scholarship give incompatible answers — the classification should not resolve this dispute by fiat; the tangled_rope classification (genuine founding coordination function + ongoing asymmetric extraction, both present) is the structurally honest reading rather than forcing either a pure-rope or pure-snare verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'Does the ratified charter text and drafting record support the guided_nationalism_reading as the dominant or exclusive reading of sovereign legitimacy, or do the secular_democratic_reading and military_custodian_reading remain simultaneously defensible readings of the same text?',
    'Close textual analysis of the charter''s preamble and sovereignty articles cross-referenced against the drafting committee''s session records and the floor debate transcripts; comparison with how the constitutional court has actually applied the sovereignty clause in its first rulings.',
    'If the text robustly supports only the guided_nationalism_reading, this constraint''s classification is stable as authored. If the text is genuinely polysemous and the secular_democratic_reading remains simultaneously live in practice (e.g. courts alternating between frameworks), the kernel is less settled than a single dominant reading implies, and downstream institutional predictions should be qualified accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the charter text uniquely supports this reading or remains multiply readable.').

omega_variable(
    founding_necessity_vs_capture,
    'Was religious-national grounding a functionally necessary response to the post-revolutionary legitimacy vacuum, or was it a capture of the drafting process by the best-organized coalition present at the constitutional convention?',
    'Comparative analysis against other post-revolutionary constitutional transitions that resolved similar legitimacy vacuums via secular-democratic or military-custodian frameworks without descending into renewed conflict — testing whether the religious-national path was the only stabilizing option or one option among several that happened to be chosen by the strongest coalition.',
    'If functionally necessary, the tangled_rope classification''s coordination component is stronger than the extraction component suggests. If a capture story, the coordination framing is closer to cover story and the constraint sits nearer snare over time as enforcement hardens without a corresponding necessity justification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_necessity_vs_capture, empirical, 'Whether the religious-national settlement was functionally required or a coalition capture of the drafting process.').

omega_variable(
    minority_exit_feasibility,
    'Is the trapped exit_options classification for religious_minority_communities accurate across the whole population, or does it obscure meaningful heterogeneity (some sub-communities with diaspora networks and resources enabling emigration, others without)?',
    'Disaggregated demographic and migration-pattern data on which minority sub-populations have actually exited versus remained since charter ratification.',
    'If exit is more heterogeneous than modeled, the aggregate victim classification may overstate suppression for some sub-groups and understate it for others with no diaspora option at all.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_exit_feasibility, empirical, 'Heterogeneity within the religious minority victim group regarding actual exit feasibility.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(july_tr_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(july_tr_t4, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 4, 0.16).
narrative_ontology:measurement(july_tr_t8, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(july_tr_t12, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(july_tr_t16, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 16, 0.28).
narrative_ontology:measurement(july_tr_t20, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(july_tr_t24, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 24, 0.32).

% Extraction over time
narrative_ontology:measurement(july_be_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(july_be_t4, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 4, 0.55).
narrative_ontology:measurement(july_be_t8, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(july_be_t12, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 12, 0.64).
narrative_ontology:measurement(july_be_t16, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 16, 0.67).
narrative_ontology:measurement(july_be_t20, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 20, 0.69).
narrative_ontology:measurement(july_be_t24, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 24, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(july_su_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(july_su_t4, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 4, 0.62).
narrative_ontology:measurement(july_su_t8, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 8, 0.68).
narrative_ontology:measurement(july_su_t12, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 12, 0.72).
narrative_ontology:measurement(july_su_t16, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 16, 0.75).
narrative_ontology:measurement(july_su_t20, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 20, 0.77).
narrative_ontology:measurement(july_su_t24, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 24, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__guided_nationalism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__guided_nationalism_reading, july_charter_sovereign_legitimacy__secular_democratic_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__guided_nationalism_reading, july_charter_sovereign_legitimacy__military_custodian_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the single natural-language label 'July Charter sovereign legitimacy clause,' per the ε-invariance principle. The three readings (guided_nationalism_reading, secular_democratic_reading, military_custodian_reading) do not share an ε value — they are structurally distinct claims about what the same founding text establishes, with different beneficiary/victim sets and different classifications. This reading (Islamic-nationalist grounding) is linked to both siblings via affects_constraints because the drafting-committee outcome that entrenched this reading structurally influenced the resourcing and legitimacy conditions available to the sibling readings' proponents in subsequent constitutional litigation and amendment attempts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
