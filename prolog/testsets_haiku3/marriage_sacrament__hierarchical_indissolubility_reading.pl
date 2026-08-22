% ============================================================================
% CONSTRAINT STORY: marriage_sacrament__hierarchical_indissolubility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_sacrament__hierarchical_indissolubility_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: marriage_sacrament__hierarchical_indissolubility_reading
 *   human_readable: Marriage Sacrament: Hierarchical Indissolubility Reading
 *   domain: religious/doctrinal/political
 *
 * SUMMARY:
 *   The Catholic doctrine of marriage indissolubility, under the
 *   hierarchical_indissolubility reading, treats marriage as an ontological
 *   reality—a sacramental bond constituted by God and administered by the
 *   church's institutional hierarchy—that cannot be unmade by human will,
 *   death, or circumstance. Divorced Catholics who wish to remarry face
 *   institutional exclusion from the sacraments (reconciliation, Eucharist)
 *   unless they obtain an annulment declaring the prior marriage never to
 *   have been sacramentally valid. This reading contrasts with a competing
 *   civic_pastoral reading (a separate constraint story) in which
 *   indissolubility is held as a profound aspiration but pastoral mercy and
 *   individual discernment are granted standing to override institutional
 *   exclusion in cases of genuine human failure. This story instantiates ONLY
 *   the hierarchical reading, with its specific beneficiary (the
 *   ecclesiastical hierarchy and the doctrine it vindicates) and victim set
 *   (remarried Catholics denied sacramental access). The claim/metric gap is
 *   by design: the constraint is CLAIMED as tangled_rope (coordination +
 *   enforcement asymmetry) while the metrics show high extractiveness and
 *   suppression—the engine measures whether the authored claim matches the
 *   structural data.
 *
 * KEY AGENTS:
 *   - ecclesiastical_hierarchy: institutional agenda-setter; administers annulment tribunals; collects authority from maintaining the indissolubility doctrine; enforces sacramental exclusion
 *   - divorced_catholics_seeking_remarriage: powerless payers; bear tribunal costs, delays, and identity-fusion suppression (sacramental exile); trapped by religious identity-lock in the constraint
 *   - sacramental_orthodoxy_doctrine: non-agent beneficiary; collects vindication from the constraint's demonstrable enforcement; each denial reinforces the doctrine's appearance of foundational truth
 *   - remarried_catholics_in_good_standing: beneficiaries of clarity and stability; their annulments granted; they bear the cost that enforcement is applied to those outside the institutional process
 *   - theology_of_marriage_advocates: observers from multiple traditions; can articulate alternative readings but cannot adjudicate within Catholic institutional life
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_sacrament__hierarchical_indissolubility_reading, 0.68).
domain_priors:suppression_score(marriage_sacrament__hierarchical_indissolubility_reading, 0.71).
domain_priors:theater_ratio(marriage_sacrament__hierarchical_indissolubility_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_sacrament__hierarchical_indissolubility_reading, tangled_rope).
narrative_ontology:human_readable(marriage_sacrament__hierarchical_indissolubility_reading, "Marriage Sacrament: Hierarchical Indissolubility Reading").
narrative_ontology:topic_domain(marriage_sacrament__hierarchical_indissolubility_reading, "religious/doctrinal/political").

domain_priors:requires_active_enforcement(marriage_sacrament__hierarchical_indissolubility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_sacrament__hierarchical_indissolubility_reading, 'a062b5eb-05eb-4921-94ff-d4eeeca187be').
narrative_ontology:cs_kernel_codification('a062b5eb-05eb-4921-94ff-d4eeeca187be', fixed_text).
narrative_ontology:cs_authority_grounding('a062b5eb-05eb-4921-94ff-d4eeeca187be', extraction).
narrative_ontology:cs_interpretation_layer_present('a062b5eb-05eb-4921-94ff-d4eeeca187be').
narrative_ontology:cs_reading_relation('a062b5eb-05eb-4921-94ff-d4eeeca187be', marriage_sacrament__civic_pastoral_reading, coexists_with).
narrative_ontology:cs_axiom('a062b5eb-05eb-4921-94ff-d4eeeca187be', foundational, marriage_as_ontological_reality).
narrative_ontology:cs_axiom_status(marriage_as_ontological_reality, holdable).
narrative_ontology:cs_axiom_grounding('a062b5eb-05eb-4921-94ff-d4eeeca187be', marriage_as_ontological_reality, deontological).
narrative_ontology:cs_axiom('a062b5eb-05eb-4921-94ff-d4eeeca187be', foundational, indissolubility_constitutive_not_aspirational).
narrative_ontology:cs_axiom_status(indissolubility_constitutive_not_aspirational, holdable).
narrative_ontology:cs_axiom_grounding('a062b5eb-05eb-4921-94ff-d4eeeca187be', indissolubility_constitutive_not_aspirational, deontological).
narrative_ontology:cs_axiom('a062b5eb-05eb-4921-94ff-d4eeeca187be', secondary, hierarchical_adjudication_required).
narrative_ontology:cs_axiom_status(hierarchical_adjudication_required, holdable).
narrative_ontology:cs_axiom_grounding('a062b5eb-05eb-4921-94ff-d4eeeca187be', hierarchical_adjudication_required, conventional).
narrative_ontology:cs_reference_frame('a062b5eb-05eb-4921-94ff-d4eeeca187be', indissolubility_as_ontological_fact).
narrative_ontology:cs_drift_state('a062b5eb-05eb-4921-94ff-d4eeeca187be', contemporary_post_vatican_ii_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a062b5eb-05eb-4921-94ff-d4eeeca187be', '').
narrative_ontology:cs_kernel_id(marriage_sacrament__hierarchical_indissolubility_reading, marriage_sacrament).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_sacrament__hierarchical_indissolubility_reading, ecclesiastical_hierarchy).
narrative_ontology:constraint_beneficiary(marriage_sacrament__hierarchical_indissolubility_reading, sacramental_orthodoxy_doctrine).
narrative_ontology:constraint_victim(marriage_sacrament__hierarchical_indissolubility_reading, divorced_catholics_seeking_remarriage).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_sacrament__hierarchical_indissolubility_reading, remarried_catholics_in_good_standing).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces the doctrine of indissolubility as an ontological claim about the nature of marriage, not merely a pastoral ideal. Administers the annulment tribunal system as the authoritative arbiter of marriage validity. Collects authority from this gatekeeping function: only the institutional church can adjudicate whether a marriage ever existed in the sacramental sense. The constraint's persistence depends on maintaining the hierarchy's monopoly on marriage adjudication and denying sacramental access to those who have remarried outside the church's validation.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, ecclesiastical_hierarchy, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Cannot access the sacrament of reconciliation or Eucharist if they have remarried without an annulment. They face the costs of the annulment tribunal process—documentation, fees, delays measured in years, and the institutional humiliation of having their marriage declared never to have existed. Their Catholic identity is fused with sacramental participation; exit means either abandoning remarriage, abandoning the church, or living in sacramental exile within their own tradition. The constraint's enforcement machinery exists to suppress this group's alternative: civil remarriage followed by sacramental re-participation.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, divorced_catholics_seeking_remarriage, payer,
    powerless, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(marriage_sacrament__hierarchical_indissolubility_reading, divorced_catholics_seeking_remarriage, excluded).

% The doctrine itself—the claim that marriage is an ontological reality that cannot be unmade by human will—collects authority from the constraint's operation. Each denied remarriage and enforced annulment tribunal reinforces the doctrine's appearance of foundational truth rather than constructed institutional practice. The doctrine is not an agent; it is a proposition that benefits from extraction in the form of demonstrable institutional commitment and compliance.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, sacramental_orthodoxy_doctrine, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(marriage_sacrament__hierarchical_indissolubility_reading, sacramental_orthodoxy_doctrine).

% A subset of the Catholic population whose remarriages were annulled by the tribunal or who obtained annulments before remarrying. They benefit from the clarity and stability the indissolubility doctrine provides to their current marriages: their sacramental status is unambiguous, and the church's enforcement against divorce protects the commitment they entered into. They bear the cost that the enforcement machinery is publicly applied to those who did not navigate the institutional process.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, remarried_catholics_in_good_standing, beneficiary,
    moderate, biographical, constrained, global).

% Academic and pastoral theologians from inside and outside the Catholic tradition who argue that indissolubility can be held as a profound aspiration without requiring institutional exclusion of the remarried. They offer competing frameworks—from Protestant indissolubility theology to secular philosophical analysis of marriage as commitment—that frame the same kernel differently. They can publish, teach, and persuade; they cannot adjudicate within Catholic institutional life.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, theology_of_marriage_advocates, observer,
    powerful, generational, mobile, global).

% Historical voices within Catholicism (Vatican II era, 1960s–1970s) who advocated for pastoral flexibility in marriage adjudication—recognizing human failure and pastoral mercy as constitutive of indissolubility doctrine rather than threats to it. They remain excluded from the formal institutional decision-making about this constraint; their reformist readings persist in pastoral practice but do not alter the binding doctrine.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, vatican_council_ii_reformers, excluded,
    powerful, generational, trapped, global).

% Protestant churches that permit remarriage and full sacramental/congregational participation for the divorced. Their existence is the institutional proof that indissolubility need not require the exclusionary enforcement structure the Catholic hierarchy maintains. They can demonstrate an alternative reading; they cannot alter the Catholic constraint from outside.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, divorced_protestant_denominations, observer,
    powerful, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_sacrament__hierarchical_indissolubility_reading, ecclesiastical_hierarchy).
narrative_ontology:fixing_cost_class(marriage_sacrament__hierarchical_indissolubility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stabilizes marriage as an ontological commitment that persists beyond human will or desire, providing doctrinal clarity about what marriage IS (an unbreakable sacramental bond, not a contract). This coordination function is genuinely held by the hierarchy: it answers the theological question 'What makes a marriage a marriage?' with 'God's bond, administered through the church, indissoluble by human choice.'
% TRANSFER_FUNCTION: Moves the authority to define marriage validity from the civil order and the individuals within it to the ecclesiastical hierarchy's tribunal system. Moves the cost of marriage dissolution from the divorcing parties to: (1) annulment applicants (fees, delays, humiliation of validation review), (2) the remarried (sacramental exclusion, identity conflict), and (3) the institutional church (tribunal administration, pastoral resistance). The transfer is from bottom-up lived experience to top-down doctrinal adjudication.
% ABSENT_VOICES: Divorced Catholics who have moved to Protestant denominations or secular frameworks are structurally silent in this constraint—they cannot appeal to the hierarchy from outside it. Remarried Catholics who have simply accepted sacramental absence are often silent by accommodation rather than voice. The hierarchy's own progressive reformers (Vatican II traditionalists, liberation theology voices emphasizing pastoral mercy) would object if granted standing; they remain institutionally excluded.
% DISAPPEARANCE_RATIONALE: If the indissolubility constraint and its enforcement machinery vanished overnight, the Catholic Church would face immediate institutional transformation: annulment tribunals would dissolve, sacramental access for the remarried would become a pastoral decision rather than an institutional bar, marriage theology would shift from ontological fixity to relational aspiration, and millions of Catholics in sacramental exile would be reincorporated. The constraint's absence would reshape the entire institutional and spiritual landscape of Catholicism.
% FOUNDING_PROBLEM: Early Christian churches faced a pastoral and doctrinal crisis: were marriages dissolvable by infidelity, death, or circumstance? How could indissolubility be taught as an ideal when human failure was universal? The constraint was built to answer this by making indissolubility not an aspiration but an ontological fact—a sacrament is indissoluble by definition, and if the sacrament is gone, it was never validly there. This transforms the problem from 'How do we hold people to commitments?' to 'How do we define what a true commitment IS?'
% FOUNDING_PROBLEM_CORROBORATION: The ecclesiastical hierarchy attests the founding problem is eternal and live: divorce remains theologically incoherent within Christian anthropology. Divorced theologians and pastoral practitioners attest the founding problem was solved differently: by distinguishing sacramental meaning from human failure, compassion became possible without doctrinal collapse. Pope Francis's expanded annulment procedures and papal language about 'pastoral accompaniment' represent institutional voice from within the benefiting seat that questions whether the original enforcement mechanism remains necessary. External observers (sociologists, Protestant theologians, secular legal theorists) attest that the founding problem has been solved by other institutional structures (secular divorce law, remarriage acceptance in other Christian denominations) and the constraint now persists as institutional identity, not problem-solving.
narrative_ontology:disappearance_verdict(marriage_sacrament__hierarchical_indissolubility_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_sacrament__hierarchical_indissolubility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_sacrament__hierarchical_indissolubility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_sacrament__hierarchical_indissolubility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_sacrament__hierarchical_indissolubility_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_sacrament__hierarchical_indissolubility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_sacrament__hierarchical_indissolubility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_sacrament__hierarchical_indissolubility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is substantial because the constraint transfers authority over marriage validity from individuals and civil society to the institutional hierarchy, imposes tangible costs (annulment delays, fees, institutional humiliation, sacramental denial) on a defined victim set, and the extraction persists even as the founding problem (How to hold people to commitments?) has been solved by other institutional structures (secular law, other Christian denominations, pastoral practice). Suppression (0.71) is even higher because the constraint's persistence depends on actively preventing the remarried from accessing sacraments and on maintaining the hierarchy's monopoly over marriage adjudication—this is structural enforcement, not voluntary coordination. Theater_ratio (0.42) is moderate-to-high: the theological justification (ontological indissolubility) is real and deeply held, but an increasing share of the ecclesiastical hierarchy's own discourse (Pope Francis's expanded annulment procedures, 'pastoral accompaniment' language, statistical ease of modern annulments) suggests the enforcement function is increasingly theatrical—the doctrine is maintained but the mechanism is relaxed, producing the appearance of fidelity while softening enforcement at the margins. The measurement series across 60 time units (plausibly the post-Vatican II era, 1960s–2020s) show extractiveness and suppression rising slightly but plateauing: the constraint's core function remains stable, but the hierarchy's own pressure (reform movements, demographic change, Pope Francis's modernization) prevents further hardening. The actor is not failing but accommodating—the theater_ratio rises faster than extraction, indicating performative maintenance.
 *
 * PERSPECTIVAL GAP:
 *   The hierarchy and the remarried-in-exile sit at opposite ends of the directionality spectrum and should compute radically different constraint types. From the hierarchy's seat: the constraint is genuine coordination (marriage means what the doctrine says it means) and the enforcement is legitimate (those who accept the coordinate meaning share in the community; those who reject it accept the consequences). From the remarried-in-exile seat: the constraint is pure extraction (they accepted the marriage as real, lived it as sacramental, and now are told it never was—a retroactive invalidation that serves only the hierarchy's institutional control). The engine computes these divergent classifications from the same structural data because the power atoms, exit options, and measured metrics are read differently through different institutional positions. The hierarchy has arbitrage-grade exit (it can change the doctrine); the remarried have identity_locked exit (they cannot leave the faith without shattering their self-understanding). This structural asymmetry should produce divergent type classifications—the hierarchy computing tangled_rope, the remarried computing snare. The authored claim (tangled_rope overall) reflects the constraint's structure at its core; the divergence is the signal.
 *
 * DIRECTIONALITY LOGIC:
 *   The ecclesiastical hierarchy sits at the beneficiary end of the directionality spectrum (d near 0.0): it sets the agenda, collects authority from the constraint's operation, and has high exit options (it can modify doctrine, relax enforcement, or maintain it—the choice is its to make). Divorced Catholics seeking remarriage sit at the target end (d near 1.0): they are identity-locked by religious affiliation, have trapped or highly constrained exit options (remarry civilly and lose sacraments, abandon the faith entirely, or accept sacramental exile), and bear the extraction directly. The sibling readings represent different d assignments at the same structural location: the civic_pastoral reading would derive lower d for the divorced (more beneficiary-like, as pastoral mercy flows to them) and lower d for the hierarchy (more symmetric, as it shares burden of compassionate discernment). This reading assigns high d to the divorced and low d to the hierarchy by treating indissolubility as ontologically fixed and adjudication as hierarchically gatekept.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint presents as tangled_rope (genuine coordination function + asymmetric extraction) rather than pure snare because the indissolubility doctrine solves a real theological problem: 'What makes a marriage a marriage?' The answer 'God's indissoluble bond, administered by the church' coordinates Christian anthropology and provides stable meaning. However, the extraction is real and asymmetric: the same doctrine that coordinates meaning among believers also transfers authority to the hierarchy and denies sacramental access to the remarried. The mandatrophy question—does the constraint persist because the coordination function remains live, or because the extraction function has become primary?—is answered differently by different seats: the hierarchy and those whose annulments were granted affirm the coordination function; remarried Catholics in sacramental exile identify the extraction function as primary. The measurement series shows extraction plateauing while theater rises, suggesting the coordination function is weaker than it was at the constraint's founding. If the founding_problem (How to hold people to commitments?) is now substantially dead (secular law, other Christian traditions, even Catholic pastoral practice have all solved versions of this problem), and the disappearance_verdict is world_rearranges (the church would reorganize), then the constraint may be drifting toward piton classification—maintained by institutional inertia and identity-fusion rather than by active problem-solving. The tangled_rope claim captures the structure at the constraint's core; the metrics capture its drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_vs_pastoral_framing,
    'Is indissolubility a constitutive metaphysical claim about what marriage IS, or a profound ethical aspiration about what marriage SHOULD BE?',
    'Comparative theological analysis across Christian traditions and across historical Catholic doctrine (pre-Vatican II vs. post-Vatican II): does the constraint''s foundational framing remain fixed, or has it drifted toward pastoral language even while the institutional enforcement persists? What does the hierarchy''s own internal discourse (papal statements, canon law revisions, tribunal procedures) reveal about which framing is actually operative?',
    'If ontological framing is truly constitutive: indissolubility is a structural feature of sacramental reality and the constraint is genuinely coordinating meaning. If pastoral framing is operative: indissolubility is aspirational and the constraint is primarily extractive (maintaining authority over marriage definition). This resolution would determine whether the constraint is tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontological_vs_pastoral_framing, conceptual, 'Whether indissolubility is framed as ontological fact or pastoral aspiration—the distinction that separates this reading from the civic_pastoral sibling.').

omega_variable(
    founding_problem_persistence,
    'Is the founding problem (How to hold people to lifetime commitments in marriage?) still live within Catholicism, or has it been solved by other institutional structures (secular law, other Christian denominations, pastoral practice)?',
    'Demographic and institutional analysis: (1) Do Catholics still appeal to indissolubility doctrine as their primary framework for understanding marriage commitment, or do they appeal to civil law, personal vows, and relational aspiration? (2) Has the hierarchy''s own discourse shifted from ''indissolubility is the solution'' to ''indissolubility is the ideal, but pastoral mercy is also essential''? (3) What proportion of remarried Catholics experience the constraint as a problem-solving mechanism vs. an institutional gate?',
    'If the founding problem is dead (solved by secular law and other traditions): the constraint is a zombie—maintained by institutional inertia and identity-fusion rather than active problem-solving, meeting piton classification criteria. If the founding problem is contested: the constraint drifts between tangled_rope and piton depending on the observer''s institutional position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether the constraint continues to solve an active problem or persists as institutional theater.').

omega_variable(
    identity_lock_mechanism_for_remarried,
    'For divorced Catholics who wish to remarry, is their sacramental exclusion produced by structural barriers (they cannot access annulment tribunals, annulments are prohibitively expensive) or by internalized identity-fusion (they accept the church''s framing that their remarriage is invalid, so they exclude themselves)?',
    'Post-exit analysis: when divorced Catholics leave the Catholic tradition for Protestant denominations or secular frameworks, does the suppression persist (they retain guilt, unworthiness, identity confusion) or dissolve (they adopt the new framework''s narrative about remarriage and recover agency)? How quickly? What proportion report resolution?',
    'If suppression is structural: the constraint''s effective suppression is lower than authored (people could escape by geographic mobility or denominational switching). If suppression is internalized: the constraint''s effective suppression is higher than authored (people carry the exclusion with them after exit). If both: the constraint exhibits high-complexity identity_locked exit and may warrant upward directionality revision for the remarried.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_for_remarried, empirical, 'Whether suppression of the remarried is structural or internalized—a critical driver of identity_lock severity.').

omega_variable(
    kernel_reading_distinction,
    'This story instantiates the hierarchical_indissolubility reading. What structural evidence distinguishes it from the civic_pastoral reading?',
    'Comparative constraint analysis: the sibling reading (civic_pastoral) would show lower extractiveness (pastoral mercy available without tribunal cost), different victim set (no remarried excluded), possibly rope classification. The kernel is shared (marriage doctrine), but the readings instantiate different constraints. This omega documents the alternative reading''s expected structure and clarifies why one story cannot be forced to cover both readings.',
    'Recognizing that both readings are instantiable from the same kernel prevents false-positive classification divergence. The engine should compute different types for the two readings; if both compute tangled_rope, the readings are not genuinely distinct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'The committer-axis structural distinction between hierarchical and pastoral readings of marriage indissolubility.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_sacrament__hierarchical_indissolubility_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(marr_tr_t0, observed).
narrative_ontology:measurement(marr_tr_t10, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement_basis(marr_tr_t10, observed).
narrative_ontology:measurement(marr_tr_t20, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement_basis(marr_tr_t20, observed).
narrative_ontology:measurement(marr_tr_t30, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement_basis(marr_tr_t30, observed).
narrative_ontology:measurement(marr_tr_t40, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement_basis(marr_tr_t40, observed).
narrative_ontology:measurement(marr_tr_t50, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 50, 0.41).
narrative_ontology:measurement_basis(marr_tr_t50, observed).
narrative_ontology:measurement(marr_tr_t60, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 60, 0.42).
narrative_ontology:measurement_basis(marr_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(marr_be_t0, observed).
narrative_ontology:measurement(marr_be_t10, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement_basis(marr_be_t10, observed).
narrative_ontology:measurement(marr_be_t20, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement_basis(marr_be_t20, observed).
narrative_ontology:measurement(marr_be_t30, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 30, 0.64).
narrative_ontology:measurement_basis(marr_be_t30, observed).
narrative_ontology:measurement(marr_be_t40, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 40, 0.66).
narrative_ontology:measurement_basis(marr_be_t40, observed).
narrative_ontology:measurement(marr_be_t50, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 50, 0.67).
narrative_ontology:measurement_basis(marr_be_t50, observed).
narrative_ontology:measurement(marr_be_t60, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement_basis(marr_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(marr_su_t0, observed).
narrative_ontology:measurement(marr_su_t10, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement_basis(marr_su_t10, observed).
narrative_ontology:measurement(marr_su_t20, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 20, 0.66).
narrative_ontology:measurement_basis(marr_su_t20, observed).
narrative_ontology:measurement(marr_su_t30, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement_basis(marr_su_t30, observed).
narrative_ontology:measurement(marr_su_t40, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 40, 0.69).
narrative_ontology:measurement_basis(marr_su_t40, observed).
narrative_ontology:measurement(marr_su_t50, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 50, 0.7).
narrative_ontology:measurement_basis(marr_su_t50, observed).
narrative_ontology:measurement(marr_su_t60, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 60, 0.71).
narrative_ontology:measurement_basis(marr_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_sacrament__hierarchical_indissolubility_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(marriage_sacrament__hierarchical_indissolubility_reading, 0.12).
narrative_ontology:affects_constraint(marriage_sacrament__hierarchical_indissolubility_reading, marriage_sacrament__civic_pastoral_reading).

% DUAL FORMULATION NOTE:
% The marriage_sacrament kernel decomposes into two structurally distinct readings: hierarchical_indissolubility_reading (this story, high extractiveness due to sacramental exclusion and tribunal gatekeeping) and civic_pastoral_reading (sibling story, lower extractiveness due to pastoral mercy and individual discernment). These are not the same constraint viewed from different angles—they have different ε values, different beneficiary/victim structures, and different classifications. The constraint family is linked via network.affects_constraints: institutional changes in one reading (e.g., Pope Francis's expanded annulment procedures) create structural pressure on the sibling reading. Authors should not force one story to handle both readings; the ε-invariance principle requires separate files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_sacrament__hierarchical_indissolubility_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
