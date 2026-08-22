% ============================================================================
% CONSTRAINT STORY: constitutional_secularism__reformist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_secularism__reformist_reading, []).

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
 *   constraint_id: constitutional_secularism__reformist_reading
 *   human_readable: Reformist Reading: Affirmative State Duty to Eliminate Oppressive Religious Practice
 *   domain: constitutional_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This constraint instantiates the reformist reading of the constitutional
 *   secularism kernel: the state carries an affirmative constitutional duty
 *   to eliminate religious practices that oppress marginalized groups within
 *   religious communities — caste-excluded worshippers, women barred from
 *   shrines or rites — and this duty supersedes claims of religious
 *   institutional autonomy, including claims that the practice is doctrinally
 *   essential. This is the most extractive of the three sibling readings with
 *   respect to religious autonomy: unlike the strict-neutrality reading
 *   (equal distance, no interference) or the principled-intervention reading
 *   (intervention limited to protecting weaker sections without a
 *   freestanding affirmative duty), the reformist reading treats religious
 *   autonomy as presumptively subordinate whenever an equality harm is
 *   identified, removing the internal-community-choice buffer entirely.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_secularism__reformist_reading, 0.71).
domain_priors:suppression_score(constitutional_secularism__reformist_reading, 0.68).
domain_priors:theater_ratio(constitutional_secularism__reformist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__reformist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_secularism__reformist_reading, "Reformist Reading: Affirmative State Duty to Eliminate Oppressive Religious Practice").
narrative_ontology:topic_domain(constitutional_secularism__reformist_reading, "constitutional_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(constitutional_secularism__reformist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__reformist_reading, '417f1cdc-f259-4097-a6e8-d33da7d070d0').
narrative_ontology:cs_kernel_codification('417f1cdc-f259-4097-a6e8-d33da7d070d0', formalized).
narrative_ontology:cs_authority_grounding('417f1cdc-f259-4097-a6e8-d33da7d070d0', lineage).
narrative_ontology:cs_interpretation_layer_present('417f1cdc-f259-4097-a6e8-d33da7d070d0').
narrative_ontology:cs_reading_relation('417f1cdc-f259-4097-a6e8-d33da7d070d0', constitutional_secularism__strict_neutrality_reading, forecloses).
narrative_ontology:cs_reading_relation('417f1cdc-f259-4097-a6e8-d33da7d070d0', constitutional_secularism__principled_intervention_reading, influences).
narrative_ontology:cs_axiom('417f1cdc-f259-4097-a6e8-d33da7d070d0', foundational, substantive_equality_overrides_group_autonomy).
narrative_ontology:cs_axiom_status(substantive_equality_overrides_group_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('417f1cdc-f259-4097-a6e8-d33da7d070d0', substantive_equality_overrides_group_autonomy, deontological).
narrative_ontology:cs_axiom('417f1cdc-f259-4097-a6e8-d33da7d070d0', foundational, affirmative_state_duty_not_discretionary_power).
narrative_ontology:cs_axiom_status(affirmative_state_duty_not_discretionary_power, holdable).
narrative_ontology:cs_axiom_grounding('417f1cdc-f259-4097-a6e8-d33da7d070d0', affirmative_state_duty_not_discretionary_power, conventional).
narrative_ontology:cs_reference_frame('417f1cdc-f259-4097-a6e8-d33da7d070d0', constitutional_founding_anti_exclusion_mandate).
narrative_ontology:cs_drift_state('417f1cdc-f259-4097-a6e8-d33da7d070d0', contemporary_minority_rights_contestation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('417f1cdc-f259-4097-a6e8-d33da7d070d0', '').
narrative_ontology:cs_kernel_id(constitutional_secularism__reformist_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, scheduled_caste_temple_entrants).
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, women_excluded_from_worship_practices).
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, reform_oriented_legislators).
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, constitutional_courts_asserting_review_power).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, religious_conservatives_across_communities).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, denominational_institutions_defending_essential_practices).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, minority_religious_boards_facing_state_intervention).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, the_state_executive).
narrative_ontology:constraint_vindicates(constitutional_secularism__reformist_reading, substantive_equality_supersedes_group_autonomy).
narrative_ontology:constraint_vindicates(constitutional_secularism__reformist_reading, state_as_guarantor_of_intragroup_justice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicates which religious practices count as oppressive enough to override claims of religious autonomy, using an 'essential practices' or substantive-equality test it largely defines itself. Its rulings expand or contract the scope of permissible state intervention and it bears no direct cost from expanding that scope.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, constitutional_courts_asserting_review_power, agenda_setter,
    institutional, generational, analytical, national).

% Draft and pass statutes overriding religious customs deemed discriminatory (temple entry acts, personal-law reform bills, anti-exclusion statutes). They gain political capital and credit for advancing equality while facing limited personal cost if enforcement provokes backlash, since enforcement burden falls on courts and local administration.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, reform_oriented_legislators, agenda_setter,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(constitutional_secularism__reformist_reading, reform_oriented_legislators, beneficiary).

% Historically barred from entering temples or performing certain rites solely due to caste status. The reformist doctrine directly enables their legal right to entry and worship, overriding claims by temple management that exclusion is a core religious practice. They had no meaningful exit from caste status itself, making the constitutional intervention their primary lever for access.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, scheduled_caste_temple_entrants, beneficiary,
    powerless, generational, trapped, regional).

% Barred from specific shrines, rites, or roles (menstruation-linked exclusions, priesthood bans) on religious grounds. The reformist doctrine authorizes courts to strike these down as discriminatory despite religious claims of essential doctrine. Their exit option is limited to litigation or informal defiance, both costly and contested within their own communities.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, women_excluded_from_worship_practices, beneficiary,
    moderate, generational, constrained, regional).

% Hold that certain contested practices are doctrinally essential, not incidental discrimination, and experience judicial or legislative override as an assault on their community's self-governance. They can litigate, protest, or attempt legislative reversal, but face a doctrine explicitly designed to subordinate their autonomy claim to an external equality test; exit from the jurisdiction is not realistic for most.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, religious_conservatives_across_communities, payer,
    organized, biographical, constrained, national).

% Temple trusts, religious boards, and denominational bodies that administer contested practices and are named as respondents when courts or legislatures intervene. They bear direct institutional costs — loss of managerial control, forced rule changes, litigation expense — and their defense that a practice is 'essential' is precisely what the reformist doctrine is built to override when it finds oppression.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, denominational_institutions_defending_essential_practices, payer,
    powerful, generational, constrained, national).

% Governing bodies of minority religious communities (personal law boards, minority trusts) that argue the reformist doctrine, though framed as protecting their own women or lower-status members, is applied more aggressively against minority communities than majority ones, compounding the intervention with a majoritarian-pressure dynamic they cannot easily contest given their minority political weight.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, minority_religious_boards_facing_state_intervention, payer,
    moderate, biographical, trapped, national).

% Enforces court orders and legislative mandates against resistant religious institutions, using police and administrative machinery. Gains an expanded constitutional mandate to regulate religious affairs whenever it can characterize a practice as oppressive, a mandate it can selectively deploy against politically disfavored communities.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, the_state_executive, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(constitutional_secularism__reformist_reading, the_state_executive, agenda_setter).

% Would support internal reform through community deliberation rather than external judicial or legislative override, but their voice is largely absent from a contest framed as state-versus-institution; their preferred incremental, internally-legitimated path is neither what conservatives defend nor what reformist doctrine delivers.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, religious_moderates_within_affected_communities, excluded,
    powerless, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_secularism__reformist_reading, diffuse).
narrative_ontology:fixing_cost_class(constitutional_secularism__reformist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism by which historically subordinated persons within a religious community — lower castes, women, other excluded groups — can obtain relief from exclusionary practices without needing the consent of the religious authority that maintains the exclusion, coordinating state power behind an equality norm that the internal hierarchy will not enforce against itself.
% TRANSFER_FUNCTION: Moves authority over defining and enforcing the boundaries of legitimate religious practice from religious institutions and their leadership to courts and legislatures, and moves social and ritual access from those groups' exclusionary gatekeepers to the previously excluded groups; the cost is borne by denominational institutions' autonomy and by conservative adherents' capacity to maintain practices they consider essential.
% ABSENT_VOICES: Religious moderates and internal reformers who might resolve exclusionary practices through community-level negotiation rarely appear in the doctrine's application, which is structured as an external state-versus-institution binary; internal reform pathways are largely bypassed rather than empowered.
% DISAPPEARANCE_RATIONALE: If the reformist doctrine were withdrawn, courts would lose the affirmative-duty basis to override claims of religious essentiality, and jurisdictions would revert to a stronger presumption of religious institutional autonomy — temple entry restrictions, exclusionary rites, and personal-law provisions currently struck down under this doctrine could be reasserted, and pending reform litigation would collapse absent a substitute doctrinal basis.
% FOUNDING_PROBLEM: Constitutional framers and early courts confronted entrenched caste-based exclusion (temple entry bans) and gender-based exclusion within religious practice that religious communities showed no internal capacity or willingness to reform, creating a felt need for an external constitutional lever to force change where internal hierarchy was the source of the harm.
% FOUNDING_PROBLEM_CORROBORATION: Dalit rights organizations and women's rights litigants outside any religious hierarchy corroborate that caste and gender exclusion within religious practice remains a live, unresolved problem in many communities, supporting the 'live' reading. Independent constitutional scholars and minority-rights advocates outside the reformist coalition corroborate a competing claim: that the doctrine is now applied asymmetrically — vigorously against minority communities' practices, more cautiously against majority-community practices with comparable exclusionary structure — suggesting the founding problem has been partially supplanted by a majoritarian-leverage function not attested to by any party with an interest in denying it.
narrative_ontology:disappearance_verdict(constitutional_secularism__reformist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_secularism__reformist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_secularism__reformist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_secularism__reformist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_secularism__reformist_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_secularism__reformist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_secularism__reformist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_secularism__reformist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises across the measured interval (0.42 to 0.71) as courts and legislatures progressively expand what counts as an 'oppressive practice' subject to override, and as the doctrine is invoked more frequently against minority religious institutions specifically. Suppression is substantial (0.68) because enforcement against a resistant temple trust or religious board requires police backing, injunctions, and administrative machinery, not voluntary compliance. Theater ratio stays comparatively low (0.22) because the state's intervention produces genuine legal and physical access changes (temple entry, priesthood access) rather than merely symbolic ones — the coordination function for the beneficiary groups is real, which is precisely what makes this a tangled rope rather than a pure snare: the extraction from religious institutions rides on a genuine emancipatory function for caste- and gender-excluded persons.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of a scheduled-caste temple entrant or an excluded woman, the doctrine is unambiguous liberation — access that centuries of internal community process never delivered. From the seat of a denominational institution or a minority religious board, the identical doctrinal machinery presents as an external body overriding self-governance using a test (oppressiveness, essentiality) that the reviewing court itself defines and applies unevenly. The engine's per-seat computation should register both readings as structurally correct for their respective seats rather than resolving them into one verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Scheduled caste entrants and excluded women are structural beneficiaries with historically no meaningful exit from the excluding institution — the doctrine is their primary lever, pushing their directionality toward the beneficiary end despite their otherwise powerless position. Religious conservatives and denominational institutions are structural targets: the doctrine is explicitly built to override their autonomy claim, and their exit options (litigation, political mobilization) do not amount to genuine escape from the constraint's operation. Courts and reform legislators are agenda-setters who bear little direct cost from expanding the doctrine's scope, which is why their directionality sits near the beneficiary/administrator end even though they do not personally 'collect' anything material.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — entrenched caste and gender exclusion that religious hierarchies showed no internal capacity to reform — remains genuinely live in specific documented practices, which prevents a blanket mandatrophy verdict. But the founding_problem_status is authored as contested because independent minority-rights scholarship documents the doctrine's asymmetric application: vigorous intervention against minority-community practices, greater judicial deference toward comparable majority-community practices. That asymmetry is not evidence the emancipatory function is dead, but it is evidence the doctrine has partly drifted from equal-protection-of-the-excluded toward a majoritarian-leverage instrument — exactly the kind of divergence a tangled-rope classification is built to hold without forcing a premature snare or rope verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    essential_practices_test_manipulability,
    'Is the ''essential religious practice'' or ''oppressiveness'' test the reformist doctrine relies on a principled constitutional standard, or a manipulable proxy that lets courts and legislatures reach outcomes they favor on other grounds while claiming doctrinal necessity?',
    'Comparative analysis of case outcomes across religious communities with structurally similar exclusionary practices: if the doctrine is applied consistently regardless of which community''s practice is at issue, it supports a principled reading; if application correlates with the political salience or minority status of the community, it supports the manipulable-proxy reading.',
    'A principled, consistent test supports classifying the doctrine''s coordination function as dominant; a manipulable, asymmetrically-applied test would push the classification toward snare, since the coordination story would be functioning primarily as cover for majoritarian or politically selective intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(essential_practices_test_manipulability, empirical, 'Whether the doctrinal test constraining intervention is principled or manipulable-as-applied.').

omega_variable(
    reformist_vs_neutrality_normative_priority,
    'Should the constitutional order treat protection of intragroup marginalized members as categorically prior to religious institutional autonomy (reformist), or should it treat non-interference as the default with intervention as a narrow exception (principled intervention), or should it treat state distance from religion as itself the paramount value (strict neutrality)?',
    'This is not resolvable by further fact-finding; it depends on a prior normative commitment about whether individual rights within a religious community outrank the community''s collective self-governance claim, and how much weight state neutrality carries as an independent value.',
    'The answer determines which of the three sibling constraints in this kernel family should be treated as the operative constitutional doctrine, but does not change the classification of any single reading considered on its own terms.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reformist_vs_neutrality_normative_priority, preference, 'Irreducible normative disagreement underlying the choice among the three kernel readings.').

omega_variable(
    committer_framing_cross_index_coupling,
    'Does the reformist reading''s expansion correlate with Power x Community-status coupling — i.e., is the doctrine''s affirmative-duty override invoked disproportionately against politically weaker (minority) religious institutions relative to numerically or politically dominant ones with comparably exclusionary practices?',
    'Cross-index coupling analysis comparing invocation rates and remedy severity against minority-community institutions versus majority-community institutions with structurally similar practices under review.',
    'Strong coupling concentrating extraction on already-marginalized religious minorities (as institutions, distinct from the marginalized individuals within them) would indicate the reformist doctrine''s emancipatory coordination function is being selectively deployed as majoritarian leverage, supporting reclassification pressure toward snare for the minority-institution victim subset specifically.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_framing_cross_index_coupling, empirical, 'Whether doctrinal application couples religious-minority status with intervention severity independent of practice severity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_secularism__reformist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_secularism__reformist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cons_tr_t8, constitutional_secularism__reformist_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(cons_tr_t16, constitutional_secularism__reformist_reading, theater_ratio, 16, 0.15).
narrative_ontology:measurement(cons_tr_t24, constitutional_secularism__reformist_reading, theater_ratio, 24, 0.18).
narrative_ontology:measurement(cons_tr_t32, constitutional_secularism__reformist_reading, theater_ratio, 32, 0.2).
narrative_ontology:measurement(cons_tr_t40, constitutional_secularism__reformist_reading, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_secularism__reformist_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(cons_be_t8, constitutional_secularism__reformist_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(cons_be_t16, constitutional_secularism__reformist_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(cons_be_t24, constitutional_secularism__reformist_reading, base_extractiveness, 24, 0.64).
narrative_ontology:measurement(cons_be_t32, constitutional_secularism__reformist_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement(cons_be_t40, constitutional_secularism__reformist_reading, base_extractiveness, 40, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_secularism__reformist_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(cons_su_t8, constitutional_secularism__reformist_reading, suppression_requirement, 8, 0.52).
narrative_ontology:measurement(cons_su_t16, constitutional_secularism__reformist_reading, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(cons_su_t24, constitutional_secularism__reformist_reading, suppression_requirement, 24, 0.62).
narrative_ontology:measurement(cons_su_t32, constitutional_secularism__reformist_reading, suppression_requirement, 32, 0.65).
narrative_ontology:measurement(cons_su_t40, constitutional_secularism__reformist_reading, suppression_requirement, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_secularism__reformist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_secularism__reformist_reading, strict_neutrality_reading).
narrative_ontology:affects_constraint(constitutional_secularism__reformist_reading, principled_intervention_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the natural-language concept 'constitutional secularism' per the ε-invariance principle: strict_neutrality_reading (lowest ε — equal-distance, non-interference), principled_intervention_reading (moderate ε — bounded, case-by-case protective intervention), and this reformist_reading (highest ε — affirmative duty presumptively subordinating religious autonomy). Each carries its own stable ε, its own beneficiary/victim structure, and its own claimed type; they are linked here rather than merged because measuring 'constitutional secularism' under each reading's own lights yields materially different extraction levels, which is the schema's signal to decompose rather than average.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
