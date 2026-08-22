% ============================================================================
% CONSTRAINT STORY: marriage_authority__federalist_millet_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__federalist_millet_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: marriage_authority__federalist_millet_reading
 *   human_readable: Fragmented Marriage Authority as Consociational Anti-Majoritarian Mechanism
 *   domain: legal_pluralism/constitutional_law/comparative_family_law
 *
 * SUMMARY:
 *   This story instantiates the federalist/millet reading of the
 *   marriage-authority kernel: legal pluralism in family law is not (as the
 *   communal_autonomy_reading holds) grounded in the intrinsic authority of
 *   religious tradition, nor (as the secularist_reading holds) a transitional
 *   embarrassment awaiting a Uniform Civil Code, nor primarily a site of
 *   gender-rights contest (gender_rights_reading), nor a target for judicial
 *   constitutional-floor imposition (judicial_harmonization_reading). Rather,
 *   on this reading the fragmentation is a deliberate elite constitutional
 *   bargain — a millet-system-style consociational device designed to prevent
 *   a legislative majority from imposing its family-law norms on minority
 *   communities, thereby holding a plural polity together. Legislative
 *   paralysis on a uniform code is, on this reading, the mechanism doing its
 *   job, not a pathology. ε is authored low because the reading treats the
 *   standing arrangement as substantially a genuine coordination solution to
 *   a real majoritarian-domination risk, with concentrated cost falling on a
 *   narrower set of intra-community dissenters and cross-community couples
 *   rather than on the median community member.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__federalist_millet_reading, 0.28).
domain_priors:suppression_score(marriage_authority__federalist_millet_reading, 0.32).
domain_priors:theater_ratio(marriage_authority__federalist_millet_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__federalist_millet_reading, rope).
narrative_ontology:human_readable(marriage_authority__federalist_millet_reading, "Fragmented Marriage Authority as Consociational Anti-Majoritarian Mechanism").
narrative_ontology:topic_domain(marriage_authority__federalist_millet_reading, "legal_pluralism/constitutional_law/comparative_family_law").

domain_priors:requires_active_enforcement(marriage_authority__federalist_millet_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__federalist_millet_reading, '49240008-1a3e-4ac0-8418-0fa64a5ca9dc').
narrative_ontology:cs_kernel_codification('49240008-1a3e-4ac0-8418-0fa64a5ca9dc', distributed).
narrative_ontology:cs_authority_grounding('49240008-1a3e-4ac0-8418-0fa64a5ca9dc', distributed).
narrative_ontology:cs_reading_relation('49240008-1a3e-4ac0-8418-0fa64a5ca9dc', marriage_authority__communal_autonomy_reading, coexists_with).
narrative_ontology:cs_reading_relation('49240008-1a3e-4ac0-8418-0fa64a5ca9dc', marriage_authority__secularist_reading, influences).
narrative_ontology:cs_reading_relation('49240008-1a3e-4ac0-8418-0fa64a5ca9dc', marriage_authority__gender_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('49240008-1a3e-4ac0-8418-0fa64a5ca9dc', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_axiom('49240008-1a3e-4ac0-8418-0fa64a5ca9dc', foundational, fragmentation_prevents_majoritarian_domination).
narrative_ontology:cs_axiom_status(fragmentation_prevents_majoritarian_domination, holdable).
narrative_ontology:cs_axiom_grounding('49240008-1a3e-4ac0-8418-0fa64a5ca9dc', fragmentation_prevents_majoritarian_domination, instrumental).
narrative_ontology:cs_axiom('49240008-1a3e-4ac0-8418-0fa64a5ca9dc', secondary, legislative_paralysis_is_stability_mechanism_not_defect).
narrative_ontology:cs_axiom_status(legislative_paralysis_is_stability_mechanism_not_defect, holdable).
narrative_ontology:cs_axiom_grounding('49240008-1a3e-4ac0-8418-0fa64a5ca9dc', legislative_paralysis_is_stability_mechanism_not_defect, instrumental).
narrative_ontology:cs_reference_frame('49240008-1a3e-4ac0-8418-0fa64a5ca9dc', founding_consociational_settlement).
narrative_ontology:cs_drift_state('49240008-1a3e-4ac0-8418-0fa64a5ca9dc', contemporary_constitutional_equality_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('49240008-1a3e-4ac0-8418-0fa64a5ca9dc', '').
narrative_ontology:cs_kernel_id(marriage_authority__federalist_millet_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__federalist_millet_reading, minority_religious_communities).
narrative_ontology:constraint_beneficiary(marriage_authority__federalist_millet_reading, consociational_political_elites).
narrative_ontology:constraint_beneficiary(marriage_authority__federalist_millet_reading, national_unity_project).
narrative_ontology:constraint_victim(marriage_authority__federalist_millet_reading, intra_community_dissenters).
narrative_ontology:constraint_victim(marriage_authority__federalist_millet_reading, cross_community_couples).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain jurisdiction over marriage, divorce, and inheritance within their own community courts and personal law codes. This is the bargained protection against a majority-imposed uniform code that would likely track majority religious or cultural norms. Exit from the community's personal law system usually means exit from the community itself.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, minority_religious_communities, beneficiary,
    organized, generational, constrained, national).

% Negotiate and maintain the power-sharing arrangement that fragments marriage authority across community lines, trading a unified code for coalition stability and minority buy-in to the state. Legislative paralysis on personal law reform is, from this seat, the mechanism working as designed rather than a failure of governance.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, consociational_political_elites, agenda_setter,
    institutional, generational, arbitrage, national).

% The abstract stability of the multi-confessional state is preserved by not forcing a single marriage code onto communities that would treat imposition as an existential threat. Named for completeness; it is a condition sustained by the arrangement, not an actor collecting anything.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, national_unity_project, beneficiary,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(marriage_authority__federalist_millet_reading, national_unity_project).

% Individuals — often women, sometimes religious minorities-within-the-minority — who want relief from their community's personal law (divorce rights, inheritance shares, custody terms) but whose only forum is the very community court structure the pluralism preserves. Formal exit exists in theory (renouncing community jurisdiction) but often costs community membership, family standing, and social protection entirely.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, intra_community_dissenters, payer,
    powerless, biographical, trapped, local).

% Couples from different personal-law communities face jurisdictional uncertainty, forced conversion pressure, or reliance on a thin civil marriage statute that lacks the social and religious recognition either community law provides. The fragmentation that protects communities as blocs offers this group no home jurisdiction at all.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, cross_community_couples, payer,
    powerless, biographical, constrained, national).

% Holds nominal power to legislate a uniform code but treats the fragmented arrangement as a durable constitutional settlement rather than a temporary gap, declining repeatedly to act — which from this reading is itself the intended equilibrium, not inaction.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, national_legislature, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__federalist_millet_reading, national_legislature, agenda_setter).

% Periodically reviews personal law provisions against constitutional equality guarantees but has historically deferred to the pluralist settlement as a matter of preserving the consociational bargain, intervening only at the margins.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, constitutional_court, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine problem of how a religiously and culturally plural state avoids civil conflict: rather than one community's marriage norms becoming the law for all, each recognized community retains its own family-law jurisdiction, and no single majority can legislate its norms onto the others through ordinary majoritarian process.
% TRANSFER_FUNCTION: Moves the power to define marriage, divorce, and inheritance norms away from a potential legislative majority and toward community elites and religious authorities; the cost of this transfer is paid by individuals inside each community who dissent from their community's internal norms and by couples who fall outside any single community's jurisdiction.
% ABSENT_VOICES: Intra-community dissenters (especially women seeking divorce or inheritance equity) and cross-community couples are structurally underrepresented in the elite bargain that maintains fragmentation — the bargain is negotiated between community leaderships and the state, not between individuals and their communities.
% DISAPPEARANCE_RATIONALE: From the federalist-millet reading, if fragmented authority disappeared overnight in favor of an imposed uniform code, minority communities would experience it as majoritarian override of a foundational constitutional bargain, threatening the state's legitimacy claim on those communities. From the payer seats' perspective, its disappearance in favor of a rights-respecting alternative would rearrange their lives considerably for the better. The verdict differs by seat, which is why this reading marks it contested rather than picking a side.
% FOUNDING_PROBLEM: At the state's founding, imposing a single marriage code risked being read by minority communities as majority religious/cultural domination through law, threatening their participation in the new state and risking secession, communal violence, or non-compliance.
% FOUNDING_PROBLEM_CORROBORATION: Consociational political elites and community leaderships attest the founding problem remains live — communal trust in state institutions is attested by political scientists studying power-sharing arrangements as still fragile. Constitutional equality scholars and organizations representing intra-community dissenters, sitting outside the community leadership structures that benefit from fragmentation, attest that the original inter-communal threat has substantially receded while the personal-law fragmentation persists mainly as elite political currency.
narrative_ontology:disappearance_verdict(marriage_authority__federalist_millet_reading, contested).
narrative_ontology:founding_problem_status(marriage_authority__federalist_millet_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__federalist_millet_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_authority__federalist_millet_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__federalist_millet_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__federalist_millet_reading_tests).
:- end_tests(marriage_authority__federalist_millet_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-moderate (0.28) and suppression is moderate (0.32): the arrangement's costs are concentrated and real for dissenters and cross-community couples but are not the primary lever of the arrangement's persistence — persistence rests on the elite bargain's continued value to communities and the state, not on active coercive suppression of alternatives for the median community member. Theater ratio is low (0.22): the paralysis is functionally load-bearing on this reading, not merely performative. The claim (rope) and the metrics are authored independently: the metrics show a genuine but non-trivial extraction margin, which is consistent with a rope that nonetheless imposes real costs on a minority-within-the-minority — this is not tuned to force a clean rope verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Minority religious communities and consociational elites sit near the beneficiary end: the fragmentation directly protects and empowers them relative to a majoritarian alternative. Intra-community dissenters and cross-community couples sit near the target end: they bear the costs of the very jurisdictional lines drawn to protect the community bloc, with limited or costly exit. The national legislature and constitutional court occupy an ambiguous position — nominally capable of resolving the tension but declining to, which this reading treats as revealed preference for the consociational equilibrium rather than institutional failure.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy risk here runs in the direction of mislabeling durable, functioning coordination as pure extraction because concentrated victims are visible and vocal (a natural pull toward the gender_rights_reading or secularist_reading framings). This reading's founding_problem apparatus is built to resist that pull: it names the founding problem (majoritarian domination risk) as still partially live per elite/community corroboration, while requiring corroboration from outside the beneficiary set (dissenter-advocacy organizations, comparative political scientists) that flags the arrangement's function has partly ossified into elite political currency rather than remaining a pure anti-tyranny safeguard. The 'contested' founding_problem_status is the honest output of that check, not a hedge.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    elite_bargain_vs_communal_sovereignty_framing,
    'Is the persistence of fragmented marriage authority better explained as an ongoing elite anti-majoritarian bargain (this reading) or as the state''s continued deference to intrinsic communal religious authority (the communal_autonomy_reading)?',
    'Examine whether political elites ever attempt to renegotiate or centralize personal law during periods of reduced inter-communal tension — if fragmentation persists even when the majoritarian-domination risk demonstrably recedes, that favors the communal_autonomy framing over the elite-bargain framing, since the millet-style justification would have lost its rationale while the arrangement continued unchanged.',
    'If the elite-bargain framing is correct, declining majoritarian risk should predict declining defense of fragmentation by elites; if the communal-sovereignty framing is correct, defense should track community leadership preference independent of majoritarian risk. This changes whether the constraint''s classification should track measures of inter-communal threat or measures of community leadership entrenchment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_bargain_vs_communal_sovereignty_framing, conceptual, 'Whether fragmentation is best explained as elite anti-tyranny bargain versus intrinsic communal authority — the structural fork between this reading and its closest sibling.').

omega_variable(
    sibling_reading_disagreement_locus,
    'Where exactly does this reading''s classification diverge from the other four kernel readings, and which structural element carries the disagreement?',
    'Compare beneficiary/victim sets and ε values across all five readings once authored: communal_autonomy_reading should show similar beneficiaries but ground them differently; secularist_reading and judicial_harmonization_reading should show materially higher ε (since they treat the standing pluralist arrangement as more purely obstructive); gender_rights_reading should show a different, narrower victim set centered specifically on gender-based harms rather than the broader dissenter/cross-community set used here.',
    'Confirms that the five readings are not merely rhetorical variations on the same constraint but instantiate structurally distinct claims with different ε values and different beneficiary/victim structures, satisfying the ε-invariance decomposition requirement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_disagreement_locus, conceptual, 'Locating the specific structural disagreement between this reading and its four siblings in the marriage_authority kernel.').

omega_variable(
    legislative_paralysis_functionality,
    'Is the observed legislative paralysis on a uniform civil code genuinely load-bearing for consociational stability, or has it become inertial — maintained by political convenience long after its stabilizing function has diminished?',
    'Track whether attempts at incremental reform (e.g., optional civil marriage statutes, narrow anti-discrimination carve-outs) are blocked specifically when they would reduce majoritarian-domination risk versus blocked reflexively regardless of content; also track whether comparable plural states without the paralysis have experienced the majoritarian-domination outcomes this reading predicts would follow.',
    'If paralysis is genuinely functional, the rope classification is well-supported; if it has become inertial defense of elite prerogative disconnected from the founding risk, the constraint drifts toward a piton (theatrical stability-preservation) or tangled_rope (coordination cover for concentrated elite benefit) reading over time — this is exactly the divergence the temporal measurements are designed to surface.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legislative_paralysis_functionality, empirical, 'Whether the paralysis remains functionally necessary or has become inertial elite-serving theater.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__federalist_millet_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority__federalist_millet_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(marr_tr_t14, marriage_authority__federalist_millet_reading, theater_ratio, 14, 0.12).
narrative_ontology:measurement(marr_tr_t28, marriage_authority__federalist_millet_reading, theater_ratio, 28, 0.15).
narrative_ontology:measurement(marr_tr_t42, marriage_authority__federalist_millet_reading, theater_ratio, 42, 0.17).
narrative_ontology:measurement(marr_tr_t56, marriage_authority__federalist_millet_reading, theater_ratio, 56, 0.2).
narrative_ontology:measurement(marr_tr_t70, marriage_authority__federalist_millet_reading, theater_ratio, 70, 0.22).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority__federalist_millet_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(marr_be_t14, marriage_authority__federalist_millet_reading, base_extractiveness, 14, 0.2).
narrative_ontology:measurement(marr_be_t28, marriage_authority__federalist_millet_reading, base_extractiveness, 28, 0.22).
narrative_ontology:measurement(marr_be_t42, marriage_authority__federalist_millet_reading, base_extractiveness, 42, 0.24).
narrative_ontology:measurement(marr_be_t56, marriage_authority__federalist_millet_reading, base_extractiveness, 56, 0.26).
narrative_ontology:measurement(marr_be_t70, marriage_authority__federalist_millet_reading, base_extractiveness, 70, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority__federalist_millet_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(marr_su_t14, marriage_authority__federalist_millet_reading, suppression_requirement, 14, 0.3).
narrative_ontology:measurement(marr_su_t28, marriage_authority__federalist_millet_reading, suppression_requirement, 28, 0.31).
narrative_ontology:measurement(marr_su_t42, marriage_authority__federalist_millet_reading, suppression_requirement, 42, 0.31).
narrative_ontology:measurement(marr_su_t56, marriage_authority__federalist_millet_reading, suppression_requirement, 56, 0.32).
narrative_ontology:measurement(marr_su_t70, marriage_authority__federalist_millet_reading, suppression_requirement, 70, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__federalist_millet_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(marriage_authority__federalist_millet_reading, 0.12).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% This story is one of five constraints decomposing the natural-language 'personal law pluralism / Uniform Civil Code debate' per the epsilon-invariance principle. Each reading of the marriage_authority kernel has a distinct epsilon: this federalist_millet_reading authors ep~0.28 (rope-leaning, elite-bargain-as-genuine-coordination); communal_autonomy_reading is expected to author a similarly low but differently-grounded epsilon; secularist_reading and judicial_harmonization_reading are expected to author higher epsilon (treating the standing pluralist arrangement as more purely obstructive of a constitutional equality floor); gender_rights_reading is expected to author a narrower victim set and a higher epsilon centered on gender-specific harms. All five share the same kernel (who has authority over marriage law) but instantiate structurally distinct constraints, linked here via affects_constraints rather than collapsed into one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
