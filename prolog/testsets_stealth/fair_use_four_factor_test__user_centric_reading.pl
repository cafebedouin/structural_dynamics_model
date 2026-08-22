% ============================================================================
% CONSTRAINT STORY: fair_use_four_factor_test__user_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_four_factor_test__user_centric_reading, []).

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
 *   constraint_id: fair_use_four_factor_test__user_centric_reading
 *   human_readable: Fair Use Four-Factor Test — User-Centric Reading (Affirmative User Right)
 *   domain: legal/intellectual_property/cultural_production
 *
 * SUMMARY:
 *   Section 107 of the US Copyright Act permits unlicensed use of copyrighted
 *   works in defined circumstances, with four statutory factors — purpose and
 *   character of the use, nature of the work, amount taken, market effect —
 *   to be weighed together. This story instantiates the user-centric reading
 *   of that arrangement: fair use as an affirmative user right, the four
 *   factors weighed to preserve public access and cultural production. On
 *   this reading the epsilon referent is the standing fair-use regime itself,
 *   assessed by the reading's own lights: unauthorized use is largely
 *   shielded rather than sanctioned, so what extraction exists concentrates
 *   on the supply side — rights holders whose works circulate without
 *   compensation — while users, institutions, and platforms operate inside a
 *   protected liberty. Beneficiaries are the publics of teaching, criticism,
 *   preservation, remix, and platform-scale indexing; victims are the authors
 *   and catalog owners who absorb uncompensated use. Family note: the
 *   colloquial label 'fair use' decomposes into three structurally distinct
 *   readings — the creator-centric and transformative-use readings are
 *   separate constraint stories with their own epsilon, beneficiary, and
 *   victim structures, linked through network.affects_constraints. KEY AGENTS
 *   (by structural relationship): - federal_courts: Agenda-setter
 *   (institutional/analytical) — administers the four-factor weighing and
 *   defines the doctrine's boundary - educational_users: Primary beneficiary
 *   (moderate/constrained) — gains unlicensed access for teaching and
 *   research - critics_and_commentators: Beneficiary (organized/constrained)
 *   — quote and recontextualize existing works as speech -
 *   secondary_creators: Dual-positioned beneficiary/payer
 *   (moderate/constrained) — take under the doctrine and yield the same claim
 *   over their own works - libraries_archives: Beneficiary
 *   (organized/generational) — preservation and access at scales licensing
 *   cannot price - individual_authors: Payer (powerless/constrained) —
 *   uncompensated circulation of their works, no enforcement capacity -
 *   corporate_rights_holders: Payer (powerful/arbitrage) — lose licensing
 *   transactions; partially contract around the doctrine -
 *   commercial_platforms: Beneficiary (powerful/arbitrage) —
 *   business-line-scale reliance on unlicensed ingestion -
 *   licensing_intermediaries: Excluded (powerful/trapped) — would monetize
 *   mandatory licensing; outside the weighing - ip_legal_scholars: Analytical
 *   observer (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__user_centric_reading, 0.36).
domain_priors:suppression_score(fair_use_four_factor_test__user_centric_reading, 0.58).
domain_priors:theater_ratio(fair_use_four_factor_test__user_centric_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, extractiveness, 0.36).
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__user_centric_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_four_factor_test__user_centric_reading, "Fair Use Four-Factor Test — User-Centric Reading (Affirmative User Right)").
narrative_ontology:topic_domain(fair_use_four_factor_test__user_centric_reading, "legal/intellectual_property/cultural_production").

domain_priors:requires_active_enforcement(fair_use_four_factor_test__user_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__user_centric_reading, 'dc7489f4-37ee-4d4f-897e-9782f5852c4c').
narrative_ontology:cs_kernel_codification('dc7489f4-37ee-4d4f-897e-9782f5852c4c', formalized).
narrative_ontology:cs_authority_grounding('dc7489f4-37ee-4d4f-897e-9782f5852c4c', lineage).
narrative_ontology:cs_interpretation_layer_present('dc7489f4-37ee-4d4f-897e-9782f5852c4c').
narrative_ontology:cs_reading_relation('dc7489f4-37ee-4d4f-897e-9782f5852c4c', fair_use_four_factor_test__creator_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('dc7489f4-37ee-4d4f-897e-9782f5852c4c', fair_use_four_factor_test__transformative_use_reading, influences).
narrative_ontology:cs_axiom('dc7489f4-37ee-4d4f-897e-9782f5852c4c', foundational, fair_use_is_affirmative_user_right).
narrative_ontology:cs_axiom_status(fair_use_is_affirmative_user_right, holdable).
narrative_ontology:cs_axiom_grounding('dc7489f4-37ee-4d4f-897e-9782f5852c4c', fair_use_is_affirmative_user_right, deontological).
narrative_ontology:cs_axiom('dc7489f4-37ee-4d4f-897e-9782f5852c4c', foundational, public_access_preservation_is_weighing_goal).
narrative_ontology:cs_axiom_status(public_access_preservation_is_weighing_goal, holdable).
narrative_ontology:cs_axiom_grounding('dc7489f4-37ee-4d4f-897e-9782f5852c4c', public_access_preservation_is_weighing_goal, empirically_contingent).
narrative_ontology:cs_reference_frame('dc7489f4-37ee-4d4f-897e-9782f5852c4c', balanced_four_factor_public_access_weighing).
narrative_ontology:cs_drift_state('dc7489f4-37ee-4d4f-897e-9782f5852c4c', post_campbell_transformative_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('dc7489f4-37ee-4d4f-897e-9782f5852c4c', '').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__user_centric_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, educational_users).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, critics_and_commentators).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, secondary_creators).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, libraries_archives).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, commercial_platforms).
narrative_ontology:constraint_victim(fair_use_four_factor_test__user_centric_reading, individual_authors).
narrative_ontology:constraint_victim(fair_use_four_factor_test__user_centric_reading, corporate_rights_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(fair_use_four_factor_test__user_centric_reading, secondary_creators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Weigh the four statutory factors case by case, publish the precedents that define where unlicensed use ends, and administer the doctrine's boundary. They neither collect licensing revenue nor pay it; their institutional product is the line itself. Exit is not meaningful — they are the forum every dispute arrives at.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, federal_courts, agenda_setter,
    institutional, generational, analytical, national).

% Teachers, students, and researchers reproduce excerpts, images, and films in courses and scholarship without negotiating licenses. What flows to them is access they could not price or clear in advance; what they risk is a cease-and-desist letter they rarely have resources to answer. Leaving the arrangement would mean teaching only from licensed or public-domain material.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, educational_users, beneficiary,
    moderate, biographical, constrained, global).

% Reviewers, journalists, and parodists quote, clip, and recontextualize existing works as the substance of their speech. The doctrine lets them publish without clearing rights first; their alternative is negotiating permissions that arrive too late for a news cycle, or not commenting at all.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, critics_and_commentators, beneficiary,
    organized, biographical, constrained, national).

% Documentary filmmakers, remix artists, and fan creators build new works out of existing ones. They take freely under the doctrine when using others' material and give up the same claim over their own outputs when others reuse them — they stand on both sides of the line depending on which work is in play.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, secondary_creators, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(fair_use_four_factor_test__user_centric_reading, secondary_creators, payer).

% Preserve, lend, and digitize collections at scale, relying on the doctrine for preservation copies, interlibrary loan, and text-and-data-mining projects. Their budgets cannot absorb per-item licensing for the volumes they handle; their horizon spans generations of collection stewardship.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, libraries_archives, beneficiary,
    organized, generational, constrained, national).

% Writers, photographers, and independent artists whose works circulate beyond their control. They receive no payment when their work is used under the doctrine and have no practical means to litigate or to license at scale; their recourse is limited to attribution norms and occasional pro bono enforcement. Guilds and collective vehicles remain a possible but unrealized lever.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, individual_authors, payer,
    powerless, biographical, constrained, global).

% Publishers, studios, labels, and image agencies hold large catalogs and monetize them through licensing. They lose transactions to unlicensed use they cannot prevent, respond with litigation and lobbying, and partially route around the doctrine through contracts, technical protection measures, and terms of service that reclaim by agreement what the doctrine yields.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, corporate_rights_holders, payer,
    powerful, generational, arbitrage, global).

% Operate search indexes, video hosting, social feeds, and model-training pipelines whose scale depends on ingesting copyrighted works without prior clearance. The doctrine shields core operations that per-use licensing would make impossible; the value that accrues here is measured in entire business lines rather than individual transactions.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, commercial_platforms, beneficiary,
    powerful, generational, arbitrage, global).

% Collective rights organizations and stock-content marketplaces sell permissions and would benefit from a world where every use required a license. The doctrine's continued existence keeps whole categories of use outside their market; they lobby for narrower readings but hold no seat in how courts weigh the factors.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, licensing_intermediaries, excluded,
    powerful, biographical, trapped, global).

% Analyze the doctrine's history, economics, and speech implications from outside the disputes. They document how the factors are actually weighed versus how opinions describe the weighing, and their assessments feed both advocacy coalitions and judicial citations.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, ip_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_four_factor_test__user_centric_reading, commercial_platforms).
narrative_ontology:fixing_cost_class(fair_use_four_factor_test__user_centric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the impossible-licensing problem: criticism, teaching, research, parody, preservation, and search indexing require using existing works at volumes and speeds where prior licensing is impracticable or impossible; the four-factor inquiry clears those uses without negotiation.
% TRANSFER_FUNCTION: Moves uncompensated use-value of copyrighted works from rights holders to users, institutions, and platforms; moves expressive and access goods to the public without a price mechanism.
% ABSENT_VOICES: Individual authors without bargaining power — whose works circulate uncompensated and who had no seat in the doctrine's design — and licensing intermediaries, whose whole market depends on uses the doctrine removes from negotiation. Both would object to the current weighting; neither is in the room where factors are weighed.
% DISAPPEARANCE_RATIONALE: If the doctrine vanished overnight, every quotation, parody, thumbnail, course excerpt, archive scan, index entry, and training ingest would need a license: search and user-generated platforms would collapse or retreat behind licensed walled gardens, documentary and educational production would slow to clearance speed, and prices would rise across publishing and media. The cultural-production economy reorganizes around whatever licensing infrastructure could be built in the aftermath.
% FOUNDING_PROBLEM: Reconcile statutory exclusive rights with the practical impossibility of licensing criticism, abridgment, scholarship, and teaching in advance — the problem articulated in Folsom v. Marsh (1841) and carried into section 107's codification.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the Supreme Court's own speech-protective holdings (Harper & Row, Campbell) attest the doctrine's free-expression function; the Copyright Clause's stated purpose ('to promote the Progress of Science') supplies constitutional text; legal historians across the spectrum document the abridgment-and-criticism origin. Rights-holder trade groups dispute the doctrine's breadth but concede its existence and rationale — no party claims the founding problem was never real.
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__user_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__user_centric_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__user_centric_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fair_use_four_factor_test__user_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_four_factor_test__user_centric_reading, 0.36, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_four_factor_test__user_centric_reading_tests).
:- end_tests(fair_use_four_factor_test__user_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.36) is moderate-low by this reading's lights: the arrangement shields use rather than taxing it, and the extraction that exists falls on rights holders as foregone compensation rather than on users as burden. Suppression (0.58) reflects the enforcement machinery that maintains the boundary — litigation risk, takedown regimes, contract overrides — and the suppression_requirement series is authored because enforcement capacity is the dynamic this story traces: it built up steadily across the interval (statutory takedown frameworks, automated content matching, mass litigation) rather than staying static. Theater (0.46) is the sharpest drift: opinions still recite all four factors, but a growing share of outcomes turn on transformativeness alone, leaving the recited balancing partly performative — a cross-reading effect, since the factor-collapse is the sibling transformative-use reading's signature move operating inside this reading's constraint. Accessibility collapse (0.42) stays mid-range because alternatives — licensing markets, open licenses — persist, though unevenly priced and unavailable to the poorest seats. Resistance (0.68) is high because catalog owners continuously litigate and lobby against expansions. All three series run on one shared six-point grid (approximate calendar mapping 1984–2024) so no metric borrows another's endpoint.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from the same structure. From the payer seats the arrangement is uncompensated taking sustained by a forum they do not control; corporate rights holders blunt it through contractual arbitrage while individual authors have no such lever — same nominal position, different effective exposure, which is the same-level lateral dynamic this story carries. From the beneficiary seats the same structure is a protected liberty that makes criticism, teaching, preservation, and platform-scale services possible at all. The courts' seat reads it as a balancing instrument whose factors they administer; the scholars' seat observes the widening gap between the recited method and the operative one.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries map to low directionality: educational users, critics, libraries, and platforms sit near the subsidized end, with the platforms' arbitrage-grade routing (terms of service, technical measures) damping their effective exposure further. Declared victims map to high directionality: individual authors sit nearest the full-target end — uncompensated use of their work with no exit and no enforcement capacity, though coalition vehicles (guilds, collective licensing) remain a latent lever the current structure does not realize. Corporate rights holders derive nearly as high a target-directionality from their victim declaration, but their arbitrage exit pulls them back from the target pole. Federal courts carry no declared beneficiary or victim position and resolve near-symmetric through the fallback; scholars are analytical and feed no chi. No directionality overrides were needed: the beneficiary/victim declarations plus exit options produce the correct d for every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling exclusive rights with the impossibility of licensing criticism, teaching, and scholarship in advance — remains live, corroborated from outside the beneficiary set by the courts' own speech-protective holdings and by the Copyright Clause's stated purpose. Because the problem is live and the world would rearrange overnight without the arrangement, the live-status x rearrange-verdict cell is coherent: no zombie flag fires. Mandatrophy discipline matters here in both directions: the genuine coordination function (impossible-licensing clearance) blocks a pure-extraction reading, while the asymmetric, actively enforced transfer to identifiable victims blocks a pure-coordination reading. The tangled-rope structure holds both facts without letting either cover story absorb the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_decomposition,
    'This constraint is one reading of the fair_use_four_factor_test kernel — what would the sibling readings change structurally?',
    'Compare compiled classifications across the three family files: the creator-centric reading flips the victim set to users and raises epsilon sharply; the transformative-use reading shifts the operative test to a single factor and raises theater.',
    'Classification here is indexical to this reading; averaging epsilon or beneficiary structure across readings would fabricate a constraint none of the parties actually holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_decomposition, conceptual, 'Committer-frame decomposition: one kernel, three readings, three constraints.').

omega_variable(
    disagreement_location_ontology_vs_goal,
    'Where in the structure do the readings actually disagree — the ontological characterization (affirmative user right vs narrow exception to property) or the weighing goal (public access vs creator incentives)?',
    'Doctrinal analysis separating burden/default assignments from factor-weighting outcomes: if courts converge on outcomes while retaining rival characterizations, the contest is ontological only; if outcomes diverge systematically, the weighing goal is load-bearing.',
    'If the ontological axis is the live one, foreclosure pressure between this reading and the creator-centric reading strengthens; if the goal axis is live, the readings are closer substitutes than their rhetoric suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disagreement_location_ontology_vs_goal, conceptual, 'Locates the structural locus of the kernel contest.').

omega_variable(
    chilling_effect_net_activity,
    'Does the doctrine''s ex-ante uncertainty suppress more lawful user activity than the arrangement enables, once litigation risk and takedown exposure are counted?',
    'Natural experiments from jurisdictions or periods with clearer safe harbors: compare rates of quotation, documentary clearance-denial, and archival publication before and after boundary-clarifying decisions.',
    'If chilling dominates, effective suppression binds users as well as rights holders and the coordination function is smaller than the shielded volume suggests; if not, suppression is borne almost entirely by the payer side.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chilling_effect_net_activity, empirical, 'Net activity effect of doctrinal uncertainty.').

omega_variable(
    compensation_foregone_materiality,
    'How much licensing revenue do rights holders actually lose to doctrine-shielded uses, as opposed to uses no market would ever have licensed?',
    'Market-harm econometrics on litigated fair-use samples separating substitutive from non-substitutive uses; licensing-market data for categories later brought to market.',
    'If most shielded uses were never licensable, victim status is nominal opportunity cost and the arrangement sits nearer pure coordination; if substitutive losses are material, the asymmetric transfer hardens toward extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(compensation_foregone_materiality, empirical, 'Materiality of the rights-holder loss.').

omega_variable(
    platform_capture_vs_public_benefit,
    'Do the arrangement''s quantifiable gains accrue predominantly to platform operators rather than to the public and educational users this reading centers?',
    'Value-attribution studies separating consumer surplus in education and criticism from platform-side revenue attributable to shielded ingestion (indexing, hosting, training corpora).',
    'If platform capture dominates, the reading''s self-description diverges from its operation and the receipt surface hardens around a single seat; if public-side surplus is comparable, gain remains spread across beneficiary seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_capture_vs_public_benefit, empirical, 'Who captures the shielded-use value.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__user_centric_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t0, fair_use_four_factor_test__user_centric_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(fair_tr_t0, observed).
narrative_ontology:measurement(fair_tr_t8, fair_use_four_factor_test__user_centric_reading, theater_ratio, 8, 0.17).
narrative_ontology:measurement_basis(fair_tr_t8, observed).
narrative_ontology:measurement(fair_tr_t16, fair_use_four_factor_test__user_centric_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement_basis(fair_tr_t16, observed).
narrative_ontology:measurement(fair_tr_t24, fair_use_four_factor_test__user_centric_reading, theater_ratio, 24, 0.33).
narrative_ontology:measurement_basis(fair_tr_t24, observed).
narrative_ontology:measurement(fair_tr_t32, fair_use_four_factor_test__user_centric_reading, theater_ratio, 32, 0.41).
narrative_ontology:measurement_basis(fair_tr_t32, observed).
narrative_ontology:measurement(fair_tr_t40, fair_use_four_factor_test__user_centric_reading, theater_ratio, 40, 0.46).
narrative_ontology:measurement_basis(fair_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(fair_be_t0, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(fair_be_t0, observed).
narrative_ontology:measurement(fair_be_t8, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 8, 0.24).
narrative_ontology:measurement_basis(fair_be_t8, observed).
narrative_ontology:measurement(fair_be_t16, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 16, 0.28).
narrative_ontology:measurement_basis(fair_be_t16, observed).
narrative_ontology:measurement(fair_be_t24, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 24, 0.31).
narrative_ontology:measurement_basis(fair_be_t24, observed).
narrative_ontology:measurement(fair_be_t32, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 32, 0.33).
narrative_ontology:measurement_basis(fair_be_t32, observed).
narrative_ontology:measurement(fair_be_t40, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 40, 0.36).
narrative_ontology:measurement_basis(fair_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t0, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(fair_su_t0, observed).
narrative_ontology:measurement(fair_su_t8, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 8, 0.34).
narrative_ontology:measurement_basis(fair_su_t8, observed).
narrative_ontology:measurement(fair_su_t16, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 16, 0.44).
narrative_ontology:measurement_basis(fair_su_t16, observed).
narrative_ontology:measurement(fair_su_t24, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 24, 0.5).
narrative_ontology:measurement_basis(fair_su_t24, observed).
narrative_ontology:measurement(fair_su_t32, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 32, 0.54).
narrative_ontology:measurement_basis(fair_su_t32, observed).
narrative_ontology:measurement(fair_su_t40, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement_basis(fair_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__user_centric_reading, resource_allocation).
narrative_ontology:affects_constraint(fair_use_four_factor_test__user_centric_reading, fair_use_four_factor_test__creator_centric_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__user_centric_reading, fair_use_four_factor_test__transformative_use_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'fair use' decomposes into three structurally distinct readings of one kernel (fair_use_four_factor_test): creator-centric (narrow exception to property right, weighed for creator incentives), transformative-use (transformativeness dominates, market harm subordinated), and this user-centric reading (affirmative user right, weighed for public access). Each is a separate constraint story with its own epsilon, beneficiary set, and victim set; they are linked here because the readings share a statutory text and compete for the same adjudicative surface — the upstream empirical confidence of the creator-centric property framing historically lent legitimacy to narrower readings, while the transformative-use reading's rise degraded this reading's factor-balancing into partial theater (visible in the theater_ratio series).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
