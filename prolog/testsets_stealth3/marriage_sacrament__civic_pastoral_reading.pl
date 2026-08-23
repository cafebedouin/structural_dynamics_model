% ============================================================================
% CONSTRAINT STORY: marriage_sacrament__civic_pastoral_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_sacrament__civic_pastoral_reading, []).

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
 *   constraint_id: marriage_sacrament__civic_pastoral_reading
 *   human_readable: Marriage Indissolubility as Pastoral Ideal with Discerned Exceptions (Civic-Pastoral Reading)
 *   domain: religious doctrine / canon law / political sociology
 *
 * SUMMARY:
 *   Since the annulment liberalizations of the 1980s-1990s and decisively
 *   since the 2016 post-synodal exhortation and the regional guidelines that
 *   followed it, a growing portion of the Catholic institutional apparatus
 *   treats marital indissolubility as an ideal whose failures are handled by
 *   case-by-case pastoral discernment rather than by uniform juridical
 *   denial. The arrangement performs a real coordination function: it retains
 *   members in irregular situations whom the uniform discipline was quietly
 *   losing, and it individualizes care for wounded marriages. It also carries
 *   a definite cost structure: traditional laity lose the normative clarity
 *   their identity runs on, enforcement is deliberately uneven across
 *   dioceses and conferences, and admission decisions come to depend on which
 *   confessor a penitent happens to meet. KEY AGENTS (by structural
 *   relationship): see key_agents. The claimed type (tangled_rope) is stated
 *   from the structure — a genuine coordination function joined to asymmetric
 *   extraction under active enforcement — and the metrics are authored
 *   independently as descriptive facts; the engine computes per-seat
 *   classifications from the structural data, and any divergence between
 *   claim and computed type is signal, not error.
 *
 * KEY AGENTS:
 *   - diocesan_pastoral_leadership: agenda-setter and principal receipt seat (institutional/constrained) — issues discernment guidelines, manages dissent from both wings, collects member retention and discretionary legitimation
 *   - divorced_remarried_catholics: primary beneficiary (moderate/constrained) — gain discerned access to sacramental life, dependent on confessor and diocese
 *   - traditional_lay_catholics: primary target (organized/identity_locked) — bear doctrinal relativization and loss of normative clarity; objections received but unanswered
 *   - parish_confessors: secondary beneficiary and local operator (moderate/constrained) — exercise delegated discernment discretion while absorbing the ambiguity it generates
 *   - traditionalist_dubia_cardinals: organized internal opposition (organized/constrained) — formal written questions submitted, corrections published, no direct answer received
 *   - lapsed_returning_catholics: incidental beneficiaries (powerless/mobile) — re-engage under the negotiable-admission regime
 *   - sister_churches_alternate_disciplines: excluded external reference class (institutional/arbitrage) — run economy-based disciplines outside the conversation
 *   - academic_religious_sociologists: analytical observer (analytical/analytical) — measure attrition, annulment volume, and implementation variance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_sacrament__civic_pastoral_reading, 0.53).
domain_priors:suppression_score(marriage_sacrament__civic_pastoral_reading, 0.49).
domain_priors:theater_ratio(marriage_sacrament__civic_pastoral_reading, 0.29).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, extractiveness, 0.53).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, suppression_requirement, 0.49).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, theater_ratio, 0.29).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_sacrament__civic_pastoral_reading, tangled_rope).
narrative_ontology:human_readable(marriage_sacrament__civic_pastoral_reading, "Marriage Indissolubility as Pastoral Ideal with Discerned Exceptions (Civic-Pastoral Reading)").
narrative_ontology:topic_domain(marriage_sacrament__civic_pastoral_reading, "religious doctrine / canon law / political sociology").

domain_priors:requires_active_enforcement(marriage_sacrament__civic_pastoral_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_sacrament__civic_pastoral_reading, '3009eb18-d25d-4c85-8608-0282c1e45c80').
narrative_ontology:cs_kernel_codification('3009eb18-d25d-4c85-8608-0282c1e45c80', fixed_text).
narrative_ontology:cs_authority_grounding('3009eb18-d25d-4c85-8608-0282c1e45c80', lineage).
narrative_ontology:cs_interpretation_layer_present('3009eb18-d25d-4c85-8608-0282c1e45c80').
narrative_ontology:cs_reading_relation('3009eb18-d25d-4c85-8608-0282c1e45c80', marriage_sacrament__hierarchical_indissolubility_reading, forecloses).
narrative_ontology:cs_axiom('3009eb18-d25d-4c85-8608-0282c1e45c80', foundational, indissolubility_ideal_not_constitutive).
narrative_ontology:cs_axiom_status(indissolubility_ideal_not_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('3009eb18-d25d-4c85-8608-0282c1e45c80', indissolubility_ideal_not_constitutive, instrumental).
narrative_ontology:cs_axiom('3009eb18-d25d-4c85-8608-0282c1e45c80', secondary, discerned_conscience_overrides_uniform_rule).
narrative_ontology:cs_axiom_status(discerned_conscience_overrides_uniform_rule, holdable).
narrative_ontology:cs_axiom_grounding('3009eb18-d25d-4c85-8608-0282c1e45c80', discerned_conscience_overrides_uniform_rule, deontological).
narrative_ontology:cs_reference_frame('3009eb18-d25d-4c85-8608-0282c1e45c80', pastoral_discernment_regime).
narrative_ontology:cs_drift_state('3009eb18-d25d-4c85-8608-0282c1e45c80', post_amoris_implementation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3009eb18-d25d-4c85-8608-0282c1e45c80', '').
narrative_ontology:cs_kernel_id(marriage_sacrament__civic_pastoral_reading, marriage_sacrament).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_sacrament__civic_pastoral_reading, divorced_remarried_catholics).
narrative_ontology:constraint_beneficiary(marriage_sacrament__civic_pastoral_reading, parish_confessors).
narrative_ontology:constraint_beneficiary(marriage_sacrament__civic_pastoral_reading, diocesan_pastoral_leadership).
narrative_ontology:constraint_victim(marriage_sacrament__civic_pastoral_reading, traditional_lay_catholics).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_sacrament__civic_pastoral_reading, lapsed_returning_catholics).
narrative_ontology:constraint_victim(marriage_sacrament__civic_pastoral_reading, traditionalist_dubia_cardinals).
narrative_ontology:constraint_vindicates(marriage_sacrament__civic_pastoral_reading, gradualism_in_moral_formation).
narrative_ontology:constraint_vindicates(marriage_sacrament__civic_pastoral_reading, internal_forum_primacy).
narrative_ontology:constraint_vindicates(marriage_sacrament__civic_pastoral_reading, situated_conscience_discernment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issue diocesan and conference-level guidelines deciding which irregular marital situations may be accompanied toward sacramental participation, appoint and instruct confessors, and respond to objections arriving from both the traditionalist and the progressive wings. They cannot step outside the institutional framework without forfeiting the office through which they act, and their planning horizon spans generations of the faithful. Member retention, avoidance of open institutional rupture, and legitimation of discretionary judgment accrue to their offices.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, diocesan_pastoral_leadership, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(marriage_sacrament__civic_pastoral_reading, diocesan_pastoral_leadership, beneficiary).

% Civilly remarried after a prior marriage ended, they were long barred from communion under the uniform discipline. Under the discernment arrangement many are accompanied back toward sacramental participation, with access depending heavily on which diocese and which confessor they encounter. Leaving the church entirely remains possible but forfeits the sacramental belonging they still seek; staying means accepting dependence on a confessor's judgment about their own conscience and history.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, divorced_remarried_catholics, beneficiary,
    moderate, biographical, constrained, global).

% Attend parishes where catechesis still teaches that a valid marriage cannot dissolve while practiced policy admits discerned exceptions alongside them. Their self-understanding runs through doctrinal fidelity, so the coexistence of taught rule and practiced exception registers for them as the dissolution of the norm itself rather than as mercy extended to others. They organize publications, petitions, formal questions, and pilgrimages; their objections are received but unanswered. Relocating to a traditional-rite community preserves liturgical forms without restoring the doctrinal clarity they say is at stake, and leaving the church altogether would sever an identity they cannot imagine laying down.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, traditional_lay_catholics, payer,
    organized, generational, identity_locked, global).

% Heard in the internal forum, they decide case by case whether an irregular situation may continue toward the sacraments, guided by conference guidelines that vary in strictness from region to region. The arrangement makes their personal judgment consequential and returns pastoral centrality to their office; it also hands them the ambiguity — inconsistent guidance from above, visibly unequal outcomes for similar cases, and the knowledge that a penitent may receive a different answer from the next confessor.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, parish_confessors, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(marriage_sacrament__civic_pastoral_reading, parish_confessors, agenda_setter).

% Senior churchmen who submitted formal written questions asking whether the post-2016 practice confirms or alters the church's teaching on marriage, and who published public corrections when no direct answer came. They operate inside the very hierarchy whose course they dispute, bound by office and consecration; their available channels are letters, signatories, and publications rather than jurisdiction, and continued service inside the institution is the price of keeping the question alive.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, traditionalist_dubia_cardinals, payer,
    organized, biographical, constrained, global).

% Baptized members alienated years ago by the marriage discipline — often divorced themselves or children of divorce — who re-engage with parish life now that admission is negotiated case by case. Their attachment is thin and reversible, and their own history demonstrates that quiet exit from the community is an ordinary, available act.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, lapsed_returning_catholics, beneficiary,
    powerless, immediate, mobile, global).

% Orthodox and some Protestant bodies that never adopted the uniform indissolubility discipline, handling marital failure instead through economy, penitential readmission, or recognition of civil remarriage. They are the standing demonstration that the underlying problem admits other solutions, yet they hold no seat in the Catholic conversation; their existence shapes the option space of everyone inside it.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, sister_churches_alternate_disciplines, excluded,
    institutional, generational, arbitrage, global).

% Track annulment volumes, communion-access variance across dioceses, attrition among divorced and traditionalist constituencies, and the authority effects of formally submitted questions that go unanswered. They publish outside the confessional framework and bear none of its costs.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, academic_religious_sociologists, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_sacrament__civic_pastoral_reading, diocesan_pastoral_leadership).
narrative_ontology:fixing_cost_class(marriage_sacrament__civic_pastoral_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the boundary-maintenance problem of a sacramental membership community whose founding rule (marital indissolubility) collides with widespread marital failure: it keeps members in irregular situations inside sacramental life by delegating admission decisions to case-by-case pastoral discernment, distributing the adjudication load across confessors and dioceses instead of resolving it once, centrally, in canon law.
% TRANSFER_FUNCTION: Moves normative certainty out of the general lay body (traditional laity lose a single clear rule) and converts it into distributed discretionary judgment held by clergy; moves sacramental access toward divorced-and-remarried Catholics previously excluded; moves deference and case-handling attention toward the discernment apparatus; and shifts institutional risk over discipline away from the center and onto local implementation.
% ABSENT_VOICES: Abandoned spouses whose marriages are discerned as dissolved have no seat at the table. Divorced Catholics excluded under the prior discipline who died or permanently left carry no testimony into the present conversation. Traditionalist objectors are nominally present — their formal questions were submitted in writing — but the questions receive no answer, so their presence yields no exchange. Sister churches running economy-based disciplines stand outside the conversation entirely.
% DISAPPEARANCE_RATIONALE: If the discernment arrangement vanished overnight, divorced-and-remarried Catholics would be re-excluded from communion, confessors would be stranded without a protocol for the cases now routed to them, dioceses would face a choice between uniform juridical denial and open defiance of it, and traditional laity would regain doctrinal clarity at the price of renewed attrition among the marginal. The membership economy of the institution visibly reorganizes around whichever discipline replaces it.
% FOUNDING_PROBLEM: The collision between absolute indissolubility discipline and the reality of widespread marital failure: divorced-and-remarried Catholics were barred from communion under a uniform rule, producing quiet attrition, clandestine sacramental arrangements, and sustained accusation of pastoral cruelty toward the wounded.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: independent survey research (Pew and academic sociology) documents divorced-Catholic disengagement predating the reform; diocesan tribunal archives and pre-reform pastoral literature record the exclusion caseload; Orthodox churches' long-standing economy-based practice attests that the problem is real and solvable by other means; and traditionalist theologians — opponents of the discernment solution — nonetheless attest the reality of the underlying problem. No corroborating source belongs to the beneficiary set.
narrative_ontology:disappearance_verdict(marriage_sacrament__civic_pastoral_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_sacrament__civic_pastoral_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_sacrament__civic_pastoral_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_sacrament__civic_pastoral_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_sacrament__civic_pastoral_reading, 0.53, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_sacrament__civic_pastoral_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_sacrament__civic_pastoral_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_sacrament__civic_pastoral_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.53 at interval end) because the arrangement delivers real mercy to a formerly excluded class while imposing a real cost on another: traditional laity lose the single clear rule their doctrinal identity presupposes, and admission becomes confessor-dependent. Suppression (0.49) is structural rather than coercive: the arrangement persists by managing dissent (unanswered formal questions, marginalization of correcting prelates, uneven guideline adoption treated as legitimate variety) rather than by punishing anyone. Theater ratio (0.29) is low-to-moderate: accompaniment is substantively performed in most adopting dioceses, but a growing share of activity is procedural performance — guidelines, seminars, synodal language — layered over decisions confessors were effectively already making. Accessibility collapse (0.38) is partial: traditional-rite communities, Orthodoxy, Protestantism, and quiet secular exit all remain reachable, but each carries an identity cost that keeps most affected members in place. Resistance (0.58) is substantial and organized: formal dubia, published corrections, conference-level refusal of guidelines, and a durable traditionalist media ecosystem. The temporal series run on one shared eight-point grid (1980-2026) so every tracked metric is authored at every examined time point; the trajectory is a ratchet rather than a cycle — synodal episodes (2014-2016) produced step increases that the grid smooths — and the suppression_requirement series is authored deliberately because the story traces enforcement-capacity build-up (guideline issuance, dissent management), not merely shifting extraction. The 2026 points are marked projected; all earlier points are observed.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the traditional-lay seat — an identity-locked target with organized but unanswered objection channels — the arrangement presents as enforced incoherence: the same institution teaches the rule and suspends it, and there is no exit that preserves the identity. From the divorced-and-remarried seat the same structure presents as recovered belonging, close to a pure coordination good. From the diocesan-leadership seat it presents as a legitimate development patiently implemented against ingratitude from both wings. From the confessor seat it presents as restored vocation entangled with unresolvable ambiguity. The engine computes these divergences from the structural data — power, exit, and declared position — not from the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Traditional_lay_catholics sit at the full-target end: declared victims, identity-locked exit amplifies their exposure toward the maximum, and their organized power buys voice but not remedy. Divorced_remarried_catholics sit near the beneficiary end but not at zero — they gain access yet accept dependence on confessional judgment, a residual cost. Parish_confessors are genuinely mixed: they collect discretionary authority (benefit) while absorbing the ambiguity burden (cost), placing them mid-range. Diocesan_pastoral_leadership sit low: they administer the arrangement and retention accrues to their offices, though they bear governance strain. Lapsed_returning_catholics sit nearest zero — thin attachment, demonstrated mobility, nearly pure subsidy. Traditionalist_dubia_cardinals sit high despite organized power: they bear reputational and jurisdictional cost inside the structure they cannot leave. Sister_churches sit near zero by arbitrage — the arrangement does not reach them, and their alternate disciplines insulate them. No directionality_overrides are declared: the derivation chain from beneficiary/victim declarations plus exit options already produces these relationships, and the override mechanism keys on power_atom alone, which would flatten the three genuinely distinct moderate-power seats (divorced_remarried, confessors, lapsed_returning) into one value.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — exclusion of the divorced-and-remarried from sacramental life — is still live, so the arrangement has not outlived its mandate and mandatrophy is not resolved. The tangled_rope classification is what prevents mislabeling in both directions: reading the arrangement as pure extraction (snare) would erase the documented mercy actually delivered to a formerly excluded class; reading it as pure coordination (rope) would erase the traditional-lay victim class whose normative clarity is consumed by the same structure that dispenses the mercy. The drift risk to monitor is piton-ward: if a future magisterial clarification settles the doctrinal question in either direction, the discernment apparatus could survive as theatrical ritual — guidelines maintained, seminars held, decisions pre-ordained — with theater_ratio climbing past 0.5 while the underlying adjudication becomes performance. The theater series is authored to catch exactly that transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_resolution,
    'This constraint is one reading of the kernel marriage_sacrament (civic_pastoral_reading). Does this reading hold as the operative instantiation, or does the sibling hierarchical_indissolubility_reading reassert itself as the authoritative account?',
    'A definitive magisterial doctrinal note resolving the status of indissolubility — constitutive versus ideal-with-discerned-exceptions — would collapse one reading into the other and terminate the co-contestation.',
    'Resolution collapses this constraint into the sibling''s structure or confirms it: the victim set, enforcement form, and epsilon would be recomputed wholesale under whichever reading prevails.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_resolution, conceptual, 'Which reading of the marriage_sacrament kernel is operative.').

omega_variable(
    enforcement_variance_authority_erosion,
    'Does diocese-to-diocese and conference-to-conference variance in admitting the divorced-and-remarried to communion reflect legitimate subsidiarity, or is it erosion of magisterial authority through inconsistent enforcement?',
    'Cross-diocesan audit of guideline adherence and actual communion-access decisions for structurally equivalent cases, controlling for local pastoral staffing.',
    'If erosion, the arrangement''s coordination function degrades into discretionary patchwork and effective extraction rises for every seat; if subsidiarity, the variance is ordinary coordination overhead and the moderate extraction estimate stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_variance_authority_erosion, empirical, 'Whether inconsistent enforcement is subsidiarity or authority decay.').

omega_variable(
    traditionalist_cost_attribution,
    'Is the traditional laity''s loss of normative clarity a genuine extraction cost borne by a victim class, or a self-chosen interpretive stance that resists a legitimate doctrinal development?',
    'Distinguish measured outcomes (attrition, movement to traditional-rite jurisdictions, reported distress surveys) from rhetorical positioning; compare trajectories against comparable constituencies that received stable doctrine over the same period.',
    'If the cost is genuine extraction, the victim declaration stands and the tangled_rope classification holds; if it is self-inflicted rigidity, the extraction estimate falls toward rope territory and the victim set thins.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(traditionalist_cost_attribution, conceptual, 'Whether traditionalist doctrinal-relativization costs are borne or chosen.').

omega_variable(
    discernment_discretion_distribution,
    'Does case-by-case discernment distribute judgment equitably across confessors, or does it concentrate arbitrary discretion such that identical cases receive opposite answers from different priests?',
    'Structured comparison of discernment outcomes for equivalent presented cases across confessors and dioceses, using standardized case vignettes where direct records are unavailable.',
    'Concentrated discretion functions as gatekeeping rent and raises both suppression and effective extraction; equitable distribution supports the coordination half of the classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discernment_discretion_distribution, empirical, 'Whether discernment discretion is fairly distributed or concentrated.').

omega_variable(
    abandoned_spouse_cost_transfer,
    'Does the discernment regime transfer costs onto abandoned spouses whose marriages are discerned as dissolved — removing the protection that the constitutive account of the bond afforded them?',
    'Longitudinal tracking of abandoned-spouse outcomes in cases where the discernment path concludes that the prior bond has failed.',
    'If the transfer is real, the victim set widens beyond traditional laity and epsilon rises; if not, the current single-group victim declaration stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(abandoned_spouse_cost_transfer, empirical, 'Whether discerned dissolution shifts costs onto abandoned spouses.').

omega_variable(
    suppression_composition_internalized,
    'Is traditionalist acquiescence under the new regime driven by structural marginalization (unanswered questions, blocked channels) or by internalized obedience formation that makes protest itself feel unfaithful?',
    'Post-exit trajectory of traditionalists who relocate to traditional-rite communities outside the mainstream structure: if dissent capacity recovers sharply after exit, the suppression was substantially structural; if it persists, a large internalized component is present.',
    'An internalized component means effective suppression exceeds the structural measure — the targets carry the constraint with them after exit — and the identity-lock dynamics strengthen.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_composition_internalized, empirical, 'Structural versus internalized composition of traditionalist suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_sacrament__civic_pastoral_reading, 1980, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1980, marriage_sacrament__civic_pastoral_reading, theater_ratio, 1980, 0.06).
narrative_ontology:measurement_basis(marr_tr_t1980, observed).
narrative_ontology:measurement(marr_tr_t1990, marriage_sacrament__civic_pastoral_reading, theater_ratio, 1990, 0.09).
narrative_ontology:measurement_basis(marr_tr_t1990, observed).
narrative_ontology:measurement(marr_tr_t1998, marriage_sacrament__civic_pastoral_reading, theater_ratio, 1998, 0.12).
narrative_ontology:measurement_basis(marr_tr_t1998, observed).
narrative_ontology:measurement(marr_tr_t2005, marriage_sacrament__civic_pastoral_reading, theater_ratio, 2005, 0.15).
narrative_ontology:measurement_basis(marr_tr_t2005, observed).
narrative_ontology:measurement(marr_tr_t2014, marriage_sacrament__civic_pastoral_reading, theater_ratio, 2014, 0.18).
narrative_ontology:measurement_basis(marr_tr_t2014, observed).
narrative_ontology:measurement(marr_tr_t2016, marriage_sacrament__civic_pastoral_reading, theater_ratio, 2016, 0.22).
narrative_ontology:measurement_basis(marr_tr_t2016, observed).
narrative_ontology:measurement(marr_tr_t2021, marriage_sacrament__civic_pastoral_reading, theater_ratio, 2021, 0.26).
narrative_ontology:measurement_basis(marr_tr_t2021, observed).
narrative_ontology:measurement(marr_tr_t2026, marriage_sacrament__civic_pastoral_reading, theater_ratio, 2026, 0.29).
narrative_ontology:measurement_basis(marr_tr_t2026, projected).

% Extraction over time
narrative_ontology:measurement(marr_be_t1980, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 1980, 0.12).
narrative_ontology:measurement_basis(marr_be_t1980, observed).
narrative_ontology:measurement(marr_be_t1990, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 1990, 0.2).
narrative_ontology:measurement_basis(marr_be_t1990, observed).
narrative_ontology:measurement(marr_be_t1998, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 1998, 0.27).
narrative_ontology:measurement_basis(marr_be_t1998, observed).
narrative_ontology:measurement(marr_be_t2005, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 2005, 0.33).
narrative_ontology:measurement_basis(marr_be_t2005, observed).
narrative_ontology:measurement(marr_be_t2014, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 2014, 0.42).
narrative_ontology:measurement_basis(marr_be_t2014, observed).
narrative_ontology:measurement(marr_be_t2016, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 2016, 0.48).
narrative_ontology:measurement_basis(marr_be_t2016, observed).
narrative_ontology:measurement(marr_be_t2021, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 2021, 0.51).
narrative_ontology:measurement_basis(marr_be_t2021, observed).
narrative_ontology:measurement(marr_be_t2026, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 2026, 0.53).
narrative_ontology:measurement_basis(marr_be_t2026, projected).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1980, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 1980, 0.08).
narrative_ontology:measurement_basis(marr_su_t1980, observed).
narrative_ontology:measurement(marr_su_t1990, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 1990, 0.14).
narrative_ontology:measurement_basis(marr_su_t1990, observed).
narrative_ontology:measurement(marr_su_t1998, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 1998, 0.2).
narrative_ontology:measurement_basis(marr_su_t1998, observed).
narrative_ontology:measurement(marr_su_t2005, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 2005, 0.26).
narrative_ontology:measurement_basis(marr_su_t2005, observed).
narrative_ontology:measurement(marr_su_t2014, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 2014, 0.34).
narrative_ontology:measurement_basis(marr_su_t2014, observed).
narrative_ontology:measurement(marr_su_t2016, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 2016, 0.44).
narrative_ontology:measurement_basis(marr_su_t2016, observed).
narrative_ontology:measurement(marr_su_t2021, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 2021, 0.47).
narrative_ontology:measurement_basis(marr_su_t2021, observed).
narrative_ontology:measurement(marr_su_t2026, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 2026, 0.49).
narrative_ontology:measurement_basis(marr_su_t2026, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_sacrament__civic_pastoral_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_sacrament__civic_pastoral_reading, marriage_sacrament__hierarchical_indissolubility_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Catholic teaching on marriage and divorce' conflates two structurally distinct constraints arising from one kernel (marriage_sacrament). This story instantiates the civic_pastoral_reading: indissolubility as ideal, discernment adjudicating failures, victim set = traditional laity losing normative clarity, enforcement = managed inconsistency. The sibling story (marriage_sacrament__hierarchical_indissolubility_reading) instantiates the hierarchical reading: indissolubility constitutive, hierarchical-juridical adjudication, victim set = divorced-and-remarried excluded from communion, enforcement = uniform denial. The epsilon values differ because the referent arrangements differ: this file authors epsilon for the pastoral-discernment regime as this reading assesses it; the sibling authors epsilon for the uniform-enforcement regime as that reading assesses it. Each reading's operation supplies the other's justification narrative — the pastoral reading cites the uniform regime's attrition costs, the hierarchical reading cites the pastoral regime's doctrinal relativization — which is why the family edge runs between them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
