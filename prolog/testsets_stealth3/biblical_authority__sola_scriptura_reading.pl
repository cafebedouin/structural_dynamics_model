% ============================================================================
% CONSTRAINT STORY: biblical_authority__sola_scriptura_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_authority__sola_scriptura_reading, []).

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
 *   constraint_id: biblical_authority__sola_scriptura_reading
 *   human_readable: Sola Scriptura: Scripture as Sole Self-Interpreting Authority
 *   domain: religious/doctrinal
 *
 * SUMMARY:
 *   The sola-scriptura arrangement holds that the canonical scriptures are
 *   the sole sufficient and self-interpreting authority for Christian
 *   doctrine and practice: no council, tradition, or magisterial office
 *   adjudicates meaning, and any competent reader may consult the final court
 *   directly. This file instantiates ONE reading of the biblical_authority
 *   kernel — the sola_scriptura_reading — as a clean, epsilon-invariant
 *   constraint; the tradition_scripture_reading and the conciliar_reading
 *   instantiate structurally different constraints in their own files (linked
 *   via network.affects_constraints) and are not averaged into this
 *   classification. The arrangement emerged from the sixteenth-century
 *   reformers' search for an authority no office could capture, hardened
 *   through confessionalized state churches, survived disestablishment by
 *   becoming voluntary, and now operates across a global field of
 *   self-governing congregations supplied by a large teaching-and-publishing
 *   market. Claim and metrics are authored independently: the claimed_type
 *   reflects my structural judgment (a hybrid that genuinely coordinates and
 *   genuinely extracts), while the metric scores describe observed operation.
 *   KEY AGENTS (by structural relationship): - lay_believers: dual-positioned
 *   principals (moderate/mobile) — declared beneficiaries who also bear the
 *   arrangement's diffuse costs - independent_congregations: organized
 *   beneficiaries (organized/constrained) — hold jurisdiction and revenue
 *   locally, absorb duplication costs - dissenting_local_members: local
 *   targets (powerless/constrained) — disciplined where their readings depart
 *   from the communal settlement - congregational_leadership: distributed
 *   agenda-setters (organized/mobile) — administer local settlements; no
 *   global adjudicative monopoly - parachurch_media_and_publishing:
 *   institutional capture seat (institutional/arbitrage) — sells the
 *   mediation the doctrine officially abolished - ecumenical_dialogue_bodies:
 *   excluded payers (institutional/constrained) — their instruments carry no
 *   weight inside the frame - tradition_based_churches: excluded
 *   institutional rivals (institutional/mobile) — holders of the sibling
 *   readings; void authority under this frame - academic_religious_studies:
 *   analytical observers (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_authority__sola_scriptura_reading, 0.52).
domain_priors:suppression_score(biblical_authority__sola_scriptura_reading, 0.48).
domain_priors:theater_ratio(biblical_authority__sola_scriptura_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(biblical_authority__sola_scriptura_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_authority__sola_scriptura_reading, tangled_rope).
narrative_ontology:human_readable(biblical_authority__sola_scriptura_reading, "Sola Scriptura: Scripture as Sole Self-Interpreting Authority").
narrative_ontology:topic_domain(biblical_authority__sola_scriptura_reading, "religious/doctrinal").

domain_priors:requires_active_enforcement(biblical_authority__sola_scriptura_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_authority__sola_scriptura_reading, '4361288e-5dde-4b9d-8805-50cfd913236b').
narrative_ontology:cs_kernel_codification('4361288e-5dde-4b9d-8805-50cfd913236b', fixed_text).
narrative_ontology:cs_authority_grounding('4361288e-5dde-4b9d-8805-50cfd913236b', distributed).
narrative_ontology:cs_reading_relation('4361288e-5dde-4b9d-8805-50cfd913236b', biblical_authority__tradition_scripture_reading, forecloses).
narrative_ontology:cs_reading_relation('4361288e-5dde-4b9d-8805-50cfd913236b', biblical_authority__conciliar_reading, forecloses).
narrative_ontology:cs_axiom('4361288e-5dde-4b9d-8805-50cfd913236b', foundational, scripture_alone_sufficient_for_faith_and_practice).
narrative_ontology:cs_axiom_status(scripture_alone_sufficient_for_faith_and_practice, holdable).
narrative_ontology:cs_axiom_grounding('4361288e-5dde-4b9d-8805-50cfd913236b', scripture_alone_sufficient_for_faith_and_practice, theological).
narrative_ontology:cs_axiom('4361288e-5dde-4b9d-8805-50cfd913236b', foundational, scriptural_perspicuity_accessible_to_lay_readers).
narrative_ontology:cs_axiom_status(scriptural_perspicuity_accessible_to_lay_readers, holdable).
narrative_ontology:cs_axiom_grounding('4361288e-5dde-4b9d-8805-50cfd913236b', scriptural_perspicuity_accessible_to_lay_readers, empirically_contingent).
narrative_ontology:cs_reference_frame('4361288e-5dde-4b9d-8805-50cfd913236b', self_interpreting_canon_baseline).
narrative_ontology:cs_drift_state('4361288e-5dde-4b9d-8805-50cfd913236b', contemporary_mediation_market_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4361288e-5dde-4b9d-8805-50cfd913236b', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(biblical_authority__sola_scriptura_reading, biblical_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, lay_believers).
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, independent_congregations).
narrative_ontology:constraint_beneficiary(biblical_authority__sola_scriptura_reading, parachurch_media_and_publishing).
narrative_ontology:constraint_victim(biblical_authority__sola_scriptura_reading, dissenting_local_members).
narrative_ontology:constraint_victim(biblical_authority__sola_scriptura_reading, ecumenical_dialogue_bodies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(biblical_authority__sola_scriptura_reading, lay_believers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Read and interpret scripture directly without a mandatory mediating office; select congregations whose teaching matches their own reading; volunteer, contribute locally, and staff congregational life. They keep the allegiance and payments once owed upward to a distant hierarchy, but absorb the labor of interpretation, constant exposure to self-appointed teachers, and — where their reading departs from their congregation's settled view — social discipline up to shunning. Leaving for another congregation is possible and common, though it costs friendships and sometimes family.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, lay_believers, beneficiary,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(biblical_authority__sola_scriptura_reading, lay_believers, payer).

% Govern themselves: call and dismiss their own ministers, hold property, settle doctrine by congregational vote or elder board, and answer to no bishop. They keep jurisdiction and resources local; they pay by duplicating schools, mission boards, and accountability structures each community must build alone, and they stand isolated when disputes exceed their own size.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, independent_congregations, beneficiary,
    organized, generational, constrained, regional).

% Hold readings that depart from their congregation's settled position — on baptism, gender roles, end-times, or worship style. They face teaching correction, loss of ministry roles, and in tighter communities shunning or expulsion; family networks and childhood formation tie their sense of self to the community, so exit means rebuilding a social world, not changing buildings. A mobile minority simply switches congregations; the embedded majority carries the cost.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, dissenting_local_members, payer,
    powerless, biographical, constrained, local).

% Pastors and elder boards preach, catechize, chair member discipline, and certify which readings count locally as 'what Scripture teaches.' Their standing and often their livelihood ride on the congregation they lead; they enforce the communal settlement while insisting they merely apply the plain text. They answer to their own flock, not to any external court — thousands of such administrations exist and none adjudicates for the others.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, congregational_leadership, agenda_setter,
    organized, biographical, mobile, regional).

% Seminaries, Bible and curriculum publishers, broadcasters, podcast networks, and conference circuits supply the study aids, translations, and celebrity exposition that occupy the space left where a teaching magisterium was removed. Revenue scales with interpretive demand; endorsements, platform algorithms, and licensing let them steer which teachers rise. They can rebrand or move audiences faster than any congregation can, and they collect directly from the interpretive demand the doctrine generates.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, parachurch_media_and_publishing, beneficiary,
    institutional, generational, arbitrage, global).

% Councils, bilateral commissions, and world-church organs pursue convergent doctrine across communions using instruments — councils, weighted consensus, shared tradition — that carry no adjudicative weight inside sola-scriptura frames. Wherever this reading governs, their findings rank as advice at best; meanwhile they absorb the cooperation costs of a fragmented field: duplicated missions, incompatible membership expectations, perpetual translation work between communions.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, ecumenical_dialogue_bodies, excluded,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(biblical_authority__sola_scriptura_reading, ecumenical_dialogue_bodies, payer).

% Catholic and Orthodox communions operate on the sibling readings: scripture read within tradition and conciliar consensus. Inside this arrangement's frame their offices hold zero adjudicative authority — they may argue, but nothing they say binds a sola-scriptura congregation. They retain complete jurisdictions, liturgies, and memberships of their own, and contest this reading polemically from fully resourced positions outside its perimeter.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, tradition_based_churches, excluded,
    institutional, civilizational, mobile, global).

% Historians and sociologists of religion trace the doctrine from late-medieval grievance through Worms, confessionalization, disestablishment, and the modern media market; they measure fragmentation, discipline, and the re-emergence of paid mediation while holding no stake in any reading's truth.
narrative_ontology:constraint_stakeholder(biblical_authority__sola_scriptura_reading, academic_religious_studies, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_authority__sola_scriptura_reading, parachurch_media_and_publishing).
narrative_ontology:fixing_cost_class(biblical_authority__sola_scriptura_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies a fixed, portable, final reference text so that geographically dispersed, self-governing congregations can claim shared doctrinal ground without a common adjudicating office; lets any believer verify teaching against its sources at near-zero institutional cost; answered the late-medieval problem of unaccountable mediation between believer and ultimate authority.
% TRANSFER_FUNCTION: Moves doctrinal adjudication out of centralized offices and into individual consciences, congregational majorities, and the teaching market; moves enforcement from hierarchical courts to local discipline; moves interpretive labor onto lay readers; and moves material support from centrally assessed clerical salaries to a competitive market of publishers, broadcasters, seminaries, and personality ministries.
% ABSENT_VOICES: Tradition-based magisteria and conciliar bodies would object that a text without living adjudication settles nothing, but inside this frame their testimony carries no standing — present in polemic, void in adjudication. Historically, the illiterate and the young depended on exactly the mediating offices the doctrine abolished; today, expelled dissenters and communities whose readings lost local votes sit outside the settlements that bind them. Their objections circulate as opinion; nothing routes them into decision.
% DISAPPEARANCE_RATIONALE: Hundreds of millions of believers and hundreds of thousands of congregations organize authority, money, discipline, and identity around this arrangement. Overnight removal would collapse the teaching-and-publishing economy built on the interpretive demand it shapes, strand congregations without a warrant structure for settling disputes, and force either mass realignment toward tradition-based and conciliar communions or improvised replacements — new courts, new confessions — recreating under other names the adjudicative layer the doctrine forbids.
% FOUNDING_PROBLEM: Late-medieval Western Christendom's mediating hierarchy had, in the reformers' eyes, become unaccountable and corrupt: indulgence traffic, plural benefices, teaching that ordinary believers could not check against sources, and councils that repeatedly failed to bind popes. The arrangement was built to install an authority no office could capture — the text itself, open to every reader.
% FOUNDING_PROBLEM_CORROBORATION: The problem's historicity is corroborated from outside the beneficiary set: pre-Reformation grievance literature (imperial Gravamina, Erasmus, Colet) and secular Reformation historiography attest the corruption crisis, and the rival communion itself conceded it — the Council of Trent's decree on indulgences expressly reformed the documented abuses. What remains contested is the remedy's continuing adequacy: tradition-based churches attest the problem was better met by reforming mediation than abolishing it, while adherents attest the problem recurs whenever mediation escapes testing against the text. No seat disputes that the founding problem existed; the seats dispute whether it is still live under this arrangement's management.
narrative_ontology:disappearance_verdict(biblical_authority__sola_scriptura_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_authority__sola_scriptura_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_authority__sola_scriptura_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(biblical_authority__sola_scriptura_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_authority__sola_scriptura_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_authority__sola_scriptura_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_authority__sola_scriptura_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_authority__sola_scriptura_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Scores describe observed operation, not aspiration. Extractiveness sits mid-range (0.52 at interval end): no center collects compulsory rents, yet the arrangement transfers real goods — interpretive labor onto lay readers, enforcement costs into local discipline, and, decisively, a market position to the teaching-and-publishing sector that re-supplies mediation at retail prices after the wholesale magisterial version was abolished. Suppression (0.48) is structural first: congregational discipline, shunning, and platform gatekeeping; a substantial internalized component (dissent experienced as rebellion against God, formed by conscience-shaping from childhood) is carried separately in the suppression-mechanism omega rather than folded silently into the scalar. Theater (0.42): the ideal of every-believer-unmediated-exegesis persists rhetorically while practice runs through credentialed clergy, study-Bible apparatus, and personality brands — performance and function coexist. Accessibility_collapse (0.62) splits the difference the doctrine itself creates: institutional alternatives (councils, magisterium) collapse almost completely inside the frame, while interpretive alternatives proliferate — fragmentation is the visible residue of incompletely collapsed alternatives. Resistance (0.58) is continuous: rival communions' polemics, ecumenical correction, and internal confessionalism that quietly re-imports tradition while calling it application. Coordination is typed information_standard: the load-bearing coordination artifact is a fixed, portable, shared reference text (with the canon defining it); identity-boundary work is real but secondary, since the arrangement survives relabeling of memberships far better than it would survive loss of the common standard — the dominance test for choosing the type. The temporal series run on one shared grid (years 0–500 of the arrangement; every tracked metric authored at every point). Suppression_requirement is non-monotonic BY DESIGN and tracks real enforcement-capacity history: confessional-era state coercion peaked around years 80–120 (consistorial discipline, established churches), collapsed through disestablishment into the voluntary era (trough near year 300), then partially rebuilt as reputational and platform discipline in the media age. Rising base_extractiveness models accumulation: each generation layered new paid mediation onto an anti-mediation foundation.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from identical structural data. From the lay-believer seat the arrangement is emancipation with overhead: direct textual access, exit into a crowded denominational marketplace, but permanent interpretive responsibility. From the dissenting-member seat the same structure operates as enforced local settlement: the congregation that promised unmediated scripture delivers a mediated line with disciplinary teeth. From the congregational-leadership seat it is stewardship — administering what the plain text plainly says. From the parachurch seat it is addressable market. From the ecumenical and tradition-based seats it is the manufacture of incoherence. Nothing in the authored data privileges one seat; the engine derives per-seat classifications from power, exit, and declared position. Identity fusion concentrates in the dissenting seat's closed-community subset: relational identity (family and congregation constituting the self) makes exit unthinkable even where physically available; if that identity frame broke, those members would behave like the mobile mainstream, and the dissenting seat's directionality would soften markedly.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real subsidies: lay_believers receive unmediated access and exit abundance; independent_congregations keep jurisdiction and revenue local; parachurch_media_and_publishing receives the demand pool the removed teaching office used to monopolize. Victim declarations map to real burdens: dissenting_local_members absorb discipline; ecumenical_dialogue_bodies absorb fragmentation's cooperation costs. Suppression enters the engine as a raw structural property, unscaled by power or scope; only extractiveness is scaled, by each agent's derived directionality and the arrangement's continental-to-global scope (verifying a 'plain reading' claim grows harder as the field widens, mildly amplifying effective extraction). One override is authored: the moderate power atom is pinned to d=0.45 because its sole holder, lay_believers, is declared a beneficiary yet is structurally dual-positioned — derivation from the beneficiaries array alone would seat them deep in subsidized territory (roughly d=0.15–0.2), undershooting the diffuse costs they carry as the arrangement's principal cost-bearers. Remaining seats derive cleanly: declared victims high-d, institutional beneficiaries low-d, the distributed agenda-setters and the analytical observer at canonical positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two symmetrical mislabels. Read as pure coordination ('freedom from corrupt mediation'), the arrangement's extraction disappears — the capture seat, the discipline costs, and the coherence losses vanish from the ledger. Read as pure predation ('fragmentation machine'), its genuine emancipation disappears — the late-medieval abuses were real and documented, the verification power placed in ordinary hands was real, and the exit abundance has no parallel in the magisterial frame. The tangled-rope reading holds both: a working coordination core (fixed portable final text; congregational self-government) with an extraction layer accreting on top of it (re-professionalized mediation; localized enforcement). Genealogically the founding problem is CONTESTED, not dead: the anti-corruption mandate was partially achieved and then partially recurs in transmuted form — the mediation it abolished returned as a market less accountable than the office it replaced — while defenders attest the original function live in every generation's duty to retest inherited teaching against the text. Because the founding problem is contested rather than dead, no mandatrophy resolution is declared. The receipt surface sharpens rather than settles the question: a named capture seat (parachurch_media_and_publishing) combined with prohibitive fixing cost is consistent with continued drift toward harder extraction if the coordination core erodes — a trajectory the interpretive_vacuum_recapture omega tracks. Fixing is prohibitive because restoration of adjudicative unity would require either reunion with the sibling readings' communions (five centuries of accumulated divergence, institutional investment, and identity) or inventing a new adjudicating body, which would contradict the reading itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This story instantiates one reading (sola_scriptura_reading) of the biblical_authority kernel; what structural facts change under the sibling readings?',
    'Compile the sibling stories (biblical_authority__tradition_scripture_reading, biblical_authority__conciliar_reading) and compare victim sets, epsilon, and seat divergence across the family. The disagreement is located in one structural element: whether interpretive authority requires an institution outside the text.',
    'Under the tradition reading, clerical extraction rises and lay autonomy falls (beneficiary/victim inversion relative to this file); under the conciliar reading, adjudicative monopoly partially returns and doctrinal coherence is preserved at the cost of conciliar enforcement. Cross-family comparison, not this file alone, establishes which victim structure the kernel produces.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one reading of a three-reading kernel; sibling readings are separate constraints with separate epsilon.').

omega_variable(
    perspicuity_empirical_status,
    'Is scripture actually self-interpreting — do readers holding the text alone converge on stable doctrine — or does all reading remain mediated in fact?',
    'Comparative convergence studies across communities sharing textual commitments but lacking shared institutions: persistent divergence under identical textual inputs falsifies operative perspicuity.',
    'If self-interpretation fails empirically, the doctrine operates as a laundering device — re-imported mediation priced as liberation — measured extractiveness understates the true transfer, and reclassification pressure toward the snare side increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(perspicuity_empirical_status, empirical, 'Empirical status of the perspicuity premise underlying the whole arrangement.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression structural (community discipline, platform gatekeeping, social cost of exit) or internalized (conscience formed from childhood so that dissent is experienced as sin)?',
    'Post-exit trajectory of dissenters: if felt obligation and fear persist after leaving the community, the internalized component is substantial; survey and counseling-outcome data separate the mechanisms.',
    'Internalized suppression travels with the agent after exit, raising effective suppression above the structural 0.48 and pushing the dissenting_local_members seat toward the full-target end of directionality; consequences concentrate in that seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Mechanism split behind the scalar suppression value.').

omega_variable(
    interpretive_vacuum_recapture,
    'Does an authority vacuum under a no-adjudicator doctrine necessarily refill with informal hierarchies (personality brands, platform algorithms, publishing gatekeepers), converting anti-clericalism into less accountable clericalism?',
    'Longitudinal concentration analysis of interpretive influence (share of teaching attention held by the top decile of teachers and platforms) across the interval tail.',
    'If refilling is structurally necessary, the coordination core degrades toward extraction-cover and the arrangement drifts snare-ward; if refilling is contingent (addressable via transparency and platform pluralism), the tangled-rope reading holds indefinitely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interpretive_vacuum_recapture, empirical, 'Whether the vacuum recapture visible in the capture seat is contingent or necessary.').

omega_variable(
    coherence_as_victim_status,
    'Is cross-communal doctrinal coherence a good whose loss counts as harm done by this arrangement, or a preference some readings legitimately decline?',
    'Not resolvable by data alone; turns on whether unity of doctrine is intrinsic to the kernel''s purpose (as the conciliar and tradition readings assert) or one value tradeable against autonomy (as this reading asserts).',
    'Counting coherence as a genuine good raises the victim count and effective extraction; treating it as a declined preference removes the largest diffuse cost from the ledger and pulls the classification rope-ward.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coherence_as_victim_status, preference, 'Preference-class ambiguity in whether fragmentation counts as harm.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_authority__sola_scriptura_reading, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sola_scriptura_reading_tr_t0, biblical_authority__sola_scriptura_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(sola_scriptura_reading_tr_t100, biblical_authority__sola_scriptura_reading, theater_ratio, 100, 0.18).
narrative_ontology:measurement(sola_scriptura_reading_tr_t200, biblical_authority__sola_scriptura_reading, theater_ratio, 200, 0.24).
narrative_ontology:measurement(sola_scriptura_reading_tr_t300, biblical_authority__sola_scriptura_reading, theater_ratio, 300, 0.29).
narrative_ontology:measurement(sola_scriptura_reading_tr_t400, biblical_authority__sola_scriptura_reading, theater_ratio, 400, 0.36).
narrative_ontology:measurement(sola_scriptura_reading_tr_t500, biblical_authority__sola_scriptura_reading, theater_ratio, 500, 0.42).

% Extraction over time
narrative_ontology:measurement(sola_scriptura_reading_be_t0, biblical_authority__sola_scriptura_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(sola_scriptura_reading_be_t100, biblical_authority__sola_scriptura_reading, base_extractiveness, 100, 0.4).
narrative_ontology:measurement(sola_scriptura_reading_be_t200, biblical_authority__sola_scriptura_reading, base_extractiveness, 200, 0.44).
narrative_ontology:measurement(sola_scriptura_reading_be_t300, biblical_authority__sola_scriptura_reading, base_extractiveness, 300, 0.46).
narrative_ontology:measurement(sola_scriptura_reading_be_t400, biblical_authority__sola_scriptura_reading, base_extractiveness, 400, 0.49).
narrative_ontology:measurement(sola_scriptura_reading_be_t500, biblical_authority__sola_scriptura_reading, base_extractiveness, 500, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(sola_scriptura_reading_su_t0, biblical_authority__sola_scriptura_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(sola_scriptura_reading_su_t100, biblical_authority__sola_scriptura_reading, suppression_requirement, 100, 0.61).
narrative_ontology:measurement(sola_scriptura_reading_su_t200, biblical_authority__sola_scriptura_reading, suppression_requirement, 200, 0.52).
narrative_ontology:measurement(sola_scriptura_reading_su_t300, biblical_authority__sola_scriptura_reading, suppression_requirement, 300, 0.41).
narrative_ontology:measurement(sola_scriptura_reading_su_t400, biblical_authority__sola_scriptura_reading, suppression_requirement, 400, 0.45).
narrative_ontology:measurement(sola_scriptura_reading_su_t500, biblical_authority__sola_scriptura_reading, suppression_requirement, 500, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_authority__sola_scriptura_reading, information_standard).
narrative_ontology:affects_constraint(biblical_authority__sola_scriptura_reading, biblical_authority__tradition_scripture_reading).
narrative_ontology:affects_constraint(biblical_authority__sola_scriptura_reading, biblical_authority__conciliar_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'biblical authority' per the epsilon-invariance principle: the label covers three structurally distinct arrangements — magisterial mediation (tradition_scripture_reading), conciliar consensus (conciliar_reading), and solo textual sufficiency (this file). Each carries its own epsilon, beneficiary/victim structure, and classification; forcing one story to span all three would make epsilon observable-dependent, which the framework forbids. This file is downstream of neither sibling chronologically (both readings predate it as claims), but the sibling arrangements function as the corrective evidence critics cite against this one, so contamination edges are declared bidirectionally through the family link.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biblical_authority__sola_scriptura_reading, moderate, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
