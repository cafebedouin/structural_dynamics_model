% ============================================================================
% CONSTRAINT STORY: constitutional_secularism__strict_neutrality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_secularism__strict_neutrality_reading, []).

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
 *   constraint_id: constitutional_secularism__strict_neutrality_reading
 *   human_readable: Strict Neutrality Secularism (Equal-Distance Non-Interference)
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   A constitutional order binds the state to equidistance from all
 *   religions: no establishment, no preferential funding or recognition, and
 *   symmetrically no interference in internal religious affairs. The
 *   arrangement is administered chiefly by courts, which police state
 *   preference on one side and refuse cognizance to grievances arising inside
 *   religious communities on the other. Its coordination core is real: it
 *   removes religion-state alignment from electoral competition, guarantees
 *   uniform rules across communities, and shields minority practice from
 *   majority preference. Its extraction is equally real and runs through the
 *   same wall: the non-interference principle leaves the weakest members of
 *   religious communities without state recourse, and facially neutral rules
 *   written from majority vantage points burden minorities while passing as
 *   even-handed. The claim/metric gap is deliberate: the arrangement is
 *   CLAIMED here as tangled_rope (genuine coordination carrying asymmetric
 *   extraction) while the metrics are authored from the arrangement's
 *   observed operation; the engine computes per-seat classifications from the
 *   structural data. Per the epsilon-referent rule, extractiveness is
 *   authored for the standing equidistant arrangement as this reading's own
 *   lights assess it — the reading accepts the arrangement as broadly
 *   legitimate while acknowledging the non-interference cost it imposes.
 *
 * KEY AGENTS:
 *   - - constitutional_courts: Agenda setter (institutional/analytical) — administers the equidistance rule, strikes state preference, declines intra-community claims
 *   - - minority_religious_communities: Primary beneficiary (organized/constrained) — protected from establishment and preference
 *   - - secular_unaffiliated_citizens: Secondary beneficiary (organized/mobile) — protected from religious governance at lowest cost
 *   - - majority_religious_institutions: Payer (powerful/constrained) — lost formal preference, retains informal cultural weight
 *   - - religious_minorities_under_neutral_rules: Payer (moderate/constrained) — burdened by facially neutral majority-normed rules
 *   - - intra_community_dissenters: Primary victim (powerless/trapped) — women under religious personal law, children, apostates; claims rendered non-cognizable
 *   - - principled_intervention_advocates: Excluded voice (organized/constrained) — reform movements arguing for state duty to act inside communities
 *   - - constitutional_scholars: Analytical observer (analytical/analytical) — maps the arrangement's comparative performance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_secularism__strict_neutrality_reading, 0.47).
domain_priors:suppression_score(constitutional_secularism__strict_neutrality_reading, 0.38).
domain_priors:theater_ratio(constitutional_secularism__strict_neutrality_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, extractiveness, 0.47).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__strict_neutrality_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_secularism__strict_neutrality_reading, "Strict Neutrality Secularism (Equal-Distance Non-Interference)").
narrative_ontology:topic_domain(constitutional_secularism__strict_neutrality_reading, "constitutional/political").

domain_priors:requires_active_enforcement(constitutional_secularism__strict_neutrality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__strict_neutrality_reading, '99328873-fb32-42a7-a801-cbbba81f95a6').
narrative_ontology:cs_kernel_codification('99328873-fb32-42a7-a801-cbbba81f95a6', fixed_text).
narrative_ontology:cs_authority_grounding('99328873-fb32-42a7-a801-cbbba81f95a6', lineage).
narrative_ontology:cs_interpretation_layer_present('99328873-fb32-42a7-a801-cbbba81f95a6').
narrative_ontology:cs_reading_relation('99328873-fb32-42a7-a801-cbbba81f95a6', constitutional_secularism__principled_intervention_reading, forecloses).
narrative_ontology:cs_reading_relation('99328873-fb32-42a7-a801-cbbba81f95a6', constitutional_secularism__reformist_reading, forecloses).
narrative_ontology:cs_axiom('99328873-fb32-42a7-a801-cbbba81f95a6', foundational, state_noninterference_in_religion_absolute).
narrative_ontology:cs_axiom_status(state_noninterference_in_religion_absolute, holdable).
narrative_ontology:cs_axiom_grounding('99328873-fb32-42a7-a801-cbbba81f95a6', state_noninterference_in_religion_absolute, deontological).
narrative_ontology:cs_axiom('99328873-fb32-42a7-a801-cbbba81f95a6', secondary, formal_equality_suffices_for_religious_liberty).
narrative_ontology:cs_axiom_status(formal_equality_suffices_for_religious_liberty, holdable).
narrative_ontology:cs_axiom_grounding('99328873-fb32-42a7-a801-cbbba81f95a6', formal_equality_suffices_for_religious_liberty, empirically_contingent).
narrative_ontology:cs_reference_frame('99328873-fb32-42a7-a801-cbbba81f95a6', strict_equidistance_noninterference).
narrative_ontology:cs_drift_state('99328873-fb32-42a7-a801-cbbba81f95a6', contemporary_accommodation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('99328873-fb32-42a7-a801-cbbba81f95a6', '').
narrative_ontology:cs_kernel_id(constitutional_secularism__strict_neutrality_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__strict_neutrality_reading, minority_religious_communities).
narrative_ontology:constraint_beneficiary(constitutional_secularism__strict_neutrality_reading, secular_unaffiliated_citizens).
narrative_ontology:constraint_victim(constitutional_secularism__strict_neutrality_reading, intra_community_dissenters).
narrative_ontology:constraint_victim(constitutional_secularism__strict_neutrality_reading, religious_minorities_under_neutral_rules).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(constitutional_secularism__strict_neutrality_reading, majority_religious_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicates disputes over state action touching religion: strikes down funding preferences and endorsements on one side, and on the other declines to hear grievances arising inside religious communities, treating internal doctrine and personal-law matters as beyond reach. Defines, case by case, where distance ends and hostility begins. Its own position is secured by the arrangement it administers.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, constitutional_courts, agenda_setter,
    institutional, generational, analytical, national).

% Worship, educate, and maintain institutions without state establishment of a rival faith and without preferential funding flowing to competitors. The arrangement protects their public existence against majoritarian political capture. Leaving the jurisdiction is possible but severs community and family; remaining means relying on courts whose sympathy varies by era.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, minority_religious_communities, beneficiary,
    organized, generational, constrained, national).

% Live under civic rules not anchored in any religious tradition and are spared obligations toward faiths they do not hold. They collect the arrangement's protection at the lowest personal cost of any seat, bearing little of what it withholds from others.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, secular_unaffiliated_citizens, beneficiary,
    organized, biographical, mobile, national).

% Lost formal establishment privileges, preferential funding, and official recognition that earlier orders provided. They continue to shape public culture informally through numbers, holidays, and social default, and they retain strong political channels. The arrangement costs them access to state power while the same non-interference rule shields their internal governance from regulation.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, majority_religious_institutions, payer,
    powerful, generational, constrained, national).

% Bear the weight of facially neutral rules written around majority practice: dress and safety codes, zoning decisions, calendar defaults, and ritual regulations that fit the majority's way of life and fit theirs poorly. Accommodation requests meet a demanding standard, and denial rarely counts as targeting because the rule applies to everyone on paper.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, religious_minorities_under_neutral_rules, payer,
    moderate, biographical, constrained, national).

% Women governed by religious personal law, children in community schools, and members who reject community doctrine. The state's refusal to enter religious affairs leaves their grievances without a forum: no court will hear what the community imposes internally. Exiting means losing family, livelihood, and belonging at once, so most endure. Their claims are not defeated in argument; they are never admitted into the conversation.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, intra_community_dissenters, payer,
    powerless, immediate, trapped, local).

% Reform movements, affected-community organizers, and scholars who argue the state has a duty to act inside religious communities to protect the vulnerable. The arrangement's framing places their entire claim outside what may be asked of the state, so they lobby at the margins and litigate losses while the doctrinal door stays shut.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, principled_intervention_advocates, excluded,
    organized, generational, constrained, national).

% Compare equidistant regimes against establishment, accommodationist, and interventionist systems across jurisdictions; document where formal neutrality tracks substantive fairness and where it diverges. They bear nothing and collect nothing; their output is the comparative record other seats argue with.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_secularism__strict_neutrality_reading, diffuse).
narrative_ontology:fixing_cost_class(constitutional_secularism__strict_neutrality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Removes religion-state alignment from political competition: prevents sectarian capture of state power, guarantees one uniform rule set across religious communities, protects minority practice from majority preference, and gives all religious institutions a stable sphere of internal autonomy.
% TRANSFER_FUNCTION: Moves legal capacity and protection: transfers immunity from state interference to religious institutions as a class; transfers enforcement priority away from grievances arising inside communities toward disputes between communities and the state; transfers former establishment privileges away from majority institutions. Net flow: preference-capacity from majorities and protection from intra-community dissenters, toward inter-communal peace and institutional autonomy.
% ABSENT_VOICES: Intra-community dissenters are the paradigmatic absent voice: the arrangement's design makes their claims non-cognizable, so they are absent not by oversight but by construction. Principled-intervention advocates are likewise outside the conversation the arrangement permits itself to have. Both would object that equidistance between communities is purchased with abandonment inside them.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, establishment contests would reignite immediately: majority movements would pursue formal recognition and funding, minority communities would lose their principal shield and organize defensively around politics rather than courts, personal-law reform battles would open in every legislature, and the courts' docket would shift from policing preference to allocating it. The religious-political settlement of the polity would rebuild itself along entirely different lines.
% FOUNDING_PROBLEM: How to govern a religiously plural polity without an official religion becoming an instrument of majority domination and without inter-sectarian struggle over state power — the historical record of establishment persecution and religious civil war that the arrangement's drafters sought to close permanently.
% FOUNDING_PROBLEM_CORROBORATION: Historians of religious conflict and comparative constitutional scholars — seated outside every benefiting party — attest both the reality of the founding problem and its continuing life, citing recurring establishment movements and sectarian mobilization across jurisdictions. International human-rights monitors document ongoing discrimination patterns that keep the problem current. Minority communities also attest the problem is live, but as beneficiaries their testimony is corroborative rather than independent; the independent attestation comes from the scholarly and monitoring record.
narrative_ontology:disappearance_verdict(constitutional_secularism__strict_neutrality_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_secularism__strict_neutrality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_secularism__strict_neutrality_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_secularism__strict_neutrality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_secularism__strict_neutrality_reading, 0.47, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_secularism__strict_neutrality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_secularism__strict_neutrality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_secularism__strict_neutrality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.47: the arrangement's operative effect splits between genuine coordination (uniform rules, minority shielding) and a systematic transfer — preference-capacity taken from majority institutions and protection withheld from intra-community dissenters — with the transfer neither dominant nor negligible. Suppression 0.38: enforcement is judicial and litigative rather than coercive, but for the dissenter seat the refusal of cognizance functions as enforced exposure with no affordable exit. Theater ratio 0.30: a persistent performative layer (ceremonial acknowledgments, holiday calendars, oath forms) coexists with formal equidistance while majority cultural practice continues informally. Accessibility collapse 0.25: alternatives remain fully live — sibling readings, accommodationist models, and establishment systems operate in comparable jurisdictions — so understanding this arrangement collapses nothing. Resistance 0.45: continuous accommodationist litigation, reform-movement pressure, and scholarly critique contest the arrangement's boundaries without threatening its core. The temporal series share one seven-point grid; the suppression_requirement hump (rise to 0.50 at midpoint, partial decay to 0.38) traces real enforcement history — doctrinal build-up through the mid-interval incorporation-and-strict-scrutiny era, then the accommodation turn — while base_extractiveness creeps upward throughout as formal neutrality hardens against accommodation claims and the non-interference cost compounds. Receipt surface: gain_flow is authored as 'diffuse' after checking every seat — the peace dividend accrues systemically and the autonomy shield is held jointly by multiple institutional seats, so no single named seat captures the proceeds; fixing_cost is 'prohibitive' because removal requires constitutional-level revision and would reopen precisely the sectarian alignment contests the arrangement was built to close.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the constitutional_courts seat the arrangement is a coherent, self-administering rule that resolves disputes by category; from the minority_religious_communities seat it is a shield; from the intra_community_dissenters seat the same wall that shields others is the thing that leaves them exposed — the identical structure presents as protection, burden, or abandonment depending on which side of which boundary a seat occupies. Majority institutions experience loss of formal privilege while retaining informal dominance, a position the raw payer label understates. The engine derives these divergent classifications from power, exit, and declared position; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive minority_religious_communities and secular_unaffiliated_citizens toward the subsidized end (low d), with the unaffiliated nearer zero-cost than the minorities, whose protection is real but conditional. Victim declarations drive intra_community_dissenters toward the full-target end (high d), amplified by their trapped exit — they cannot leave the community without losing family, livelihood, and belonging. religious_minorities_under_neutral_rules sit near-target with somewhat better exit than dissenters. majority_religious_institutions derive a high d from their payer declaration, moderated by the autonomy shield they collect from the same non-interference rule; the derivation captures the net position imperfectly, which is documented rather than overridden. constitutional_courts sit near-symmetric as administrators who neither fund nor bear the arrangement. No directionality overrides were needed: the beneficiary/victim declarations plus exit options produce the correct ordering.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — governing religiously plural polities without state religion becoming an instrument of majority domination — remains live, so no mandatrophy is declared and the R5 interview returns status=live against verdict=world_rearranges, producing no zombie flag. The classification work here is preventive: defenders of the arrangement present it as pure coordination (a rope of mutual restraint), and critics present it as pure extraction (a machine for entrenching majority norms behind a neutrality mask). Both descriptions are half-right, and the tangled_rope classification forces both into the record: the coordination function is genuine and would be missed by a snare verdict, while the asymmetric transfer through the non-interference wall would be laundered by a rope verdict. The piton alternative fails on the evidence — the arrangement's function has not atrophied, its enforcement is active, and its beneficiaries actively defend it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint instantiates the strict_neutrality_reading of the constitutional_secularism kernel; how would the sibling readings (principled_intervention_reading, reformist_reading) restructure the assessment of the same standing arrangement?',
    'Author the sibling stories separately and compare: adoption of a sibling reading converts the non-interference wall into a managed-intervention regime in which intra_community_dissenters become cognizable claimants, religious-institutional autonomy contracts, and the identical facts re-author with materially higher epsilon from the reformist seat.',
    'Classification of this arrangement is reading-relative: a reformist assessor authors substantially higher extraction for the same referent. Cross-reading comparisons must run story-to-story through network edges; averaging epsilon across readings would fabricate a constraint none of the parties holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: this story is one of three readings of the secularism kernel; sibling readings change the victim set and the extraction profile.').

omega_variable(
    formal_vs_substantive_neutrality,
    'Does facially neutral uniform application actually treat communities equally, or do majority-normed defaults embedded in ''neutral'' rules constitute covert preference?',
    'Burden-distribution analysis of facially neutral rules (dress codes, zoning, calendars, ritual-slaughter and ceremonial regulations) across communities, plus natural experiments from jurisdictions that shifted between formal and substantive neutrality standards.',
    'If covert preference is established, effective extraction on the religious_minorities_under_neutral_rules seat rises sharply and the arrangement trends toward snare for that seat; if formal neutrality tracks substantive neutrality, the coordination component dominates and the rope side of the hybrid strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formal_vs_substantive_neutrality, empirical, 'Whether the reading''s formal-equality premise survives contact with differential burden data.').

omega_variable(
    noninterference_harm_attribution,
    'Are the harms borne by intra_community_dissenters caused by the neutrality arrangement itself, or by the underlying religious practices the arrangement merely declines to touch?',
    'Cross-regime comparison of dissenter outcomes under interventionist versus equidistant arrangements, holding the underlying practices constant; legislative records of refused intervention petitions.',
    'If attributed to the arrangement, epsilon includes the full non-interference cost and the dissenter seat computes near full-target; if attributed to the practices, those harms belong to a separate personal-law constraint story and this story''s epsilon drops accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(noninterference_harm_attribution, conceptual, 'Attribution of the non-interference cost: the arrangement''s own extraction versus the price of tolerating practices it does not create.').

omega_variable(
    accommodation_turn_trajectory,
    'Will the late-interval relaxation of enforcement intensity (the accommodation turn visible in the suppression_requirement series) continue, stabilize, or reverse?',
    'Track apex-court appointment composition and doctrine, plus legislative accommodation statutes, over the coming decade.',
    'Continued relaxation pushes suppression_requirement lower while potentially raising extraction through carve-outs that favor majority practice; reversal restores the mid-interval enforcement peak and re-hardens uniform application.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(accommodation_turn_trajectory, empirical, 'Direction of the enforcement trajectory after the mid-interval peak.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_secularism__strict_neutrality_reading, 0, 72).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(strict_neutrality_tr_t0, constitutional_secularism__strict_neutrality_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(strict_neutrality_tr_t12, constitutional_secularism__strict_neutrality_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(strict_neutrality_tr_t24, constitutional_secularism__strict_neutrality_reading, theater_ratio, 24, 0.26).
narrative_ontology:measurement(strict_neutrality_tr_t36, constitutional_secularism__strict_neutrality_reading, theater_ratio, 36, 0.27).
narrative_ontology:measurement(strict_neutrality_tr_t48, constitutional_secularism__strict_neutrality_reading, theater_ratio, 48, 0.28).
narrative_ontology:measurement(strict_neutrality_tr_t60, constitutional_secularism__strict_neutrality_reading, theater_ratio, 60, 0.29).
narrative_ontology:measurement(strict_neutrality_tr_t72, constitutional_secularism__strict_neutrality_reading, theater_ratio, 72, 0.3).

% Extraction over time
narrative_ontology:measurement(strict_neutrality_be_t0, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 0, 0.36).
narrative_ontology:measurement(strict_neutrality_be_t12, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 12, 0.39).
narrative_ontology:measurement(strict_neutrality_be_t24, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 24, 0.41).
narrative_ontology:measurement(strict_neutrality_be_t36, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 36, 0.43).
narrative_ontology:measurement(strict_neutrality_be_t48, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 48, 0.45).
narrative_ontology:measurement(strict_neutrality_be_t60, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 60, 0.46).
narrative_ontology:measurement(strict_neutrality_be_t72, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 72, 0.47).

% Suppression requirement over time
narrative_ontology:measurement(strict_neutrality_su_t0, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(strict_neutrality_su_t12, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 12, 0.34).
narrative_ontology:measurement(strict_neutrality_su_t24, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 24, 0.42).
narrative_ontology:measurement(strict_neutrality_su_t36, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 36, 0.5).
narrative_ontology:measurement(strict_neutrality_su_t48, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 48, 0.48).
narrative_ontology:measurement(strict_neutrality_su_t60, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 60, 0.43).
narrative_ontology:measurement(strict_neutrality_su_t72, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 72, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_secularism__strict_neutrality_reading, identity_coordination).
narrative_ontology:affects_constraint(constitutional_secularism__strict_neutrality_reading, constitutional_secularism__principled_intervention_reading).
narrative_ontology:affects_constraint(constitutional_secularism__strict_neutrality_reading, constitutional_secularism__reformist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'constitutional secularism' decomposes into three structurally distinct readings of one kernel. This story (strict_neutrality_reading) authors epsilon for the equidistant arrangement as its own lights assess it; the sibling stories author epsilon for the SAME standing arrangement from intervention-permitting and reform-mandating seats, yielding higher values and different victim sets. The upstream/downstream structure runs from this reading outward: strict neutrality is the baseline against which both siblings define themselves as departures, and each sibling story links back here. Cross-reading comparisons must traverse the network edges; no single file may hedge epsilon across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
