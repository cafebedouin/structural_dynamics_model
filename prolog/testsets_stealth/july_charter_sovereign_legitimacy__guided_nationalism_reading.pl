% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__guided_nationalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: July Charter — Guided Nationalism Reading (Religious Identity as Sovereign Legitimacy Ground)
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   A post-revolutionary constituent assembly drafted a charter to answer the
 *   question the revolution left open: what makes governmental authority
 *   legitimate? This file instantiates exactly one reading of that charter —
 *   the guided-nationalism reading, under which the operative charter fixes
 *   the nation's religious identity as the ground of sovereign legitimacy,
 *   elevates certified religious norms to constitutional checkpoints over
 *   legislation, and correspondingly narrows the standing of secular
 *   institutions and of citizens outside the majority faith. Per the
 *   epsilon-invariance discipline, the secular-democratic and
 *   military-custodian readings are separate constraints in separate files
 *   with their own epsilon values, beneficiary sets, and victim sets; they
 *   appear here only through network edges and reading_relations. Epsilon in
 *   this file refers to the standing arrangement — the charter as it operates
 *   under this reading — assessed by this reading's own lights, never to the
 *   arrangement a sibling reading would install. KEY AGENTS (by structural
 *   relationship): islamist_constituent_coalition — agenda-setter
 *   (institutional/arbitrage), authored the framework and collects office and
 *   agenda control; official_religious_establishment — primary beneficiary
 *   (institutional/arbitrage), holds interpretive authority converted into
 *   constitutional checkpoints; pious_citizen_majority — net beneficiary
 *   (organized/constrained), receives the identity-affirming legal order,
 *   bears diffuse enforcement costs; secular_civil_society — primary target
 *   (moderate/constrained), bears association and expression constraints;
 *   religious_minorities — target (powerless/constrained), unequal standing
 *   in office and personal-status law; heterodox_muslim_sects — target
 *   (powerless/identity_locked), dissent possible only inside the category
 *   that binds them; military_establishment — constrained institutional actor
 *   (powerful/constrained), custodial claim displaced, force monopoly
 *   retained; constitutional_conformity_court — enforcement administrator
 *   (institutional/constrained); international_rights_monitors — excluded
 *   critic (powerful/mobile); comparative_constitutional_scholars —
 *   analytical observer (analytical/analytical).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.66).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.75).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__guided_nationalism_reading, tangled_rope).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__guided_nationalism_reading, "July Charter — Guided Nationalism Reading (Religious Identity as Sovereign Legitimacy Ground)").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__guided_nationalism_reading, "constitutional/political").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__guided_nationalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__guided_nationalism_reading, 'c83e04eb-a353-40f3-b7ec-15251a0bf8c2').
narrative_ontology:cs_kernel_codification('c83e04eb-a353-40f3-b7ec-15251a0bf8c2', fixed_text).
narrative_ontology:cs_authority_grounding('c83e04eb-a353-40f3-b7ec-15251a0bf8c2', lineage).
narrative_ontology:cs_interpretation_layer_present('c83e04eb-a353-40f3-b7ec-15251a0bf8c2').
narrative_ontology:cs_reading_relation('c83e04eb-a353-40f3-b7ec-15251a0bf8c2', july_charter_sovereign_legitimacy__secular_democratic_reading, forecloses).
narrative_ontology:cs_reading_relation('c83e04eb-a353-40f3-b7ec-15251a0bf8c2', july_charter_sovereign_legitimacy__military_custodian_reading, influences).
narrative_ontology:cs_axiom('c83e04eb-a353-40f3-b7ec-15251a0bf8c2', foundational, religious_identity_grounds_sovereignty).
narrative_ontology:cs_axiom_status(religious_identity_grounds_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('c83e04eb-a353-40f3-b7ec-15251a0bf8c2', religious_identity_grounds_sovereignty, theological).
narrative_ontology:cs_axiom('c83e04eb-a353-40f3-b7ec-15251a0bf8c2', secondary, certified_religious_norms_bind_legislation).
narrative_ontology:cs_axiom_status(certified_religious_norms_bind_legislation, holdable).
narrative_ontology:cs_axiom_grounding('c83e04eb-a353-40f3-b7ec-15251a0bf8c2', certified_religious_norms_bind_legislation, conventional).
narrative_ontology:cs_reference_frame('c83e04eb-a353-40f3-b7ec-15251a0bf8c2', foundational_religious_national_sovereignty).
narrative_ontology:cs_drift_state('c83e04eb-a353-40f3-b7ec-15251a0bf8c2', contemporary_post_transition_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c83e04eb-a353-40f3-b7ec-15251a0bf8c2', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__guided_nationalism_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, islamist_constituent_coalition).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, official_religious_establishment).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, pious_citizen_majority).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, secular_civil_society).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_minorities).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, heterodox_muslim_sects).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, pious_citizen_majority).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, military_establishment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Dominated the constituent assembly that drafted the charter and now holds legislative majorities under it. Wrote the articles fixing the nation's religious identity as the source of governmental authority and staffs the offices those articles create. Amendment requires supermajorities it controls. Its members can leave office and re-enter civil life without losing standing under the rules they wrote.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, islamist_constituent_coalition, agenda_setter,
    institutional, generational, arbitrage, national).

% The state-recognized clerical body whose scholars certify whether legislation conforms to religious norms. The charter converts its opinions into constitutional checkpoints: bills touching family law, education, and public morality pass through its review. It gains jurisdiction, budget lines, and veto-shaped influence it did not hold before ratification, and its scholars circulate through universities and courts regardless of which faction governs.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, official_religious_establishment, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__guided_nationalism_reading, official_religious_establishment, agenda_setter).

% Devout citizens for whom the charter affirms the moral order they already live: public holidays, schooling, and family law track their commitments. They also fund the enforcement apparatus through taxation and live with the narrowing of pluralism it produces, but few experience that cost directly.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, pious_citizen_majority, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__guided_nationalism_reading, pious_citizen_majority, payer).

% Associations, unions, publishers, and research centers organized around non-religious civic purposes. Their registration, funding, and public assembly run through discretionary approvals keyed to the charter's identity clauses; several organizations have been dissolved or had assets frozen. Individual members can emigrate, but the institutional web they built cannot move.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, secular_civil_society, payer,
    moderate, biographical, constrained, national).

% Non-Muslim communities with long histories in the country. The charter's identity clauses place them outside the definition of the political community that sovereignty expresses: certain senior offices are reserved, personal-status matters route through confessional courts they did not choose, and building or repairing places of worship requires permits that stall. Communal leadership can negotiate accommodations case by case but cannot alter the underlying articles.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_minorities, payer,
    powerless, biographical, constrained, national).

% Minority traditions within the majority religion whose doctrines differ from the state-certified interpretation. Because the charter defines the political community in terms of the majority's faith, these sects cannot reject the framework without renouncing membership in the national religious community itself; their worship, publications, and endowments operate under supervision, and their leaders face prosecution when doctrine strays into public view.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, heterodox_muslim_sects, payer,
    powerless, generational, identity_locked, national).

% The armed forces, whose claim to guard the state independently of electoral politics predates the charter. The framework locates guardianship of the national order in its religious identity rather than in the officer corps, demoting the military's self-description to servant of an order it does not define. It retains budget autonomy, internal courts, and the monopoly of force, and its commanders alternate between enforcing the framework during crises and negotiating exemptions from it.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, military_establishment, payer,
    powerful, generational, constrained, national).

% The high court that hears challenges under the charter and certifies legislation against its identity clauses. Its docket, its appointment pipeline, and the standard it applies are all defined by the framework it administers; individual justices who dissent publicly have been passed over for renewal.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, constitutional_conformity_court, agenda_setter,
    institutional, biographical, constrained, national).

% Treaty bodies, foreign ministries, and nongovernmental organizations that review the country's compliance with international obligations. They publish findings on minority treatment and association freedoms and condition some aid and cooperation on reforms, but they have no standing inside the domestic constitutional conversation; their recommendations enter only through the government's discretion.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, international_rights_monitors, excluded,
    powerful, biographical, mobile, global).

% Academic specialists in transitional constitutions who track the gap between the charter's text and administrative practice across successive governments. They publish analyses that neither coalition controls and that travel through journals and archives rather than through the domestic enforcement field.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(july_charter_sovereign_legitimacy__guided_nationalism_reading, islamist_constituent_coalition).
narrative_ontology:fixing_cost_class(july_charter_sovereign_legitimacy__guided_nationalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the post-revolutionary constituent-power problem: after the old order collapsed, the charter fixes a single legitimacy ground — the nation's religious identity — so that offices, laws, and courts draw authority from one uncontested source, and it coordinates majority expectations about the moral character of legislation.
% TRANSFER_FUNCTION: Moves political standing and institutional access from secular civil society and religious minorities toward the religious-nationalist coalition and the official clergy; moves interpretive authority over public life to the certified religious establishment; moves enforcement and prosecutorial attention toward policing conformity with the identity clauses.
% ABSENT_VOICES: Banned secular parties, heterodox sect leadership, minority women's organizations, and diaspora dissidents were absent from drafting and are absent from the conformity-review conversation; international human-rights monitors are heard only externally, through the government's discretion. Unanimity in the founding conversation arose partly because these seats were never in the room.
% DISAPPEARANCE_RATIONALE: Offices, courts, school curricula, personal-status jurisdiction, and the clerical certification pipeline all hang off the legitimacy ground the charter fixes. Overnight removal would force a refounding: every institution claiming authority through religious identity would need a new source of legitimacy, and the excluded seats would immediately re-enter the conversation the articles currently close.
% FOUNDING_PROBLEM: The post-revolutionary authority vacuum: after the old regime fell, no agreed source of legitimate authority existed, and competing grounds — electoral majorities, street mobilization, military guardianship, religious mandate — contended openly. The charter was built to fix sovereignty in the nation's enduring religious identity to stabilize the transition.
% FOUNDING_PROBLEM_CORROBORATION: Comparative constitutional scholarship and opposition-party legal testimony — sources outside the benefiting parties — attest the founding vacuum was real but substantially resolved within the first decade; minority community councils attest the same from their seat. The coalition and the clerical establishment attest the vacuum remains live, citing sectarian fragmentation and coup risk. Corroboration of the problem's reality is solid; its continuing liveness is disputed along the beneficiary/victim line.
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__guided_nationalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__guided_nationalism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__guided_nationalism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__guided_nationalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is substantial (0.66 at interval end) because the identity clauses convert demographic majority into differential legal standing: office eligibility, personal-status jurisdiction, and associational permission all flow through religious identity, and the costs land on minorities and secular civil society while the benefits concentrate in the ruling coalition and clerical establishment. Suppression (0.75) reflects the enforcement machinery the framework requires — conformity review, registration discretion, contempt and blasphemy prosecutions — not participant preference; the framework could not hold by acquiescence alone because two of its principal constituencies would exit if exit were free. Theater (0.44) is rising: symbolic piety performances, commemorative jurisprudence, and ritualized conformity certifications increasingly substitute for the framework's original consolidating function. Accessibility collapse is moderate (0.50): public institutional alternatives have collapsed, but discursive alternatives remain arguable — which is precisely why the charter remains a contested kernel with live sibling readings. Resistance (0.58) is real and cyclical. The measurement series run on one shared grid and show a full oscillatory cycle: consolidation hardening (t3–t6), an accommodation opening under economic and diplomatic pressure (t9), re-hardening after a security incident (t12–t15), partial relaxation (t18), renewed consolidation (t21–t24). The oscillation is itself functional for the framework: each tightening phase teaches constituencies that visible contestation triggers reprisal, so self-censorship deepens even in relaxed phases — intermittent reinforcement operating as an extraction mechanism rather than noise. Base_properties report the end-state (t24) values, late in the cycle's consolidation phase. The claimed type (tangled_rope) is authored from structure — a genuine coordination function joined to asymmetric extraction — independently of these metric values; the engine computes per-seat types from the structural data, and any divergence between claim and computed type is the measurement the corpus exists to take.
 *
 * PERSPECTIVAL GAP:
 *   From the coalition's seat the charter is the founding coordination that ended the authority vacuum: it solved, once, the question every post-revolutionary order must answer, and the costs it imposes are the price of a stable moral order the majority endorses. From the minority and secular seats the identical articles operate as a standing exclusion machine: the clause that stabilizes legitimacy for the coalition removes their standing within it. The military experiences a third structure — not extraction from its resources but displacement of its legitimating self-description, with material prerogatives intact. The conformity court experiences the framework as neutral administration, a standard to apply, because its docket and appointments presuppose the framework it polices. Same-level divergence is visible among nominally equal citizens: pious majority members and secular professionals hold the same nominal standing, but the identity clauses differentiate their exit options — the majority's identity is affirmed by the framework, the secular professional's is taxed by it. These divergences are computed per seat from the structural data; nothing in the authored claim adjudicates among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries derive low directionality: the coalition and clerical establishment sit near the beneficiary end (the framework subsidizes them directly), and the pious majority sits beneficiary-side with a slight pull toward symmetric from its diffuse tax-borne costs. Declared victims derive high directionality: secular civil society (constrained exit — individuals can emigrate, institutions cannot) and religious minorities (case-by-case bargaining only) sit near the full-target end; heterodox sects sit nearest it because identity-lock removes even the exit that emigration or conversion offers others — their relationship to the framework is constitutive, not chosen. One override is authored: for the powerful power-atom, d = 0.55. The derivation would read the military's victim declaration as near-full-target, but the charter leaves its budget autonomy, internal jurisdiction, and force monopoly intact — it is displaced symbolically while shielded materially, so its true position is mid-scale. Among powerful actors only the military sits inside the domestic enforcement field; the excluded international monitors stand outside the arrangement the derivation describes, which is what their exclusion consists in. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and spatial scope, in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite errors. Reading the charter as pure coordination would launder the exclusion machinery behind the real founding function the framework performed; reading it as pure extraction would erase the genuine constituent-power problem it solved — the authority vacuum was corroborated by actors outside the benefiting parties, and the framework did resolve it for a generation. The tangled-rope structure holds both facts: coordination function (legitimacy consolidation, majority-expectation alignment) and asymmetric extraction (standing transferred from minorities and secularists to the coalition and clergy) through the same articles, held in place by active enforcement. On the genealogy interview: the founding problem is attested as real by comparative scholarship and opposition legal testimony from outside the beneficiary set, but its status is contested — the coalition and clergy attest it remains live, while opposition and minority seats attest it has closed and the identity clauses now function as incumbent entrenchment. Disappearance verdict is world_rearranges: offices, courts, curricula, and personal-status jurisdiction all hang off the legitimacy ground. The mismatch consumer watches the (status x verdict) pair: contested x world_rearranges raises no zombie flag yet, but if status flips to dead while the arrangement persists on theatrical maintenance, the drift path runs toward inertial persistence — the rising theater series (0.22 to 0.44) is the leading indicator to watch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Which reading of the founding articles is the charter''s operative meaning — guided nationalism, secular democracy, or military custodianship?',
    'Track authoritative interpretation practice across successive court benches and amendment politics: which reading''s consequences (victim sets, institutional arrangements) the enforcement system actually produces over a full judicial-generation cycle.',
    'If the secular-democratic reading becomes operative, the victim set empties and effective extraction collapses toward coordination cost; if the military-custodian reading becomes operative, the victim set shifts to elected civilians and the religious establishment loses its checkpoint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Committer-frame ambiguity: one kernel, three readings, three structurally different constraints.').

omega_variable(
    piety_provisions_coordination_or_cover,
    'Do the religious-status provisions track the majority population''s actual lived normative commitments, or do they entrench the coalition''s and clergy''s positions behind majority identity?',
    'Survey evidence on majority preferences over specific provisions (personal-status jurisdiction, office eligibility, education content) compared against the provisions'' actual operation, plus legislative-pattern analysis of who initiates conformity referrals.',
    'Genuine tracking supports the coordination half of the structure and lowers effective extraction for the majority seat; divergence indicates identity framing deployed as cover for elite entrenchment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(piety_provisions_coordination_or_cover, empirical, 'Whether the identity-coordination framing is doing real coordinating work or serving as cover.').

omega_variable(
    suppression_structural_vs_internalized,
    'For secular civil society and the minorities, how much of the measured suppression is external enforcement versus internalized self-limitation that would persist if enforcement were lifted?',
    'Post-relaxation expression trajectories: compare publication, assembly, and litigation rates in relaxed phases against pre-charter baselines; persistent gaps indicate internalized components.',
    'If substantially internalized, effective suppression exceeds the structural measure and outlasts any enforcement reform; the framework''s hold on its targets would survive formal liberalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized suppression mechanism for the target seats.').

omega_variable(
    majority_benefit_authenticity,
    'Is the pious majority''s benefit a genuine identity good the majority would defend on its own, or a manufactured benefit sustained by the enforcement apparatus itself?',
    'Revealed-preference tests: majority behavior when enforcement lapses (unpoliced violations of morality provisions), and support levels for specific provisions when decoupled from coalition endorsement.',
    'Manufactured benefit would push the majority seat from beneficiary toward payer-by-proxy and sharpen the asymmetry reading of the whole structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(majority_benefit_authenticity, conceptual, 'Authenticity of the majority seat''s benefit under the identity framework.').

omega_variable(
    military_alignment_trajectory,
    'Will the military remain a materially shielded but symbolically displaced actor inside the framework, or will it reassert an independent custodial claim that competes with the religious legitimacy ground?',
    'Observe command behavior across the next enforcement cycle: whether interventions justify themselves in religious-national terms (inside the framework) or in stability-and-guardianship terms (competing framework).',
    'Reassertion would pressure the kernel toward the military-custodian reading, shifting the victim set toward elected civilians and destabilizing this file''s classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(military_alignment_trajectory, empirical, 'Alignment trajectory of the military seat relative to the religious legitimacy ground.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(july_tr_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(july_tr_t3, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 3, 0.26).
narrative_ontology:measurement(july_tr_t6, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 6, 0.33).
narrative_ontology:measurement(july_tr_t9, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 9, 0.3).
narrative_ontology:measurement(july_tr_t12, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 12, 0.34).
narrative_ontology:measurement(july_tr_t15, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement(july_tr_t18, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 18, 0.36).
narrative_ontology:measurement(july_tr_t21, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 21, 0.41).
narrative_ontology:measurement(july_tr_t24, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 24, 0.44).

% Extraction over time
narrative_ontology:measurement(july_be_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(july_be_t3, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 3, 0.62).
narrative_ontology:measurement(july_be_t6, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 6, 0.7).
narrative_ontology:measurement(july_be_t9, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 9, 0.6).
narrative_ontology:measurement(july_be_t12, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 12, 0.64).
narrative_ontology:measurement(july_be_t15, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 15, 0.72).
narrative_ontology:measurement(july_be_t18, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 18, 0.63).
narrative_ontology:measurement(july_be_t21, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 21, 0.68).
narrative_ontology:measurement(july_be_t24, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 24, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(july_su_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(july_su_t3, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 3, 0.58).
narrative_ontology:measurement(july_su_t6, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 6, 0.68).
narrative_ontology:measurement(july_su_t9, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 9, 0.62).
narrative_ontology:measurement(july_su_t12, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 12, 0.66).
narrative_ontology:measurement(july_su_t15, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 15, 0.74).
narrative_ontology:measurement(july_su_t18, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 18, 0.69).
narrative_ontology:measurement(july_su_t21, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 21, 0.73).
narrative_ontology:measurement(july_su_t24, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 24, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__guided_nationalism_reading, identity_coordination).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__guided_nationalism_reading, july_charter_sovereign_legitimacy__secular_democratic_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__guided_nationalism_reading, july_charter_sovereign_legitimacy__military_custodian_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'the July Charter.' The label conflates three structurally distinct claims about what the founding articles DO: this file (guided_nationalism_reading — religious identity as the sovereign legitimacy ground, with secular civil society and religious minorities in the victim set), the secular_democratic_reading (citizen sovereignty indifferent to religion, civilian supremacy, no religious-law constitutional status — near-zero extraction on its own referent), and the military_custodian_reading (the charter ratifies permanent military guardianship — victim set shifts to elected civilians). Each reading gets its own epsilon, beneficiary set, and victim set per the epsilon-invariance principle; they share the fixed charter text as kernel and are linked through network edges and reading_relations rather than averaged into one story. Upstream/downstream: the charter text is the common upstream artifact; each reading is a distinct downstream constraint whose enforcement practice feeds back into the others' viability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(july_charter_sovereign_legitimacy__guided_nationalism_reading, powerful, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
