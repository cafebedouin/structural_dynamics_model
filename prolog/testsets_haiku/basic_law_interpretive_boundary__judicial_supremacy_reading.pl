% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_boundary__judicial_supremacy_reading, []).

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
 *   constraint_id: basic_law_interpretive_boundary__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy in Basic Law Interpretation and Enforcement
 *   domain: constitutional_law
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the Basic Law interpretive
 *   boundary—the judicial supremacy reading. The constraint story models the
 *   standing arrangement under contest: the Knesset's legislative authority
 *   is subject to Supreme Court review and nullification on Basic Law
 *   grounds. This reading holds that the Basic Laws constitute a binding
 *   constitutional framework that courts must interpret and enforce, with
 *   vertical judicial veto over ordinary legislation. Sibling readings
 *   (parliamentary_sovereignty_reading, balanced_contestation_reading) would
 *   author different ε values for different structural assumptions about who
 *   holds ultimate interpretive authority and what remedies are available
 *   when courts and legislatures clash. This story instantiates the judicial
 *   supremacy premises only.
 *
 * KEY AGENTS:
 *   - Supreme Court: institutional agenda-setter, holds de facto authority to nullify legislation deemed to contradict Basic Laws, enforces via binding adjudication
 *   - Individual rights-claimants: beneficiaries, gain litigation channel and potential judicial veto on rights-restrictive legislation
 *   - Knesset legislative majority: payer, faces constraint on legislative sovereignty—statutes may be nullified even if passed with simple majority
 *   - Civil society advocacy coalitions: beneficiaries, amplify their political voice through litigation and amicus participation
 *   - Right-wing legislative actors: secondary payers, have experienced nullification of settlement and nationality legislation, perceive judicial overreach
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.68).
domain_priors:suppression_score(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.71).
domain_priors:theater_ratio(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_boundary__judicial_supremacy_reading, "Judicial Supremacy in Basic Law Interpretation and Enforcement").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__judicial_supremacy_reading, "constitutional_law").

domain_priors:requires_active_enforcement(basic_law_interpretive_boundary__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__judicial_supremacy_reading, 'c31a6432-6cdd-4181-ac71-9e7f520beda1').
narrative_ontology:cs_kernel_codification('c31a6432-6cdd-4181-ac71-9e7f520beda1', fixed_text).
narrative_ontology:cs_authority_grounding('c31a6432-6cdd-4181-ac71-9e7f520beda1', extraction).
narrative_ontology:cs_interpretation_layer_present('c31a6432-6cdd-4181-ac71-9e7f520beda1').
narrative_ontology:cs_reading_relation('c31a6432-6cdd-4181-ac71-9e7f520beda1', basic_law_interpretive_boundary__parliamentary_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('c31a6432-6cdd-4181-ac71-9e7f520beda1', basic_law_interpretive_boundary__balanced_contestation_reading, influences).
narrative_ontology:cs_axiom('c31a6432-6cdd-4181-ac71-9e7f520beda1', foundational, basic_laws_constitute_higher_order_framework).
narrative_ontology:cs_axiom_status(basic_laws_constitute_higher_order_framework, holdable).
narrative_ontology:cs_axiom_grounding('c31a6432-6cdd-4181-ac71-9e7f520beda1', basic_laws_constitute_higher_order_framework, conventional).
narrative_ontology:cs_axiom('c31a6432-6cdd-4181-ac71-9e7f520beda1', foundational, judicial_interpretation_binds_all_branches).
narrative_ontology:cs_axiom_status(judicial_interpretation_binds_all_branches, holdable).
narrative_ontology:cs_axiom_grounding('c31a6432-6cdd-4181-ac71-9e7f520beda1', judicial_interpretation_binds_all_branches, deontological).
narrative_ontology:cs_reference_frame('c31a6432-6cdd-4181-ac71-9e7f520beda1', constitutional_hierarchy_with_judicial_apex).
narrative_ontology:cs_drift_state('c31a6432-6cdd-4181-ac71-9e7f520beda1', contemporary_2025, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('c31a6432-6cdd-4181-ac71-9e7f520beda1', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, individual_rights_claimants).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court_institutional_authority).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__judicial_supremacy_reading, knesset_legislative_majority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, civil_society_advocacy_coalitions).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__judicial_supremacy_reading, right_wing_legislative_actors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Basic Laws and has de facto authority to strike down Knesset legislation that contradicts them. Justifies this role as faithful interpretation of the constitutional framework and protection of fundamental rights. The court enforces the interpretive boundary by receiving petitions, adjudicating disputes over legislative scope, and issuing binding nullifications that the Knesset cannot unilaterally reverse without amending the Basic Laws themselves. This power is exercised through abstract review (before legislation takes effect) and concrete review (in post-enactment disputes).
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court, agenda_setter,
    institutional, generational, trapped, national).

% Citizens seeking to challenge legislation they claim violates Basic Law protections of rights. The constraint gives them a forum (Supreme Court petition) and a potential veto (judicial nullification) they would not have under pure parliamentary sovereignty. They bear no direct cost of court enforcement; their exit is constrained by political marginalization if the legislature dislikes their claims, but they gain access to a second arena for contesting laws.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, individual_rights_claimants, beneficiary,
    powerless, biographical, constrained, national).

% Enacts legislation but faces the constraint that the Supreme Court may nullify statutes that a judicial majority reads as contradicting the Basic Laws. The majority must either accept the court's interpretation, amend the Basic Laws through super-majority procedures (a costly political operation), or accept the reputational and practical damage of attempting to override the court (which modern norms make costly). Their exit—ignoring judicial nullification—is constrained by international standing, domestic institutional legitimacy, and the court's ability to enforce contempt sanctions. The constraint is experienced as a binding limit on legislative sovereignty.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, knesset_legislative_majority, payer,
    organized, biographical, constrained, national).

% Organizations representing marginalized groups, religious minorities, or political opponents of the legislative majority. They benefit from the constraint by gaining litigation as a political channel—a way to challenge laws without electoral power. They coordinate litigation campaigns, file amicus briefs, and shape the discourse of rights claims that courts receive. The constraint amplifies their voice beyond their electoral weight.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, civil_society_advocacy_coalitions, beneficiary,
    organized, biographical, constrained, national).

% Comparative constitutionalists, international law scholars, and NGOs monitoring the Israeli system from outside. They evaluate whether the constraint constitutes genuine constitutional supremacy or judicial overreach. Their observations feed back into legitimacy debates and can influence pressure on Israel through UN bodies and peer constitutional courts.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, academic_and_international_observers, observer,
    analytical, generational, analytical, global).

% Parliamentary actors who have attempted to enact legislation on settlement policy, Jewish nationality, or state-religion matters that the Supreme Court has struck down or constrained via this interpretive boundary. They experience the constraint as blocking their legislative agenda and perceive it as judicial activism rather than constitutional interpretation. Their option to simply override the court is theoretically available (via Basic Law amendment) but practically costly. They advocate loudly for limiting judicial review but remain organized within the parliamentary system rather than exiting it.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, right_wing_legislative_actors, payer,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__judicial_supremacy_reading, right_wing_legislative_actors, excluded).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court).
narrative_ontology:fixing_cost_class(basic_law_interpretive_boundary__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable interpretive authority for the constitutional framework: one institutional seat (the Supreme Court) holds the power to settle disputes about what the Basic Laws permit and forbid. Without this authority, different actors (Knesset committees, executive agencies, lower courts, private parties) would interpret the Basic Laws inconsistently, leading to legal fragmentation and forum shopping. The constraint creates a single appellate resolver.
% TRANSFER_FUNCTION: Transfers veto power over legislation from a simple Knesset majority to a Supreme Court majority. Any coalition commanding 61 Knesset seats can enact statutes; any coalition commanding a Supreme Court majority can nullify them. This reallocation of who sets binding legal boundaries moves political power away from electoral coalitions and toward judicial appointment networks. It also transfers power from the legislative majority to rights-claimants and civil society actors who can litigate.
% ABSENT_VOICES: Elected legislators who lose laws to judicial nullification are not absent—they are present and actively contest the court's authority (see right_wing_legislative_actors). The absent voices are: (1) the Knesset's own institutional voice as a coordinate branch (a Knesset speaker, if one existed with institutional standing, would have grounds to contest the constraint, but Israel's legislative branch is weak in comparative terms); (2) the conceptual voice of pure parliamentary sovereignty—the position that the Knesset as the repository of the people's will should have ultimate interpretive authority, even over constitutional law. That voice is present in political discourse but lacks an institutional seat in this constraint story.
% DISAPPEARANCE_RATIONALE: If judicial review of Basic Laws vanished and the Knesset regained unilateral interpretive authority, the system would reorganize: legislation striking down rights protections could proceed without court invalidation; marginalized groups would lose the litigation channel; rights-claimants would shift to extra-legal mobilization or exit (immigration); international standing of the legal system would weaken; constitutional interpretation would become a purely parliamentary negotiation process. The system would not collapse but its character would shift fundamentally toward majoritarian legislative sovereignty.
% FOUNDING_PROBLEM: Post-1950, Israel had no written constitution and no clear hierarchy of legal norms. The Knesset could legislate on any topic including restrictions on citizenship, freedom of conscience, and property rights, with no procedural brake. By the 1990s, when the Basic Laws were assembled into an informal constitutional framework, the founding problem was: how do you protect fundamental rights in a purely parliamentary system without a codified constitution? The judicial supremacy reading answers: courts must be empowered to enforce the Basic Laws as constitutional limits on ordinary legislation.
% FOUNDING_PROBLEM_CORROBORATION: The Supreme Court and civil rights organizations attest that the founding problem remains live: without judicial review, democratic majorities could erode rights protections. The Knesset and right-wing legislative coalitions attest that the founding problem has been addressed by the Basic Laws themselves—the legislative framework now exists, and courts should interpret it within bounds set by ordinary legislation and political norms, not override the legislature. International constitutional scholars and comparative law experts (e.g., from the United States and Canada, where constitutional courts have strong review powers) attest that the problem is indeed live—unreviewed legislatures regularly attempt rights erosions—and that judicial enforcement is a standard solution. However, the Israeli parliamentary majority disputes whether the court's particular reading of its own authority matches the Basic Laws as actually written.
narrative_ontology:disappearance_verdict(basic_law_interpretive_boundary__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_boundary__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__judicial_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(basic_law_interpretive_boundary__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_boundary__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_boundary__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.42 (1992, when the Basic Laws were first informally constitutionalized but court power was still being negotiated) to 0.68 (2025, after three decades of court-nullified legislation and accumulating precedent establishing judicial supremacy). The rise reflects: (1) an increase in the frequency and scope of court nullifications of Knesset legislation; (2) the court's expansion of which legislation counts as constitutionally reviewable (originally only explicitly Basic Law amendments; now all ordinary statutes); (3) the court's broadening of which rights are protected (from explicit text to penumbral inferences). Suppression rises from 0.48 to 0.71 because the Knesset's options narrow: initially, legislators could claim uncertainty about what the court would do; by 2025, they face a clear precedent that controversial legislation will be challenged and reviewed. Theater rises from 0.22 to 0.42 because judicial rhetoric increasingly frames nullifications in terms of 'constitutional interpretation' and 'judicial duty,' even as the practical effect is to veto legislation the court dislikes on policy grounds—the performative legitimacy work intensifies. The accessibility_collapse (0.78) reflects that once a party understands judicial review exists, the alternative of 'legislation without court oversight' is structurally unavailable without Basic Law amendment (a super-majority operation). Resistance (0.59) is moderate-high because right-wing and religious parties actively contest the court's authority through political campaigns, proposed Basic Law reforms, and rhetorical challenges to judicial legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   The Supreme Court and rights-claimants experience this constraint as coordination (courts settle constitutional disputes; rights are protected). The Knesset legislative majority experiences it as extraction (the court takes away laws they passed). These are not reconcilable within a single reading—they reflect genuinely different structural positions. The judicial supremacy reading builds in this asymmetry: from the court's vantage, it is interpreting a higher law; from the Knesset's vantage, the court is rewriting the law. The engine should compute different types from different seats: the court experiences coordination or even beneficiary status (its institutional power expands); the Knesset experiences constraint and extraction. The authored metrics describe the constraint as experienced from the Knesset's structural position (legislative majority bearing the cost of nullification).
 *
 * DIRECTIONALITY LOGIC:
 *   The Supreme Court is the beneficiary: its institutional authority expands, its rulings bind the coordinate branches, and it collects legitimacy and influence. Derivation: institutional power, long time horizon, exit is trap (court cannot exit the constitutional system without dissolution), low d (benefits from the constraint). Individual rights-claimants are beneficiaries: they gain access to a court forum and potential veto. Derivation: powerless agents, but beneficiary role, low d (benefits despite powerlessness). Civil society coalitions are beneficiaries: their litigation amplifies their voice. The Knesset legislative majority is a payer: it bears the cost of legislation being nullified. Derivation: organized power, biographical time horizon, constrained exit (can amend the Basic Laws but at high political cost), high d (fully target of the constraint). Right-wing legislative actors are secondary payers because they have specifically had legislation nullified; their exit options (organized power, mobile) are somewhat better than the generic Knesset majority, but they remain targets of judicial nullification. No directionality overrides are needed: the structural data produces accurate d values through the derivation chain.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting rights without a codified constitution) remains contested, not dead. However, there is a case that the constraint has drifted from a genuine coordination function (settling constitutional interpretation) toward pure extraction (using constitutional review as a veto on policies the court dislikes). The theater_ratio rise from 0.22 to 0.42 suggests that performative justification is growing relative to functional settlement. The resistance of 0.59 and the political mobilization against the court (attempts to appoint justices who will constrain review, proposed Basic Law reforms to limit court power) indicate that the constraint is not stable—there is active pressure to reduce or reframe it. The constraint is neither a piton (it is still functionally enforced and carries real costs) nor a pure rope (it asymmetrically benefits the court and rights-claimants at the Knesset's expense). It is best classified as tangled_rope: genuine coordination function (constitutional interpretation) laced with extraction (court power over legislative agenda).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    basic_law_text_vs_judicial_gloss,
    'Do the written Basic Laws actually authorize the judicial supremacy that the Supreme Court has claimed, or has the Court read this authority into the text?',
    'Comparative textual analysis of the Basic Law language against judicial opinions claiming authority; review of legislative history and parliamentary debate on the Basic Laws; analysis of whether the Court''s reading is faithful to the text or a creative constitutional interpretation.',
    'If the Basic Laws do not actually grant the Court supremacy, the constraint is an instance of judicial overreach (snare on the Knesset side, piton on the Court side—institutional inertia masquerading as constitutional authority). If they do grant it, the constraint is a genuine constitutional hierarchy (tangled rope at worst, coordination at best).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(basic_law_text_vs_judicial_gloss, empirical, 'Whether the Basic Law text supports the judicial supremacy doctrine or whether the Court has read authority into silence.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the Knesset''s constraint on legislative freedom structural (the Court simply invalidates laws) or internalized (the Knesset preemptively self-censors, avoiding laws it predicts the Court will strike down)?',
    'Historical analysis of proposed legislation: what bills were drafted but not introduced due to anticipated court opposition, vs. bills introduced and subsequently nullified? Interviews with legislative drafters and committees about their perception of the court''s scope. Analysis of whether suppression decreased in periods when the Court was less activist or different justices held office.',
    'If largely structural, the suppression is a raw property of the constraint (external coercion). If internalized, the suppression persists even if the Court''s actual nullification rate dropped—the Knesset would carry the constraint inside itself. Internalized suppression suggests the constraint operates through ideology or institutional identity rather than pure coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of legislative alternatives is external (court nullification) or internal (preemptive self-censorship).').

omega_variable(
    reading_foreclosure_via_legitimacy,
    'Does the judicial supremacy reading logically foreclose the parliamentary sovereignty reading, or do they remain live positions for different institutional actors?',
    'Doctrinal analysis: can a single constitutional framework coherently hold that both the Court has supremacy AND the Knesset retains ultimate authority? Or are these mutually exclusive premises? Empirical check: do Israeli constitutional scholars and jurists treat these as rival readings (coexist_with) or as logically incompatible (forecloses)?',
    'If foreclosed, then the basic_law_interpretive_boundary kernel has a forced answer—only one reading is logically sustainable. If coexist, then the kernel remains truly contested; different institutional coalitions can hold different readings, and the constraint persists through institutional contest rather than doctrinal resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_via_legitimacy, conceptual, 'Whether judicial supremacy and parliamentary sovereignty are logically exclusive or merely institutionally rival.').

omega_variable(
    international_pressure_coupling,
    'To what extent does international human rights pressure (UN bodies, international courts, peer democracies) amplify or sustain the Supreme Court''s judicial supremacy claim?',
    'Analysis of Court opinions citing international law and human rights standards; documentation of international pressure on Israel to maintain judicial review; comparison with Israeli court decisions pre- and post-international human rights framework adoption (1990s onwards). Examine whether the Court''s authority would persist without international legitimacy backing.',
    'If international pressure is substantial, the constraint''s persistence depends partly on external enforcement (international reputation costs for the Knesset overriding the Court). If minimal, the constraint is sustained by domestic institutional configuration alone. High international coupling makes the constraint globally interdependent and more vulnerable to changes in international human rights discourse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_pressure_coupling, empirical, 'Degree to which judicial supremacy is sustained by international human rights pressure vs. domestic institutional balance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__judicial_supremacy_reading, 1992, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t1992, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 1992, 0.22).
narrative_ontology:measurement(basi_tr_t2000, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 2000, 0.28).
narrative_ontology:measurement(basi_tr_t2010, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 2010, 0.35).
narrative_ontology:measurement(basi_tr_t2018, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 2018, 0.39).
narrative_ontology:measurement(basi_tr_t2025, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(basi_be_t1992, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 1992, 0.42).
narrative_ontology:measurement(basi_be_t2000, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 2000, 0.51).
narrative_ontology:measurement(basi_be_t2010, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(basi_be_t2018, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 2018, 0.65).
narrative_ontology:measurement(basi_be_t2025, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t1992, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 1992, 0.48).
narrative_ontology:measurement(basi_su_t2000, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(basi_su_t2010, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 2010, 0.62).
narrative_ontology:measurement(basi_su_t2018, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 2018, 0.68).
narrative_ontology:measurement(basi_su_t2025, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 2025, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.12).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary__parliamentary_sovereignty_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary__balanced_contestation_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__judicial_supremacy_reading, knesset_legislative_majority_power).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__judicial_supremacy_reading, israeli_human_rights_protection_framework).

% DUAL FORMULATION NOTE:
% This constraint is part of the basic_law_interpretive_boundary constraint family, which decomposes the single phrase 'Basic Laws bind the Knesset' into three structurally distinct readings. Each reading produces different ε values, different beneficiary/victim sets, and different type classifications because each reading embodies different answers to the fundamental question: who ultimately interprets the Basic Laws? The judicial_supremacy_reading presented here models the Supreme Court's institutional position and the structural consequences of its claimed authority. The parliamentary_sovereignty_reading models the Knesset's retained authority and the structural consequences of rejecting judicial supremacy. The balanced_contestation_reading models an intermediate position where both institutions have bounded authority. These are not the same constraint viewed from different angles—they are genuinely different arrangements with different ε referents and different extraction profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(basic_law_interpretive_boundary__judicial_supremacy_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
