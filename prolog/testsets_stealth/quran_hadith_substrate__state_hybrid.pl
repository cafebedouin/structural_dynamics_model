% ============================================================================
% CONSTRAINT STORY: quran_hadith_substrate__state_hybrid
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_hadith_substrate__state_hybrid, []).

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
 *   constraint_id: quran_hadith_substrate__state_hybrid
 *   human_readable: State-Hybrid Selective Sharia Codification Settlement
 *   domain: legal/religious-political
 *
 * SUMMARY:
 *   A sovereign state operates a split legal order: classical fiqh rulings
 *   are codified and enforced in family law and selected criminal provisions,
 *   while commercial, administrative, and much procedural law runs on secular
 *   or reformist frameworks. The settlement is presented as faithful
 *   synthesis; in operation it is a portfolio managed for political return.
 *   The state collects religious legitimacy from the designated domains and
 *   economic credibility from the insulated ones, disciplines both scholarly
 *   flanks that object to the selection mechanism, and freezes
 *   personal-status rulings that citizens cannot revisit. KEY AGENTS (by
 *   structural relationship): ruling_state_elites — agenda setter
 *   (institutional/arbitrage), designates domains and collects legitimacy
 *   rents; state_religious_bureaucracy — co-opted administrator
 *   (organized/identity_locked), trades independence for platform;
 *   independent_traditionalist_scholars — primary target
 *   (moderate/identity_locked), comprehensive vision truncated;
 *   reformist_intellectuals — primary target (moderate/constrained), methods
 *   appropriated, critique censored; citizens_under_personal_status_law —
 *   diffuse target (powerless/trapped); commercial_investor_class — secondary
 *   beneficiary (powerful/arbitrage); islamist_opposition_movements —
 *   excluded challenger (organized/trapped);
 *   international_human_rights_monitors — analytical observer
 *   (institutional/analytical).
 *
 * KEY AGENTS:
 *   - ruling_state_elites: Agenda setter (institutional/arbitrage) — decides which domains are governed by classical rulings and which by secular codes; collects legitimacy rents
 *   - state_religious_bureaucracy: Co-opted beneficiary (organized/identity_locked) — administers designated domains; traded independence for the only platform of official authority
 *   - independent_traditionalist_scholars: Primary target (moderate/identity_locked) — comprehensive fiqh vision truncated to state-designated corners
 *   - reformist_intellectuals: Primary target (moderate/constrained) — frameworks borrowed where useful, critique of the selection mechanism suppressed
 *   - citizens_under_personal_status_law: Diffuse target (powerless/trapped) — adjudicated under frozen codified rulings with no opt-out
 *   - commercial_investor_class: Secondary beneficiary (powerful/arbitrage) — buys contractual predictability under insulated commercial codes
 *   - islamist_opposition_movements: Excluded challenger (organized/trapped) — criminalized when organizing toward comprehensive implementation
 *   - international_human_rights_monitors: Analytical observer (institutional/analytical) — documents the asymmetry, conditions external resources
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_hadith_substrate__state_hybrid, 0.38).
domain_priors:suppression_score(quran_hadith_substrate__state_hybrid, 0.62).
domain_priors:theater_ratio(quran_hadith_substrate__state_hybrid, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, extractiveness, 0.38).
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__state_hybrid, tangled_rope).
narrative_ontology:human_readable(quran_hadith_substrate__state_hybrid, "State-Hybrid Selective Sharia Codification Settlement").
narrative_ontology:topic_domain(quran_hadith_substrate__state_hybrid, "legal/religious-political").

domain_priors:requires_active_enforcement(quran_hadith_substrate__state_hybrid).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__state_hybrid, 'bc6903b7-a6fe-433e-930b-f70912c0ede3').
narrative_ontology:cs_kernel_codification('bc6903b7-a6fe-433e-930b-f70912c0ede3', fixed_text).
narrative_ontology:cs_authority_grounding('bc6903b7-a6fe-433e-930b-f70912c0ede3', extraction).
narrative_ontology:cs_interpretation_layer_present('bc6903b7-a6fe-433e-930b-f70912c0ede3').
narrative_ontology:cs_reading_relation('bc6903b7-a6fe-433e-930b-f70912c0ede3', quran_hadith_substrate__traditionalist_taqlid, influences).
narrative_ontology:cs_reading_relation('bc6903b7-a6fe-433e-930b-f70912c0ede3', quran_hadith_substrate__reformist_ijtihad, influences).
narrative_ontology:cs_axiom('bc6903b7-a6fe-433e-930b-f70912c0ede3', foundational, political_sovereignty_adjudicates_sharia_scope).
narrative_ontology:cs_axiom_status(political_sovereignty_adjudicates_sharia_scope, holdable).
narrative_ontology:cs_axiom_grounding('bc6903b7-a6fe-433e-930b-f70912c0ede3', political_sovereignty_adjudicates_sharia_scope, conventional).
narrative_ontology:cs_axiom('bc6903b7-a6fe-433e-930b-f70912c0ede3', foundational, domain_partitioned_scriptural_application).
narrative_ontology:cs_axiom_status(domain_partitioned_scriptural_application, holdable).
narrative_ontology:cs_axiom_grounding('bc6903b7-a6fe-433e-930b-f70912c0ede3', domain_partitioned_scriptural_application, instrumental).
narrative_ontology:cs_reference_frame('bc6903b7-a6fe-433e-930b-f70912c0ede3', sovereign_selective_codification).
narrative_ontology:cs_drift_state('bc6903b7-a6fe-433e-930b-f70912c0ede3', contemporary, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('bc6903b7-a6fe-433e-930b-f70912c0ede3', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__state_hybrid, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__state_hybrid, ruling_state_elites).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__state_hybrid, state_religious_bureaucracy).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__state_hybrid, commercial_investor_class).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, independent_traditionalist_scholars).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, reformist_intellectuals).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, citizens_under_personal_status_law).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, islamist_opposition_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, state_religious_bureaucracy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Executives, ruling parties, and sovereign councils decide which areas of law are designated sharia and which run on secular or reformist codes. They recalibrate the mix as legitimacy needs and investor demands shift, stage codifications ceremonially, and collect the political returns of religious authenticity without binding themselves to doctrinal consistency. Their exit option is the machinery of sovereignty itself: any domain can be redesignated by decree.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, ruling_state_elites, agenda_setter,
    institutional, generational, arbitrage, national).

% Official muftis, ministries of religious endowments, and state-appointed ulama councils administer the designated domains, issue vetted opinions, and staff the family courts. Salaries, rank, and the only legal platform for public religious authority flow to them from the state; in exchange they surrender independent judgment and defend the official line. Generations of appointment have fused the corps with the state, and leaving would mean abandoning the only institutional home their authority has.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, state_religious_bureaucracy, beneficiary,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__state_hybrid, state_religious_bureaucracy, payer).

% Scholars formed in the classical schools outside or at the margins of state institutions hold that the revealed corpus governs all of life. The settlement confines their competence to whatever corners the state designates, requires licenses to teach or publish, and prosecutes unlicensed opinions. Their vocation is constituted by fidelity to the whole corpus, so retreat into private circles feels like betrayal while open insistence on comprehensive application ends their careers or worse.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, independent_traditionalist_scholars, payer,
    moderate, generational, identity_locked, regional).

% Jurists and public thinkers argue for contextual re-reading of the sources in light of contemporary ethics and public interest. The state borrows their methods where commercial flexibility pays and silences their critique of the selection mechanism itself. Careers ride on licenses and university posts that can be revoked; some emigrate, most self-censor, and their readership shrinks under pressures they cannot publicly name.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, reformist_intellectuals, payer,
    moderate, biographical, constrained, national).

% Families have marriage, divorce, custody, and inheritance adjudicated under codified classical rulings the state froze decades ago. They cannot choose another personal-law regime, cannot invoke the interpretive flexibility the codification displaced, and experience the designated law as an inherited fact rather than a lived doctrine. Court fees, delays, and adverse rulings land on them with no appellate route outside the same system.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, citizens_under_personal_status_law, payer,
    powerless, biographical, trapped, national).

% Domestic business elites and foreign investors contract under commercial and administrative codes deliberately insulated from religious designation. They gain enforceable, predictable instruments priced for global markets, and they can move capital if the religious-commercial mix shifts against them — leverage without exposure to the designated domains.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, commercial_investor_class, beneficiary,
    powerful, immediate, arbitrage, global).

% Organized movements demand comprehensive application of the revealed law. They are courted as an audience when regimes need religious credentials and criminalized when they organize toward their program; they are never admitted to the codification process itself. Prison, exile, or underground organization bound their participation.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, islamist_opposition_movements, excluded,
    organized, generational, trapped, regional).

% Treaty bodies, special rapporteurs, and NGOs document the gap between official religious claims and secular commercial practice, and the harms of frozen personal-status rulings. They condition loans, ratings, and partnerships, and they publish findings the state disputes, but they hold no seat in the domestic interpretive settlement.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, international_human_rights_monitors, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_hadith_substrate__state_hybrid, ruling_state_elites).
narrative_ontology:fixing_cost_class(quran_hadith_substrate__state_hybrid, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies legal administration across a religiously diverse population: one court system, one commercial code, and designated religious domains solve the problem of running a modern economy and bureaucracy while maintaining a common religious-legal framework for personal life.
% TRANSFER_FUNCTION: Moves interpretive authority over the Quran-Hadith corpus from independent scholarly networks to state organs; moves legitimacy capital from doctrinal fidelity to political sovereignty; moves legal predictability in commerce to investors and elites; moves adjudication costs in personal status onto citizens who cannot opt out.
% ABSENT_VOICES: Independent traditionalist scholars sit outside the codification conversation except as licensed contractors; unlicensed reformist critics publish at legal risk; islamist opposition movements are addressed as an audience to be managed, never as authors; ordinary citizens subject to personal-status codes were never consulted on which rulings were frozen.
% DISAPPEARANCE_RATIONALE: If the selective-adoption settlement vanished overnight, every hybrid state would immediately face the choice its arrangement defers: comprehensive sharia codification or full secular unification. Family courts, criminal codes, banking regulation, and the official clergy's mandate would all be redrawn; investor contracts priced on the current mix would be repriced; both scholarly flanks would escalate from protest to constitutional struggle.
% FOUNDING_PROBLEM: Late Ottoman and post-colonial state builders inherited European-style codes and centralized bureaucracies while governing Muslim populations whose legitimacy expectations ran through sharia; the founding problem was how to consolidate sovereign legal unity without surrendering either religious legitimacy or economic modernity.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional preambles across hybrid states still designate sharia 'a principal source' of legislation — an admission that the legitimacy problem persists. Opposition manifestos and reformist journals attack the settlement from outside the benefiting parties, and comparative scholarship on Islamic law (Hallaq, An-Na'im, Asad) documents the unresolved tension from academic seats. No party inside the settlement attests its obsolescence.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__state_hybrid, world_rearranges).
narrative_ontology:founding_problem_status(quran_hadith_substrate__state_hybrid, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__state_hybrid, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quran_hadith_substrate__state_hybrid, 'none', 1).
narrative_ontology:epsilon_provenance(quran_hadith_substrate__state_hybrid, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_hadith_substrate__state_hybrid_tests).
:- end_tests(quran_hadith_substrate__state_hybrid_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.38, the modal of the declared low-to-moderate band: the settlement transfers interpretive authority and legitimacy rents to the state and imposes compliance costs on both scholarly flanks and on citizens under frozen personal law, while delivering real adjudication and commercial predictability. Suppression (0.62) is authored as a raw structural property — licensing regimes, prosecution of unlicensed opinion, co-optation discipline — and is deliberately left unscaled; only extractiveness is scaled by directionality and scope, and the engine owns that arithmetic. Theater (0.46) reflects a near-even split between functioning adjudication and ceremonial codification whose display value increasingly substitutes for doctrinal content. Accessibility collapse (0.5): independent fiqh platforms are absorbed and unlicensed publication is risky, but private observance, quietism, and emigration persist, so alternatives narrow without vanishing. Resistance (0.58): recurring mobilization from the traditionalist flank, dissent from the reformist flank, and litigation against personal-status codes. All three tracked series share one time grid (points every 8 units across 0–80) so every metric is authored at every examined point. Suppression_requirement oscillates with roughly 24-unit periodicity — peaks near t=16, 40, and 64 track opposition mobilization waves followed by crackdowns; troughs follow successful repression. The oscillation is itself a disciplinary mechanism (intermittent reinforcement is cheaper than constant repression), riding on a rising enforcement baseline. End-state values match the base_properties scalars; the scalars were read at the closing phase of the last cycle.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat the settlement is flexible statecraft: a portfolio of legal regimes rebalanced as incentives shift, computing near the coordination end. From the identity-locked traditionalist seat the same structure reads as usurpation — comprehensive obligation truncated by decree — and computes toward the enforced-extraction end. From the constrained reformist seat it is a censorship regime that consumes its vocabulary while borrowing its methods. From the arbitrage-grade investor seat it is benign predictability. One arrangement, four computed experiences; the divergence is produced by power, exit, and directionality, not by the authored claim, which is left unreconciled to any predicted output.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map directly onto structure. ruling_state_elites and commercial_investor_class sit at the beneficiary end (d near 0): the first sets the rules and collects legitimacy rents, the second purchases predictability it can exit. state_religious_bureaucracy derives low d from its beneficiary declaration; although co-optation costs it independence, its net position remains beneficiary-side, so no override is warranted. Targets: independent_traditionalist_scholars (identity-locked, near-full d), reformist_intellectuals (constrained, high d), citizens_under_personal_status_law (trapped, high d), islamist_opposition_movements (high d despite organized power — organization here raises the value of suppressing them rather than lowering their exposure). Spatial scope is national for the domestic seats and global for investors and monitors, so the engine's verification-difficulty amplification bites hardest at the wide-scope seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The settlement is routinely defended as transitional — a temporary accommodation until society matures — but it carries no sunset clause and the selectivity is its steady-state operating principle, not a bridge; authoring it as scaffold would launder an open-ended arrangement as a phase. The opposite mislabel — pure snare, mere hypocrisy — would erase the genuine coordination work: one court system, communal personal-law accommodation, investable commercial codes. Tangled rope keeps both facts: real coordination function, asymmetric extraction through the same structure, held together by active enforcement. On genealogy: the founding problem (consolidating sovereign legal unity without surrendering religious legitimacy or modernity) remains live and is corroborated from outside the benefiting parties, so no mandatrophy resolution is declared. The rising theater series is the early-warning signature: if the coordination function hollows further while display grows, the arrangement drifts toward inertial maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cross_state_epsilon_variability,
    'Does the authored epsilon of 0.38 hold across the range of state-hybrid regimes, from heavily Islamized hybrids to lightly decorated ones?',
    'Comparative coding of personal-status codes, censorship records, and commercial-law insulation across a panel of hybrid states; regress effective extraction on regime type.',
    'At the Islamized pole epsilon approaches snare territory (targets dominate, coordination thins); at the secularized pole it approaches rope; the modal tangled_rope classification holds only in the middle band.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cross_state_epsilon_variability, empirical, 'Epsilon varies widely across state contexts; 0.38 is a modal estimate, not a universal.').

omega_variable(
    kernel_reading_committer_structure,
    'This constraint instantiates only the state_hybrid reading of the quran_hadith_substrate kernel — what structurally changes under the sibling readings traditionalist_taqlid and reformist_ijtihad?',
    'Author and compile the sibling stories; compare victim sets (under taqlid the state is a usurping outsider rather than agenda-setter; under ijtihad the state''s selective appropriations become the primary contested object) and epsilon referents.',
    'Neither sibling shares this story''s beneficiary structure or its sovereignty-grounded axioms; folding them into one constraint would produce an observable-dependent epsilon in violation of invariance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one of three readings of one kernel; siblings are separate constraints linked by network edges.').

omega_variable(
    authority_grounding_framing,
    'Is the state''s authority over the kernel grounded in political sovereignty (self-enforcing coercion) or in extraction of legitimacy rents from controlled interpretation?',
    'Observe cases where sovereignty and rent-control diverge: a regime secure enough to democratize interpretation but refusing to do so reveals extraction; a regime that delegates interpretation once secure reveals genuine sovereignty grounding.',
    'A sovereignty framing yields self_enforcing authority with no interpretive buffer; the authored extraction framing captures the co-opted ulama layer and routes foreclosure computation differently for sibling axioms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_framing, conceptual, 'CS framing under-determination between self_enforcing and extraction authority groundings.').

omega_variable(
    elite_doctrinal_sincerity,
    'Do ruling elites treat the designated classical rulings as binding on themselves, or purely as legitimacy instruments?',
    'Revealed-preference audit: compliance of elite families with the personal-status courts they maintain; exemption patterns; offshore arrangements that route elites around their own codes.',
    'Pure instrumentality raises the extraction component (display without commitment) and pushes theater_ratio upward; sincere partial commitment converts part of the measured epsilon into coordination cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(elite_doctrinal_sincerity, empirical, 'Whether sharia designation is belief or instrument for the agenda-setting seat.').

omega_variable(
    cross_flank_coalition_potential,
    'Can traditionalist and reformist opponents of the hybrid overcome doctrinal hostility to form a joint front against the selection mechanism itself?',
    'Historical scan for joint statements, shared platforms, or alliances targeting the state''s interpretive monopoly rather than each other.',
    'A durable cross-flank coalition would raise resistance sharply and threaten the enforcement economics; its persistent absence confirms the divide-and-designate mechanism works.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cross_flank_coalition_potential, empirical, 'Coalition potential among opposed target flanks sharing one antagonist.').

omega_variable(
    suppression_mechanism_split,
    'Is the measured suppression primarily structural (licensing, prosecution, jurisdiction control) or internalized (clerical self-censorship, popular habituation to frozen personal law)?',
    'Post-liberalization trajectory: if criticism and doctrinal experimentation surge when formal barriers lift, prior suppression was largely structural; if habits persist unchanged, the internalized share is large.',
    'A large internalized share means effective suppression exceeds the structural measure and outlasts any single regime''s enforcement capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_split, empirical, 'Structural versus internalized composition of the suppression scalar.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__state_hybrid, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_hadith_substrate__state_hybrid, theater_ratio, 0, 0.22).
narrative_ontology:measurement(qura_tr_t8, quran_hadith_substrate__state_hybrid, theater_ratio, 8, 0.25).
narrative_ontology:measurement(qura_tr_t16, quran_hadith_substrate__state_hybrid, theater_ratio, 16, 0.29).
narrative_ontology:measurement(qura_tr_t24, quran_hadith_substrate__state_hybrid, theater_ratio, 24, 0.32).
narrative_ontology:measurement(qura_tr_t32, quran_hadith_substrate__state_hybrid, theater_ratio, 32, 0.35).
narrative_ontology:measurement(qura_tr_t40, quran_hadith_substrate__state_hybrid, theater_ratio, 40, 0.38).
narrative_ontology:measurement(qura_tr_t48, quran_hadith_substrate__state_hybrid, theater_ratio, 48, 0.4).
narrative_ontology:measurement(qura_tr_t56, quran_hadith_substrate__state_hybrid, theater_ratio, 56, 0.42).
narrative_ontology:measurement(qura_tr_t64, quran_hadith_substrate__state_hybrid, theater_ratio, 64, 0.44).
narrative_ontology:measurement(qura_tr_t72, quran_hadith_substrate__state_hybrid, theater_ratio, 72, 0.45).
narrative_ontology:measurement(qura_tr_t80, quran_hadith_substrate__state_hybrid, theater_ratio, 80, 0.46).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_hadith_substrate__state_hybrid, base_extractiveness, 0, 0.26).
narrative_ontology:measurement(qura_be_t8, quran_hadith_substrate__state_hybrid, base_extractiveness, 8, 0.27).
narrative_ontology:measurement(qura_be_t16, quran_hadith_substrate__state_hybrid, base_extractiveness, 16, 0.29).
narrative_ontology:measurement(qura_be_t24, quran_hadith_substrate__state_hybrid, base_extractiveness, 24, 0.3).
narrative_ontology:measurement(qura_be_t32, quran_hadith_substrate__state_hybrid, base_extractiveness, 32, 0.31).
narrative_ontology:measurement(qura_be_t40, quran_hadith_substrate__state_hybrid, base_extractiveness, 40, 0.34).
narrative_ontology:measurement(qura_be_t48, quran_hadith_substrate__state_hybrid, base_extractiveness, 48, 0.35).
narrative_ontology:measurement(qura_be_t56, quran_hadith_substrate__state_hybrid, base_extractiveness, 56, 0.36).
narrative_ontology:measurement(qura_be_t64, quran_hadith_substrate__state_hybrid, base_extractiveness, 64, 0.37).
narrative_ontology:measurement(qura_be_t72, quran_hadith_substrate__state_hybrid, base_extractiveness, 72, 0.375).
narrative_ontology:measurement(qura_be_t80, quran_hadith_substrate__state_hybrid, base_extractiveness, 80, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_hadith_substrate__state_hybrid, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(qura_su_t8, quran_hadith_substrate__state_hybrid, suppression_requirement, 8, 0.47).
narrative_ontology:measurement(qura_su_t16, quran_hadith_substrate__state_hybrid, suppression_requirement, 16, 0.56).
narrative_ontology:measurement(qura_su_t24, quran_hadith_substrate__state_hybrid, suppression_requirement, 24, 0.51).
narrative_ontology:measurement(qura_su_t32, quran_hadith_substrate__state_hybrid, suppression_requirement, 32, 0.58).
narrative_ontology:measurement(qura_su_t40, quran_hadith_substrate__state_hybrid, suppression_requirement, 40, 0.67).
narrative_ontology:measurement(qura_su_t48, quran_hadith_substrate__state_hybrid, suppression_requirement, 48, 0.58).
narrative_ontology:measurement(qura_su_t56, quran_hadith_substrate__state_hybrid, suppression_requirement, 56, 0.62).
narrative_ontology:measurement(qura_su_t64, quran_hadith_substrate__state_hybrid, suppression_requirement, 64, 0.69).
narrative_ontology:measurement(qura_su_t72, quran_hadith_substrate__state_hybrid, suppression_requirement, 72, 0.6).
narrative_ontology:measurement(qura_su_t80, quran_hadith_substrate__state_hybrid, suppression_requirement, 80, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__state_hybrid, enforcement_mechanism).
narrative_ontology:affects_constraint(quran_hadith_substrate__state_hybrid, quran_hadith_substrate__traditionalist_taqlid).
narrative_ontology:affects_constraint(quran_hadith_substrate__state_hybrid, quran_hadith_substrate__reformist_ijtihad).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'sharia in modern states' decomposes, per the epsilon-invariance principle, into three readings of one kernel. This file carries the state_hybrid reading; its epsilon (0.38) is indexed to the state's selective-adoption arrangement as referent. The upstream sibling (traditionalist_taqlid) supplies the classical rulings this reading selectively adopts — the hybrid cites school consensus as evidence of fidelity while truncating its scope — so the influence edge to taqlid runs through co-optation pressure on scholarly institutions. The edge to reformist_ijtihad runs through appropriation-and-censorship: reformist methods are adopted where commercial flexibility pays and suppressed where they indict the selection mechanism. Each sibling gets its own epsilon, beneficiaries, and victims in its own file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
