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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: quran_hadith_substrate__state_hybrid
 *   human_readable: State Hybrid Islamic Law: Selective Classical/Reformist Application
 *   domain: legal/religious/political
 *
 * SUMMARY:
 *   This reading instantiates the state hybrid framing of Quranic and
 *   Hadithic jurisprudential authority. The state claims to apply classical
 *   Islamic law where it aligns with state interests and governance capacity
 *   (family, criminal) and reformist or secular frameworks where they enable
 *   state function (commercial, administrative, labor). The reading grounds
 *   legitimacy in political sovereignty — the state's right to determine
 *   which interpretive tradition applies in which domain — rather than pure
 *   doctrinal fidelity to either taqlid or ijtihad. This differs structurally
 *   from the traditionalist reading (comprehensive taqlid applied uniformly)
 *   and the reformist reading (contextual ijtihad applied everywhere). The
 *   kernel remains fixed: what is the authoritative relationship between
 *   Quranic ethics, Hadithic precedent, classical fiqh schools, and
 *   contemporary jurisprudence? This reading answers: the state decides,
 *   domain by domain, which framework serves legitimate governance needs.
 *
 * KEY AGENTS:
 *   - state_political_elites: Institutional agenda-setter; decides which tradition applies where
 *   - traditionalist_ulema: Organized payer; advocates comprehensive taqlid; identity-locked into Islamic legal scholarship
 *   - reformist_activists: Moderate-power payer; demand contextual ijtihad everywhere; constrained by state surveillance
 *   - commercial_actors: Powerful beneficiary; enjoy secular commercial frameworks; mobile exit options
 *   - conservative_constituencies: Moderate-power payer; experience cognitive dissonance from selective application; identity-locked
 *   - women subject to family law: Powerless payer; trapped under classical rulings; no meaningful exit
 *   - international observers: Analytical seat; document structural asymmetry without embedded legitimacy stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_hadith_substrate__state_hybrid, 0.38).
domain_priors:suppression_score(quran_hadith_substrate__state_hybrid, 0.62).
domain_priors:theater_ratio(quran_hadith_substrate__state_hybrid, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, extractiveness, 0.38).
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__state_hybrid, tangled_rope).
narrative_ontology:human_readable(quran_hadith_substrate__state_hybrid, "State Hybrid Islamic Law: Selective Classical/Reformist Application").
narrative_ontology:topic_domain(quran_hadith_substrate__state_hybrid, "legal/religious/political").

domain_priors:requires_active_enforcement(quran_hadith_substrate__state_hybrid).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__state_hybrid, 'ad4e39f9-b6a8-44d3-8fc4-e23db3ed6609').
narrative_ontology:cs_kernel_codification('ad4e39f9-b6a8-44d3-8fc4-e23db3ed6609', fixed_text).
narrative_ontology:cs_authority_grounding('ad4e39f9-b6a8-44d3-8fc4-e23db3ed6609', extraction).
narrative_ontology:cs_interpretation_layer_present('ad4e39f9-b6a8-44d3-8fc4-e23db3ed6609').
narrative_ontology:cs_reading_relation('ad4e39f9-b6a8-44d3-8fc4-e23db3ed6609', quran_hadith_substrate__traditionalist_taqlid, coexists_with).
narrative_ontology:cs_reading_relation('ad4e39f9-b6a8-44d3-8fc4-e23db3ed6609', quran_hadith_substrate__reformist_ijtihad, coexists_with).
narrative_ontology:cs_axiom('ad4e39f9-b6a8-44d3-8fc4-e23db3ed6609', foundational, state_sovereignty_over_interpretation).
narrative_ontology:cs_axiom_status(state_sovereignty_over_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('ad4e39f9-b6a8-44d3-8fc4-e23db3ed6609', state_sovereignty_over_interpretation, conventional).
narrative_ontology:cs_axiom('ad4e39f9-b6a8-44d3-8fc4-e23db3ed6609', secondary, domain_specific_doctrine_permissible).
narrative_ontology:cs_axiom_status(domain_specific_doctrine_permissible, holdable).
narrative_ontology:cs_axiom_grounding('ad4e39f9-b6a8-44d3-8fc4-e23db3ed6609', domain_specific_doctrine_permissible, instrumental).
narrative_ontology:cs_reference_frame('ad4e39f9-b6a8-44d3-8fc4-e23db3ed6609', state_legislative_sovereignty_framework).
narrative_ontology:cs_drift_state('ad4e39f9-b6a8-44d3-8fc4-e23db3ed6609', contemporary_transnational_pressure, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ad4e39f9-b6a8-44d3-8fc4-e23db3ed6609', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__state_hybrid, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__state_hybrid, state_political_elites).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__state_hybrid, commercial_actors).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__state_hybrid, secular_administrators).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, traditionalist_ulema).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, reformist_activists).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, conservative_constituencies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, women_affected_by_family_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and enforce the hybrid application: classical Islamic law in family (marriage, inheritance, custody) and criminal codes (hudud, qisas); reformist or secular frameworks in commercial, administrative, labor law. Justify the arrangement as respecting Islamic identity while enabling modern governance and economic function. Benefit from the legitimacy claim ('Islamic state') while preserving policy flexibility in domains that affect state revenue and development. Control the interpretation apparatus and can shift the boundary between classical and reformist domains.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, state_political_elites, agenda_setter,
    institutional, generational, arbitrage, national).

% Advocate for comprehensive taqlid — following established madhhab rulings uniformly across all domains of law. See the hybrid arrangement as truncating sharia's scope and instrumentalizing Islamic law for state convenience. Their canonical vision of unified Islamic jurisprudence is displaced by selective application. Cannot easily exit because their professional and spiritual identity is bound to Islamic legal scholarship; opposition threatens institutional standing and access to state-controlled religious positions.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, traditionalist_ulema, payer,
    organized, civilizational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__state_hybrid, traditionalist_ulema, excluded).

% Argue for contextual ijtihad across all domains: human rights norms, gender equality, and public interest should reshape classical rulings everywhere, not only in commercial law. The state's selective application blocks their critical project in family and criminal law, the most visible domains. Exit options are limited by state surveillance and legal constraints on political organizing; advocacy in these zones risks state repression.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, reformist_activists, payer,
    moderate, biographical, constrained, national).

% Operate under reformist and secular commercial law, enabling conventional finance, corporate structure, labor contracts, and administrative procedure compatible with global commerce. Benefit from predictability and flexibility that classical Islamic commercial strictures would constrain. Have substantial exit options (capital flight, operating through offshore entities, moving operations to more accommodating jurisdictions).
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, commercial_actors, beneficiary,
    powerful, biographical, mobile, national).

% Operate ministries, central banks, development agencies, and regulatory bodies under secular administrative frameworks. The hybrid arrangement permits them to govern modern economies and institutions without reformulating every regulation through classical Islamic law. They face no meaningful constraint from the classical-law domains they do not administer and benefit from operational autonomy in their own spheres.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, secular_administrators, beneficiary,
    institutional, generational, arbitrage, national).

% Expect comprehensive Islamic law and identify with the state's claim to Islamic legitimacy. When they experience classical family law (which may constrain women's rights, permit practices they associate with tradition) alongside secular commercial law that permits practices they view as un-Islamic (interest, gambling-like financial instruments), they experience cognitive dissonance. Their objections (which reading is authentic?) are displaced by the state's claim to sovereignty. Cannot easily organize exit because their identity is embedded in the national Islamic framework.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, conservative_constituencies, payer,
    moderate, biographical, identity_locked, national).

% Subject to classical Islamic family law rulings (marriage, divorce, custody, inheritance) that may restrict guardianship autonomy, divorce access, or inheritance share. The state's claim that this is 'authentic' Islamic law forecloses appeals to international norms or ijtihadic reformulation. No meaningful exit from family-law jurisdiction; resistance is framed as apostasy or Western cultural imperialism by state and traditionalist alike.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, women_affected_by_family_law, payer,
    powerless, biographical, trapped, national).

% Include human rights bodies, international law scholars, comparative legal analysts. They observe the constraint from outside, noting the structural asymmetry: classical law applied where it affects family and bodily autonomy, reformist law applied where it affects state revenue and foreign investment. They document the pattern without being embedded in the legitimacy claim.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, international_observers, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_hadith_substrate__state_hybrid, state_political_elites).
narrative_ontology:fixing_cost_class(quran_hadith_substrate__state_hybrid, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables the state to claim Islamic legitimacy and identity continuity while maintaining a governance apparatus (commercial, administrative, financial) compatible with modern state capacity and global economic participation. Solves the coordination problem of how a polity grounded in Islamic tradition can operate institutions requiring secular legal frameworks without appearing to abandon Islamic law altogether.
% TRANSFER_FUNCTION: Transfers interpretive authority from traditionalist ulema (who would enforce comprehensive taqlid) and reformist activists (who would mandate contemporary ethical reinterpretation) to state political elites. The state extracts the legitimacy benefit of 'Islamic law' in domains where it can afford to apply it (family, criminal) while retaining policy flexibility in domains that generate revenue and enable development (commercial, administrative). It moves authority downward in some domains and upward in others, concentrating interpretive power in the state's hands.
% ABSENT_VOICES: Those who would hold the state to comprehensive sharia accountability (traditionalists arguing for uniform taqlid across all law), those who would systematically apply human rights critique to family and criminal law (reformists arguing contextualism everywhere), and citizens whose understanding of 'Islamic state' is incompatible with the selective application (conservative constituencies, women subject to classical family law). Their objections are structurally excluded because the arrangement's legitimacy depends on silencing the very question of which reading is authoritative.
% DISAPPEARANCE_RATIONALE: If the state hybrid vanished, the polity would face acute legitimacy negotiation: either return to comprehensive taqlid (reorganizing commercial, administrative, labor law around classical fiqh constraints), or adopt systematic ijtihad (and defend why women's rights and human rights norms now reshape family and criminal law). The arrangement's disappearance would force explicit resolution of the kernel contest currently managed by state authority.
% FOUNDING_PROBLEM: Post-colonial Islamic states inherited — or adopted — constitutional frameworks and modern state structures while claiming Islamic identity and seeking to incorporate Islamic law. The founding problem is: how to maintain Islamic legitimacy while operating institutions (bureaucracies, commercial systems, courts of first instance) that require non-Islamic legal frameworks to function?
% FOUNDING_PROBLEM_CORROBORATION: State authorities attest the founding problem is live and the hybrid solution is pragmatic Islamic governance. Traditionalist ulema argue the problem was falsely posed — comprehensive sharia was always viable and the hybrid reflects state capture by secular elites. Reformists argue the founding problem is misdiagnosed — Islamic law is adaptable to modern norms and the real problem is state refusal to permit ijtihadic reinterpretation everywhere, not just in commercial domains. International legal scholars observe the arrangement empirically and document its structure; comparative analysis from Egypt, Pakistan, Turkey, Saudi Arabia, Iran, Malaysia, and Indonesia confirms the pattern across different Islamic governance contexts.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__state_hybrid, world_rearranges).
narrative_ontology:founding_problem_status(quran_hadith_substrate__state_hybrid, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__state_hybrid, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quran_hadith_substrate__state_hybrid, 'none', 1).
narrative_ontology:epsilon_provenance(quran_hadith_substrate__state_hybrid, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness measures at 0.38 (moderate, within the 0.25-0.45 bin of the kernel contest) because the state extracts interpretive authority and the legitimacy benefit of 'Islamic law' without surrendering policy flexibility in domains that generate state revenue or constrain economic function. The extraction is not purely coercive — state elites genuinely solve a coordination problem (how to claim Islamic identity while operating modern institutions) — but the solution concentrates power asymmetrically. Suppression rises from 0.48 to 0.68 over the interval and then moderates to 0.62, tracking the intensity of state enforcement against traditionalist and reformist challengers: early, the hybrid is pragmatic; as it matures, the state must suppress both traditionalists (who demand comprehensive taqlid) and reformists (whose critical reinterpretation threatens regime stability); later, suppression moderates as the arrangement becomes institutionalized and normalized. Theater ratio (0.58 endpoint) is elevated because the state's claim that it applies 'classical Islamic law' in family/criminal domains obscures the fact that it selects which classical rulings, applies them selectively, and disregards others — significant performative maintenance of the 'authentic Islamic law' story. The theater peaks around t=25 and remains substantial, indicating that preserving the legitimacy narrative requires ongoing effort even as the enforcement regime stabilizes.
 *
 * PERSPECTIVAL GAP:
 *   From the state's seat: the hybrid is pragmatic coordination, balancing Islamic identity with modern governance — a rope with genuine coordination value. From the traditionalist seat: the hybrid is pure extraction masquerading as Islamic law — a snare where state power displaces Islamic authority. From the reformist seat: the hybrid is selective application blocking human rights progress in critical domains — a tangled rope with asymmetric extraction in family/criminal law but coordination in commercial. From the conservative constituencies' seat: the hybrid is incoherent — claiming Islamic legitimacy while applying secular law where they see un-Islamic behavior. The engine computes all four classifications from the structural data; the authored claim (tangled_rope from the reading's own standpoint) sits between the state's benign frame and the traditionalist/target seats' adversarial frames. This divergence is exactly what the kernel contest is about.
 *
 * DIRECTIONALITY LOGIC:
 *   State political elites: d ≈ 0.1 (full beneficiary). They extract interpretive authority, legitimacy, and policy flexibility; face no meaningful exit constraints; shape the rules. Traditionalist ulema: d ≈ 0.85 (nearly full target). Their comprehensive vision is displaced; they are identity-locked into Islamic law and cannot easily exit; they bear suppression when they organize resistance. Reformists: d ≈ 0.8 (high target). Their critical project is blocked in family/criminal domains; they face state repression; constrained exit. Commercial actors: d ≈ 0.15 (beneficiary-leaning). They enjoy reformist/secular frameworks; face no extraction; have mobile exit options. Secular administrators: d ≈ 0.05 (beneficiary). They operate autonomously in their domains; benefit from institutional flexibility. Conservative constituencies: d ≈ 0.7 (target). They experience suppression of their interpretation (no systematic ijtihad), trapped in cognitive dissonance, identity-locked. Women in family law: d ≈ 0.95 (nearly full target). They are subject to classical rulings they cannot exit; no alternatives available; trapped and identity-locked by the state's framing of 'Islamic law.'
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to claim Islamic identity while operating modern institutions) was live at state formation and remains contested. The hybrid arrangement does not resolve the underlying kernel contest — which reading of Quranic/Hadithic authority is correct? — but rather sidesteps it by claiming state sovereignty over interpretation. This is mandatrophy material: the founding coordination problem (we need both Islamic legitimacy and modern governance capacity) persists, but the solution mechanism (state selective application) actually prevents resolution by suppressing both traditionalist and reformist voices that might offer comprehensive alternatives. The suppression trajectory (rising to t=25, then moderating) tracks the regime's commitment to maintaining the mandatrophy: early enforcement against challengers, later institutionalization so the suppression becomes invisible. A genuine resolution would either adopt comprehensive taqlid or systematic ijtihad; the hybrid prevents both, making it a mandatrophy-vulnerable arrangement unless one treats political sovereignty over interpretation as itself a legitimate doctrinal solution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_doctrinal_legitimacy,
    'Is political sovereignty a valid grounds for determining Islamic law application across domains, or does Islamic jurisprudence require doctrinal consistency across all governance spheres?',
    'Analysis of whether other Islamic governance traditions (historical caliphates, modern religious authorities) have accepted sovereignty-based selective application, or whether they insist on doctrinal unity; examination of whether Islamic political theory (al-Mawardi, Ibn Taymiyyah, modern scholars) permits this arrangement.',
    'If sovereignty IS accepted grounds for selective application, the state_hybrid reading becomes legitimate within Islamic jurisprudence itself; if it is NOT, the arrangement is extractive state power masked by Islamic framing, and the constraint reclassifies toward snare. This omega directly affects the claim/metric divergence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_vs_doctrinal_legitimacy, conceptual, 'Whether state political sovereignty can ground selective Islamic law application').

omega_variable(
    regime_instrumentalization_vs_pragmatic_necessity,
    'Does the state''s selective application reflect genuine pragmatic necessity (you cannot operate a modern central bank under classical Islamic prohibitions on interest) or regime instrumentalization (the state chooses selective application to avoid constraints on political power)?',
    'Comparative analysis across Islamic governance contexts: do different regimes apply the same rules, or does selective application correlate with state revenue sources and political threat vectors? Examination of whether states have attempted comprehensive taqlid or ijtihadic frameworks and what prevented adoption.',
    'If pragmatic necessity: the coordination benefit is real and the tangled_rope typing is correct. If regime instrumentalization: the extraction is higher and the constraint trends toward snare. This omega addresses whether the beneficiary structure is describing genuine coordination or power asymmetry.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regime_instrumentalization_vs_pragmatic_necessity, empirical, 'Whether selective application is pragmatically necessary or politically chosen').

omega_variable(
    suppression_mechanism_identity_vs_structural,
    'Is the measured suppression (0.62) primarily structural (state legal enforcement against traditionalist organizing, surveillance of reformist networks) or internalized (traditionalists and reformists self-censor because they have fused their identity with the framework)?',
    'Post-state-change suppression trajectory: if traditionalists and reformists continue organizing against Islamic law after state enforcement ends, suppression is structural; if organizing ceases, suppression is internalized. Examination of exile communities and diaspora networks.',
    'If structural: the state could reduce suppression via legal liberalization; if internalized: exit itself does not resolve the suppression because the agents carry the framework with them. This affects whether the constraint can be reformed through political change or requires deeper cultural shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_identity_vs_structural, empirical, 'Is suppression of traditionalists/reformists structural enforcement or internalized identity fusion?').

omega_variable(
    kernel_reading_boundary_clarity,
    'Is the boundary between state_hybrid (this reading), traditionalist_taqlid (sibling), and reformist_ijtihad (sibling) a genuine structural difference, or is the state_hybrid reading logically unstable — collapsing toward either taqlid (in family/criminal) or ijtihad (in commercial) under scrutiny?',
    'Detailed examination of specific rulings: does the state apply classical taqlid rulings exactly as traditionalist schools define them, or does it modify them? Does it apply reformist ijtihad systematically in commercial law or apply secular law that happens to align with some reformist arguments? If neither pure taqlid nor pure ijtihad, the reading is distinct; if the state oscillates between them or blends them incoherently, the reading is unstable.',
    'If the reading is logically unstable, the constraint may be better modeled as two separate constraints (one per domain) or as a piton (an incoherent arrangement maintained theatrically). This omega directly addresses whether the claimed_type (tangled_rope) is defensible or whether classification should shift.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_boundary_clarity, conceptual, 'Whether state_hybrid is a stable reading or collapses toward taqlid or ijtihad').

omega_variable(
    women_and_powerless_agents_consent,
    'Can the constraint be classified as coordination (tangled_rope implies both coordination and extraction; both are necessary features) when some agents (women in family law, powerless constituencies) have zero meaningful exit and cannot consent even in principle?',
    'Theoretical: does coordination require at least the possibility of consent, or can a system coordinate some agents while suppressing others? Empirical: do women and conservative constituencies report the arrangement as coordination (however asymmetric) or as pure coercion? Do legal reform movements from these constituencies frame the issue as ''we want different law'' (disagreement) or ''we want exit options'' (coercion)? ',
    'If zero-exit agents cannot participate in coordination, the arrangement may not qualify as tangled_rope at all but as snare (with a coordination shell for some agents and pure extraction for others). This omega addresses whether the constraint''s heterogeneous directionality vector can coexist in one tangled_rope typing or requires decomposition into multiple constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(women_and_powerless_agents_consent, preference, 'Can zero-exit agents participate in coordination, or does their powerlessness reclassify the constraint toward snare?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__state_hybrid, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_hadith_substrate__state_hybrid, theater_ratio, 0, 0.42).
narrative_ontology:measurement_basis(qura_tr_t0, observed).
narrative_ontology:measurement(qura_tr_t8, quran_hadith_substrate__state_hybrid, theater_ratio, 8, 0.48).
narrative_ontology:measurement_basis(qura_tr_t8, observed).
narrative_ontology:measurement(qura_tr_t16, quran_hadith_substrate__state_hybrid, theater_ratio, 16, 0.54).
narrative_ontology:measurement_basis(qura_tr_t16, observed).
narrative_ontology:measurement(qura_tr_t25, quran_hadith_substrate__state_hybrid, theater_ratio, 25, 0.62).
narrative_ontology:measurement_basis(qura_tr_t25, observed).
narrative_ontology:measurement(qura_tr_t35, quran_hadith_substrate__state_hybrid, theater_ratio, 35, 0.61).
narrative_ontology:measurement_basis(qura_tr_t35, observed).
narrative_ontology:measurement(qura_tr_t50, quran_hadith_substrate__state_hybrid, theater_ratio, 50, 0.58).
narrative_ontology:measurement_basis(qura_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_hadith_substrate__state_hybrid, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(qura_be_t0, observed).
narrative_ontology:measurement(qura_be_t8, quran_hadith_substrate__state_hybrid, base_extractiveness, 8, 0.32).
narrative_ontology:measurement_basis(qura_be_t8, observed).
narrative_ontology:measurement(qura_be_t16, quran_hadith_substrate__state_hybrid, base_extractiveness, 16, 0.38).
narrative_ontology:measurement_basis(qura_be_t16, observed).
narrative_ontology:measurement(qura_be_t25, quran_hadith_substrate__state_hybrid, base_extractiveness, 25, 0.42).
narrative_ontology:measurement_basis(qura_be_t25, observed).
narrative_ontology:measurement(qura_be_t35, quran_hadith_substrate__state_hybrid, base_extractiveness, 35, 0.39).
narrative_ontology:measurement_basis(qura_be_t35, observed).
narrative_ontology:measurement(qura_be_t50, quran_hadith_substrate__state_hybrid, base_extractiveness, 50, 0.38).
narrative_ontology:measurement_basis(qura_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_hadith_substrate__state_hybrid, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(qura_su_t0, observed).
narrative_ontology:measurement(qura_su_t8, quran_hadith_substrate__state_hybrid, suppression_requirement, 8, 0.54).
narrative_ontology:measurement_basis(qura_su_t8, observed).
narrative_ontology:measurement(qura_su_t16, quran_hadith_substrate__state_hybrid, suppression_requirement, 16, 0.61).
narrative_ontology:measurement_basis(qura_su_t16, observed).
narrative_ontology:measurement(qura_su_t25, quran_hadith_substrate__state_hybrid, suppression_requirement, 25, 0.68).
narrative_ontology:measurement_basis(qura_su_t25, observed).
narrative_ontology:measurement(qura_su_t35, quran_hadith_substrate__state_hybrid, suppression_requirement, 35, 0.64).
narrative_ontology:measurement_basis(qura_su_t35, observed).
narrative_ontology:measurement(qura_su_t50, quran_hadith_substrate__state_hybrid, suppression_requirement, 50, 0.62).
narrative_ontology:measurement_basis(qura_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__state_hybrid, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(quran_hadith_substrate__state_hybrid, 0.22).
narrative_ontology:affects_constraint(quran_hadith_substrate__state_hybrid, quran_hadith_substrate__traditionalist_taqlid).
narrative_ontology:affects_constraint(quran_hadith_substrate__state_hybrid, quran_hadith_substrate__reformist_ijtihad).

% DUAL FORMULATION NOTE:
% Part of the quran_hadith_substrate kernel family (three constraint stories, one kernel, three readings). The state_hybrid reading competes with traditionalist_taqlid and reformist_ijtihad as competing answers to the same kernel question: what is the authoritative relationship between Quranic/Hadithic authority and contemporary Islamic law? The state_hybrid reading grounds authority in political sovereignty rather than doctrinal consensus or ethical reinterpretation. The three readings are structurally distinct constraints with different ε, beneficiary/victim structures, and type classifications. Shared referent: the standing arrangement of Islamic law authority in modern Islamic states. Different readings → different referents for what counts as the arrangement being evaluated (state discretion vs. taqlid consensus vs. ijtihadic ethics).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quran_hadith_substrate__state_hybrid, organized, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
