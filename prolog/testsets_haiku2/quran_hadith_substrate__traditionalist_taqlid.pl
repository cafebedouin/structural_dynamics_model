% ============================================================================
% CONSTRAINT STORY: quran_hadith_substrate__traditionalist_taqlid
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_hadith_substrate__traditionalist_taqlid, []).

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
 *   constraint_id: quran_hadith_substrate__traditionalist_taqlid
 *   human_readable: Classical Fiqh Authority & Taqlid Obligation (Traditionalist Reading)
 *   domain: religious/legal/institutional
 *
 * SUMMARY:
 *   This is the traditionalist reading of the contested kernel: Qur'an/hadith
 *   interpretation and Islamic legal authority. Under this reading, the early
 *   establishment of four canonical Sunni schools (Hanafi, Maliki, Shafi'i,
 *   Hanbali) and recognized Shi'a jurisprudential traditions represents
 *   divinely-guided consensus (ijma). Contemporary Muslims are religiously
 *   obligated to follow a chosen school's rulings via taqlid (imitative
 *   compliance) rather than performing independent legal reasoning (ijtihad),
 *   which is reserved to a qualified scholarly elite. The constraint operates
 *   as both genuine coordination (unified jurisprudential frameworks enabling
 *   Muslim legal practice) and asymmetric extraction (institutional control
 *   over interpretation, suppression of reform-minded alternatives, legal
 *   immobilization of women and minorities under classical rulings). The
 *   measurement series and coercion grid track the intensification of
 *   enforcement mechanisms from 1400 to 2026: the theater ratio rises
 *   (increasing share of enforcement activity defending institutional
 *   authority rather than serving coordination), extractiveness grows
 *   (margins of extraction widen as classical rulings diverge from
 *   contemporary ethics), and suppression intensifies (especially at
 *   organizational and individual levels as traditionalist institutions
 *   harden against reformist alternatives).
 *
 * KEY AGENTS:
 *   - ulama_establishment — institutional power, identity-locked to classical scholarship; sets authoritative positions and benefits from taqlid-based deference
 *   - madhhab_institutions — institutional power, arbitrage-mobile in principle but locked to classical frameworks in practice; collectively capture institutional prestige and financial resources
 *   - laypeople_following_taqlid — powerless, identity-locked (religious identity fused with madhhab membership); receive coordination benefit but bear cost of legal immobility
 *   - progressive_muslims — moderate power, constrained exit; suppressed by institutional labeling of contextualized ijtihad as bid'a (innovation)
 *   - women_seeking_equality — powerless, identity-locked; trapped in classical rulings on marriage, divorce, inheritance
 *   - religious_minorities — powerless, trapped; subject to classical dhimmi frameworks; no formal voice in jurisprudential discourse
 *   - reformist_ijtihad_scholars — powerful globally, but excluded from traditionalist institutional authority; operate in parallel legitimacy structures
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_hadith_substrate__traditionalist_taqlid, 0.68).
domain_priors:suppression_score(quran_hadith_substrate__traditionalist_taqlid, 0.77).
domain_priors:theater_ratio(quran_hadith_substrate__traditionalist_taqlid, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, extractiveness, 0.68).
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 0.77).
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__traditionalist_taqlid, tangled_rope).
narrative_ontology:human_readable(quran_hadith_substrate__traditionalist_taqlid, "Classical Fiqh Authority & Taqlid Obligation (Traditionalist Reading)").
narrative_ontology:topic_domain(quran_hadith_substrate__traditionalist_taqlid, "religious/legal/institutional").

domain_priors:requires_active_enforcement(quran_hadith_substrate__traditionalist_taqlid).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__traditionalist_taqlid, '4f8c9377-6a6a-4de7-ba94-ad41cf455dab').
narrative_ontology:cs_kernel_codification('4f8c9377-6a6a-4de7-ba94-ad41cf455dab', fixed_text).
narrative_ontology:cs_authority_grounding('4f8c9377-6a6a-4de7-ba94-ad41cf455dab', lineage).
narrative_ontology:cs_interpretation_layer_present('4f8c9377-6a6a-4de7-ba94-ad41cf455dab').
narrative_ontology:cs_reading_relation('4f8c9377-6a6a-4de7-ba94-ad41cf455dab', quran_hadith_substrate__reformist_ijtihad, forecloses).
narrative_ontology:cs_reading_relation('4f8c9377-6a6a-4de7-ba94-ad41cf455dab', quran_hadith_substrate__state_hybrid, coexists_with).
narrative_ontology:cs_axiom('4f8c9377-6a6a-4de7-ba94-ad41cf455dab', foundational, classical_ijma_binding).
narrative_ontology:cs_axiom_status(classical_ijma_binding, holdable).
narrative_ontology:cs_axiom_grounding('4f8c9377-6a6a-4de7-ba94-ad41cf455dab', classical_ijma_binding, deontological).
narrative_ontology:cs_axiom('4f8c9377-6a6a-4de7-ba94-ad41cf455dab', foundational, taqlid_obligatory_laypeople).
narrative_ontology:cs_axiom_status(taqlid_obligatory_laypeople, holdable).
narrative_ontology:cs_axiom_grounding('4f8c9377-6a6a-4de7-ba94-ad41cf455dab', taqlid_obligatory_laypeople, conventional).
narrative_ontology:cs_axiom('4f8c9377-6a6a-4de7-ba94-ad41cf455dab', secondary, madhhab_schools_divinely_guided).
narrative_ontology:cs_axiom_status(madhhab_schools_divinely_guided, holdable).
narrative_ontology:cs_axiom_grounding('4f8c9377-6a6a-4de7-ba94-ad41cf455dab', madhhab_schools_divinely_guided, theological).
narrative_ontology:cs_reference_frame('4f8c9377-6a6a-4de7-ba94-ad41cf455dab', classical_consensus_authority).
narrative_ontology:cs_drift_state('4f8c9377-6a6a-4de7-ba94-ad41cf455dab', contemporary_pluralistic_islam, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4f8c9377-6a6a-4de7-ba94-ad41cf455dab', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__traditionalist_taqlid, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, ulama_establishment).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, madhhab_institutions).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, mosque_hierarchies).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, progressive_muslims).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, women_seeking_equality).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, religious_minorities).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, laypeople_without_taqlid_access).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, laypeople_following_taqlid).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, laypeople_following_taqlid).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Recognized religious scholars who interpret, transmit, and enforce classical fiqh rulings. They set the authoritative positions within their respective madhhabs and determine what constitutes valid taqlid. Their authority is grounded in chains of transmission (isnad) and institutional recognition. They benefit from taqlid-based deference: their rulings are binding without reexamination by followers, and their institutional position depends on the constraint's persistence.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, ulama_establishment, agenda_setter,
    institutional, generational, identity_locked, global).

% The four major Sunni schools (Hanafi, Maliki, Shafi'i, Hanbali) and Shi'a jurisprudential traditions maintain curricula, certify scholars, and transmit authoritative positions. They collect institutional prestige, financial resources (endowments, student fees, institutional patronage), and structural authority over Islamic legal interpretation. Their survival depends on the taqlid framework remaining binding.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, madhhab_institutions, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__traditionalist_taqlid, madhhab_institutions, agenda_setter).

% Mosque committees and imams operationalize classical rulings in local practice. They benefit from having a fixed, established jurisprudence to transmit rather than continuously adjudicating each issue. They derive authority and stability from representing an established madhhab tradition rather than improvising on each question.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, mosque_hierarchies, beneficiary,
    organized, biographical, constrained, regional).

% Ordinary Muslims obligated to follow a chosen madhhab's rulings without independent legal reasoning (ijtihad). They receive the coordination benefit of clear, stable, religiously sanctioned guidance. They bear the cost of legal immobility: unable to seek alternative rulings when contemporary circumstances clash with classical positions; unable to access ijtihad because it is reserved to qualified scholars. Their identity as 'good Muslims' is fused with following the madhhab they are born into or choose, making exit psychologically costly.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, laypeople_following_taqlid, payer,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__traditionalist_taqlid, laypeople_following_taqlid, beneficiary).

% Muslims seeking to reconcile Islamic ethics with contemporary human rights norms, gender equality, or scientific evidence. Traditionalist taqlid renders their interpretations illegitimate within orthodox frameworks. They are suppressed by institutional religious authority structures that label contextualized ijtihad as bid'a (innovation). Many seek alternative readings but face social and familial pressure, fatwa-based condemnation, and exclusion from mainstream mosque spaces.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, progressive_muslims, payer,
    moderate, biographical, constrained, global).

% Muslim women bound by classical fiqh rulings on marriage, divorce, inheritance, and testimony that institutionalize unequal legal status. Under taqlid, these rulings are treated as binding consensus. Women seeking reform face suppression from both institutional ulama and family structures. Their exit option is either accepting classical status or leaving Islam altogether—both psychologically costly given identity fusion. Even when contemporary maslaha (public interest) arguments support reform, taqlid frameworks treat classical positions as immutable.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, women_seeking_equality, payer,
    powerless, biographical, identity_locked, local).

% Non-Muslims living under classical Islamic law in Muslim-majority contexts where taqlid-based fiqh is institutionalized. Classical madhhab rulings include dhimmi (protected non-Muslim) frameworks establishing unequal legal status, restricted rights, and subordinate social position. Where state law enforces traditionalist fiqh, minorities cannot exit; where they can emigrate, the cost is displacement and loss of homeland.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, religious_minorities, payer,
    powerless, biographical, trapped, local).

% Islamic scholars advocating contextual reinterpretation of classical texts for contemporary ethics and circumstances. Within traditionalist taqlid frameworks, they are excluded from authoritative jurisprudential discourse: their ijtihad is labeled illegitimate or heretical by establishment ulama. They operate in parallel institutions (reform-oriented madrasas, NGOs, academic settings) and through popular media but lack the institutional enforcement machinery the classical schools possess.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, reformist_ijtihad_scholars, excluded,
    powerful, biographical, constrained, global).

% Secular legal frameworks and state institutions that would apply civil law to matters the traditionalist fiqh claims to govern. In secular states, they have arbitrage (civil divorce, civil marriage, secular inheritance). In traditionalist-dominant states, they are excluded from formal authority over these domains; in hybrid states, they operate in parallel but subordinate to religious law in family matters.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, state_secular_actors, excluded,
    powerful, generational, arbitrage, national).

% Contemporary Islamic organizations that variously defend traditionalist fiqh, advocate selective reform, or synthesize classical and modern frameworks. They take multiple positions on taqlid, from institutional defense to critical engagement. They observe how the constraint shapes legitimacy and practice across Muslim communities.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__traditionalist_taqlid, islamic_revival_movements, observer,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_hadith_substrate__traditionalist_taqlid, madhhab_institutions).
narrative_ontology:fixing_cost_class(quran_hadith_substrate__traditionalist_taqlid, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable, recognized jurisprudential framework for Islamic legal practice that all followers within a madhhab can rely on without each individual performing independent legal reasoning. Solves the coordination problem of ensuring that Islamic communities operate under consistent, authority-backed rulings rather than fragmenting into personalized interpretations. Reduces the cognitive burden on laypeople by delegating legal determination to qualified scholars.
% TRANSFER_FUNCTION: Transfers religious and legal authority from the Qur'an and hadith (which are open to multiple interpretations) to the established madhhab schools and their recognized scholars. Moves decision-making power from individuals (who cannot perform ijtihad) to institutional authorities (the ulama and school hierarchies). Channels religious prestige, institutional resources, and social influence to beneficiary institutions. Extracts obedience and deference from followers, who must accept rulings without reexamination. Restricts women, minorities, and progressive Muslims to subordinate legal status by treating classical rulings as immutable consensus.
% ABSENT_VOICES: Reformist scholars who advocate contextual ijtihad; Muslim women whose voices are silenced in classical jurisprudence; religious minorities under classical dhimmi frameworks who have no seat in the interpretation process; contemporary Muslim majorities (outside the scholarly elite) whose lived circumstances clash with classical rulings but have no mechanism to raise them for reconsideration. These actors would object to the constraint's suppression of alternative readings and its enforcement of outdated legal frameworks, but are structurally excluded from authoritative discourse.
% DISAPPEARANCE_RATIONALE: If taqlid obligation vanished overnight, Muslim communities would immediately fragment into competing ijtihadi positions; the institutional authority of madhhab schools would collapse; women and minorities would seek alternative legal frameworks or reform-oriented scholars; progressive Muslims would establish parallel jurisprudential traditions; institutional ulama would lose their monopoly on authoritative interpretation; mosque and madrasa hierarchies would reorganize around different legitimacy claims. The Islamic legal landscape would shift from centralized institutional authority to pluralistic, contestable interpretation.
% FOUNDING_PROBLEM: Early Islamic communities faced legal pluralism and fragmenting interpretations of Qur'an and hadith. Establishing authoritative schools with consensus-based rulings solved the coordination problem: unified frameworks allowed large Muslim populations to operate under shared legal understanding, grounded in scholarship and institutional transmission rather than individual whim. The four Sunni schools emerged through this consolidation process (late 2nd/3rd century AH), creating stable jurisprudential traditions.
% FOUNDING_PROBLEM_CORROBORATION: Islamic historians and traditionalist ulama attest the founding problem was real and the madhhab consolidation solved coordination failure. However, contemporary reformist scholars and human-rights advocates attest that the founding problem is substantially resolved by modern communications, written legal codes, and the existence of multiple schools—yet taqlid obligation persists as institutional enforcement of classical positions rather than genuine need for centralized authority. Sociological analysis shows that taqlid now functions primarily as institutional legitimacy maintenance rather than coordination solution. No neutral external party substantiates the ongoing necessity of taqlid obligation in contemporary contexts; only traditionalist institutions claim the founding problem remains live.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__traditionalist_taqlid, world_rearranges).
narrative_ontology:founding_problem_status(quran_hadith_substrate__traditionalist_taqlid, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__traditionalist_taqlid, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quran_hadith_substrate__traditionalist_taqlid, 'none', 1).
narrative_ontology:epsilon_provenance(quran_hadith_substrate__traditionalist_taqlid, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_hadith_substrate__traditionalist_taqlid_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quran_hadith_substrate__traditionalist_taqlid, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quran_hadith_substrate__traditionalist_taqlid_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness climbs from 0.45 (1400) to 0.68 (2026) as the margin between classical rulings and contemporary ethics widens: the same constraint that coordinated a fragmented 15th-century Islamic world now enforces outdated legal frameworks on a globalized, pluralistic community. Theater ratio rises from 0.25 to 0.42 because a growing proportion of enforcement activity defends institutional authority and suppresses reformist alternatives rather than solving original coordination problems. Suppression increases from 0.52 to 0.77 as traditionalist institutions strengthen enforcement mechanisms (fatwa-based condemnation, mosque hierarchies, madrasas, state law enforcement in Muslim-majority countries) to counter reformist pressure. The coercion grid shows asymmetric intensification: organizational-level suppression (0.52→0.81) outpaces individual-level suppression (0.55→0.78) because institutions harden while some individuals access reform-oriented alternatives through diaspora communities and digital networks. Resistance rises steeply (0.25→0.62 at structural level) as reformist scholarship, feminist Islamic movements, and secular state actors challenge traditionalist monopoly. The constraint computes as tangled_rope because genuine coordination function persists alongside asymmetric extraction: Muslims do receive stable, authority-backed legal guidance (the coordination good), but institutional control over that guidance produces extractive asymmetries that now far exceed the cost of providing stable interpretation.
 *
 * PERSPECTIVAL GAP:
 *   The ulama_establishment and madhhab_institutions perceive the constraint as pure coordination: they have maintained jurisprudential stability and guided Muslim practice for centuries, and taqlid obligation ensures their rulings are followed faithfully. From their seat, the constraint is a beneficiary-favoring rope. From the laypeople_following_taqlid seat, especially women and minorities, the constraint computes as snare-adjacent: genuine coordination benefit exists, but institutional suppression of alternatives and legal immobilization create extraction that the coordination good does not justify. From the reformist_ijtihad_scholars seat (powerful but excluded), the constraint computes as pure snare: it is institutional gate-keeping that suppresses legitimate alternative interpretation while claiming to represent divine consensus. From progressive_muslims and women_seeking_equality (powerless, identity-locked), the constraint computes as high-extraction snare masked by coordination framing. The engine computes per-seat types from this divergent directionality data.
 *
 * DIRECTIONALITY LOGIC:
 *   Ulama and madhhab institutions are full beneficiaries (d ≈ 0.15): they collect institutional authority, prestige, financial resources, and deference without bearing costs of ijtihadi reasoning (they are exempt from taqlid themselves). Laypeople_following_taqlid are symmetric-to-target (d ≈ 0.55): genuine coordination benefit (stable rulings), but also bearing the cost of legal immobility and constrained judgment. Progressive_muslims are target (d ≈ 0.85): suppressed alternatives, institutional condemnation, social pressure. Women and minorities are target (d ≈ 0.90): trapped by classical legal frameworks, no mechanism for reform, identity-lock prevents exit. Reformist_ijtihad_scholars are target (d ≈ 0.80): powerful globally, but excluded from institutional authority, forced into parallel legitimacy structures. State_secular_actors are excluded (role='excluded'): arbitrage in secular contexts, trapped in traditionalist-dominant contexts, not part of the coordination problem the constraint purports to solve.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — early Islamic legal fragmentation — was genuine and the taqlid constraint solved it by establishing institutional consensus-based schools. The founding problem is now contested-to-dead: modern communications, written legal codes, multiple competing schools, and the existence of reformist scholarship show that centralized taqlid is not functionally necessary for Islamic legal coordination in contemporary contexts. Yet the constraint persists with intensifying enforcement (suppression 0.52→0.77) because institutional beneficiaries have strong incentives to maintain it. The measurement series shows rising theater_ratio (0.25→0.42): as the founding problem attrophies, an increasing share of enforcement activity defends institutional authority rather than solving coordination needs. This is the mandatrophy trajectory: a coordination solution whose original function is no longer live, but which persists due to institutional inertia and beneficiary capture. The constraint shows piton-adjacent characteristics (atrophied coordination function, rising theater) but retains enough active institutional enforcement to maintain snare-like extraction, landing it in tangled_rope: genuine coordination benefit for laypeople (still real) combined with institutional suppression of alternatives and legal immobilization of women/minorities (intensifying extraction). The coercion grid (organizational suppression 0.52→0.81 outpacing individual suppression 0.55→0.78) shows the institutional investment in maintaining the constraint even as individual-level alternatives proliferate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ijma_consensus_vs_institutional_capture,
    'Does the classical consensus (ijma) of the madhhab schools represent genuine scholarly agreement, or has it been institutionally captured by the ulama establishment to defend their authority?',
    'Historiographical analysis: compare the process by which early consensus emerged (9th-10th century AH) versus contemporary defense of classical rulings. If contemporary defense invokes ijma as static principle but historical evidence shows ijma as dynamic process of scholarly contestation, then institutional capture is indicated. Contemporary reformist scholars attempting ijtihad demonstrate that the scholarly disagreements underlying classical schools are still live, contradicting the ''binding consensus'' framing.',
    'If genuine consensus, the constraint legitimately represents scholarly agreement and taqlid carries coordination value. If institutionally captured, the constraint is primarily extractive gate-keeping disguised as consensus-defense. This shifts classification from tangled_rope toward snare and raises the true suppression metric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ijma_consensus_vs_institutional_capture, empirical, 'Whether classical ijma represents genuine scholarly consensus or has been institutionally monopolized').

omega_variable(
    identity_lock_internalization,
    'Is the measured suppression of reformist-minded Muslims structural (institutional barriers, fatwa-based exclusion, denial of mosque platforms) or internalized (followers have psychologically fused their Islamic identity with taqlid obedience and cannot imagine alternative readings)?',
    'Post-exit analysis: if Muslims who physically leave traditionalist communities (diaspora, emigration to secular states, digital-community participation) rapidly adopt reformist or personalized ijtihadi positions, then suppression was largely structural and externally maintained. If they retain taqlid-obedience patterns even after the structural barriers are removed, internalization is substantial. Empirical witness: diaspora Muslim communities that encounter reformist scholarship often adopt it; this suggests structural rather than deeply internalized suppression.',
    'If structural, exit is more accessible than the locked-identity codification suggests; the identity-lock status might downgrade to ''constrained'' for some actors, lowering effective suppression. If substantially internalized, the institutional enforcement creates persistent psychological barriers even after structural barriers are removed, and true suppression is higher than the structural measure alone captures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_internalization, empirical, 'Whether suppression of reformist alternatives is structural or internalized').

omega_variable(
    natural_law_vs_constructed_reading,
    'Is the traditionalist taqlid obligation grounded in the Qur''an and hadith as divinely-mandated command, or is it a constructed institutional framework that benefits from being presented as natural law?',
    'Exegetical analysis: the Qur''an and hadith do not explicitly command followers to adopt one of four canonical schools or forbid ijtihad. The obligation to follow taqlid is derived from interpretations of verses on obedience to authority and following the ''straight path.'' Compare these derivations against reformist exegesis that emphasizes Qur''anic encouragement of reasoning (tadabbur) and hadith permission for ijad (struggling to derive correct ruling). If the natural-law reading requires suppressing reform exegeses through institutional authority rather than through scriptural obviousness, then it is constructed.',
    'If natural law, the constraint is a mountain: unavoidable structure of Islamic practice. If constructed, it is subject to re-reading and reform; classification shifts toward tangled_rope or snare, and mandatrophy analysis becomes urgent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_reading, conceptual, 'Whether taqlid obligation is divinely commanded or constructed institutional arrangement').

omega_variable(
    kernel_foreclosure_vs_coexistence,
    'Does the traditionalist reading''s core claim (classical schools represent binding ijma) logically foreclose the reformist reading''s core claim (contemporary ijtihad mandatory when ethics conflict), or do both remain live options for different Islamic communities?',
    'Logical analysis: if a Muslim simultaneously holds ''classical rulings are binding consensus'' AND ''scholars must reinterpret when ethics conflict,'' are they in contradiction? Yes—within any single framework, one cannot privilege both static consensus and dynamic reinterpretation. However, the empirical fact is that different Muslim communities worldwide are organized around traditionalist and reformist readings simultaneously. This suggests the readings are not logically related (one foreclosing the other) but rather sustained by different institutional power structures and different communities. If foreclosure were operative, we would expect one reading to eliminate the other; instead, they coexist via institutional separation.',
    'If foreclosure applies, one reading is destined to eliminate the other (strategic implication: traditionalist institutions are in zero-sum competition with reformist institutions). If coexistence is the stable pattern, the kernel is genuinely contested and multiple readings are live (strategic implication: the competition is for institutional authority and community allegiance, not for logical truth). The reading_relations declaration in cs_structure depends on this resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_foreclosure_vs_coexistence, conceptual, 'Logical relationship between traditionalist taqlid and reformist ijtihad readings').

omega_variable(
    state_enforcement_amplification,
    'How much of the measured increase in suppression (0.52→0.77) is due to traditionalist institutions'' own enforcement capacity versus amplification by state institutions enforcing traditionalist fiqh in law?',
    'Institutional analysis across Muslim-majority countries: compare suppression intensity in contexts where state law enforces traditionalist fiqh (Saudi Arabia, Iran classical periods, Sudan) versus contexts where state is secular but traditionalist community institutions remain strong (Turkey, some Arab countries) versus contexts where traditionalist institutions have minimal enforcement infrastructure (diaspora communities in Western countries). If state enforcement drives suppression intensity, suppression should vary dramatically across these contexts; if institutional ulama capacity is primary, suppression should remain higher even where state support is withdrawn.',
    'High state amplification means suppression is contingent on political regime; reform possibilities open when state power shifts. Low state amplification means traditionalist institutions have captured enforcement independently; reform requires institutional change within Islam itself, not just political change. Coercion grid shows organizational-level suppression (0.52→0.81) outpacing individual-level: if state amplification is primary, organizational level should show the strongest intensification (state machinery operates at organizational level). The data pattern supports this, suggesting state enforcement is significant but not total—institutions themselves are strengthening.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_enforcement_amplification, empirical, 'Degree to which state power amplifies traditionalist institutional suppression').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__traditionalist_taqlid, 1400, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trad_taqlid_tr_t1400, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 1400, 0.25).
narrative_ontology:measurement(trad_taqlid_tr_t1600, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 1600, 0.28).
narrative_ontology:measurement(trad_taqlid_tr_t1800, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 1800, 0.31).
narrative_ontology:measurement(trad_taqlid_tr_t1920, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 1920, 0.36).
narrative_ontology:measurement(trad_taqlid_tr_t1980, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 1980, 0.4).
narrative_ontology:measurement(trad_taqlid_tr_t2010, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 2010, 0.41).
narrative_ontology:measurement(trad_taqlid_tr_t2026, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 2026, 0.42).

% Extraction over time
narrative_ontology:measurement(trad_taqlid_be_t1400, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 1400, 0.45).
narrative_ontology:measurement(trad_taqlid_be_t1600, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 1600, 0.48).
narrative_ontology:measurement(trad_taqlid_be_t1800, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 1800, 0.52).
narrative_ontology:measurement(trad_taqlid_be_t1920, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 1920, 0.58).
narrative_ontology:measurement(trad_taqlid_be_t1980, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 1980, 0.64).
narrative_ontology:measurement(trad_taqlid_be_t2010, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 2010, 0.67).
narrative_ontology:measurement(trad_taqlid_be_t2026, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 2026, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(trad_taqlid_su_t1400, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 1400, 0.52).
narrative_ontology:measurement(trad_taqlid_su_t1600, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 1600, 0.55).
narrative_ontology:measurement(trad_taqlid_su_t1800, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 1800, 0.6).
narrative_ontology:measurement(trad_taqlid_su_t1920, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 1920, 0.68).
narrative_ontology:measurement(trad_taqlid_su_t1980, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 1980, 0.74).
narrative_ontology:measurement(trad_taqlid_su_t2010, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 2010, 0.76).
narrative_ontology:measurement(trad_taqlid_su_t2026, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 2026, 0.77).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1400, tn=2026
narrative_ontology:measurement(trad_taqlid_grid_01, quran_hadith_substrate__traditionalist_taqlid, accessibility_collapse(class), 1400, 0.62).
narrative_ontology:measurement(trad_taqlid_grid_02, quran_hadith_substrate__traditionalist_taqlid, accessibility_collapse(class), 2026, 0.71).
narrative_ontology:measurement(trad_taqlid_grid_03, quran_hadith_substrate__traditionalist_taqlid, accessibility_collapse(individual), 1400, 0.58).
narrative_ontology:measurement(trad_taqlid_grid_04, quran_hadith_substrate__traditionalist_taqlid, accessibility_collapse(individual), 2026, 0.65).
narrative_ontology:measurement(trad_taqlid_grid_05, quran_hadith_substrate__traditionalist_taqlid, accessibility_collapse(organizational), 1400, 0.75).
narrative_ontology:measurement(trad_taqlid_grid_06, quran_hadith_substrate__traditionalist_taqlid, accessibility_collapse(organizational), 2026, 0.82).
narrative_ontology:measurement(trad_taqlid_grid_07, quran_hadith_substrate__traditionalist_taqlid, accessibility_collapse(structural), 1400, 0.68).
narrative_ontology:measurement(trad_taqlid_grid_08, quran_hadith_substrate__traditionalist_taqlid, accessibility_collapse(structural), 2026, 0.74).
narrative_ontology:measurement(trad_taqlid_grid_09, quran_hadith_substrate__traditionalist_taqlid, resistance(class), 1400, 0.18).
narrative_ontology:measurement(trad_taqlid_grid_10, quran_hadith_substrate__traditionalist_taqlid, resistance(class), 2026, 0.61).
narrative_ontology:measurement(trad_taqlid_grid_11, quran_hadith_substrate__traditionalist_taqlid, resistance(individual), 1400, 0.28).
narrative_ontology:measurement(trad_taqlid_grid_12, quran_hadith_substrate__traditionalist_taqlid, resistance(individual), 2026, 0.52).
narrative_ontology:measurement(trad_taqlid_grid_13, quran_hadith_substrate__traditionalist_taqlid, resistance(organizational), 1400, 0.22).
narrative_ontology:measurement(trad_taqlid_grid_14, quran_hadith_substrate__traditionalist_taqlid, resistance(organizational), 2026, 0.58).
narrative_ontology:measurement(trad_taqlid_grid_15, quran_hadith_substrate__traditionalist_taqlid, resistance(structural), 1400, 0.25).
narrative_ontology:measurement(trad_taqlid_grid_16, quran_hadith_substrate__traditionalist_taqlid, resistance(structural), 2026, 0.62).
narrative_ontology:measurement(trad_taqlid_grid_17, quran_hadith_substrate__traditionalist_taqlid, stakes_inflation(class), 1400, 0.52).
narrative_ontology:measurement(trad_taqlid_grid_18, quran_hadith_substrate__traditionalist_taqlid, stakes_inflation(class), 2026, 0.71).
narrative_ontology:measurement(trad_taqlid_grid_19, quran_hadith_substrate__traditionalist_taqlid, stakes_inflation(individual), 1400, 0.62).
narrative_ontology:measurement(trad_taqlid_grid_20, quran_hadith_substrate__traditionalist_taqlid, stakes_inflation(individual), 2026, 0.76).
narrative_ontology:measurement(trad_taqlid_grid_21, quran_hadith_substrate__traditionalist_taqlid, stakes_inflation(organizational), 1400, 0.48).
narrative_ontology:measurement(trad_taqlid_grid_22, quran_hadith_substrate__traditionalist_taqlid, stakes_inflation(organizational), 2026, 0.64).
narrative_ontology:measurement(trad_taqlid_grid_23, quran_hadith_substrate__traditionalist_taqlid, stakes_inflation(structural), 1400, 0.55).
narrative_ontology:measurement(trad_taqlid_grid_24, quran_hadith_substrate__traditionalist_taqlid, stakes_inflation(structural), 2026, 0.68).
narrative_ontology:measurement(trad_taqlid_grid_25, quran_hadith_substrate__traditionalist_taqlid, suppression(class), 1400, 0.45).
narrative_ontology:measurement(trad_taqlid_grid_26, quran_hadith_substrate__traditionalist_taqlid, suppression(class), 2026, 0.76).
narrative_ontology:measurement(trad_taqlid_grid_27, quran_hadith_substrate__traditionalist_taqlid, suppression(individual), 1400, 0.55).
narrative_ontology:measurement(trad_taqlid_grid_28, quran_hadith_substrate__traditionalist_taqlid, suppression(individual), 2026, 0.78).
narrative_ontology:measurement(trad_taqlid_grid_29, quran_hadith_substrate__traditionalist_taqlid, suppression(organizational), 1400, 0.52).
narrative_ontology:measurement(trad_taqlid_grid_30, quran_hadith_substrate__traditionalist_taqlid, suppression(organizational), 2026, 0.81).
narrative_ontology:measurement(trad_taqlid_grid_31, quran_hadith_substrate__traditionalist_taqlid, suppression(structural), 1400, 0.48).
narrative_ontology:measurement(trad_taqlid_grid_32, quran_hadith_substrate__traditionalist_taqlid, suppression(structural), 2026, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__traditionalist_taqlid, identity_coordination).
narrative_ontology:boltzmann_floor_override(quran_hadith_substrate__traditionalist_taqlid, 0.12).
narrative_ontology:affects_constraint(quran_hadith_substrate__traditionalist_taqlid, quran_hadith_substrate__reformist_ijtihad).
narrative_ontology:affects_constraint(quran_hadith_substrate__traditionalist_taqlid, quran_hadith_substrate__state_hybrid).

% DUAL FORMULATION NOTE:
% This story is one reading of a contested kernel: the interpretation and authority structure for Islamic law grounded in Qur'an and hadith. The traditionalist_taqlid reading claims classical schools represent binding consensus requiring followers to accept madhhab rulings via taqlid. The reformist_ijtihad reading claims contemporary scholars must perform contextual reinterpretation when classical rulings conflict with contemporary ethics and maslaha. The state_hybrid reading claims state political authority can select which classical rulings to enforce and which to supersede. These are NOT measurements of the same constraint—they are structurally different constraints with different ε values, different beneficiary/victim sets, and different institutional persistence mechanisms. The sibling readings are separate JSON files linked via network.affects_constraints. The disagreement is not about measurement perspective; it is about the referent itself (what obligation rules Islamic legal interpretation), and each reading has its own ε-invariant description of that referent. Decomposition per OQ-47 (ε-invariance principle): when a natural-language concept (Islamic legal authority) covers multiple structurally distinct claims (one of which instantiates binding taqlid, another of which instantiates mandatory ijtihad, a third of which instantiates state authority), author separate constraint stories for each. Do not force one story to handle all three readings via measurement-basis flexibility or observable selection; each reading gets its own ε, its own stakeholders, and its own classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
