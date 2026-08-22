% ============================================================================
% CONSTRAINT STORY: quran_ontological_status__uncreated_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_ontological_status__uncreated_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: quran_ontological_status__uncreated_reading
 *   human_readable: Doctrine of the Uncreated Eternal Qur'an (Kalam Allah Qadim)
 *   domain: religious/theological/political
 *
 * SUMMARY:
 *   The doctrine that the Qur'an is God's own eternal speech — uncreated,
 *   coeternal with Him — entered institutional constraint space as an
 *   ontological fact rather than a human arrangement: on this reading,
 *   revelation is a permanent feature of reality, and textual meaning is
 *   fixed divine fact rather than contingent artifact. The story traces the
 *   doctrine's operation from its post-inquisition consolidation (mid-ninth
 *   century) to the present: a position whose adherents were flogged and
 *   imprisoned under the ninth-century inquisition became, within a
 *   generation, the enforced orthodoxy of the Sunni world, maintained since
 *   through madrasa curricula, creedal formulae, judicial doctrine, and state
 *   religious law. The claim/metrics gap is deliberate and is the measurement
 *   this story exists to take: the reading CLAIMS mountain (an ontic
 *   constraint needing no defenders), while the authored metrics describe an
 *   arrangement with identifiable beneficiaries, identifiable payers, a
 *   documented enforcement history, and accumulating extraction — the engine
 *   evaluates whether the self-presentation survives the structure. KEY
 *   AGENTS (by structural relationship): - traditional_jurists: agenda-setter
 *   and principal collector (institutional/identity_locked) — administer
 *   courts, curricula, and certification; office, lineage-warrant, and
 *   livelihood flow through the doctrine -
 *   literalist_hermeneutic_communities: beneficiary
 *   (organized/identity_locked) — plain-sense method carries divine warrant
 *   under the doctrine - anti_rationalist_theological_schools: beneficiary
 *   (organized/identity_locked) — transmitted knowledge outranks speculation
 *   - rationalist_theologians: primary payer (moderate/constrained) — bear
 *   censure and exclusion for subjecting the text to demonstration -
 *   metaphorical_interpreters: payer (moderate/constrained) — devout insiders
 *   censured for figurative method - reform_movements: payer
 *   (organized/constrained) — seek reopened questions inside narrowed bounds
 *   - state_religious_apparatuses: secondary agenda-setter
 *   (institutional/mobile) — carry the doctrine into statute and classroom,
 *   adjusting intensity with politics - ordinary_believers: beneficiary with
 *   diffuse conformity costs (powerless/identity_locked) -
 *   academic_historians_of_islam: analytical observer — see the full
 *   consolidation record
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__uncreated_reading, 0.64).
domain_priors:suppression_score(quran_ontological_status__uncreated_reading, 0.58).
domain_priors:theater_ratio(quran_ontological_status__uncreated_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, extractiveness, 0.64).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__uncreated_reading, mountain).
narrative_ontology:human_readable(quran_ontological_status__uncreated_reading, "Doctrine of the Uncreated Eternal Qur'an (Kalam Allah Qadim)").
narrative_ontology:topic_domain(quran_ontological_status__uncreated_reading, "religious/theological/political").

domain_priors:requires_active_enforcement(quran_ontological_status__uncreated_reading).
domain_priors:emerges_naturally(quran_ontological_status__uncreated_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__uncreated_reading, 'c771a62f-e520-40d6-bfe2-454681e60042').
narrative_ontology:cs_kernel_codification('c771a62f-e520-40d6-bfe2-454681e60042', fixed_text).
narrative_ontology:cs_authority_grounding('c771a62f-e520-40d6-bfe2-454681e60042', lineage).
narrative_ontology:cs_interpretation_layer_present('c771a62f-e520-40d6-bfe2-454681e60042').
narrative_ontology:cs_reading_relation('c771a62f-e520-40d6-bfe2-454681e60042', quran_ontological_status__created_reading, forecloses).
narrative_ontology:cs_reading_relation('c771a62f-e520-40d6-bfe2-454681e60042', quran_ontological_status__state_enforced_creation_reading, forecloses).
narrative_ontology:cs_axiom('c771a62f-e520-40d6-bfe2-454681e60042', foundational, divine_speech_uncreated_coeternal).
narrative_ontology:cs_axiom_status(divine_speech_uncreated_coeternal, holdable).
narrative_ontology:cs_axiom_grounding('c771a62f-e520-40d6-bfe2-454681e60042', divine_speech_uncreated_coeternal, theological).
narrative_ontology:cs_axiom('c771a62f-e520-40d6-bfe2-454681e60042', secondary, revealed_meaning_fixed_not_contingent).
narrative_ontology:cs_axiom_status(revealed_meaning_fixed_not_contingent, holdable).
narrative_ontology:cs_axiom_grounding('c771a62f-e520-40d6-bfe2-454681e60042', revealed_meaning_fixed_not_contingent, theological).
narrative_ontology:cs_reference_frame('c771a62f-e520-40d6-bfe2-454681e60042', coeternal_divine_speech_frame).
narrative_ontology:cs_drift_state('c771a62f-e520-40d6-bfe2-454681e60042', contemporary, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('c771a62f-e520-40d6-bfe2-454681e60042', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__uncreated_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, traditional_jurists).
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, literalist_hermeneutic_communities).
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, anti_rationalist_theological_schools).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, rationalist_theologians).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, metaphorical_interpreters).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, reform_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, ordinary_believers).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, ordinary_believers).
narrative_ontology:constraint_vindicates(quran_ontological_status__uncreated_reading, eternal_attributes_realism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Staff the courts, madrasas, and mufti councils that certify correct belief and practice. Their standing rests on transmitting a text whose meaning they hold to be fixed by its divine speaker; appointment, ordination, and livelihood flow through institutions that require affirmation of the doctrine. Leaving the framework would forfeit the office, the chain of transmission that warrants it, and the community standing built on both.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, traditional_jurists, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(quran_ontological_status__uncreated_reading, traditional_jurists, beneficiary).

% Communities whose religious life centers on reading the revealed text in its plain sense. The doctrine assures them that plain-sense reading reaches God's own meaning rather than a human artifact, so their method carries divine warrant; membership and marriage networks reinforce affirmation. Departing the plain-sense commitment would unsettle kin ties and self-understanding, not merely methodology.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, literalist_hermeneutic_communities, beneficiary,
    organized, biographical, identity_locked, global).

% Schools in the Hanbali and Athari lineages that reject speculative theology as innovation. The doctrine's victory over rationalist theology validated their epistemology: transmitted knowledge outranks syllogistic demonstration. Their seminaries reproduce the affirmation generationally; adopting rationalist method would dissolve the school's reason for being.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, anti_rationalist_theological_schools, beneficiary,
    organized, generational, identity_locked, regional).

% Theologically trained thinkers who subject revelation's claims to rational demonstration, heirs of the schools that held state favor during the ninth-century inquisition before losing it. Affirming the doctrine costs them their central thesis; withholding affirmation costs them posts, students, and publication venues inside religious institutions. Some work in secular universities where the cost is lower; within the religious economy they operate under permanent suspicion.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, rationalist_theologians, payer,
    moderate, biographical, constrained, global).

% Readers who take the anthropomorphic descriptions of God in the text figuratively — hand, face, throne — while remaining devout. Many affirm the doctrine's truth outright; their offense is method, not creed. They face censure from pulpit and classroom, exclusion from teaching posts, and accusations of deviation, though few face legal penalty. Exit would mean abandoning the interpretive practice that makes the text livable for them.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, metaphorical_interpreters, payer,
    moderate, biographical, constrained, national).

% Modernist and reform currents seeking to reopen questions the fixed-meaning settlement closed — inheritance shares, penal verses, the standing of earlier scriptures. They argue from inside the tradition, which narrows their available remedies: they may reweigh sources but may not demote the text's divine fixity without forfeiting their audience. State registration laws and clerical bodies police the boundary; several movements survive in diaspora or academic niches.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, reform_movements, payer,
    organized, generational, constrained, continental).

% Government ministries, directorates, and court systems that carry the doctrine into law: blasphemy and apostasy statutes, compulsory religious curricula, licensing of imams. They adjust enforcement intensity with political conditions — tightening after perceived threats, relaxing under diplomatic pressure — and bear little doctrinal cost either way, since their warrant is state sovereignty rather than the doctrine itself.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, state_religious_apparatuses, agenda_setter,
    institutional, generational, mobile, national).

% The mass of Muslims for whom the doctrine is background fact learned in childhood: it structures prayer, oath-taking, and the felt presence of a speaking God. They receive the community and meaning the doctrine anchors and pay small daily conformity costs — formulae of respect, restricted questions. Doubt is privately common and publicly costly; most never test the boundary.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, ordinary_believers, beneficiary,
    powerless, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(quran_ontological_status__uncreated_reading, ordinary_believers, payer).

% Scholars in comparative religion and Islamic studies who trace the doctrine's consolidation through manuscript, court, and curriculum records. They take no side in the creed; their accounts of the ninth-century inquisition and its reversal are cited by every party and bind none.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, academic_historians_of_islam, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_ontological_status__uncreated_reading, traditional_jurists).
narrative_ontology:fixing_cost_class(quran_ontological_status__uncreated_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Anchors a single authoritative revelation for a linguistically diverse, geographically dispersed community: fixing the text as God's own speech supplies one shared standard of orthodoxy, one warrant for judicial reliance on the text, and one boundary for communal membership that does not depend on local custom or ruler preference.
% TRANSFER_FUNCTION: Moves interpretive authority and certifying power from dissenting and rationalist readers to the juristic establishment; moves affirmation and conformity — creedal assent, restricted interpretive method, public observance — from the general body of believers to the institutions that certify orthodoxy; and moves status and livelihood to those who staff the certifying institutions.
% ABSENT_VOICES: Rationalist theologians excluded from orthodoxy-certifying bodies sit outside the madrasa and azhar credentialing system; secular and non-Muslim scholars of the text are absent from curriculum-setting councils; lay questioners — including women's study circles historically barred from the ulama — raise objections that reach the certifying institutions only through intermediaries who filter them.
% DISAPPEARANCE_RATIONALE: State constitutions naming the sharia or the Qur'an as a source of law, compulsory religious curricula, blasphemy and apostasy statutes, judicial reliance on the text's fixed meaning, and the ordination chains of the juristic class would all lose their warrant simultaneously; reform currents currently operating inside narrow bounds would broaden immediately; the certifying institutions would need to re-found authority on consent or scholarship rather than transmission.
% FOUNDING_PROBLEM: An early crisis over God's relation to His revealed word — whether affirming the Qur'an's eternality compromised divine unity, or calling it created diminished revelation — entangled with the practical question of who may speak for a text whose meaning disputants already read differently.
% FOUNDING_PROBLEM_CORROBORATION: Academic historians of Islamic doctrine — outside the beneficiary set — corroborate that the settlement allocated interpretive authority to the juristic class and that the underlying question of textual authority remains unsettled; rationalist theologians and reform scholars attest from the paying side that the arrangement still governs which questions may be opened. What the parties dispute is the warrant (divine versus historical), not the arrangement's continued operation.
narrative_ontology:disappearance_verdict(quran_ontological_status__uncreated_reading, world_rearranges).
narrative_ontology:founding_problem_status(quran_ontological_status__uncreated_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__uncreated_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quran_ontological_status__uncreated_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quran_ontological_status__uncreated_reading, 0.64, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_ontological_status__uncreated_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quran_ontological_status__uncreated_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, ExtMetricName, E),
    domain_priors:suppression_score(quran_ontological_status__uncreated_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(quran_ontological_status__uncreated_reading),
    narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(quran_ontological_status__uncreated_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.64: the doctrine channels interpretive authority to the juristic class, forecloses rationalist reconstruction of the text's meaning, and prices dissent in offices, students, and standing; it is not maximal because sincere assent is widespread and the communal good the doctrine anchors is real. Suppression 0.58 (a raw structural property, unscaled by power or scope — only extractiveness is scaled, by directionality and scope, in the engine's computation): enforcement today runs through blasphemy and apostasy statutes, credentialing exclusion, and social death rather than flogging; the scalar describes the machinery's present force. Theater 0.38: as live contestation faded, maintenance shifted toward ritualized recitation — catechism, creedal affirmation — a growing share of which is performative, though boundary-maintenance work remains real. Accessibility collapse 0.72: inside the committed framework alternatives collapse nearly completely — if the speech is God's own, treating the text as contingent artifact is not a live intra-framework option — but alternatives persisted for centuries at severe cost outside it (the rationalist schools survived roughly half a millennium), so the collapse falls short of natural-law totality. Resistance 0.60: recurrent, organized, multi-century — inquisitorial-era defiance, the rationalist schools' persistence, modern reform currents — repeatedly mounted and repeatedly contained. Temporal shape: suppression_requirement traces a decay-and-ratchet arc (inquisition aftermath 0.66, routinization trough 0.40, modern re-ratchet 0.58) as enforcement moved from bodily penalty through institutional routine to statutory machinery; base_extractiveness accumulates late as reform pressure raises the price of conformity; theater_ratio climbs monotonically as defense gives way to recitation. Identity-lock composition: the juristic lock is institutional and professional fused — the school has become its function; the literalist-community lock is relational and ideological; breaking either frame would require re-founding authority on something other than transmitted fixed meaning, which is why exit reads as unthinkable from inside. Suppression mechanism: overwhelmingly structural (statute, credentialing, social sanction) with an internalized component among believers formed in childhood, for whom doubt arrives pre-coded as sin. Coalition note: the payer seats have repeatedly coalized — the rationalist schools were organized movements, not isolated individuals — and were still contained once the certifying institutions aligned; coalition power alone did not break the arrangement.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat should compute differently. From the juristic seat the doctrine is not experienced as a constraint at all but as the condition of intelligible discourse — the ground that makes ruling, teaching, and praying with the text possible; from the rationalist and reform seats the same structure operates as prior closure — questions ruled out of order before they are asked. Ordinary believers sit nearer the middle: they receive the meaning-world the doctrine anchors and rarely price its conformity costs. The engine computes these divergences from the structural data; the authored mountain claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive d toward zero for the juristic class (their authority is constituted by the arrangement they administer — the beneficiary signal is amplified by identity_locked exit, which converts benefit into dependence), for literalist communities, and for the anti-rationalist schools. Victim declarations drive d toward one for rationalist theologians, metaphorical interpreters, and reform movements; constrained exit keeps them near the full-target end — they cannot arbitrage out without abandoning the tradition that constitutes their audience. Metaphorical interpreters are the instructive edge case: many affirm the doctrine's truth outright, yet the victim declaration correctly dominates their d, because their costs are borne precisely inside assent. Ordinary believers derive near-symmetric: genuine communal benefit, diffuse conformity cost. State religious apparatuses administer without doctrinal dependence — mobile exit places them off the extraction gradient's sharp end despite their enforcement role.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — who may speak for a fixed revelation — remains live, so no mandatrophy resolution is declared; the arrangement's function has not outlived its mandate. Two failure modes are guarded against. First, mistaking the doctrine's self-presentation (an ontic fact needing no defenders) for enforcement-independence: the false-summit signature exists precisely because a mountain claim with declared beneficiaries is how a constructed arrangement dresses as natural law, and the omega variables document that ambiguity rather than resolving it by fiat. Second, premature obsolescence: theater_ratio's slow climb tracks recitation displacing defense, and if the certifying function ever fully atrophied while creedal performance continued, the arrangement would drift toward inertial maintenance — the measurement series is what would date that transition. The classification thus prevents both reading coordination as pure extraction (the doctrine genuinely anchors a meaning-world for a large share of humanity) and reading extraction as coordination cost (the payer seats' losses are specific, documented, and enforced).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_doctrine,
    'Is the uncreated status of the Qur''an a genuine ontological fact about divine speech — a limit no party arranged and none collects from — or a constructed doctrinal arrangement whose persistence serves identifiable parties?',
    'Structural test rather than metaphysical proof: observe the arrangement''s behavior where enforcement capacity collapses (colonial disruptions, post-conflict vacuums). A constraint that persists unchanged without enforcement behaves as natural law; one that decays into open contestation behaves as constructed.',
    'Genuine-naturality evidence would support mountain certification; constructed-evidence routes the story through the false-summit signature toward a hybrid coordination/extraction classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_doctrine, conceptual, 'Whether the doctrine is natural law or constructed arrangement — the false-summit ambiguity.').

omega_variable(
    kernel_committer_structure,
    'This constraint is one reading of the kernel quran_ontological_status — what would the sibling readings change structurally, and where is the disagreement located?',
    'Committer documentation: created_reading deletes the ontic-fixity premise (text as produced artifact — hermeneutic flexibility returns, juristic monopoly weakens, this story''s beneficiary set loses its warrant); state_enforced_creation_reading couples the created doctrine to inquisition machinery (adding a victim class — uncreated-affirmers — and an enforcement surface this story''s own operation does not contain). The disagreement is located in the ontological predicate: whether ''speech'' predicated of God is eternal essence or temporal act.',
    'Adopting a sibling reading replaces this constraint wholesale — different epsilon, different victims, different type — rather than modifying it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_committer_structure, conceptual, 'Kernel/reading/sibling committer structure for the uncreated reading.').

omega_variable(
    epsilon_referent_persecution_inversion,
    'The doctrine''s adherents were persecuted before its consolidation and became administrators after it: does epsilon measure the standing arrangement (post-consolidation onward) or the full arc including the adversarial phase?',
    'Fix the referent to the standing arrangement from consolidation forward; treat the pre-consolidation inquisition as context explaining the enforcement machinery''s shape, not as part of the measured arrangement.',
    'Misdating the referent would shift measured extraction backward onto a phase when this doctrine''s holders were themselves the persecuted party, distorting the trajectory''s baseline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_referent_persecution_inversion, empirical, 'Referent anchoring for a constraint whose victim/beneficiary roles inverted historically.').

omega_variable(
    enforcement_form_vs_genuine_assent,
    'Does the modern suppression level indicate that assent remains enforcement-dependent, or has assent become self-sustaining while enforcement merely changed form (statute and social death replacing flogging)?',
    'Compare dissent and interpretive-deviance incidence across regions with matched doctrine but divergent enforcement capacity (statutory enforcement versus customary-only settings); survey private doubt against public affirmation.',
    'Enforcement-independent assent would lower effective suppression and pull the arrangement toward the coordination pole; enforcement-dependent assent sustains the measured suppression and pressures toward the extraction pole.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_form_vs_genuine_assent, empirical, 'Whether measured suppression reflects needed coercion or habitual machinery.').

omega_variable(
    jurist_identity_lock_composition,
    'What composes the juristic class''s identity lock — professional (career path dependence), relational (community standing), ideological (creedal conviction), or institutional (the school has become its function)?',
    'Track defectors: jurists who publicly revised their position — which costs they actually paid, which ties held, whether re-founding authority on non-transmitted bases occurred.',
    'A predominantly institutional lock means the arrangement survives individual defection (classification robust); a predominantly ideological lock means mass conviction shift could break it quickly (classification fragile).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(jurist_identity_lock_composition, conceptual, 'Composition of the identity lock binding the administrator-beneficiary seat.').

omega_variable(
    cs_framing_under_determination,
    'Is the declared framing — kernel equals the ontological doctrine, authority grounded in lineage (transmission chains) — the only defensible one, or is the real kernel the ulama''s transmission-based charismatic authority with the doctrine as its instrument?',
    'Test framing substitution: if the kernel is re-declared as the legitimacy claim of the transmitting class, authority_grounding shifts from lineage toward extraction (authority fed by preventing kernel revision), and the classification of the authority structure changes accordingly.',
    'Under the alternative framing the arrangement reads as authority-extraction in doctrinal dress rather than doctrine administered through lineage — a materially different verdict about the same historical facts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_under_determination, conceptual, 'Framing under-determination in the commitment-system declaration.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__uncreated_reading, 0, 1170).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quran_uncreated_tr_t0, quran_ontological_status__uncreated_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(quran_uncreated_tr_t150, quran_ontological_status__uncreated_reading, theater_ratio, 150, 0.17).
narrative_ontology:measurement(quran_uncreated_tr_t300, quran_ontological_status__uncreated_reading, theater_ratio, 300, 0.2).
narrative_ontology:measurement(quran_uncreated_tr_t450, quran_ontological_status__uncreated_reading, theater_ratio, 450, 0.24).
narrative_ontology:measurement(quran_uncreated_tr_t600, quran_ontological_status__uncreated_reading, theater_ratio, 600, 0.27).
narrative_ontology:measurement(quran_uncreated_tr_t750, quran_ontological_status__uncreated_reading, theater_ratio, 750, 0.29).
narrative_ontology:measurement(quran_uncreated_tr_t900, quran_ontological_status__uncreated_reading, theater_ratio, 900, 0.31).
narrative_ontology:measurement(quran_uncreated_tr_t1050, quran_ontological_status__uncreated_reading, theater_ratio, 1050, 0.34).
narrative_ontology:measurement(quran_uncreated_tr_t1170, quran_ontological_status__uncreated_reading, theater_ratio, 1170, 0.38).

% Extraction over time
narrative_ontology:measurement(quran_uncreated_be_t0, quran_ontological_status__uncreated_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(quran_uncreated_be_t150, quran_ontological_status__uncreated_reading, base_extractiveness, 150, 0.5).
narrative_ontology:measurement(quran_uncreated_be_t300, quran_ontological_status__uncreated_reading, base_extractiveness, 300, 0.55).
narrative_ontology:measurement(quran_uncreated_be_t450, quran_ontological_status__uncreated_reading, base_extractiveness, 450, 0.58).
narrative_ontology:measurement(quran_uncreated_be_t600, quran_ontological_status__uncreated_reading, base_extractiveness, 600, 0.56).
narrative_ontology:measurement(quran_uncreated_be_t750, quran_ontological_status__uncreated_reading, base_extractiveness, 750, 0.55).
narrative_ontology:measurement(quran_uncreated_be_t900, quran_ontological_status__uncreated_reading, base_extractiveness, 900, 0.54).
narrative_ontology:measurement(quran_uncreated_be_t1050, quran_ontological_status__uncreated_reading, base_extractiveness, 1050, 0.59).
narrative_ontology:measurement(quran_uncreated_be_t1170, quran_ontological_status__uncreated_reading, base_extractiveness, 1170, 0.64).

% Suppression requirement over time
narrative_ontology:measurement(quran_uncreated_su_t0, quran_ontological_status__uncreated_reading, suppression_requirement, 0, 0.66).
narrative_ontology:measurement(quran_uncreated_su_t150, quran_ontological_status__uncreated_reading, suppression_requirement, 150, 0.52).
narrative_ontology:measurement(quran_uncreated_su_t300, quran_ontological_status__uncreated_reading, suppression_requirement, 300, 0.47).
narrative_ontology:measurement(quran_uncreated_su_t450, quran_ontological_status__uncreated_reading, suppression_requirement, 450, 0.45).
narrative_ontology:measurement(quran_uncreated_su_t600, quran_ontological_status__uncreated_reading, suppression_requirement, 600, 0.43).
narrative_ontology:measurement(quran_uncreated_su_t750, quran_ontological_status__uncreated_reading, suppression_requirement, 750, 0.42).
narrative_ontology:measurement(quran_uncreated_su_t900, quran_ontological_status__uncreated_reading, suppression_requirement, 900, 0.4).
narrative_ontology:measurement(quran_uncreated_su_t1050, quran_ontological_status__uncreated_reading, suppression_requirement, 1050, 0.49).
narrative_ontology:measurement(quran_uncreated_su_t1170, quran_ontological_status__uncreated_reading, suppression_requirement, 1170, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__uncreated_reading, identity_coordination).
narrative_ontology:affects_constraint(quran_ontological_status__uncreated_reading, quran_ontological_status__created_reading).
narrative_ontology:affects_constraint(quran_ontological_status__uncreated_reading, quran_ontological_status__state_enforced_creation_reading).

% DUAL FORMULATION NOTE:
% 'The Qur'an's ontological status' is a colloquial label covering three structurally distinct constraints, decomposed per the epsilon-invariance principle: created_reading (the minimal doctrinal claim — the Qur'an as created speech), state_enforced_creation_reading (downstream coupling of that claim to inquisition-style state coercion, adding a victim class this story's own operation lacks), and this file's uncreated_reading (the rival branch that won institutional power and enforced its own settlement). Each story carries its own epsilon, beneficiaries, and victims; the edges here record family kinship, not shared measurement. Upstream/downstream: the created claim is the minimal premise both enforced variants elaborate; this reading shaped its siblings' operating environment historically — its post-inquisition victory raised the cost of holding the created view for centuries.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
