% ============================================================================
% CONSTRAINT STORY: jihad_quranic_corpus__revolutionary_vanguard_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jihad_quranic_corpus__revolutionary_vanguard_reading, []).

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
 *   constraint_id: jihad_quranic_corpus__revolutionary_vanguard_reading
 *   human_readable: Revolutionary Vanguard Jihad Doctrine: Fard 'Ayn via Takfir and Emergency Jurisprudence
 *   domain: religious/political-theological
 *
 * SUMMARY:
 *   The kernel is the jihad corpus of the Qur'an and sunna as transmitted
 *   through classical jurisprudence; three readings compete for it. This
 *   story instantiates the revolutionary_vanguard_reading: the doctrinal
 *   arrangement, codified from Qutb's Milestones (1964) through Faraj's The
 *   Neglected Duty (1979) to the ISIS-era applications, under which armed
 *   jihad is an immediate individual obligation (fard 'ayn) against rulers
 *   declared apostate and against occupying forces, self-authorized by each
 *   believer, with the classical preconditions (imam authorization,
 *   invitation, non-combatant immunity) suspended by emergency jurisprudence
 *   and the target set expanded by takfir. Its structural deltas against the
 *   siblings: apostate Muslims and occupiers enter the victim set; authority
 *   decentralizes to the individual believer and the emergent emir; the state
 *   monopoly on legitimate violence is eliminated; emergency doctrine
 *   overrides the classical safeguards; civilians become combatants via
 *   collective guilt. Per the epsilon-referent rule for kernel-reading
 *   stories, epsilon is authored for the standing arrangement under contest,
 *   this doctrine as it actually operates, not for the classical arrangement
 *   the reading claims to restore and not for any sibling reading. The
 *   claim/metrics independence rule is observed: the reading presents itself
 *   as a restored divine obligation, while the authored metrics describe a
 *   constructed, actively enforced arrangement with the widest victim set in
 *   the kernel family; the engine measures that divergence rather than the
 *   story reconciling it.
 *
 * KEY AGENTS:
 *   - vanguard_leadership: agenda-setting seat (organized/identity_locked) — authors the doctrine's applications, declares takfir, collects manpower, money, and interpretive authority
 *   - vanguard_fighters: dual seat, payer and beneficiary (moderate/identity_locked) — execute operations; pay in bodies and futures, collect meaning, status, salvation
 *   - muslim_civilians_conflict_zones: primary civilian target (powerless/trapped) — made liable by collective-guilt reasoning
 *   - dissenting_muslim_scholars: institutional objectors (institutional/constrained) — declared apostates, assassinated, delegitimized
 *   - apostate_rulers: declared targets (institutional/constrained) — stripped of religious legitimacy, violence licensed against them
 *   - occupying_forces: declared targets (institutional/mobile) — the doctrine's trigger condition and primary enemy
 *   - rival_jihadist_factions: same-corpus competitors (organized/trapped) — takfir-war casualties, outside the doctrine's authority
 *   - global_muslim_communities: the claimed constituency (organized/constrained) — acted for without consent; latent target set
 *   - comparative_religion_scholars: analytical observer (analytical/analytical) — maps the doctrine against the tradition it claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.84).
domain_priors:suppression_score(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.78).
domain_priors:theater_ratio(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, extractiveness, 0.84).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__revolutionary_vanguard_reading, tangled_rope).
narrative_ontology:human_readable(jihad_quranic_corpus__revolutionary_vanguard_reading, "Revolutionary Vanguard Jihad Doctrine: Fard 'Ayn via Takfir and Emergency Jurisprudence").
narrative_ontology:topic_domain(jihad_quranic_corpus__revolutionary_vanguard_reading, "religious/political-theological").

domain_priors:requires_active_enforcement(jihad_quranic_corpus__revolutionary_vanguard_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__revolutionary_vanguard_reading, '167cacc1-8e8b-47b5-9fa9-81e52d22de4d').
narrative_ontology:cs_kernel_codification('167cacc1-8e8b-47b5-9fa9-81e52d22de4d', fixed_text).
narrative_ontology:cs_authority_grounding('167cacc1-8e8b-47b5-9fa9-81e52d22de4d', distributed).
narrative_ontology:cs_reading_relation('167cacc1-8e8b-47b5-9fa9-81e52d22de4d', jihad_quranic_corpus__defensive_spiritual_reading, coexists_with).
narrative_ontology:cs_reading_relation('167cacc1-8e8b-47b5-9fa9-81e52d22de4d', jihad_quranic_corpus__expansionist_legalist_reading, influences).
narrative_ontology:cs_axiom('167cacc1-8e8b-47b5-9fa9-81e52d22de4d', foundational, emergency_self_declaration_licenses_fard_ayn).
narrative_ontology:cs_axiom_status(emergency_self_declaration_licenses_fard_ayn, holdable).
narrative_ontology:cs_axiom_grounding('167cacc1-8e8b-47b5-9fa9-81e52d22de4d', emergency_self_declaration_licenses_fard_ayn, empirically_contingent).
narrative_ontology:cs_axiom('167cacc1-8e8b-47b5-9fa9-81e52d22de4d', foundational, takfir_of_apostate_rulers_licenses_resistance).
narrative_ontology:cs_axiom_status(takfir_of_apostate_rulers_licenses_resistance, holdable).
narrative_ontology:cs_axiom_grounding('167cacc1-8e8b-47b5-9fa9-81e52d22de4d', takfir_of_apostate_rulers_licenses_resistance, theological).
narrative_ontology:cs_axiom('167cacc1-8e8b-47b5-9fa9-81e52d22de4d', secondary, collective_guilt_extends_liability_to_civilians).
narrative_ontology:cs_axiom_status(collective_guilt_extends_liability_to_civilians, holdable).
narrative_ontology:cs_axiom_grounding('167cacc1-8e8b-47b5-9fa9-81e52d22de4d', collective_guilt_extends_liability_to_civilians, conventional).
narrative_ontology:cs_reference_frame('167cacc1-8e8b-47b5-9fa9-81e52d22de4d', salaf_unmediated_obligation_model).
narrative_ontology:cs_drift_state('167cacc1-8e8b-47b5-9fa9-81e52d22de4d', contemporary_post_caliphate_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('167cacc1-8e8b-47b5-9fa9-81e52d22de4d', '').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__revolutionary_vanguard_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__revolutionary_vanguard_reading, vanguard_leadership).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__revolutionary_vanguard_reading, vanguard_fighters).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, apostate_rulers).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, occupying_forces).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, muslim_civilians_conflict_zones).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, dissenting_muslim_scholars).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, global_muslim_communities).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, rival_jihadist_factions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, vanguard_fighters).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__revolutionary_vanguard_reading, qutbist_vanguard_theory).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__revolutionary_vanguard_reading, faraj_neglected_duty_doctrine).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__revolutionary_vanguard_reading, permanent_emergency_jurisprudence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ideologues and commanders who author the doctrine's applications: they declare which rulers are apostate, which populations are liable, and which rivals are unbelievers; they issue operational guidance, run training and financing networks, and speak in the name of the ummah without seeking its consent. What flows to them: manpower including foreign fighters, donations and taxation where territory is held, and ultimate interpretive authority, since every act performed under the doctrine's banner redounds to whoever speaks for it. Leaving is not a live option: the doctrine they administer defines departure as apostasy, and its own machinery treats apostates as legitimate targets; their biographies, networks, and claimed salvation are fused with the doctrine's truth.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, vanguard_leadership, agenda_setter,
    organized, generational, identity_locked, global).

% Rank-and-file militants, disproportionately young men, many recruited as teenagers or conscripted in territories the vanguard holds. They execute operations, bombings, assassinations, frontline fighting, and suicide missions, under commanders they did not choose. What flows to them: brotherhood, status, a guaranteed place in paradise, sometimes salary and bride-price. What flows from them: their bodies, their futures, and in suicide operations their lives, spent at rates the leadership sets. The doctrine compresses their horizon to the immediate: obligation is now, reward is now, deliberation is doubt. Exit runs through apostasy, since desertion is unbelief and the movement kills deserters; many also face prosecution at home if they return.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, vanguard_fighters, payer,
    moderate, immediate, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__revolutionary_vanguard_reading, vanguard_fighters, beneficiary).

% People living in the theaters where the doctrine operates: markets, mosques, funerals, weddings. The doctrine's collective-guilt reasoning counts them as combatants by proximity, by taxes paid to regimes it deems apostate, or by failure to distance themselves from occupation, and bombs follow. They cannot leave war zones, cannot publicly dissent without becoming targets themselves, and are conscripted for labor, and their children for fighting, where the vanguard governs. They hold no seat in any authority that decides their status.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, muslim_civilians_conflict_zones, payer,
    powerless, immediate, trapped, regional).

% Mainstream ulama, al-Azhar faculty, Amman Message signatories, local imams, who hold that the doctrine's takfir is forbidden, that non-combatant immunity is inviolable, and that armed jihad without legitimate authority is invalid. The doctrine answers them by declaring them apostates or scholars of the rulers: some are assassinated, in a pattern running from the 1977 killing of Shaykh al-Dahabi through imam murders in Iraq and the Sahel; others live under guard; all compete against a movement claiming their own scripture. Recanting would destroy the authority that makes their objection matter, and many cannot emigrate.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, dissenting_muslim_scholars, payer,
    institutional, generational, constrained, global).

% Rulers the doctrine has declared unbelievers, the category that made Anwar Sadat a target in 1981 and that names most heads of state in Muslim-majority countries. What flows from them under the doctrine: their religious legitimacy is stripped as rule by kufr, violence against them and their officials is licensed, and their security apparatuses become legitimate targets. They cannot concede the doctrine's frame without abdicating its core claim that their rule is Islamic, so they respond with repression, which the doctrine then cites as further evidence of apostasy.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, apostate_rulers, payer,
    institutional, generational, constrained, national).

% Foreign militaries deployed in Muslim-majority territories: the doctrine's trigger condition and primary declared target. They face insurgency shaped by the doctrine's rules of engagement, with no surrender expectation, no proportional-exchange calculus, and civilians on their side also counted as liable. Their exit is real but priced: withdrawal ends their exposure yet is framed by the doctrine as victory-confirming, so staying versus leaving is the recurring debate their presence generates in their home politics.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, occupying_forces, payer,
    institutional, biographical, mobile, regional).

% Other militant movements operating from the same corpus, nationalist-insurgent lines, Muslim Brotherhood descendants, al-Qaeda central, that reject this reading's takfir expansiveness, its priority of fighting other Muslims, or its claim to a caliphate. The doctrine treats their objection as unbelief: the intra-jihadist wars it licenses, in Iraq and Syria after 2013 and across the Sahel, have killed thousands of their fighters. They stand entirely outside the doctrine's authority structure, so their rebuttals change nothing inside it, while they bear its costs directly.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, rival_jihadist_factions, payer,
    organized, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__revolutionary_vanguard_reading, rival_jihadist_factions, excluded).

% The ummah in whose name the doctrine acts. Polling consistently shows overwhelming rejection of its methods, yet its claim to speak for Islam follows these communities everywhere: mosque attacks are justified by their governments' policies, their scholars are murdered for objecting, and each atrocity performed under the doctrine's banner raises the cost of their religious identity in non-Muslim polities. In principle the doctrine's takfir logic can reach any Muslim, which is its radical break with a tradition that nearly prohibited takfir, so the whole community lives one interpretive step from the target set. They have no vote in any body that administers the doctrine.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, global_muslim_communities, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__revolutionary_vanguard_reading, global_muslim_communities, excluded).

% Academic specialists in Islamic law, sectarian violence, and comparative religion who map the doctrine against its sources: which proof-texts it selects, which classical safeguards it discards, how its apostasy boundary has moved across six decades. They collect nothing and bear nothing under the doctrine; their seat is the outside view that distinguishes the doctrine's claims about the tradition from the tradition's own record.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jihad_quranic_corpus__revolutionary_vanguard_reading, vanguard_leadership).
narrative_ontology:fixing_cost_class(jihad_quranic_corpus__revolutionary_vanguard_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real mobilization deadlock. Classical law gates armed jihad on conditions (authorization by a legitimate ruler, invitation, capability) that are structurally unmeetable under occupation and unislamic rule, which produces inaction. The doctrine dissolves the gate: every Muslim becomes his own authorizing authority the moment he judges the emergency conditions met, converting a collective-action deadlock into distributed action. It also solves the commitment problem, why fight and die for an abstraction, with martyrdom assurance, brotherhood, and status.
% TRANSFER_FUNCTION: Moves lives, resources, and authority. From recruits: their bodies and futures, spent at rates the leadership sets. From civilian populations in the theaters: their safety, converted into coercive leverage over occupiers and regimes. From rival institutions: scholarly authority and jihadist manpower, seized by takfir. To the vanguard leadership: command, donations, taxation where territory is held, and the interpretive throne, since every operation performed under the doctrine's banner accrues to whoever speaks for it.
% ABSENT_VOICES: The ummah itself: the doctrine claims to act in its defense while its consent is neither sought nor required. Polling shows mass rejection, and that rejection has no seat inside the doctrine's authority structure. The classical scholarly consensus, which nearly prohibited takfir and conditions armed jihad on legitimate authority, is present only as a target. Women and the non-combatant majorities of affected societies are entirely absent from the doctrine's deliberations, which are confined to armed men.
% DISAPPEARANCE_RATIONALE: If the doctrine vanished overnight: the takfir wars against rival Muslims and the scholar assassinations stop, since nothing else in those conflicts licenses them; indiscriminate attacks on marketplaces and funerals lose their justification; large numbers of current recruits demobilize, because participation is obligatory only under this reading's rules. Occupiers and autocrats would still face resistance, but of the conventional insurgent kind governed by classical constraints, and mainstream jurisprudence would reabsorb the mobilization energy it currently cannot channel. Occupation itself would not end, since the grievance predates the doctrine, but who dies, and how, is doctrine-shaped.
% FOUNDING_PROBLEM: The post-1967 condition: Muslim lands under occupation, Palestine first and later Afghanistan and Iraq, with states either complicit, defeated, or unwilling to fight; rulers judged unislamic yet unremovable by any lawful means, because classical law's preconditions for rebellion and armed jihad could never be satisfied. Qutb's Milestones (1964) diagnosed the paralysis; Faraj's The Neglected Duty (1979) codified the answer: the obligation is individual and immediate, and the emergency suspends the conditions.
% FOUNDING_PROBLEM_CORROBORATION: The occupation grievance is independently documented: UN records, international-law scholarship, and the occupiers' own presence attest it. Mainstream scholars who most forcefully condemn the doctrine, the Amman Message signatories and al-Azhar among them, acknowledge the underlying grievances while rejecting its answer, and foreign-fighter studies record recruits citing occupation and regime brutality as their motive. What no party outside the movement attests is the doctrine's specific claim that a permanent emergency suspends the classical preconditions; that claim is contested by every scholarly body outside it.
narrative_ontology:disappearance_verdict(jihad_quranic_corpus__revolutionary_vanguard_reading, world_rearranges).
narrative_ontology:founding_problem_status(jihad_quranic_corpus__revolutionary_vanguard_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__revolutionary_vanguard_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jihad_quranic_corpus__revolutionary_vanguard_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.84, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jihad_quranic_corpus__revolutionary_vanguard_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jihad_quranic_corpus__revolutionary_vanguard_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jihad_quranic_corpus__revolutionary_vanguard_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.84 at interval end) because the doctrine converts believers' lives into the leadership's operational currency at rates the leadership alone sets, renders whole civilian populations liable, and seizes the authority of rival institutions; the 2003 and 2016 peaks track the trigger condition's mass activation (Iraq, then the Syrian collapse and the caliphate), not measurement noise. Suppression (0.45 rising to 0.86, settling at 0.78) is structural and organizational: the takfir machinery makes dissent apostasy and apostasy capital, deserters are killed, rival scholars are assassinated. Suppression is authored as a raw structural property, unscaled; only extractiveness is scaled by directionality and scope in the engine's computation. Theater (0.18 to 0.42) rises as the media and inspirational layer grows relative to territorial function: after the caliphate's military defeat the performative share grows while the function persists as distributed insurgency. Accessibility collapse (0.7) is frame-relative: within the doctrine's frame the alternatives collapse almost completely, since one cannot coherently hold fard 'ayn-now and wait-for-the-imam; but the collapse does not reach the wider community, where the classical alternatives persist and are actively defended, which is why the value sits below the mountain-range rather than in it. Resistance (0.8) is among the highest sustained in this corpus's neighborhood: state suppression everywhere, mainstream scholarly counter-mobilization (the Amman Message), intra-jihadist war, and mass popular rejection. The trajectory is step-shaped rather than monotone: exogenous trigger events (1979 Afghanistan, 2003 Iraq, 2011-2014 Syria) produce the jumps, and the doctrine's own operations partly manufacture later triggers, which the emergency_condition_endogeneity omega tracks. The measurement series run on one shared time grid; every tracked metric is authored at every point. Receipt and cost surface: the gains demonstrably accrue to the leadership seat, so gain_flow names it; fixing is prohibitive for every seat that could attempt it, because the trigger conditions are geopolitically locked and suppression without grievance-resolution has regenerated the arrangement in every cycle since 1979.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the leadership seat the arrangement is the ummah's rescue: obligation, authority, and salvation aligned in one structure it administers. From the fighter seat the same structure is meaning purchased with the body, the dual payer/beneficiary position the directionality override encodes. From the civilian and scholar seats it is indistinguishable from predation: the same doctrine that coordinates the vanguard's action is the instrument that makes them liable. The rival-faction and global-ummah seats experience the arrangement's authority claims with no access to its authority structure. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations: vanguard_leadership (collects authority, manpower, money; sits near the beneficiary end) and vanguard_fighters (collect meaning, status, salvation). Victim declarations: apostate_rulers, occupying_forces, muslim_civilians_conflict_zones, dissenting_muslim_scholars, global_muslim_communities, rival_jihadist_factions, the widest victim set of the kernel's three readings, which is precisely this reading's structural delta. One override is authored: the fighters' beneficiary declaration would derive a low d, but their net structural position is target-like, since what flows from them (bodies, futures, lives spent at leadership-set rates) dominates what flows to them, and their exit runs through apostasy; d is overridden to 0.60 for the moderate power atom, which in this story only the fighters occupy. Occupying forces carry mobile exit, which damps their d below the trapped victims'; civilians and scholars are trapped or constrained and sit near the full-target end; the leadership's identity lock binds it to the arrangement it administers without making it a target, since identity-lock raises effective commitment, not cost borne.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two mislabels. Reading the arrangement as pure coordination (the leadership's self-description: the ummah's obligatory rescue) would erase the victim set; the takfir wars, the scholar assassinations, and the collective-guilt bombings are not coordination costs. Reading it as pure extraction with a coordination cover would erase why it persists and recruits: the mobilization deadlock it dissolves is real, the grievance feeding it is independently documented, and its trigger conditions keep recurring. The founding problem is live and attested from outside the movement, so this is not mandate atrophy; the mandatrophy risk here runs the other way. A doctrine that declares the emergency permanent has built itself a mandate no event can retire: defeat is persecution, withdrawal is vindication, and the founding problem regenerates wherever occupation or unislamic rule appears. The post-2016 theater rise marks the early phase of that dynamic, as territorial function degrades and the arrangement increasingly performs itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the jihad_quranic_corpus kernel (reading: revolutionary_vanguard_reading). Is the fard ''ayn-via-takfir structure what the corpus itself requires, or a constructed radicalization that selects proof-texts and discards the classical tradition''s safeguards?',
    'Comparative jurisprudential analysis weighing the reading''s proof-texts against the classical consensus it overrides (the near-prohibition of takfir, imam-conditioning of offensive jihad, non-combatant immunity), and testing whether the authorities it cites, notably Ibn Taymiyya''s emergency provision, support its permanence claim or only a bounded conditional one.',
    'If the corpus does not yield this reading, the arrangement is constructed against the tradition and the sibling readings'' victim sets and authority structures are the correct baselines for comparison; if the reading''s emergency premises are textually forced, part of its structure inherits the kernel''s own authority and its extraction profile must be discounted accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Whether this reading is forced by the kernel or constructed against the classical tradition.').

omega_variable(
    emergency_condition_endogeneity,
    'Is the emergency that triggers the obligation exogenous (occupation and apostate rule existing independently of the movement) or endogenous (manufactured by the doctrine''s own operations, which provoke crackdowns, fracture states, and invite intervention)?',
    'Historical counterfactual analysis of activation episodes: compare doctrine activation where occupation predated the movement (Afghanistan 1979) against cases where vanguard operations produced the power vacuum the doctrine then filled (Syria 2011-2014; the post-2003 sectarian escalation in Iraq).',
    'If endogenous, the emergency jurisprudence is a self-licensing loop, the justification for the violence being a condition the violence produces, which pushes the arrangement toward the pure-extraction end and thins the coordination-function claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergency_condition_endogeneity, empirical, 'Whether the doctrine''s trigger conditions are independent of its own operations.').

omega_variable(
    takfir_boundary_stability,
    'Does the doctrine contain a principled limit on who can be declared apostate, or is the boundary set by operational necessity, whoever obstructs the vanguard becoming an unbeliever?',
    'Track the boundary''s movement across the doctrine''s history: Faraj restricted takfir to rulers; Zarqawi extended it to the Shia; ISIS extended it to rival jihadists, tribes, and eventually most resisters. If the boundary tracks the leadership''s enemies rather than any textual criterion, the limit is operational, not principled.',
    'If unbounded, every Muslim is latently in the victim set and the doctrine''s coordination story thins toward cover for leadership power consolidation; if bounded in principle, the hybrid coordination-and-extraction reading holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(takfir_boundary_stability, empirical, 'Whether the apostasy boundary is principled or operational.').

omega_variable(
    fighter_participation_basis,
    'What fraction of rank-and-file participation rests on doctrinal conviction rather than coercion, payment, protection, or status unavailable elsewhere to otherwise-unemployed young men?',
    'Foreign-fighter registry studies, defector interviews, and comparison of recruitment yields before and after salary collapse in defeated territories.',
    'If coercion and opportunity dominate, the fighters'' declared benefit (meaning, status, salvation) overstates their net position; their effective directionality toward the arrangement is more target-like than the beneficiary declaration suggests, and the coordination function thins.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fighter_participation_basis, empirical, 'Conviction versus coercion in the fighter rank and file.').

omega_variable(
    suppression_mechanism_composition,
    'Is the measured suppression primarily organizational coercion (hisbah enforcement, assassination of dissenters, killing of deserters) or internalized doctrinal fear (believers who police their own doubt because doubt is apostasy)?',
    'Compare suppression persistence in territories where the organization was militarily defeated but the doctrine persists (post-2017 inspirational attacks, Sahel affiliates) against territories under direct organizational control.',
    'If substantially internalized, suppression outlives enforcement capacity: dismantling the organization does not dismantle the arrangement, and the arrangement''s effective suppression is higher than any organizational measure records.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_composition, empirical, 'Organizational versus internalized suppression mechanisms.').

omega_variable(
    counterfactual_violence_accounting,
    'Does the doctrine increase total violence in its theaters, or does it mainly redirect violence that would have occurred anyway under other banners?',
    'Comparative conflict data on violence levels before and after vanguard entry into a theater, controlling for occupation intensity and state capacity, with specific attention to violence categories only this doctrine licenses (takfir wars, scholar assassinations, collective-guilt bombings).',
    'If the doctrine adds violence, its removal is net violence-reducing; if it mainly redirects, the arrangement''s cost accounting shifts toward the grievances that produce violence under any frame, changing what its persistence is evidence of.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_violence_accounting, empirical, 'Net versus redirected violence under the doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__revolutionary_vanguard_reading, 1964, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jihad_rev_vanguard_tr_t1964, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 1964, 0.18).
narrative_ontology:measurement_basis(jihad_rev_vanguard_tr_t1964, observed).
narrative_ontology:measurement(jihad_rev_vanguard_tr_t1977, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 1977, 0.22).
narrative_ontology:measurement_basis(jihad_rev_vanguard_tr_t1977, observed).
narrative_ontology:measurement(jihad_rev_vanguard_tr_t1989, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 1989, 0.26).
narrative_ontology:measurement_basis(jihad_rev_vanguard_tr_t1989, observed).
narrative_ontology:measurement(jihad_rev_vanguard_tr_t1998, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 1998, 0.28).
narrative_ontology:measurement_basis(jihad_rev_vanguard_tr_t1998, observed).
narrative_ontology:measurement(jihad_rev_vanguard_tr_t2003, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 2003, 0.3).
narrative_ontology:measurement_basis(jihad_rev_vanguard_tr_t2003, observed).
narrative_ontology:measurement(jihad_rev_vanguard_tr_t2011, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 2011, 0.33).
narrative_ontology:measurement_basis(jihad_rev_vanguard_tr_t2011, observed).
narrative_ontology:measurement(jihad_rev_vanguard_tr_t2016, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 2016, 0.38).
narrative_ontology:measurement_basis(jihad_rev_vanguard_tr_t2016, observed).
narrative_ontology:measurement(jihad_rev_vanguard_tr_t2021, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 2021, 0.44).
narrative_ontology:measurement_basis(jihad_rev_vanguard_tr_t2021, observed).
narrative_ontology:measurement(jihad_rev_vanguard_tr_t2025, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 2025, 0.42).
narrative_ontology:measurement_basis(jihad_rev_vanguard_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(jihad_rev_vanguard_be_t1964, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 1964, 0.42).
narrative_ontology:measurement_basis(jihad_rev_vanguard_be_t1964, observed).
narrative_ontology:measurement(jihad_rev_vanguard_be_t1977, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 1977, 0.58).
narrative_ontology:measurement_basis(jihad_rev_vanguard_be_t1977, observed).
narrative_ontology:measurement(jihad_rev_vanguard_be_t1989, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 1989, 0.66).
narrative_ontology:measurement_basis(jihad_rev_vanguard_be_t1989, observed).
narrative_ontology:measurement(jihad_rev_vanguard_be_t1998, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 1998, 0.7).
narrative_ontology:measurement_basis(jihad_rev_vanguard_be_t1998, observed).
narrative_ontology:measurement(jihad_rev_vanguard_be_t2003, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 2003, 0.8).
narrative_ontology:measurement_basis(jihad_rev_vanguard_be_t2003, observed).
narrative_ontology:measurement(jihad_rev_vanguard_be_t2011, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 2011, 0.76).
narrative_ontology:measurement_basis(jihad_rev_vanguard_be_t2011, observed).
narrative_ontology:measurement(jihad_rev_vanguard_be_t2016, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 2016, 0.88).
narrative_ontology:measurement_basis(jihad_rev_vanguard_be_t2016, observed).
narrative_ontology:measurement(jihad_rev_vanguard_be_t2021, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 2021, 0.82).
narrative_ontology:measurement_basis(jihad_rev_vanguard_be_t2021, observed).
narrative_ontology:measurement(jihad_rev_vanguard_be_t2025, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 2025, 0.84).
narrative_ontology:measurement_basis(jihad_rev_vanguard_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(jihad_rev_vanguard_su_t1964, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 1964, 0.45).
narrative_ontology:measurement_basis(jihad_rev_vanguard_su_t1964, observed).
narrative_ontology:measurement(jihad_rev_vanguard_su_t1977, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 1977, 0.6).
narrative_ontology:measurement_basis(jihad_rev_vanguard_su_t1977, observed).
narrative_ontology:measurement(jihad_rev_vanguard_su_t1989, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 1989, 0.66).
narrative_ontology:measurement_basis(jihad_rev_vanguard_su_t1989, observed).
narrative_ontology:measurement(jihad_rev_vanguard_su_t1998, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 1998, 0.68).
narrative_ontology:measurement_basis(jihad_rev_vanguard_su_t1998, observed).
narrative_ontology:measurement(jihad_rev_vanguard_su_t2003, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 2003, 0.78).
narrative_ontology:measurement_basis(jihad_rev_vanguard_su_t2003, observed).
narrative_ontology:measurement(jihad_rev_vanguard_su_t2011, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 2011, 0.74).
narrative_ontology:measurement_basis(jihad_rev_vanguard_su_t2011, observed).
narrative_ontology:measurement(jihad_rev_vanguard_su_t2016, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 2016, 0.86).
narrative_ontology:measurement_basis(jihad_rev_vanguard_su_t2016, observed).
narrative_ontology:measurement(jihad_rev_vanguard_su_t2021, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 2021, 0.8).
narrative_ontology:measurement_basis(jihad_rev_vanguard_su_t2021, observed).
narrative_ontology:measurement(jihad_rev_vanguard_su_t2025, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 2025, 0.78).
narrative_ontology:measurement_basis(jihad_rev_vanguard_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jihad_quranic_corpus__revolutionary_vanguard_reading, identity_coordination).
narrative_ontology:affects_constraint(jihad_quranic_corpus__revolutionary_vanguard_reading, defensive_spiritual_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__revolutionary_vanguard_reading, expansionist_legalist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the jihad_quranic_corpus kernel decomposes into three readings with distinct victim sets, authority structures, and epsilon values. This revolutionary vanguard reading carries the widest victim set and the highest extraction of the three; the defensive spiritual reading carries the narrowest. The siblings are separate constraint stories, not observables of this one: per the epsilon-invariance principle, the colloquial label 'jihad' covers structurally distinct claims, and the family disambiguates the label rather than one story carrying a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jihad_quranic_corpus__revolutionary_vanguard_reading, moderate, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
