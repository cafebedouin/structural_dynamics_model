% ============================================================================
% CONSTRAINT STORY: jihad_quranic_corpus__defensive_spiritual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jihad_quranic_corpus__defensive_spiritual_reading, []).

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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jihad_quranic_corpus__defensive_spiritual_reading
 *   human_readable: Quranic Jihad: Defensive Spiritual and Armed Struggle Reading
 *   domain: religious/political_theology/jurisprudence
 *
 * SUMMARY:
 *   This constraint story instantiates ONE READING of the contested kernel
 *   'jihad_quranic_corpus': the defensive-spiritual reading, which interprets
 *   jihad primarily as internal spiritual struggle (jihad al-nafs) and as
 *   legitimate armed response only when defensive, constrained by
 *   proportionality and non-combatant immunity. The reading privileges state
 *   authority for legitimate armed jihad, excludes non-Muslims from mandatory
 *   victim/target status unless they are aggressors, and grounds legitimacy
 *   in Quranic revelation, prophetic precedent (sunna), and classical
 *   jurisprudential consensus. The other sibling readings —
 *   expansionist-legalist and revolutionary-vanguard — are separate
 *   constraint stories with different ε values, different victim sets, and
 *   different structural relationships. This story describes only this
 *   reading's constraint structure.
 *
 * KEY AGENTS:
 *   - Quranic exegetical authority: the textual corpus and its classical interpretation, establishing the framework
 *   - Scholarly jurisprudential tradition: ulama and institutional interpreters, maintaining and elaborating the reading
 *   - Muslim ummah collective: beneficiaries whose identity is fused with the tradition
 *   - Communities under aggression: beneficiaries of the defensive authorization and proportionality constraints
 *   - Non-Muslim coexistence communities: beneficiaries of exclusion from target status unless aggressors
 *   - State authority structure: gatekeeper for legitimate armed jihad
 *   - Expansionist-legalist scholars: excluded, would argue for different reading
 *   - Revolutionary vanguard movements: excluded, claim individual obligation
 *   - Western geopolitical actors: observers with structural interests in which reading prevails
 *   - Comparative religious scholarship: external corroborating observers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__defensive_spiritual_reading, 0.31).
domain_priors:suppression_score(jihad_quranic_corpus__defensive_spiritual_reading, 0.22).
domain_priors:theater_ratio(jihad_quranic_corpus__defensive_spiritual_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__defensive_spiritual_reading, rope).
narrative_ontology:human_readable(jihad_quranic_corpus__defensive_spiritual_reading, "Quranic Jihad: Defensive Spiritual and Armed Struggle Reading").
narrative_ontology:topic_domain(jihad_quranic_corpus__defensive_spiritual_reading, "religious/political_theology/jurisprudence").

domain_priors:requires_active_enforcement(jihad_quranic_corpus__defensive_spiritual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__defensive_spiritual_reading, '05feae19-e9e2-4bbd-91d2-805ff9241236').
narrative_ontology:cs_kernel_codification('05feae19-e9e2-4bbd-91d2-805ff9241236', fixed_text).
narrative_ontology:cs_authority_grounding('05feae19-e9e2-4bbd-91d2-805ff9241236', lineage).
narrative_ontology:cs_interpretation_layer_present('05feae19-e9e2-4bbd-91d2-805ff9241236').
narrative_ontology:cs_reading_relation('05feae19-e9e2-4bbd-91d2-805ff9241236', jihad_quranic_corpus__expansionist_legalist_reading, influences).
narrative_ontology:cs_reading_relation('05feae19-e9e2-4bbd-91d2-805ff9241236', jihad_quranic_corpus__revolutionary_vanguard_reading, forecloses).
narrative_ontology:cs_axiom('05feae19-e9e2-4bbd-91d2-805ff9241236', foundational, quranic_proportionality_supremacy).
narrative_ontology:cs_axiom_status(quranic_proportionality_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('05feae19-e9e2-4bbd-91d2-805ff9241236', quranic_proportionality_supremacy, empirically_contingent).
narrative_ontology:cs_axiom('05feae19-e9e2-4bbd-91d2-805ff9241236', foundational, state_authority_monopoly).
narrative_ontology:cs_axiom_status(state_authority_monopoly, holdable).
narrative_ontology:cs_axiom_grounding('05feae19-e9e2-4bbd-91d2-805ff9241236', state_authority_monopoly, deontological).
narrative_ontology:cs_axiom('05feae19-e9e2-4bbd-91d2-805ff9241236', secondary, spiritual_internal_jihad_priority).
narrative_ontology:cs_axiom_status(spiritual_internal_jihad_priority, holdable).
narrative_ontology:cs_axiom_grounding('05feae19-e9e2-4bbd-91d2-805ff9241236', spiritual_internal_jihad_priority, deontological).
narrative_ontology:cs_created_at('05feae19-e9e2-4bbd-91d2-805ff9241236', '').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__defensive_spiritual_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, muslim_ummah_collective).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, communities_under_aggression).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, scholarly_jurisprudential_tradition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, non_muslim_coexistence_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The corpus of Quranic verses and their classical exegetical tradition (tafsir) establish the framework within which jihad is interpreted. This reading holds that the textual tradition privileges spiritual/internal struggle (jihad al-nafs) and defensive armed response (qital) constrained by proportionality and protection of non-combatants. The authority of the text is claimed as binding on all subsequent jurisprudence.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, quranic_exegetical_authority, agenda_setter,
    institutional, civilizational, analytical, global).

% Classical and contemporary Islamic scholars (ulama) who transmit and elaborate this defensive-spiritual reading maintain intellectual authority and custodianship of the tradition. Their interpretive work vindicates and stabilizes this reading's legitimacy. They derive authority from transmitted knowledge (isnad chains) and scholarly consensus (ijma') that this reading represents the orthodoxy of classical jurisprudence.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, scholarly_jurisprudential_tradition, beneficiary,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__defensive_spiritual_reading, scholarly_jurisprudential_tradition, agenda_setter).

% Muslim communities and believers collectively benefit from a reading that prioritizes spiritual development and defensive legitimacy over offensive expansion. This reading permits them to maintain Islamic identity and practice without obligation to aggressive conquest, aligns with coexistence frameworks in pluralistic societies, and provides theological grounding for peaceful Islam. Identity fusion with Islamic tradition makes exit from the interpretive framework itself a form of apostasy-as-perceived-by-tradition.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, muslim_ummah_collective, beneficiary,
    organized, civilizational, identity_locked, global).

% Muslim-majority or Muslim communities facing active military aggression or occupation benefit from a framework that legitimizes defensive armed resistance while constraining it to proportional, defensive response. This reading provides both moral authorization and jurisprudential limits on response (non-combatant immunity, proportionality) that reduce collective punishment and indiscriminate harm.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, communities_under_aggression, beneficiary,
    moderate, biographical, constrained, regional).

% Non-Muslim communities in Muslim-majority or mixed-faith regions benefit from a reading that excludes them from the victim/target set unless they are aggressors, permits coexistence through protected-minority (dhimmi/ahl al-dhimma) frameworks, and constrains armed jihad to defensive and proportional response. They are not obligatory targets of conversion or conquest under this reading.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, non_muslim_coexistence_communities, beneficiary,
    moderate, biographical, constrained, regional).

% This reading grants legitimate armed jihad authority only to state actors (imam/sultan) with recognized authority to command armies and declare defensive wars. Non-state actors, individuals, and revolutionary movements are excluded from legitimate jihad declaration under this framework. State authority becomes the gatekeeper for armed response.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, state_authority_structure, agenda_setter,
    institutional, generational, analytical, regional).

% Scholars advocating the expansionist-legalist reading (jihad as obligation to establish Islamic governance where absent, permitting offensive campaigns under strict conditions) are structurally excluded from co-authoring this constraint's framework. They would argue that the Quranic corpus permits and obligates proactive expansion under jurisprudential conditions this reading does not recognize. Their disagreement centers on what the source texts permit and require.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, expansionist_legalist_scholars, excluded,
    institutional, civilizational, analytical, global).

% Non-state militant movements and revolutionary actors claiming immediate individual obligation (fard 'ayn) and bypassing state authority through takfir doctrine are structurally excluded from this reading's legitimacy framework. The requirement for state authority and the prohibition on takfir (excommunication) of Muslims without explicit textual ground directly bar their operational claims.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, revolutionary_vanguard_movements, excluded,
    organized, biographical, trapped, regional).

% Non-Muslim state actors, secular powers, and geopolitical observers read this constraint as defining the boundary between legitimate Islamic resistance and terrorism. They have structural interest in which reading prevails (expansionist readings justify intervention; defensive readings constrain justification for intervention). They are observers rather than stakeholders because the constraint's legitimacy structure does not grant them voice in its interpretation.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, western_geopolitical_actors, observer,
    institutional, biographical, analytical, global).

% Academic specialists in Islamic law, comparative theology, and religious jurisprudence document and analyze the competing readings. They provide external corroboration of which readings hold authority in the tradition and which represent fringe positions or modern innovations.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, comparative_religious_scholarship, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jihad_quranic_corpus__defensive_spiritual_reading, diffuse).
narrative_ontology:fixing_cost_class(jihad_quranic_corpus__defensive_spiritual_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent interpretive framework authorizing Islamic military resistance to aggression and spiritual development that enables coexistence with non-Muslim communities and constrains violence through proportionality and non-combatant immunity. Solves the coordination problem: how can Islamic tradition authorize both spiritual development and defensive military response while constraining scope, preventing unlimited expansion, and enabling pluralistic coexistence?
% TRANSFER_FUNCTION: Transfers interpretive authority from literal-reading fundamentalism and individual impulse violence to scholarly jurisprudential tradition, institutional state authority, and transmitted knowledge chains. Transfers the definition of 'legitimate victim' from all non-Muslims (expansionist reading) to only aggressors. Transfers authorization from uncontrolled militant movements to recognized state actors. Transfers the understanding of 'jihad' from external warfare to internal spiritual struggle as primary.
% ABSENT_VOICES: Revolutionary vanguard movements claiming individual obligation (fard 'ayn) are structurally excluded from this reading's legitimacy framework. Expansionist-legalist scholars arguing for offensive obligations are excluded. Non-Muslim communities have no voice in interpreting the Quranic tradition they are subjects of, though comparative scholars speak on their behalf. Geopolitical actors outside Islam have interests in the reading's stability but no voice in its adjudication.
% DISAPPEARANCE_RATIONALE: If this reading disappeared and one of the sibling readings became hegemonic, Islamic jurisprudence and practice would transform: the expansionist reading would revive offensive obligations to establish Islamic governance; the revolutionary reading would authorize non-state armed movements to declare jihad without state authorization. The structural world would not physically rearrange, but the legitimacy landscape and the threat environment would shift. Different parties dispute whether this reading's disappearance would represent loss of authentic doctrine (scholars' view, making disappearance catastrophic) or liberation from cover-story constraints (revolutionary view, making disappearance clarification). The verdict is contested because what 'disappearance' MEANS depends on which reading one accepts as authentic.
% FOUNDING_PROBLEM: The founding problem this reading was constructed to solve: How to authorize Islamic military resistance and spiritual development while (a) constraining violence to proportional, defensive response; (b) protecting non-Muslim communities from mandatory conquest; (c) establishing state authority monopoly over legitimate armed action (preventing warlordism and uncontrolled expansion); (d) maintaining coherence with Quranic principles of mercy, compassion, and proportionality; (e) enabling Islamic communities to coexist peacefully with non-Muslims in pluralistic societies. This problem emerged historically from: early Islamic expansion followed by consolidation into regional empires; long periods of peaceful coexistence with non-Muslim neighbors; the practical necessity of stable governance rather than perpetual conquest; theological tensions in the Quranic corpus between verses permitting offensive expansion and verses emphasizing mercy and coexistence.
% FOUNDING_PROBLEM_CORROBORATION: Classical Islamic scholars (al-Nawawi, Ibn Qayyim al-Jawziyyah, al-Mawardi, al-Ghazali) elaborate this reading extensively, attesting that the founding problem was live in their era: how to square Islamic identity with peaceful governance. Contemporary comparative scholars (Khaled Abou El Fadl, Asma Afsaruddin, Sherman Jackson) corroborate that this reading represents mainstream classical jurisprudence and is deployed by Muslim communities to legitimate peaceful Islam in pluralistic societies. Islamic organizations emphasizing coexistence and counter-extremism (CAIR, Minhaj-ul-Quran, scholars in Muslim Majority societies) attest the reading's continued importance. However, revolutionary movements and expansionist scholars attest the founding problem has been 'solved' by rejecting what they view as imposed constraints — but this corroboration comes from positions that reject the reading's authority entirely, so it is not external corroboration by neutral parties. No secular observers or non-Muslim communities provide independent corroboration that the founding problem is solved or even real — their position is that it is permanently contested and its solution is perpetually at stake.
narrative_ontology:disappearance_verdict(jihad_quranic_corpus__defensive_spiritual_reading, contested).
narrative_ontology:founding_problem_status(jihad_quranic_corpus__defensive_spiritual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__defensive_spiritual_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jihad_quranic_corpus__defensive_spiritual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jihad_quranic_corpus__defensive_spiritual_reading, 0.31, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jihad_quranic_corpus__defensive_spiritual_reading_tests).
:- end_tests(jihad_quranic_corpus__defensive_spiritual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured as LOW (0.31 at interval end) because the constraint operates primarily as a coordination framework, not an extraction mechanism. The benefit flows to multiple stakeholder groups without concentrated capture: Muslim communities receive spiritual authorization and military legitimacy; non-Muslim communities receive coexistence status; scholars receive interpretive authority; states receive monopoly on legitimate armed declaration. No single seat captures the constraint's output. Suppression is also LOW (0.22) because the constraint's persistence depends on scholarly consensus and traditional authority transmission, not on active enforcement against resistance. The scholarly tradition IS the enforcement mechanism — interpretation replicates itself through training and transmission. Theater ratio is LOW-MODERATE (0.18) because the constraint does generate some performative elements (ceremonial invocation of proportionality rules, theatrical declarations of defensive necessity by state actors) but the primary function (spiritual development authorization, scholarly legitimacy maintenance) is genuine. Accessibility collapse is MODERATE-HIGH (0.64): once the Quranic corpus is understood as establishing this reading, alternatives collapse for believers who accept the authority of revelation and tradition. But the collapse is not complete because rival readings remain live for parties who reject classical authority (revolutionary movements, modernist reformers). Resistance is MODERATE-HIGH (0.58): the constraint faces substantial active resistance from revolutionary movements and expansionist scholars who claim the reading misrepresents the tradition, who argue takfir doctrine is justified, and who deny state authority monopoly. The measurement trajectory from t=0 to t=1400 (approximately 1400 years of Islamic history) shows a slow upward drift in extractiveness from 0.15 to 0.31, driven by the progressive layering of interpretive constraints and state monopolization claims — as the classical jurisprudence crystallized and institutional authority hardened, the reading's suppressive power increased modestly. Theater ratio shows similar drift (0.08 to 0.18), reflecting the historical shift from early Islamic period (genuine theological dispute, lower theatricality) to later periods with more institutionalized performance and ceremonial invocation. Suppression requirement also drifts upward (0.12 to 0.22) as the constraint required more active defense against rival readings.
 *
 * PERSPECTIVAL GAP:
 *   The scholarly-tradition seat and the revolutionary-vanguard seat should compute very differently. From the scholarly seat, this reading IS the authentic tradition, legitimate extraction of authority is minimal (scholars are custodians, not appropriators), and suppression is minimal (the tradition self-replicates through consent and training). From the revolutionary seat, this reading IS a cover story constraining true doctrine, what appears as low extraction to scholars appears as theft of authority (interpretive monopoly), and what appears as self-replication by consent appears as indoctrination and suppression of alternative readings. The engine computes these divergences from the structural data: scholars have analytical exit (can study the tradition or reject it intellectually, but identity-fusion makes full exit costly); revolutionary actors have constrained/trapped exit (they cannot exit the tradition without losing religious identity, yet the tradition bars their operational claims). This creates asymmetric perception of the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   The scholarly tradition and state authority are near the beneficiary end (d near 0.0-0.3): they set the terms, enforce the reading, and extract interpretive authority without bearing proportional costs. Muslim communities are near symmetric (d near 0.5): they benefit from spiritual authorization and military legitimacy, but also bear identity-fusion costs (cannot exit without apostatizing) and constraints on action (must follow proportionality, non-combatant rules). Non-Muslim coexistence communities are near symmetric (d near 0.5): they benefit from exclusion from target status but pay diffuse costs through potential discrimination within the framework (protected-minority status has subordinate legal status). Revolutionary movements and excluded scholars are targets (d near 0.8-1.0): they bear the cost of exclusion from legitimacy without benefits of participation. Western geopolitical observers are analytical (d near 0.5): they read this constraint as beneficial when stable (less extremism), costly when it weakens (more militant movements), but they are observers, not seats within the constraint's authority structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint does NOT show mandatrophy. The founding problem (how to authorize Islamic military response while constraining it, protecting non-Muslims, enabling spiritual development) remains LIVE in contemporary Islam. Muslim communities still engage in internal spiritual struggle, states still declare defensive wars under this framework, scholars still teach classical jurisprudence including proportionality and non-combatant protection. The founding problem has NOT been resolved or superseded by a different problem. Parties contest whether the constraint represents authentic doctrine or imposed cover (the 'contested' verdict), but the founding problem itself is contested, not dead. A constraint with a dead founding problem that persists would show mandatrophy markers (theater ratio rising sharply, extractiveness disconnected from any stated coordination function, suppression becoming the only mechanism). This constraint shows low theater ratio and genuine coordination function, so mandatrophy is not triggered.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quranic_corpus_coherence,
    'Is the Quranic corpus internally coherent on the jihad question, or does it contain genuinely contradictory directives that multiple readings can each claim to honor?',
    'Systematic textual analysis comparing verses that permit offensive expansion (e.g., 9:5, 9:29) with verses emphasizing mercy and proportionality (e.g., 2:190, 22:39-40) and verses permitting peaceful coexistence (e.g., 2:256, 60:8-9). Determine whether the text can support one unified reading or contains authentic tensions requiring interpretive choices.',
    'If the corpus is genuinely coherent, one reading can claim monopoly on authentic interpretation and others are distortions. If the corpus contains authentic tensions, all readings are legitimate extractions from different parts of the same text, and the choice between them is interpretive, not purely textual — each reading emphasizes different Quranic themes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quranic_corpus_coherence, conceptual, 'Whether the Quranic corpus permits multiple legitimate readings or establishes one authoritative interpretation.').

omega_variable(
    classical_consensus_formation,
    'Did classical Islamic jurisprudence arrive at the defensive-spiritual reading through genuine scholarly consensus (ijma''), or does the historical record show suppression of alternative readings and institutional imposition of orthodoxy?',
    'Historical analysis of jurisprudential development: were dissenting views (expansionist, revolutionary, antimilitarist) present in early Islamic scholarship? Were they debated as legitimate alternatives or branded as heretical? What role did state power play in crystallizing ''orthodox'' consensus?',
    'If consensus emerged through open debate of live alternatives, this reading''s legitimacy rests on scholarly persuasion and can be contested intellectually. If consensus was imposed by institutional power and dissent was suppressed, the reading''s claim to natural tradition is undermined and revolutionary/expansionist readings retain concealed legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(classical_consensus_formation, empirical, 'Whether classical Islamic orthodoxy emerged through genuine consensus or institutional imposition.').

omega_variable(
    identity_lock_mechanism,
    'For Muslim believers, is the fusion of personal identity with Islamic tradition a structural feature of the faith (genuine identity_lock) or a culturally contingent form of attachment that can be separated from belief in Islamic principles?',
    'Compare Muslim communities in secular pluralistic societies (where religious identity is discretionary) with communities where Islamic identity is coterminous with ethnic/national identity (where exit appears impossible). If identity-lock dissolves in the first context but persists in the second, then lock is contingent on social structure, not intrinsic to faith. If identity-lock persists across contexts, it is a structural feature.',
    'If lock is contingent, exit options for Muslim beneficiaries are more mobile than currently classified — they can adopt alternative readings without apostasy in pluralistic contexts. If lock is structural, the constraint''s suppression is higher than measured: believers cannot exit without identity dissolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether identity-lock in Islamic tradition is structural or contextual.').

omega_variable(
    state_authority_legitimacy,
    'This reading grants legitimate armed jihad authority only to recognized state actors. But what counts as a ''recognized state'' under Islamic jurisprudence? Do failed states, revolutionary governments, or non-state armed groups claiming state-level governance qualify?',
    'Examine how classical and contemporary Islamic jurisprudence applies the state-authority requirement to: Ottoman provinces, post-colonial nation-states, revolutionary Islamic republics (Iran, Taliban), stateless armed movements (Hamas, Hezbollah, PKK). Determine whether the jurisprudence permits flexible redefinition of ''state'' or enforces a strict definition that excludes most contemporary non-state actors.',
    'If the definition is flexible and contemporary movements can claim state-level authority, the state-authority gate becomes permeable and the constraint''s suppression of non-state actors weakens. If the definition is strict, non-state actors must accept exclusion from legitimacy and the constraint''s enforcement strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_authority_legitimacy, conceptual, 'Whether the state-authority requirement is a fixed constraint or a flexible principle permitting redefinition.').

omega_variable(
    kernel_committer_ambiguity,
    'Does the Quranic corpus constitute a single kernel (one underlying commitment with multiple readings) or multiple kernels (separate commitments that different readings address)?',
    'Determine whether all three readings claim to interpret the same Quranic verses and prophetic precedent (single kernel, different hermeneutics) or whether they claim different scriptural foundations (multiple kernels). If single kernel: all readings are interpretations of shared authority. If multiple kernels: readings are not in genuine disagreement but in different domains.',
    'If single kernel: the three readings are truly in contest; one may foreclose others if it proves textually superior. If multiple kernels: the readings coexist by occupying different scriptural territories, and no reading can foreclose another because they are not interpreting the same text.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_committer_ambiguity, conceptual, 'Whether the three readings share a single kernel or operate in separate scriptural domains.').

omega_variable(
    proportionality_measurement_ambiguity,
    'The constraint claims proportionality and non-combatant immunity as limits on armed jihad. But how is proportionality measured? Is it proportional to the original aggression, to the strategic objective, or to the means available? Different measures produce wildly different permitted responses.',
    'Examine jurisprudential texts on proportionality (mizan, musawat): do they specify a calculus for measuring harm vs. benefit? Do they permit state actors discretion to define proportionality, or is there an objective standard? Test the standard against historical jihad declarations: did classical scholars enforce proportionality constraints or permit expansionist justifications?',
    'If proportionality has a clear objective standard, the constraint''s suppression of excessive response is genuine. If proportionality is subjective and state-determined, the constraint becomes performative: states invoke it rhetorically while defining it flexibly to permit their preferred level of force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_measurement_ambiguity, empirical, 'Whether proportionality is an objective constraint or a subjective rhetorical principle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__defensive_spiritual_reading, 0, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jiha_tr_t0, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(jiha_tr_t200, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 200, 0.09).
narrative_ontology:measurement(jiha_tr_t400, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 400, 0.11).
narrative_ontology:measurement(jiha_tr_t700, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 700, 0.14).
narrative_ontology:measurement(jiha_tr_t1000, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 1000, 0.17).
narrative_ontology:measurement(jiha_tr_t1200, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 1200, 0.18).
narrative_ontology:measurement(jiha_tr_t1400, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 1400, 0.18).

% Extraction over time
narrative_ontology:measurement(jiha_be_t0, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(jiha_be_t200, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 200, 0.18).
narrative_ontology:measurement(jiha_be_t400, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 400, 0.22).
narrative_ontology:measurement(jiha_be_t700, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 700, 0.28).
narrative_ontology:measurement(jiha_be_t1000, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 1000, 0.29).
narrative_ontology:measurement(jiha_be_t1200, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 1200, 0.31).
narrative_ontology:measurement(jiha_be_t1400, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 1400, 0.31).

% Suppression requirement over time
narrative_ontology:measurement(jiha_su_t0, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(jiha_su_t200, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 200, 0.14).
narrative_ontology:measurement(jiha_su_t400, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 400, 0.16).
narrative_ontology:measurement(jiha_su_t700, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 700, 0.19).
narrative_ontology:measurement(jiha_su_t1000, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 1000, 0.21).
narrative_ontology:measurement(jiha_su_t1200, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 1200, 0.22).
narrative_ontology:measurement(jiha_su_t1400, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 1400, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jihad_quranic_corpus__defensive_spiritual_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jihad_quranic_corpus__defensive_spiritual_reading, 0.12).
narrative_ontology:affects_constraint(jihad_quranic_corpus__defensive_spiritual_reading, jihad_quranic_corpus__expansionist_legalist_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__defensive_spiritual_reading, jihad_quranic_corpus__revolutionary_vanguard_reading).

% DUAL FORMULATION NOTE:
% The jihad_quranic_corpus kernel decomposes into three structurally distinct constraints instantiating different readings: (1) defensive_spiritual_reading (this story) — emphasizes internal spiritual development and defensive armed response constrained by proportionality and non-combatant immunity, with state authority required. ε ≈ 0.31; non-Muslims outside victim set unless aggressors; scholarly consensus claimed. (2) expansionist_legalist_reading — emphasizes obligation to establish Islamic governance where absent, permitting offensive campaigns under jurisprudential conditions (invitation first, imam authority, proportionality). ε higher; broader victim set (those preventing Islam's spread); scholarly support claimed. (3) revolutionary_vanguard_reading — emphasizes immediate individual obligation (fard 'ayn) against apostate rulers and occupiers, bypassing state authority via takfir. ε much higher; victim set includes Muslim rulers and state structures; marginal in classical tradition, live in contemporary movements. All three readings claim the same Quranic corpus as kernel but extract different constraints from it. They have different ε values, different victim sets, different beneficiary structures, and different relationships to state authority. Each is a separate ε-invariant story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jihad_quranic_corpus__defensive_spiritual_reading, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
