% ============================================================================
% CONSTRAINT STORY: madhab_institutional_persistence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-18
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_madhab_institutional_persistence, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: madhab_institutional_persistence
 *   human_readable: Madhab Institutional Persistence in Sunni Jurisprudence
 *   domain: islamic_jurisprudence/legal_theory/comparative_law
 *
 * SUMMARY:
 *   The madhab system represents one of the most successful examples of
 *   pluralistic legal coordination in human history. Four competing schools
 *   of Sunni jurisprudence have coexisted for over twelve centuries without
 *   displacement, each claiming authority through different methodological
 *   approaches to the same textual sources (Quran, Hadith, consensus,
 *   analogy). The system's persistence raises a central question: is this a
 *   pure coordination mechanism (solving the problem of legal interpretation
 *   across a geographically dispersed civilization) or does it contain
 *   extractive elements (scholarly lineages collecting institutional
 *   authority at the cost of interpretive flexibility)? The constraint
 *   exhibits remarkably low theater ratio (0.35) compared to other legal
 *   systems — madhab scholars genuinely engage in interpretive reasoning
 *   rather than merely performing it. Extraction has increased over the
 *   interval (0.15 → 0.28) as institutional entrenchment grew, but remains
 *   moderate. Suppression shows a similar increase (0.25 → 0.42), driven by
 *   state patronage of specific madhabs and colonial-era codification,
 *   followed by a recent decline as diaspora contexts and internet access
 *   reduce institutional control. The system's defining feature is voluntary
 *   adherence — switching between madhabs is permissible, and many
 *   contemporary Muslims follow mixed rulings (talfiq) without controversy.
 *   This structural mobility is why the baseline classification is rope
 *   rather than tangled_rope, though identity-locked subpopulations
 *   experience the constraint differently.
 *
 * KEY AGENTS:
 *   - Madhab Scholarly Lineages: Primary beneficiaries (institutional/arbitrage) — preserve interpretive authority and institutional resources through lineage continuity
 *   - Regional Legal Communities: Coordinated actors (institutional/constrained) — benefit from legal predictability while bearing institutional switching costs
 *   - Individual Muqallid (Follower): Primary coordinated population (moderate/mobile) — benefits from stable interpretive framework with genuine exit capacity
 *   - Modernist Reform Coalition: Organized challengers (organized/mobile) — advocate for direct textualism; see madhab authority as temporary scaffold
 *   - Identity-Fused Adherents: Subset bearing mixed costs (moderate/identity_locked) — benefit from coordination while bearing identity constraint
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees madhab system as low-coercion coordination solution to collective-action problem
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(madhab_institutional_persistence, 0.28).
domain_priors:suppression_score(madhab_institutional_persistence, 0.42).
domain_priors:theater_ratio(madhab_institutional_persistence, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(madhab_institutional_persistence, extractiveness, 0.28).
narrative_ontology:constraint_metric(madhab_institutional_persistence, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(madhab_institutional_persistence, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(madhab_institutional_persistence, rope).
narrative_ontology:human_readable(madhab_institutional_persistence, "Madhab Institutional Persistence in Sunni Jurisprudence").
narrative_ontology:topic_domain(madhab_institutional_persistence, "islamic_jurisprudence/legal_theory/comparative_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(madhab_institutional_persistence, '7b705fad-2196-4ba9-be67-e126279724cd').
narrative_ontology:cs_kernel_codification('7b705fad-2196-4ba9-be67-e126279724cd', formalized).
narrative_ontology:cs_authority_grounding('7b705fad-2196-4ba9-be67-e126279724cd', lineage).
narrative_ontology:cs_interpretation_layer_present('7b705fad-2196-4ba9-be67-e126279724cd').
narrative_ontology:cs_reading_relation('7b705fad-2196-4ba9-be67-e126279724cd', madhab_institutional_persistence__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('7b705fad-2196-4ba9-be67-e126279724cd', madhab_institutional_persistence__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('7b705fad-2196-4ba9-be67-e126279724cd', madhab_institutional_persistence__shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('7b705fad-2196-4ba9-be67-e126279724cd', madhab_institutional_persistence__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('7b705fad-2196-4ba9-be67-e126279724cd', foundational, scholarly_lineage_authority_primacy).
narrative_ontology:cs_axiom_status(scholarly_lineage_authority_primacy, holdable).
narrative_ontology:cs_axiom_grounding('7b705fad-2196-4ba9-be67-e126279724cd', scholarly_lineage_authority_primacy, conventional).
narrative_ontology:cs_axiom('7b705fad-2196-4ba9-be67-e126279724cd', secondary, methodological_school_stability).
narrative_ontology:cs_axiom_status(methodological_school_stability, holdable).
narrative_ontology:cs_axiom_grounding('7b705fad-2196-4ba9-be67-e126279724cd', methodological_school_stability, conventional).
narrative_ontology:cs_reference_frame('7b705fad-2196-4ba9-be67-e126279724cd', classical_madhab_equilibrium).
narrative_ontology:cs_drift_state('7b705fad-2196-4ba9-be67-e126279724cd', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7b705fad-2196-4ba9-be67-e126279724cd', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(madhab_institutional_persistence, madhab_scholarly_lineages).
narrative_ontology:constraint_beneficiary(madhab_institutional_persistence, regional_legal_communities).
narrative_ontology:constraint_beneficiary(madhab_institutional_persistence, jurisprudential_method_preservation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(madhab_institutional_persistence, mobile_muqallid).
narrative_ontology:constraint_beneficiary(madhab_institutional_persistence, regional_legal_institution).
narrative_ontology:constraint_victim(madhab_institutional_persistence, identity_fused_adherent).
narrative_ontology:constraint_vindicates(madhab_institutional_persistence, interpretive_pluralism_stability).
narrative_ontology:constraint_vindicates(madhab_institutional_persistence, lineage_based_authority_model).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains interpretive lineage through teaching, fatwas, and text preservation. Controls access to madhab methodology and authorizes new interpretive positions. Can shift between madhabs or engage in comparative jurisprudence while maintaining scholarly authority. Benefits from institutional resources (endowments, educational positions) tied to madhab continuity.
narrative_ontology:constraint_stakeholder(madhab_institutional_persistence, madhab_scholar, agenda_setter,
    institutional, generational, arbitrage, regional).

% Follows madhab rulings as pragmatic choice for daily practice. Can switch madhabs without significant cost, especially in diaspora contexts. Benefits from stable interpretive framework without being trapped by it. Social costs of switching exist but are surmountable.
narrative_ontology:constraint_stakeholder(madhab_institutional_persistence, mobile_muqallid, beneficiary,
    moderate, biographical, mobile, regional).

% Madhab membership is constitutive of personal or communal identity. Structurally permitted to switch but psychologically unable to do so without abandoning core self-concept. Benefits from coordination function while bearing identity constraint. Exit would require becoming a different person.
narrative_ontology:constraint_stakeholder(madhab_institutional_persistence, identity_fused_adherent, payer,
    moderate, biographical, identity_locked, regional).

% Courts, educational systems, and endowments organized around specific madhab. Benefits from legal predictability and institutional continuity. High switching costs due to infrastructure entrenchment, but not impossible — legal reform can shift madhab affiliation or adopt mixed approaches. Experiences constraint as coordination with institutional inertia.
narrative_ontology:constraint_stakeholder(madhab_institutional_persistence, regional_legal_institution, beneficiary,
    institutional, generational, constrained, national).

% Organized movements (Salafi, Islamic modernist) advocating direct Quran/Hadith interpretation without madhab intermediation. Excluded from traditional scholarly authority structures but building parallel institutions. See madhab system as temporary coordination structure that should sunset. Have agency and exit capacity, low extraction experienced.
narrative_ontology:constraint_stakeholder(madhab_institutional_persistence, modernist_reformer, excluded,
    organized, generational, mobile, global).

% Analytical view from outside the system. Observes madhab persistence as successful coordination solution to interpretive pluralism across dispersed civilization. Sees low coercive overhead and voluntary adherence as evidence of genuine coordination function. No stake in outcome.
narrative_ontology:constraint_stakeholder(madhab_institutional_persistence, civilizational_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The madhab system coordinates legal interpretation across a geographically dispersed civilization without centralized political authority. It provides stable interpretive frameworks that allow Muslims in different regions and centuries to derive legal rulings from the same textual sources (Quran, Hadith) while accommodating local customs and maintaining doctrinal coherence. The four-school equilibrium prevents both fragmentation (everyone doing independent interpretation) and ossification (a single interpretation enforced uniformly).
% TRANSFER_FUNCTION: The system transfers interpretive authority and institutional resources (judicial appointments, endowment control, educational positions) from the general Muslim population to scholarly lineages that maintain madhab continuity. It also transfers legal predictability and coordination stability from madhab scholars to followers. The transfer is substantially bidirectional — scholars provide genuine interpretive service, not merely extraction.
% ABSENT_VOICES: Independent mujtahids (scholars qualified for independent reasoning) who are excluded from institutional authority if they challenge madhab frameworks. Also absent: ordinary Muslims who want direct textual engagement without scholarly intermediation but lack the training to do so confidently. The modernist/Salafi movements represent these voices organizationally, but individual direct-engagement practitioners remain marginalized in traditional legal institutions. Their absence affects the unanimity of madhab acceptance — when included (as in contemporary internet-enabled contexts), consensus fractures and alternative authority structures emerge.
% DISAPPEARANCE_RATIONALE: If the madhab system disappeared overnight, Islamic legal practice would rearrange substantially. Regional legal institutions, educational curricula, judicial appointment criteria, and fatwa-issuing processes are all organized around madhab affiliation. Individual Muslims would need alternative frameworks for deriving legal rulings — either independent interpretation (requiring extensive training most lack), direct textualism (Salafi approach, which has not demonstrated comparable coordination stability), or a new form of scholarly authority. The system's persistence over twelve centuries across diverse political contexts suggests it is solving a coordination problem that persists regardless of madhab institutional structures. However, the nature of the rearrangement is contested: modernists argue direct textualism would provide sufficient coordination, while madhab defenders argue fragmentation or new hierarchies would emerge.
% FOUNDING_PROBLEM: In the early centuries of Islam (8th-10th CE), the Muslim community faced a coordination crisis: how to derive legal rulings from textual sources (Quran, Hadith) in a geographically expanding civilization without centralized political authority. Different regions were developing divergent interpretive approaches, risking legal fragmentation. Simultaneously, unrestricted independent interpretation by unqualified practitioners risked doctrinal chaos. The madhab system emerged as a solution: authorized scholarly lineages that preserved methodological rigor while allowing regional variation and interpretive competition between schools.
% FOUNDING_PROBLEM_CORROBORATION: The modernist coalition argues the founding problem is dead — internet access, mass literacy, and printed Hadith collections make direct textual engagement feasible for ordinary Muslims, eliminating the coordination crisis that justified scholarly intermediation. They point to Salafi movements as proof that direct textualism can coordinate legal practice. Madhab defenders (corroborated by traditional scholarly institutions and many academic observers of Islamic law) argue the founding problem persists: interpretive pluralism without fragmentation remains a live challenge, direct textualism has produced sectarian fragmentation where attempted, and the madhab system continues to provide coordination value. The status is genuinely contested rather than clearly live or dead.
narrative_ontology:disappearance_verdict(madhab_institutional_persistence, world_rearranges).
narrative_ontology:founding_problem_status(madhab_institutional_persistence, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL FOLLOWER (ROPE) — Mobile between madhabs in practice, especially in diaspora contexts. The madhab system coordinates legal interpretation without trapping adherents. Switching costs are social rather than structural. Benefits from stable interpretive framework while retaining genuine exit capacity.
constraint_indexing:constraint_classification(madhab_institutional_persistence, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 2: REGIONAL LEGAL COMMUNITY (ROPE) — Higher switching costs due to institutional infrastructure (courts, endowments, educational institutions tied to specific madhab). Still experiences the system as coordination: the madhab provides legal predictability and community coherence. Constrained exit reflects institutional inertia, not extraction.
constraint_indexing:constraint_classification(madhab_institutional_persistence, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SCHOLARLY LINEAGE (ROPE) — Primary beneficiaries of madhab persistence. Arbitrage-level exit: scholars can shift between madhabs, engage in comparative fiqh, or establish new interpretive positions while maintaining scholarly authority. The system preserves lineage authority without extracting from other participants.
constraint_indexing:constraint_classification(madhab_institutional_persistence, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MODERNIST REFORM COALITION (SCAFFOLD) — Organized movements (Salafism, Islamic modernism) see madhab persistence as a temporary coordination structure that should sunset in favor of direct Quran/Hadith interpretation. They perceive the scholarly lineage authority as an intermediate stage, not a permanent feature. The coalition has agency and exit capacity, experiencing low extraction while advocating for systemic change.
constraint_indexing:constraint_classification(madhab_institutional_persistence, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: IDENTITY-FUSED ADHERENT (TANGLED ROPE) — Subset of followers whose personal or communal identity is constituted through madhab membership. Structurally mobile (madhab switching is permissible) but identity-locked (exit would require abandoning a core component of self-concept or community belonging). Benefits from the coordination function while bearing psychological costs of the identity constraint. Mixed coordination and extraction through the same structure.
constraint_indexing:constraint_classification(madhab_institutional_persistence, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ROPE) — From a civilizational view, the madhab system solved a genuine collective-action problem: how to maintain legal interpretive stability across a geographically dispersed civilization without centralized authority. The four-school equilibrium represents a coordination solution with remarkably low coercive overhead. Extraction is minimal — the system operates through voluntary adherence and scholarly persuasion rather than suppression of alternatives.
constraint_indexing:constraint_classification(madhab_institutional_persistence, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(madhab_institutional_persistence_tests).
:- end_tests(madhab_institutional_persistence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-moderate. The madhab system does concentrate interpretive authority in scholarly lineages, and this authority can be converted to institutional resources (endowments, judicial appointments, educational control). However, the extraction is substantially lower than typical institutional legal monopolies because: (1) switching between madhabs is permissible and practiced, (2) no single madhab has enforcement monopoly, (3) the four schools compete rather than collude, (4) direct engagement with textual sources remains open to qualified scholars. The increase from 0.15 (classical period) to 0.35 (modern peak) reflects state patronage and colonial codification creating institutional lock-in, with recent decline to 0.28 as internet access and diaspora contexts reduce control. Suppression (0.42): Moderate. Barriers to alternative interpretive approaches exist but are not absolute. A scholar challenging madhab authority faces reputational costs and institutional exclusion, but these costs have varied significantly across time and geography. Classical period suppression was low (0.25) — independent ijtihad was respected even if rare. Ottoman-era codification and state patronage raised suppression substantially (peak 0.55 in modern state-building period), as madhab affiliation became tied to legal employment and educational access. Contemporary suppression (0.42) is falling as alternative interpretive communities (Salafi, modernist, independent muftis) establish parallel authority structures. Theater ratio (0.35): Low-moderate. Madhab scholarship is substantially functional, not performative. Scholars engage in genuine interpretive reasoning, develop new rulings for novel situations, and maintain sophisticated methodological debates. Theater has increased from classical period (0.25) as institutional routines calcified and rote taqlid (following without understanding) became more common, but the system retains interpretive vitality. The recent decline (0.42 → 0.35) reflects internet-enabled access to primary sources reducing reliance on institutional gatekeepers.
 *
 * PERSPECTIVAL GAP:
 *   The madhab system produces a perspectival gap between mobile and identity-locked followers, but NOT a gap between followers and scholarly lineages (both see rope). This is unusual and diagnostic. In most institutional legal systems, the gap runs between enforcers (who see coordination) and subjects (who see extraction). Here, the gap runs within the subject population based on identity fusion. Mobile followers see pure coordination — the madhab provides interpretive stability without trapping them. Identity-locked followers see tangled rope — they benefit from the coordination while bearing psychological costs of the identity constraint. The scholarly lineages see rope from an institutional position — they benefit more than followers, but through providing a genuine coordination service rather than through extraction. The modernist reform coalition sees scaffold — they perceive madhab authority as a temporary stage that should sunset in favor of direct textualism. The analytical observer sees rope at civilizational scale — the madhab system solved a genuine collective-action problem (how to maintain legal coherence across a dispersed civilization without centralized authority) with remarkably low coercive overhead. The four-school equilibrium represents a coordination solution, not an extraction mechanism masquerading as coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   The madhab system shows a tripartite directionality structure: (1) Scholarly lineages are clear beneficiaries — they collect institutional authority, educational control, and endowment resources from madhab persistence. Their arbitrage-level exit capacity (they can shift between madhabs, engage in comparative fiqh, or establish independent positions) combined with beneficiary status produces low or negative effective extraction. (2) Regional legal communities have higher switching costs due to institutional infrastructure, but still benefit from the coordination function. They are net beneficiaries with constrained exit, producing low-moderate effective extraction. (3) Individual followers are the primary coordinated population. Most have genuine mobility (madhab switching is permissible and practiced), and they benefit from stable interpretive frameworks. Their mobile exit + beneficiary status produces low effective extraction, consistent with rope classification. The critical subpopulation is identity-fused adherents (identity_locked exit) who experience madhab membership as constitutive of personal or communal identity. For this group, the constraint contains tangled-rope dynamics — coordination and identity constraint operate through the same structure. The proportion of identity-locked versus mobile adherents is an open empirical question (omega variable identity_lock_proportion), but available evidence suggests mobile adherents are the majority, especially in diaspora contexts.
 *
 * MANDATROPHY ANALYSIS:
 *   The madhab system's mandate was to provide stable legal interpretation across a geographically dispersed Islamic civilization without centralized political authority. This function remains live — the madhabs continue to provide interpretive frameworks that coordinate legal practice across regions and centuries. However, the question of whether the mandate has outlived its function (mandatrophy) is contested and maps to the scaffold vs. rope disagreement. The modernist coalition argues the madhab system was a medieval coordination technology appropriate to pre-printing, pre-travel, pre-internet conditions, and that direct access to Quran/Hadith now makes scholarly lineage authority obsolete. From this view, madhab persistence represents mandatrophy — the coordination function could be performed by direct textualism, and continued madhab authority is institutional inertia. The analytical observer counter-argues that the madhab system addresses a problem (interpretive pluralism without fragmentation) that persists regardless of technology, and that direct textualism has not produced comparable coordination stability where attempted. The measurements show moderate extraction accumulation (0.15 → 0.35 → 0.28) consistent with partial mandatrophy during the state-patronage period, but the recent decline and the system's continued voluntary adherence suggest the coordination function remains genuine. The DR framework resolves this by recognizing both views as perspectivally valid: the modernist coalition is structurally positioned to see mandatrophy (they are organized, mobile, and advocating alternatives), while the analytical observer is positioned to see continued coordination function. Neither is 'wrong' — they are measuring from different structural positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    lineage_authority_naturalness,
    'Is madhab-based lineage authority a natural emergent property of interpretive communities, or a constructed institutional arrangement that benefits scholarly classes?',
    'Comparative analysis of other interpretive traditions (Jewish halakha, Catholic canon law, common law precedent). Do lineage-based authority structures emerge independently? What alternative coordination mechanisms exist?',
    'If natural: madhab persistence is a stable equilibrium reflecting inherent features of legal interpretation. If constructed: the system is contingent and could be displaced by alternative authority models (direct textualism, rationalist ijtihad).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lineage_authority_naturalness, conceptual, 'Whether lineage authority is natural or constructed').

omega_variable(
    geographic_variation_significance,
    'Does the regional clustering of madhabs (Hanafi in Ottoman/South Asian territories, Maliki in North/West Africa, Shafi''i in Southeast Asia) indicate path-dependent institutional lock-in or genuine methodological fit to local contexts?',
    'Historical analysis of madhab adoption patterns. Were schools selected for methodological reasons (local custom integration, analogical reasoning styles) or political reasons (state patronage, conquest patterns)? Do methodological differences correlate with regional jurisprudential needs?',
    'If path-dependent: madhab persistence is partly extractive (institutional inertia masking alternatives). If methodologically fitted: persistence reflects genuine coordination value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geographic_variation_significance, empirical, 'Whether madhab geographic clustering reflects lock-in or fit').

omega_variable(
    modernist_challenge_resolution,
    'Will the modernist/Salafi challenge to madhab authority dissolve the school system, create a fifth school, or be reabsorbed into the existing framework?',
    'Longitudinal observation of institutional evolution over next 50-100 years. Track: (1) madhab affiliation rates in Muslim-majority countries, (2) legal education curriculum changes, (3) fatwa-issuing authority structures.',
    'Dissolution: scaffold perspective vindicated; the madhab system was temporary. Fifth school: madhab framework is stable but internally flexible. Reabsorption: madhab persistence is structurally robust, modernism becomes another interpretive variant within the system.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(modernist_challenge_resolution, empirical, 'Future trajectory of madhab system under modernist pressure').

omega_variable(
    identity_lock_proportion,
    'What proportion of madhab adherents are structurally mobile (following a school as pragmatic choice) versus identity-locked (madhab membership is constitutive of personal or communal identity)?',
    'Survey research and ethnographic study of madhab affiliation patterns. Measure: (1) stated willingness to follow rulings from other madhabs on specific issues, (2) emotional/identity investment in madhab membership, (3) actual madhab-switching rates in diaspora contexts where social costs are lower.',
    'If high mobile proportion: madhab system is pure coordination (rope from most perspectives). If high identity-locked proportion: system contains substantial tangled-rope dynamics (coordination + identity constraint through the same structure).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_proportion, empirical, 'Proportion of identity-locked versus mobile madhab adherents').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(madhab_institutional_persistence, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(madhab_theater_classical, madhab_institutional_persistence, theater_ratio, 0, 0.25).
narrative_ontology:measurement(madhab_theater_ottoman, madhab_institutional_persistence, theater_ratio, 400, 0.3).
narrative_ontology:measurement(madhab_theater_colonial, madhab_institutional_persistence, theater_ratio, 800, 0.35).
narrative_ontology:measurement(madhab_theater_modern, madhab_institutional_persistence, theater_ratio, 1000, 0.38).
narrative_ontology:measurement(madhab_theater_diaspora, madhab_institutional_persistence, theater_ratio, 1100, 0.42).
narrative_ontology:measurement(madhab_theater_contemporary, madhab_institutional_persistence, theater_ratio, 1200, 0.35).

% Extraction over time
narrative_ontology:measurement(madhab_extract_classical, madhab_institutional_persistence, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(madhab_extract_ottoman, madhab_institutional_persistence, base_extractiveness, 400, 0.2).
narrative_ontology:measurement(madhab_extract_colonial, madhab_institutional_persistence, base_extractiveness, 800, 0.28).
narrative_ontology:measurement(madhab_extract_modern, madhab_institutional_persistence, base_extractiveness, 1000, 0.32).
narrative_ontology:measurement(madhab_extract_diaspora, madhab_institutional_persistence, base_extractiveness, 1100, 0.35).
narrative_ontology:measurement(madhab_extract_contemporary, madhab_institutional_persistence, base_extractiveness, 1200, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(madhab_suppress_classical, madhab_institutional_persistence, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(madhab_suppress_ottoman, madhab_institutional_persistence, suppression_requirement, 400, 0.35).
narrative_ontology:measurement(madhab_suppress_colonial, madhab_institutional_persistence, suppression_requirement, 800, 0.48).
narrative_ontology:measurement(madhab_suppress_modern, madhab_institutional_persistence, suppression_requirement, 1000, 0.55).
narrative_ontology:measurement(madhab_suppress_diaspora, madhab_institutional_persistence, suppression_requirement, 1100, 0.52).
narrative_ontology:measurement(madhab_suppress_contemporary, madhab_institutional_persistence, suppression_requirement, 1200, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(madhab_institutional_persistence, identity_coordination).
narrative_ontology:affects_constraint(madhab_institutional_persistence, taqlid_ijtihad_boundary).
narrative_ontology:affects_constraint(madhab_institutional_persistence, islamic_legal_pluralism).
narrative_ontology:affects_constraint(madhab_institutional_persistence, scholarly_authority_legitimation).

% DUAL FORMULATION NOTE:
% The madhab institutional persistence constraint is upstream of more specific constraints about taqlid/ijtihad boundaries and scholarly authority legitimation. Those downstream constraints inherit the madhab system's coordination/extraction profile but add their own domain-specific dynamics. This story focuses on the madhab SYSTEM's persistence; the individual madhabs' distinct methodologies and the inter-madhab comparative dynamics are separate constraints that should be modeled in their own stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
