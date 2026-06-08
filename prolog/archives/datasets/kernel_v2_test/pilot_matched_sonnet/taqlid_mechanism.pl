% ============================================================================
% CONSTRAINT STORY: taqlid_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-07
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_taqlid_mechanism, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: taqlid_mechanism
 *   human_readable: Taqlid Mechanism in Islamic Jurisprudence
 *   domain: islamic_jurisprudence/legal_theory/comparative_law
 *
 * SUMMARY:
 *   The taqlid mechanism in Islamic jurisprudence institutionalizes the
 *   practice of following established madhab (school) authority rather than
 *   engaging in independent legal reasoning (ijtihad). Emerging in the
 *   classical period (9th-11th centuries CE) as madhab consolidation
 *   stabilized, taqlid coordinates legal interpretation across a
 *   geographically dispersed tradition with no central ecclesiastical
 *   authority. The constraint preserves madhab plurality: four Sunni schools
 *   (Hanafi, Maliki, Shafi'i, Hanbali) coexist without requiring unification,
 *   each maintaining distinct methodological commitments while all claiming
 *   fidelity to the same foundational sources (Quran, Sunnah, ijma', qiyas).
 *   The structural tension: taqlid prevents interpretive chaos and provides
 *   legal predictability, but it also creates gatekeeping around mujtahid
 *   status and constrains cross-madhab synthesis. The constraint's
 *   extractiveness increased during the Ottoman period (state madhab
 *   monopolies) and colonial period (madhab rigidity as anti-colonial
 *   identity marker) but has declined in the contemporary era as collective
 *   ijtihad institutions and transnational fiqh councils create alternative
 *   coordination pathways. Theater ratio remains low (0.22) because madhab
 *   training and fatwa issuance retain genuine functional content — the
 *   ritual of following school authority is not primarily performative. The
 *   constraint is downstream of the four madhab reading constraints: each
 *   school's epistemological commitments (Hanafi rationalism, Maliki
 *   customary practice, Shafi'i hadith systematization, Hanbali text
 *   literalism) are stabilized and transmitted through taqlid's institutional
 *   infrastructure.
 *
 * KEY AGENTS:
 *   - Madhab Institutional Continuity: Primary beneficiary (institutional/mobile) — collects legitimacy and coordinates legal training, fatwa issuance, scholarly succession
 *   - Ordinary Muqallid Jurists: Primary beneficiary (moderate/constrained) — benefit from stable career path and legal consistency; bear limited interpretive autonomy cost
 *   - Legal Predictability Seekers: Secondary beneficiary (moderate/constrained) — laypersons and judges who benefit from madhab-based legal stability
 *   - Independent Mujtahid Aspirants: Primary victim (powerless/identity_locked) — identity-fused with madhab training; claiming mujtahid status requires abandoning scholarly self-concept
 *   - Cross-Madhab Synthesizers: Secondary victim (organized/constrained) — reformers who see coordination value but bear exclusion costs from madhab-specific resources
 *   - Contemporary Fiqh Councils: Organized agents (organized/mobile) — building post-madhab collective ijtihad infrastructure; see taqlid as transitional
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees genuine coordination problem (maintaining legal continuity without central authority) with secondary extraction (gatekeeping)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(taqlid_mechanism, 0.28).
domain_priors:suppression_score(taqlid_mechanism, 0.35).
domain_priors:theater_ratio(taqlid_mechanism, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(taqlid_mechanism, extractiveness, 0.28).
narrative_ontology:constraint_metric(taqlid_mechanism, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(taqlid_mechanism, theater_ratio, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(taqlid_mechanism, rope).
narrative_ontology:human_readable(taqlid_mechanism, "Taqlid Mechanism in Islamic Jurisprudence").
narrative_ontology:topic_domain(taqlid_mechanism, "islamic_jurisprudence/legal_theory/comparative_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(taqlid_mechanism, '926aa956-b766-4958-b1b6-28f1c27b4c7f').
narrative_ontology:cs_kernel_codification('926aa956-b766-4958-b1b6-28f1c27b4c7f', formalized).
narrative_ontology:cs_authority_grounding('926aa956-b766-4958-b1b6-28f1c27b4c7f', lineage).
narrative_ontology:cs_interpretation_layer_present('926aa956-b766-4958-b1b6-28f1c27b4c7f').
narrative_ontology:cs_reference_frame('926aa956-b766-4958-b1b6-28f1c27b4c7f', madhab_consolidation_equilibrium).
narrative_ontology:cs_drift_state('926aa956-b766-4958-b1b6-28f1c27b4c7f', contemporary_collective_ijtihad_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('926aa956-b766-4958-b1b6-28f1c27b4c7f', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(taqlid_mechanism, madhab_institutional_continuity).
narrative_ontology:constraint_beneficiary(taqlid_mechanism, ordinary_muqallid_jurists).
narrative_ontology:constraint_beneficiary(taqlid_mechanism, legal_predictability_seekers).
narrative_ontology:constraint_victim(taqlid_mechanism, independent_mujtahid_aspirants).
narrative_ontology:constraint_victim(taqlid_mechanism, cross_madhab_synthesizers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(taqlid_mechanism, madhab_institutional_structure).
narrative_ontology:constraint_beneficiary(taqlid_mechanism, ordinary_muqallid_jurist).
narrative_ontology:constraint_beneficiary(taqlid_mechanism, legal_predictability_seeker).
narrative_ontology:constraint_victim(taqlid_mechanism, independent_mujtahid_aspirant).
narrative_ontology:constraint_victim(taqlid_mechanism, cross_madhab_synthesizer).
narrative_ontology:constraint_vindicates(taqlid_mechanism, interpretive_stability_doctrine).
narrative_ontology:constraint_vindicates(taqlid_mechanism, madhab_plurality_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The madhab as an institution sets the agenda for legal training, fatwa issuance, and scholarly succession. It coordinates transmission chains and stabilizes school identity across centuries. Mobile across interpretive disputes — can adapt madhab doctrine to new contexts without abandoning school identity. Collects legitimacy from taqlid's stabilization function while providing genuine coordination infrastructure.
narrative_ontology:constraint_stakeholder(taqlid_mechanism, madhab_institutional_structure, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(taqlid_mechanism, madhab_institutional_structure, beneficiary).

% A scholar trained within a madhab tradition who follows established school authority in legal reasoning. Constrained by training requirements and institutional expectations but genuinely benefits from the coordination function: madhab affiliation provides career stability, access to scholarly networks, and legal consistency. Bears the cost of limited interpretive autonomy but experiences this as a fair trade for the coordination benefits.
narrative_ontology:constraint_stakeholder(taqlid_mechanism, ordinary_muqallid_jurist, beneficiary,
    moderate, biographical, constrained, national).

% A scholar who aspires to independent legal reasoning (ijtihad) but is identity-locked by madhab-specific training. The scholarly self-concept is constituted through years of madhab methodology — claiming mujtahid status would require abandoning this identity, not merely paying a career cost. Structurally mobile (has the training and credentials) but functionally trapped by the identity frame. Bears the full extraction of the gatekeeping mechanism.
narrative_ontology:constraint_stakeholder(taqlid_mechanism, independent_mujtahid_aspirant, payer,
    powerless, biographical, identity_locked, regional).

% Reformist movements (Salafi, modernist) that attempt to synthesize across madhab boundaries or bypass madhab authority entirely. Organized and constrained — must maintain Islamic legal legitimacy while challenging madhab gatekeeping. Benefits from the stability taqlid provides to the broader tradition but bears exclusion costs: limited access to madhab-specific institutional resources, fatwa market segmentation, legitimacy challenges from madhab-affiliated scholars.
narrative_ontology:constraint_stakeholder(taqlid_mechanism, cross_madhab_synthesizer, payer,
    organized, generational, constrained, continental).

% Laypersons, judges, and legal practitioners who benefit from madhab-based legal consistency. Constrained by the need to navigate madhab-specific rulings but benefit from the predictability taqlid provides: knowing which madhab a judge follows allows prediction of legal outcomes. Not directly involved in the scholarly gatekeeping but benefits from its stabilization function.
narrative_ontology:constraint_stakeholder(taqlid_mechanism, legal_predictability_seeker, beneficiary,
    moderate, biographical, constrained, national).

% International fiqh councils (OIC Fiqh Academy, European Council for Fatwa and Research) building institutional infrastructure for collective ijtihad. Mobile across madhab boundaries — council membership includes scholars from multiple schools. Sets the agenda for post-madhab legal reasoning by creating alternative coordination pathways. Sees taqlid as transitional: its madhab-specific form has a sunset as collective institutions mature.
narrative_ontology:constraint_stakeholder(taqlid_mechanism, contemporary_fiqh_council, agenda_setter,
    organized, civilizational, mobile, global).

% The analytical observer examining taqlid from a civilizational perspective sees a genuine coordination problem: how to maintain legal continuity and prevent interpretive chaos across a geographically dispersed tradition with no central ecclesiastical authority. Taqlid coordinates madhab plurality (four schools coexist without requiring unification) while preserving interpretive stability within each school. Extraction exists (gatekeeping of mujtahid status) but is secondary to the coordination function.
narrative_ontology:constraint_stakeholder(taqlid_mechanism, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Taqlid coordinates legal interpretation across a geographically dispersed Islamic tradition with no central ecclesiastical authority. It stabilizes madhab plurality (four Sunni schools coexist without requiring unification) while preserving interpretive consistency within each school. The mechanism solves the problem of how to maintain legal continuity and prevent interpretive chaos when the foundational sources (Quran, Sunnah) are fixed but their application to new contexts requires ongoing reasoning.
% TRANSFER_FUNCTION: Taqlid transfers interpretive authority from individual scholars to madhab institutional structures. It moves legitimacy (the right to issue authoritative legal rulings) from independent mujtahids to madhab-affiliated scholars who follow established school methodology. It transfers career stability and institutional resources to muqallid jurists who accept madhab authority, while imposing identity costs and resource exclusion on scholars who claim independent reasoning.
% ABSENT_VOICES: Independent mujtahid aspirants who are identity-locked by madhab training are structurally present but functionally silenced — their scholarly self-concept is constituted through madhab methodology, making exit unthinkable from within. Cross-madhab synthesizers who challenge school boundaries are excluded from madhab-specific institutional resources and face legitimacy challenges. These voices would object that taqlid's coordination story naturalizes institutional gatekeeping, but they cannot speak from within the madhab framework without abandoning their scholarly identity.
% DISAPPEARANCE_RATIONALE: If taqlid disappeared overnight, the Islamic legal tradition would face immediate interpretive chaos: no stable mechanism for coordinating legal rulings across regions, no institutional infrastructure for training jurists, no predictable legal outcomes for laypersons and judges. Madhab institutional structures would lose their coordination function. Independent reasoning would proliferate without quality control. Legal predictability would collapse. The world rearranges because arrangements (madhab training, fatwa issuance, legal consistency) depend on taqlid's stabilization function. This is not a natural fact — it is a constructed coordination mechanism.
% FOUNDING_PROBLEM: The founding problem was interpretive chaos in the early Islamic centuries: as the Muslim community expanded geographically and encountered new legal contexts, the need for systematic legal reasoning from foundational sources (Quran, Sunnah) created proliferating interpretive disputes. With no central ecclesiastical authority to adjudicate, the risk was fragmentation into incompatible legal systems. Madhab consolidation (9th-11th centuries) stabilized four major schools with distinct methodologies. Taqlid emerged as the mechanism to preserve this plurality while preventing further fragmentation: follow established school authority rather than proliferate new independent reasoning.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's status is contested between two camps: (1) Traditionalists (madhab-affiliated scholars, institutional jurists) argue the problem remains live — interpretive chaos is still a risk, and taqlid's coordination function is still necessary. They point to contemporary fatwa proliferation and legal inconsistency in contexts where madhab authority has weakened. (2) Reformers (Salafi movements, modernist synthesizers, fiqh council advocates) argue the problem is dead or transformed — the original chaos was a product of early institutional immaturity, and contemporary collective ijtihad institutions can coordinate legal reasoning without madhab-specific gatekeeping. They point to successful transnational fiqh councils as evidence that post-madhab coordination is viable. The corroboration is asymmetric: traditionalists are madhab beneficiaries (self-interested testimony), while reformers include both victims (cross-madhab synthesizers excluded from resources) and external observers (academic scholars of Islamic law). The contested status is itself structural evidence: if the founding problem were clearly dead, taqlid would be a piton (maintained through inertia); if clearly live, it would be uncontested rope. The dispute reveals that the constraint is in transition.
narrative_ontology:disappearance_verdict(taqlid_mechanism, world_rearranges).
narrative_ontology:founding_problem_status(taqlid_mechanism, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDEPENDENT MUJTAHID ASPIRANT (SNARE) — Identity-locked by professional formation within a madhab tradition; claiming mujtahid status requires abandoning the scholarly identity constructed through years of madhab-specific training. The gate of ijtihad is structurally closed not by formal prohibition but by the identity cost of exit. Experiences the constraint as pure extraction: the coordination story (preventing chaos) is cover for institutional gatekeeping.
constraint_indexing:constraint_classification(taqlid_mechanism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 2: ORDINARY MUQALLID JURIST (ROPE) — Constrained by training requirements and institutional expectations but genuinely benefits from the coordination function. Following established madhab authority solves the real problem of legal consistency and provides a stable career path. Extraction is present (limited interpretive autonomy) but coordination function dominates the experience.
constraint_indexing:constraint_classification(taqlid_mechanism, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MADHAB INSTITUTIONAL STRUCTURE (ROPE) — Mobile across interpretive disputes; benefits from taqlid's stabilization of school identity and transmission chains. The constraint coordinates legal training, fatwa issuance, and scholarly succession. Extraction is minimal from this position: the institution collects legitimacy but also provides genuine coordination infrastructure.
constraint_indexing:constraint_classification(taqlid_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: CROSS-MADHAB SYNTHESIS MOVEMENT (TANGLED ROPE) — Organized reformers (Salafi movements, modernist synthesizers) who see both coordination value and extractive gatekeeping. Constrained by the need to maintain Islamic legal legitimacy while challenging madhab boundaries. Benefits from the stability taqlid provides to the broader tradition while bearing costs of exclusion from madhab-specific institutional resources.
constraint_indexing:constraint_classification(taqlid_mechanism, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: CONTEMPORARY FIQH COUNCIL (SCAFFOLD) — International fiqh councils (OIC Fiqh Academy, European Council for Fatwa) see taqlid as transitional coordination during the reconstruction of collective ijtihad mechanisms. Mobile across madhab boundaries; building institutional infrastructure for post-madhab collective reasoning. The constraint's coordination function is real but its madhab-specific form has a sunset: emerging collective ijtihad institutions are creating alternative pathways.
constraint_indexing:constraint_classification(taqlid_mechanism, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ROPE) — From a civilizational perspective, taqlid solves a genuine coordination problem: how to maintain legal continuity and prevent interpretive chaos across a geographically dispersed tradition with no central authority. The constraint coordinates madhab plurality (four schools coexist without requiring unification) while preserving interpretive stability within each school. Extraction exists (gatekeeping of mujtahid status) but is secondary to the coordination function. The analytical classification matches the claimed type.
constraint_indexing:constraint_classification(taqlid_mechanism, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(taqlid_mechanism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(taqlid_mechanism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(taqlid_mechanism, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(taqlid_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-moderate. Taqlid creates real gatekeeping around mujtahid status and constrains independent reasoning, but the extraction is substantially lower than the upstream madhab reading constraints (each at ~0.45-0.55) because taqlid coordinates ACROSS madhabs rather than enforcing a single school's monopoly. The mechanism preserves plurality while stabilizing transmission — a coordination function with embedded extraction. The value increased during Ottoman/colonial periods (state enforcement, anti-colonial rigidity) but has declined in the contemporary era as alternative institutions emerge. Suppression (0.35): Low-moderate. Barriers to claiming mujtahid status include: (1) identity lock from madhab-specific training, (2) institutional resource access (fatwa market segmentation favors madhab-affiliated scholars), (3) social legitimacy costs of breaking from school authority. But suppression is not severe — some scholars do successfully claim mujtahid status, and collective ijtihad institutions are creating alternative pathways. The suppression trajectory shows intensification during Ottoman/colonial periods and relaxation in the contemporary era. Theater ratio (0.22): Low. Madhab training involves genuine skill transmission (usul al-fiqh methodology, source text mastery, analogical reasoning techniques). Fatwa issuance within madhab frameworks has real functional content — the legal reasoning is not primarily performative. Theater increased slightly during the colonial period (madhab identity as symbolic resistance) but remains low because the constraint's coordination function is substantive.
 *
 * PERSPECTIVAL GAP:
 *   The independent mujtahid aspirant sees pure extraction (Snare) — the coordination story is cover for institutional gatekeeping, and the identity lock makes exit unthinkable from within the madhab-trained scholarly self-concept. The ordinary muqallid jurist sees coordination (Rope) — following madhab authority solves the real problem of legal consistency and provides career stability; extraction is present but secondary. The madhab institutional structure sees coordination (Rope) — taqlid stabilizes school identity and transmission chains with minimal extraction from this position. Cross-madhab synthesizers see mixed coordination and extraction (Tangled Rope) — the stability taqlid provides to the broader tradition is valuable, but madhab boundaries constrain synthesis and exclude reformers from institutional resources. Contemporary fiqh councils see transitional coordination (Scaffold) — taqlid's madhab-specific form has a sunset as collective ijtihad institutions create alternative pathways. The analytical observer sees coordination (Rope) — taqlid solves the genuine problem of maintaining legal continuity across a dispersed tradition with no central authority; madhab plurality is coordinated rather than eliminated; extraction (gatekeeping) is secondary to coordination function. The perspectival gap is narrower than in the upstream madhab reading constraints because taqlid operates at a higher level of abstraction (coordinating ACROSS schools rather than enforcing one school's epistemology).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: Madhab institutional structures are the primary beneficiaries — they collect legitimacy and coordinate legal infrastructure. Ordinary muqallid jurists benefit from stable career paths and legal predictability. Legal predictability seekers (laypersons, judges) benefit from madhab-based consistency. These agents experience low or negative effective extraction (d → 0.0-0.3 range). Victims: Independent mujtahid aspirants are the primary victims — identity-locked by madhab training, they bear the full cost of the gatekeeping mechanism. Cross-madhab synthesizers are secondary victims — constrained by madhab boundaries and excluded from school-specific resources. These agents experience higher effective extraction (d → 0.6-0.8 range). The directionality derivation is straightforward from the structural declarations: beneficiaries with mobile or constrained exit get low d; victims with identity_locked or constrained exit get high d. No overrides needed — the structural data accurately captures the relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   Taqlid resolves the mandatrophy by demonstrating that coordination and extraction can coexist at different magnitudes. The constraint genuinely coordinates madhab plurality and legal stability (coordination function is real and substantial), while also creating gatekeeping around mujtahid status (extraction function is real but secondary). The analytical classification (Rope) reflects that coordination dominates extraction at the civilizational scale. The Snare classification from the mujtahid aspirant's perspective reflects that extraction dominates coordination from the identity-locked position. Both are structurally accurate readings of the same constraint from different observational contexts. The constraint is NOT a false summit (no naturalization of contingent arrangements as immutable law) and NOT a piton (theater ratio is low; functional content remains high). It is a genuine coordination mechanism with embedded extraction — exactly what Tangled Rope would describe, except that the analytical perspective sees coordination as dominant (Rope) rather than balanced (Tangled Rope). The perspectival gap between Rope (analytical, institutional, ordinary muqallid) and Snare (mujtahid aspirant) is the measurement the framework exists to take.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gate_closure_historicity,
    'Was the ''closing of the gate of ijtihad'' a historical event (10th-11th century) or a retrospective construction by later reformers?',
    'Systematic analysis of pre-modern juristic biographies: proportion claiming mujtahid status across centuries; examination of whether the ''closure'' narrative appears in classical sources or only in modern reform discourse',
    'If historical event: taqlid is a response to genuine institutional crisis (coordination). If retrospective construction: taqlid is naturalized gatekeeping (extraction narrative masking institutional control).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gate_closure_historicity, empirical, 'Whether gate closure was historical event or retrospective construction').

omega_variable(
    madhab_plurality_necessity,
    'Is madhab plurality a structural necessity (four schools prevent monopoly) or a contingent outcome (could have been one school or seven)?',
    'Comparative analysis: other legal traditions'' school structures; game-theoretic modeling of madhab competition and cooperation; historical counterfactuals of attempted unification movements',
    'If structural necessity: taqlid''s coordination of plurality is irreducible (rope from more perspectives). If contingent outcome: the four-school equilibrium is one possible configuration among many, and taqlid''s role in stabilizing it is extractive institutional lock-in.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(madhab_plurality_necessity, conceptual, 'Whether madhab plurality is structurally necessary or contingent').

omega_variable(
    collective_ijtihad_viability,
    'Can contemporary collective ijtihad institutions (fiqh councils) actually replace madhab-based taqlid, or do they reproduce the same gatekeeping under different labels?',
    'Longitudinal tracking of fiqh council outputs: proportion of novel rulings vs madhab-derivative rulings; composition analysis of council membership (madhab distribution, generational turnover); reception analysis (do rulings gain authority independent of madhab endorsement)',
    'If viable replacement: scaffold perspective confirmed (taqlid has real sunset). If reproducing gatekeeping: scaffold is aspirational, and the constraint persists under new institutional forms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_ijtihad_viability, empirical, 'Whether collective ijtihad institutions can replace madhab taqlid').

omega_variable(
    identity_lock_mechanism,
    'Is the barrier to claiming mujtahid status primarily identity-based (scholarly self-concept formed through madhab training) or materially structural (institutional resource access, fatwa market segmentation)?',
    'Comparative case studies: scholars who successfully claimed mujtahid status vs those who attempted and failed; analysis of what changed (identity reframing, institutional position, external validation); examination of whether exit from taqlid requires becoming a different kind of scholar or merely accessing different resources',
    'If identity-based: the constraint''s binding mechanism is cognitive capture (identity_locked exit is accurate). If materially structural: the constraint operates through resource control (constrained or trapped exit is more accurate), and the identity narrative is cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether mujtahid status barrier is identity-based or materially structural').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(taqlid_mechanism, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taqlid_theater_classical, taqlid_mechanism, theater_ratio, 0, 0.15).
narrative_ontology:measurement(taqlid_theater_medieval, taqlid_mechanism, theater_ratio, 400, 0.18).
narrative_ontology:measurement(taqlid_theater_ottoman, taqlid_mechanism, theater_ratio, 800, 0.22).
narrative_ontology:measurement(taqlid_theater_colonial, taqlid_mechanism, theater_ratio, 1000, 0.28).
narrative_ontology:measurement(taqlid_theater_contemporary, taqlid_mechanism, theater_ratio, 1100, 0.22).

% Extraction over time
narrative_ontology:measurement(taqlid_extract_classical, taqlid_mechanism, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(taqlid_extract_medieval, taqlid_mechanism, base_extractiveness, 400, 0.25).
narrative_ontology:measurement(taqlid_extract_ottoman, taqlid_mechanism, base_extractiveness, 800, 0.3).
narrative_ontology:measurement(taqlid_extract_colonial, taqlid_mechanism, base_extractiveness, 1000, 0.38).
narrative_ontology:measurement(taqlid_extract_contemporary, taqlid_mechanism, base_extractiveness, 1100, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(taqlid_suppress_classical, taqlid_mechanism, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(taqlid_suppress_medieval, taqlid_mechanism, suppression_requirement, 400, 0.3).
narrative_ontology:measurement(taqlid_suppress_ottoman, taqlid_mechanism, suppression_requirement, 800, 0.4).
narrative_ontology:measurement(taqlid_suppress_colonial, taqlid_mechanism, suppression_requirement, 1000, 0.5).
narrative_ontology:measurement(taqlid_suppress_contemporary, taqlid_mechanism, suppression_requirement, 1100, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(taqlid_mechanism, identity_coordination).

% DUAL FORMULATION NOTE:
% Taqlid is downstream of the four madhab reading constraints (hanafi_reading, maliki_reading, shafii_reading, hanbali_reading). Each school's epistemological commitments are stabilized and transmitted through taqlid's institutional infrastructure. The madhab readings have higher extractiveness (0.45-0.55) because they enforce specific methodological monopolies within their domains; taqlid has lower extractiveness (0.28) because it coordinates ACROSS madhabs rather than enforcing a single school's authority. The constraint family structure: usul_al_fiqh_method (kernel) → four madhab readings (each a tangled_rope) → taqlid_mechanism (rope coordinating the plurality).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
