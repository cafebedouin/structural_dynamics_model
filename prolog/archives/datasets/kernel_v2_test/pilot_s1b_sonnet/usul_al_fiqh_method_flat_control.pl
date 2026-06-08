% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method_flat_control, []).

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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: usul_al_fiqh_method_flat_control
 *   human_readable: Usul al-Fiqh Four-Source Methodology
 *   domain: islamic_jurisprudence/legal_theory/comparative_law
 *
 * SUMMARY:
 *   The usul al-fiqh framework is Islamic jurisprudence's shared
 *   methodological infrastructure: a four-source hierarchy (Quran, Sunnah,
 *   ijma, qiyas) for deriving legal rulings. The framework enables profound
 *   interpretive diversity — Hanafi, Maliki, Shafi'i, and Hanbali schools
 *   read the sources' relative weight, scope, and application differently —
 *   while maintaining systemic coherence. This is coordination at
 *   civilizational scale: the shared hierarchy allows scholars to disagree
 *   productively rather than fragment into incommensurable traditions. The
 *   constraint's extractiveness (0.22) is moderate and has varied over time:
 *   initially low (0.12) during the formative period when the framework was
 *   genuinely open, rising through the classical period (0.24) as
 *   institutional madhabs consolidated gatekeeping authority, then declining
 *   slightly (0.22) in the contemporary period as reform movements and
 *   cross-madhab dialogue have reduced monopolistic interpretation.
 *   Suppression (0.35) reflects real barriers to methodological innovation —
 *   proposing a fifth source or rejecting qiyas entirely carries
 *   institutional cost — but the barriers are not absolute. Theater ratio
 *   (0.18) is low: the framework is functionally genuine coordination, not
 *   performance. The minimal theater reflects ritualized invocation of
 *   source-priority in fatwas where the actual reasoning is pragmatic, but
 *   this is a small fraction of total jurisprudential activity. The
 *   time-series shows extractiveness and suppression rising through the
 *   classical consolidation period (years 0-800) then declining modestly in
 *   the contemporary period as institutional monopolies weakened.
 *
 * KEY AGENTS:
 *   - Legal Scholars: Primary beneficiary (moderate to institutional / mobile to arbitrage depending on school affiliation) — the framework structures their professional activity and legitimizes their interpretive authority
 *   - Judicial Institutions: Beneficiary (institutional / arbitrage) — established madhab institutions benefit from coordinating function without coercive enforcement
 *   - Muslim Communities Seeking Guidance: Beneficiary (powerless to moderate / constrained) — receive structured access to jurisprudential reasoning across diverse legal questions
 *   - Minority-School Jurists: Mixed position (moderate / constrained) — benefit from framework legitimacy but bear extraction from dominant schools' institutional advantages
 *   - Modernist Reform Movements: Organized agents (organized / constrained) — see framework as transitional; engage it to maintain legitimacy while seeking methodological evolution
 *   - Analytical Observer: Civilizational view (analytical / analytical) — risks naturalizing the four-source hierarchy as inevitable rather than recognizing it as a historically stabilized convention
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method_flat_control, 0.22).
domain_priors:suppression_score(usul_al_fiqh_method_flat_control, 0.35).
domain_priors:theater_ratio(usul_al_fiqh_method_flat_control, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method_flat_control, extractiveness, 0.22).
narrative_ontology:constraint_metric(usul_al_fiqh_method_flat_control, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(usul_al_fiqh_method_flat_control, theater_ratio, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method_flat_control, rope).
narrative_ontology:human_readable(usul_al_fiqh_method_flat_control, "Usul al-Fiqh Four-Source Methodology").
narrative_ontology:topic_domain(usul_al_fiqh_method_flat_control, "islamic_jurisprudence/legal_theory/comparative_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method_flat_control, 'eb3d413b-511f-4ac1-8e0d-869dbd4b7559').
narrative_ontology:cs_kernel_codification('eb3d413b-511f-4ac1-8e0d-869dbd4b7559', formalized).
narrative_ontology:cs_authority_grounding('eb3d413b-511f-4ac1-8e0d-869dbd4b7559', lineage).
narrative_ontology:cs_interpretation_layer_present('eb3d413b-511f-4ac1-8e0d-869dbd4b7559').
narrative_ontology:cs_created_at('eb3d413b-511f-4ac1-8e0d-869dbd4b7559', '2024-01-09T00:00:00Z').

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(usul_al_fiqh_method_flat_control, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method_flat_control, legal_scholars).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method_flat_control, judicial_institutions).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method_flat_control, muslim_communities_seeking_guidance).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(usul_al_fiqh_method_flat_control, minority_school_jurists).
narrative_ontology:constraint_victim(usul_al_fiqh_method_flat_control, modernist_reform_movements).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method_flat_control, textual_primacy_doctrine).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method_flat_control, communal_consensus_validity).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method_flat_control, analogical_reasoning_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Scholars trained in one madhab can engage the four-source framework to produce fatwas, write commentaries, and teach. They benefit from the framework's legitimizing function — it structures their professional authority and enables productive disagreement with other scholars. Exit options are mobile: scholars can work across madhabs or advocate methodological reforms, though with some career cost.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method_flat_control, legal_scholars, beneficiary,
    moderate, biographical, mobile, national).

% Established madhab institutions (Hanafi courts in Central Asia, Maliki courts in North Africa, Shafi'i in Southeast Asia, Hanbali in Arabian Peninsula) benefit from the framework's coordinating function without requiring coercive enforcement. The framework enables jurisprudential continuity across centuries while accommodating internal evolution. Arbitrage exit: institutions can adapt interpretive methodology while maintaining the four-source structure.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method_flat_control, judicial_institutions, beneficiary,
    institutional, generational, arbitrage, global).

% Observant Muslims who seek rulings on daily life questions (halal food, marriage contracts, inheritance, financial transactions) receive structured guidance through the four-source framework. They benefit from the methodology's systematicity — the framework coordinates diverse scholars' reasoning and enables them to receive consistent answers across different legal questions. Exit is constrained: they can change madhabs with social cost but cannot abandon the framework entirely within an observant community.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method_flat_control, muslim_communities_seeking_guidance, beneficiary,
    powerless, biographical, constrained, local).

% Jurists from smaller madhabs or minority interpretive traditions (Zahiri, Ja'fari, contemporary reformist schools) experience the framework as both coordination and extraction. The four-source hierarchy legitimizes their school's existence and enables engagement with the broader tradition, but dominant schools (Hanafi, Maliki, Shafi'i, Hanbali) control institutional resources — court appointments, university positions, fatwa councils — and claim interpretive authority over the framework itself. These jurists can argue within the framework but cannot abandon it without losing jurisprudential standing. They bear extraction from asymmetric institutional access.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method_flat_control, minority_school_jurists, payer,
    moderate, biographical, constrained, regional).

% Contemporary reform movements (advocates of ijtihad revival, contextualist interpretation, gender-egalitarian readings, human rights integration) engage the four-source framework to maintain legitimacy but seek to transform its internal logic. They view the traditional source-weighting as transitional — they aim to preserve Quran/Sunnah centrality while reconfiguring ijma and qiyas for modern contexts. These movements set agendas for methodological evolution but also pay costs: institutional resistance, accusations of innovation (bid'ah), exclusion from traditional scholarly networks. Exit is constrained: they must engage the framework to be heard, but their goal is to transcend its current form.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method_flat_control, modernist_reform_movements, agenda_setter,
    organized, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method_flat_control, modernist_reform_movements, payer).

% Scholars studying Islamic law from comparative or analytical perspectives observe the four-source framework as a coordination solution to the problem of deriving rulings systematically from foundational texts while accommodating interpretive diversity. From a civilizational timescale, the framework appears stable and necessary — some methodological infrastructure is required for any legal tradition to maintain coherence. This perspective risks naturalizing the specific four-source hierarchy as inevitable rather than recognizing it as a historically contingent solution that stabilized among many possible alternatives.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method_flat_control, comparative_law_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The usul al-fiqh framework solves the coordination problem of deriving legal rulings systematically from foundational texts (Quran and Sunnah) while accommodating interpretive diversity across time, geography, and scholarly traditions. Without the shared four-source hierarchy, Islamic jurisprudence would fragment into incommensurable schools with no basis for productive disagreement or mutual recognition.
% TRANSFER_FUNCTION: The framework transfers interpretive authority from the foundational texts to qualified scholars who apply the methodology. It moves jurisprudential legitimacy from the community of believers (who read the texts directly) to those trained in the four-source method. It also transfers institutional resources (court appointments, teaching positions, fatwa authority) to scholars and institutions affiliated with established madhabs, creating asymmetric access for minority schools.
% ABSENT_VOICES: Alternative methodologies that were suppressed or marginalized during the classical consolidation period: Zahiri literalism that rejected qiyas entirely; Mu'tazili rationalism that elevated reason as a source; contemporary reformist approaches that seek to add maqasid (higher objectives) as a formal fifth source. These voices exist but occupy peripheral institutional positions — they would object to the framework's rigidity and gatekeeping but lack the institutional power to restructure it. They are not absent from discourse but are excluded from authoritative institutional positions.
% DISAPPEARANCE_RATIONALE: If the four-source framework disappeared overnight, Islamic jurisprudence would lose its coordinating infrastructure. The immediate rearrangement: scholars would fragment into competing methodologies with no shared basis for mutual recognition. Courts in different regions would produce incommensurable rulings. Lay Muslims seeking guidance would face contradictory authorities with no framework to adjudicate between them. Over time, either (a) a new methodological framework would stabilize (possibly incorporating maqasid, rationalist elements, or other alternatives), or (b) jurisprudential authority would collapse into textual literalism (direct Quran/Sunnah reading without intermediary methodology) or charismatic authority (following individual scholars without systematic method). The disappearance would not leave the world unchanged — the framework structures real institutional arrangements and scholarly practices.
% FOUNDING_PROBLEM: The founding problem was the death of the Prophet Muhammad and the end of direct divine guidance through revelation. Early Muslim communities faced novel legal questions not explicitly addressed in Quran or Sunnah: inheritance disputes in new contexts, commercial transactions in expanding trade networks, governance questions in a rapidly growing polity. The four-source framework emerged over the 8th-10th centuries CE to solve the problem of how to extend Quranic and Prophetic guidance to new situations systematically, while maintaining fidelity to the foundational texts and accommodating the diversity of regional practice and scholarly reasoning. The founding problem was real: without some methodological framework, jurisprudential authority would have fragmented or collapsed into arbitrary individual opinion.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem remains live in the 21st century: Muslim communities still face novel legal questions (bioethics, digital finance, climate policy, gender equality, minority rights in secular states) not explicitly addressed in classical texts. Contemporary scholars across all madhabs — traditionalist, reformist, and modernist — agree that the core challenge persists: how to derive rulings for unprecedented situations while maintaining continuity with foundational sources. Corroboration comes from practitioners across the spectrum: traditional scholars like Yusuf al-Qaradawi, reformists like Tariq Ramadan, and academic observers like Wael Hallaq all acknowledge (though they disagree on the solution) that the problem of extending guidance to new contexts is structural, not resolved. The contestation is about whether the four-source framework is the optimal solution, not whether the founding problem exists.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method_flat_control, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method_flat_control, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OBSERVANT LAYPERSON (ROPE) — Experiences the four-source framework as coordination that enables receiving consistent guidance across different legal questions. Constrained exit (can change madhabs with social cost but not abandon framework entirely within observant community). Low extraction — the methodology benefits them by providing structured access to jurisprudential reasoning.
constraint_indexing:constraint_classification(usul_al_fiqh_method_flat_control, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 2: REFORM-ORIENTED SCHOLAR (ROPE) — Sees the four-source framework as flexible coordination infrastructure. Mobile exit (can work across madhabs or advocate methodological innovations). The shared hierarchy enables constructive disagreement — scholars debate source-weighting within a common framework rather than fragmenting into incommensurable traditions. Moderate extraction from institutional gatekeeping but net beneficiary of the coordinating function.
constraint_indexing:constraint_classification(usul_al_fiqh_method_flat_control, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: MINORITY-SCHOOL JURIST (TANGLED ROPE) — Experiences both coordination (the shared framework legitimizes their school's existence) and extraction (dominant schools control institutional resources and claim interpretive authority over the source hierarchy itself). Constrained exit (can argue within the framework but cannot abandon it without losing jurisprudential standing). Moderate effective extraction from asymmetric institutional access.
constraint_indexing:constraint_classification(usul_al_fiqh_method_flat_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: ESTABLISHED LEGAL SCHOOL (ROPE) — Institutional beneficiary of the coordinating framework. Arbitrage exit (can adapt interpretive methodology while maintaining the four-source structure). The shared hierarchy enables jurisprudential continuity across centuries while accommodating internal evolution. Low effective extraction — the framework serves institutional reproduction without requiring coercive enforcement.
constraint_indexing:constraint_classification(usul_al_fiqh_method_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: MODERNIST REFORM MOVEMENT (SCAFFOLD) — Views the four-source hierarchy as transitional coordination during the shift from medieval to contemporary jurisprudence. Constrained exit (must engage the framework to maintain legitimacy but seeks to transcend it). Sees the traditional source-weighting as temporary — expects eventual methodological pluralism that retains Quran/Sunnah centrality but reconfigures ijma and qiyas for modern contexts. The sunset is implicit: reform movements aim to preserve the framework's coordinating function while transforming its internal logic.
constraint_indexing:constraint_classification(usul_al_fiqh_method_flat_control, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational/universal perspective, some shared methodological framework is necessary for any legal tradition to maintain coherence across time and geography. The four-source hierarchy solves an irreducible coordination problem: how to derive rulings systematically from foundational texts while accommodating interpretive diversity. This perspective risks naturalizing a historically contingent solution — the specific four-source structure is not inevitable, even if some framework is necessary.
constraint_indexing:constraint_classification(usul_al_fiqh_method_flat_control, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method_flat_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(usul_al_fiqh_method_flat_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(usul_al_fiqh_method_flat_control, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(usul_al_fiqh_method_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.22): Moderate and historically variable. The framework initially enabled genuine pluralism (0.12), but extraction increased (to 0.24) as institutional madhabs consolidated interpretive authority and gatekept access to jurisprudential legitimacy. Contemporary extractiveness (0.22) is lower than the classical peak but higher than the formative baseline — reform movements and cross-madhab dialogue have reduced monopolistic extraction, but institutional advantages persist. The extraction is not from the framework's coordination function (which is genuine) but from asymmetric institutional access to authoritative interpretation. Suppression (0.35): Moderate-high and historically variable. Real barriers exist to proposing alternative methodologies (a fifth source, rejecting qiyas, abandoning ijma) — such proposals carry institutional cost and risk exclusion from the scholarly community. But suppression is not total: minority positions exist (Zahiri school's rejection of qiyas, Mu'tazili rationalist approaches), and contemporary reform movements operate within and around the framework. The suppression trajectory shows classical-period hardening (to 0.42) followed by contemporary softening (to 0.35) as institutional monopolies weakened. Theater ratio (0.18): Low. The framework is functionally genuine — it coordinates jurisprudential reasoning and enables systematic derivation of rulings. The minimal theater consists of ritualized invocations of source-hierarchy in fatwas where the actual reasoning is pragmatic or policy-driven, but this is a small fraction of total activity. The slight increase over time (0.15 to 0.19, then stabilizing at 0.18) reflects periods where institutional madhabs performed adherence to the framework while exercising discretionary authority, but the framework never became primarily performative.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates significant perspectival variation from identical base properties. Observant laypeople, reform-oriented scholars, and established legal schools all see rope — coordination that enables their diverse needs (receiving guidance, advocating reforms, maintaining tradition). The minority-school jurist sees tangled_rope — genuine coordination combined with asymmetric institutional extraction. The modernist reform movement sees scaffold — transitional coordination during methodological evolution, with an implicit sunset as the framework adapts to contemporary contexts or is transcended. The analytical observer sees mountain — some methodological framework is necessary for legal coherence, and the four-source hierarchy appears immutable from a civilizational timescale. This mountain risks being a false summit: the specific four-source structure is historically contingent, even if some coordination framework is necessary. The omegas capture the irreducible uncertainties: whether the hierarchy is natural or conventional, whether ijma ambiguity is feature or bug, whether qiyas constraints prevent or enable extraction, and whether the modernist sunset is realistic. No single type is definitive — the presheaf over observation positions is the complete structural description.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. Legal scholars, judicial institutions, and observant communities are declared beneficiaries — the framework coordinates their access to jurisprudential reasoning and legitimizes interpretive authority. No direct victims are declared because the framework's coordination function is genuine at the base level. However, the minority-school jurist perspective captures asymmetric extraction: scholars from smaller madhabs experience the framework as both coordination (it legitimizes their school's existence) and extraction (dominant schools control institutional resources and claim interpretive authority over the framework itself). This is modeled through constrained exit and the tangled_rope classification at that perspective, not through victim declaration — the extraction is structural position within the coordinating framework, not extraction by the framework itself. The analytical observer with mountain classification risks naturalizing the four-source hierarchy, creating a false-summit candidate — but no beneficiaries are declared at the analytical level because the naturalization risk is perspectival (the observer's framing) rather than institutional (entities collecting rents). The omega addressing source-hierarchy naturalization documents this ambiguity.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint demonstrates mandatrophy resolution through perspectival distribution. The apparent contradiction — is this coordination (rope) or naturalized extraction (false summit mountain) — dissolves when context is specified. From the layperson's biographical/local context, it is rope: genuine coordination enabling access to guidance. From the minority jurist's biographical/regional context, it is tangled_rope: coordination with embedded asymmetric extraction. From the modernist movement's civilizational/global context, it is scaffold: transitional coordination with implicit sunset. From the analytical civilizational/universal context, it appears as mountain but is flagged as potential false summit by the omega addressing naturalization. The framework's mandate — coordinate jurisprudential reasoning across interpretive diversity — is intact from most perspectives, but the extraction trajectory (low initial, rising through classical consolidation, declining in contemporary period) suggests periods where institutional gatekeeping threatened to transform coordination into extraction. The contemporary decline in both extractiveness and suppression indicates the mandate is being renewed rather than exhausted, but the modernist scaffold perspective suggests the mandate may eventually be transcended rather than maintained indefinitely.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    source_hierarchy_naturalization,
    'Is the four-source hierarchy (Quran > Sunnah > ijma > qiyas) a discovered natural order of epistemic authority, or a constructed coordination convention that stabilized historically?',
    'Historical analysis of early Islamic legal development; examination of alternative hierarchies proposed by minority traditions (Zahiri rejection of qiyas, Mu''tazili rationalist approaches); cross-traditional comparison with other legal systems'' source hierarchies.',
    'If natural: mountain classification from analytical perspective is correct. If constructed: the analytical mountain is a false summit — the framework naturalizes a contingent solution that benefits established schools by foreclosing alternative methodologies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(source_hierarchy_naturalization, conceptual, 'Whether the four-source hierarchy is natural law or historical convention').

omega_variable(
    ijma_scope_contestation,
    'Does ijma (consensus) require unanimous agreement of all qualified scholars, or majority agreement, or agreement of a particular generation''s scholars? Each definition produces radically different constraint boundaries.',
    'Each madhab resolves this differently: Shafi''i requires unanimity of a generation; Hanafi allows majority; Maliki weights Medinan practice. The question is whether these are legitimate readings of an ambiguous concept or competitive claims about a determinate standard.',
    'If ambiguous: the framework is under-specified coordination (distributed kernel). If determinate: current madhab diversity represents suppressed alternatives (tangled rope with higher extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ijma_scope_contestation, conceptual, 'Whether ijma scope ambiguity is feature or bug').

omega_variable(
    qiyas_constraint_sufficiency,
    'Do the constraints on analogical reasoning (qiyas) — requirement of shared effective cause (''illa), prohibition of analogy for fixed ritual matters — successfully prevent jurisprudential drift, or do they create extractive discretion for those who identify the ''illa?',
    'Historical case analysis: track analogical rulings over centuries; identify instances where ''illa identification was contested; measure correlation between institutional power and successful ''illa claims.',
    'If constraints sufficient: qiyas is genuine coordination (rope from more perspectives). If insufficient: ''illa discretion is extraction mechanism (tangled rope or snare from minority-school perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(qiyas_constraint_sufficiency, empirical, 'Whether qiyas constraints prevent extraction or enable it').

omega_variable(
    modernist_sunset_realism,
    'Is the modernist reform movement''s implicit sunset claim realistic — can the framework evolve to accommodate contemporary contexts while retaining its coordinating function — or does modernization require abandoning the source hierarchy itself?',
    'Empirical tracking of reform attempts: which innovations were absorbed into traditional madhab structures vs which required breaking with the framework; success rate of reforms that preserved vs abandoned the four-source logic.',
    'If realistic: scaffold classification from modernist perspective is structural. If unrealistic: scaffold is aspirational rather than descriptive, and the framework is stable rope (or tangled rope if reforms are systematically suppressed).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(modernist_sunset_realism, empirical, 'Whether the framework can evolve or must be transcended').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method_flat_control, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t0, usul_al_fiqh_method_flat_control, theater_ratio, 0, 0.15).
narrative_ontology:measurement(usul_tr_t200, usul_al_fiqh_method_flat_control, theater_ratio, 200, 0.16).
narrative_ontology:measurement(usul_tr_t400, usul_al_fiqh_method_flat_control, theater_ratio, 400, 0.17).
narrative_ontology:measurement(usul_tr_t600, usul_al_fiqh_method_flat_control, theater_ratio, 600, 0.18).
narrative_ontology:measurement(usul_tr_t800, usul_al_fiqh_method_flat_control, theater_ratio, 800, 0.19).
narrative_ontology:measurement(usul_tr_t1000, usul_al_fiqh_method_flat_control, theater_ratio, 1000, 0.18).

% Extraction over time
narrative_ontology:measurement(usul_be_t0, usul_al_fiqh_method_flat_control, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(usul_be_t200, usul_al_fiqh_method_flat_control, base_extractiveness, 200, 0.15).
narrative_ontology:measurement(usul_be_t400, usul_al_fiqh_method_flat_control, base_extractiveness, 400, 0.18).
narrative_ontology:measurement(usul_be_t600, usul_al_fiqh_method_flat_control, base_extractiveness, 600, 0.22).
narrative_ontology:measurement(usul_be_t800, usul_al_fiqh_method_flat_control, base_extractiveness, 800, 0.24).
narrative_ontology:measurement(usul_be_t1000, usul_al_fiqh_method_flat_control, base_extractiveness, 1000, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t0, usul_al_fiqh_method_flat_control, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(usul_su_t200, usul_al_fiqh_method_flat_control, suppression_requirement, 200, 0.28).
narrative_ontology:measurement(usul_su_t400, usul_al_fiqh_method_flat_control, suppression_requirement, 400, 0.32).
narrative_ontology:measurement(usul_su_t600, usul_al_fiqh_method_flat_control, suppression_requirement, 600, 0.38).
narrative_ontology:measurement(usul_su_t800, usul_al_fiqh_method_flat_control, suppression_requirement, 800, 0.42).
narrative_ontology:measurement(usul_su_t1000, usul_al_fiqh_method_flat_control, suppression_requirement, 1000, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method_flat_control, identity_coordination).

% DUAL FORMULATION NOTE:
% This is the flat control construction of the usul al-fiqh substrate. It models the framework as a single constraint with perspectival disagreement captured through the (P,T,E,S) tuple and omegas. The same substrate could be decomposed into reading-level constraints (one per madhab's interpretation of source-weighting), but that decomposition is not performed in this flat construction. The contestation about whether the four-source hierarchy is natural or conventional, and about the scope of ijma and constraints on qiyas, is modeled here as omega variables (irreducible uncertainties) rather than as distinct constraints with different epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
