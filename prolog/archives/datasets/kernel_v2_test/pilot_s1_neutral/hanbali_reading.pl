% ============================================================================
% CONSTRAINT STORY: hanbali_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hanbali_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hanbali_reading
 *   human_readable: Hanbali Jurisprudential Method: Text Literalism and Qiyas Restriction
 *   domain: islamic_jurisprudence/legal_theory/usul_al_fiqh
 *
 * SUMMARY:
 *   The Hanbali reading of Islamic jurisprudential method (usul al-fiqh)
 *   instantiates a specific methodological commitment: textual literalism
 *   with restriction on qiyas (analogy) and preference for weak authenticated
 *   hadith over rationalist reconstruction. This reading exists as one of
 *   four major madhab positions within Sunni Islamic jurisprudence, alongside
 *   Hanafi (rationalist), Maliki (maslaha-inclusive), and Shafi'i (balanced)
 *   schools. The Hanbali constraint exhibits the perspectival multiplicity
 *   characteristic of kernel readings: what appears to traditionalist
 *   institutional gatekeepers as necessary legal rigor (Rope) appears to
 *   rationalist jurists as constraining identity-lock (Snare) and to reform
 *   movements as a temporary barrier to be overcome through methodological
 *   innovation (Scaffold). The constraint's extractiveness (0.35) reflects
 *   genuine but moderate institutional gatekeeping — traditionalist schools
 *   benefit from methodological authority concentration, but the gatekeeping
 *   is not total: rationalist methods persist within and across madhabs, and
 *   modern jurisprudence increasingly authorizes flexible interpretation. The
 *   suppression trajectory (declining from 0.65 to 0.45 over the interval)
 *   models the erosion of hard enforcement: early Hanbali institutional
 *   consolidation required strict adherence; contemporary globalized Islamic
 *   jurisprudence permits methodological pluralism even within
 *   Hanbali-identified scholars. Theater ratio (0.38, rising to 0.38 by
 *   interval end) suggests the constraint retains substantial institutional
 *   performance value — certification of Hanbali scholarly identity —
 *   alongside genuine methodological gatekeeping.
 *
 * KEY AGENTS:
 *   - Traditionalist institutional gatekeepers (institutional/arbitrage): Scholars and institutions with authority to certify Hanbali orthodoxy; beneficiaries of methodological concentration
 *   - Rationalist jurists (moderate/identity_locked): Scholars who adopt Hanbali affiliation but operate within or strain against literalist constraints; bears cognitive lock and reduced methodological flexibility
 *   - Secondary madhab communities (moderate/constrained): Followers of other schools or regional traditions affected by Hanbali institutional influence in shared jurisdictions
 *   - Modern jurisprudential reform movement (organized/mobile): Scholars and institutes promoting maqasid-based methodology, comparative jurisprudence, and explicit authorization of flexible interpretation
 *   - Historical Hanbali transmission institution (institutional/arbitrage): Ijaza chains, hadith authentication networks, scholarly certification apparatus maintaining methodological continuity
 *   - Analytical observer (analytical/analytical): Trans-historical perspective risking naturalization of contingent methodological choice as necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hanbali_reading, 0.35).
domain_priors:suppression_score(hanbali_reading, 0.45).
domain_priors:theater_ratio(hanbali_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hanbali_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(hanbali_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(hanbali_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hanbali_reading, tangled_rope).
narrative_ontology:human_readable(hanbali_reading, "Hanbali Jurisprudential Method: Text Literalism and Qiyas Restriction").
narrative_ontology:topic_domain(hanbali_reading, "islamic_jurisprudence/legal_theory/usul_al_fiqh").

domain_priors:requires_active_enforcement(hanbali_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hanbali_reading, '3dde28ea-6b6e-4c9f-87cc-43647e05832e').
narrative_ontology:cs_kernel_codification('3dde28ea-6b6e-4c9f-87cc-43647e05832e', fixed_text).
narrative_ontology:cs_authority_grounding('3dde28ea-6b6e-4c9f-87cc-43647e05832e', lineage).
narrative_ontology:cs_interpretation_layer_present('3dde28ea-6b6e-4c9f-87cc-43647e05832e').
narrative_ontology:cs_reading_relation('3dde28ea-6b6e-4c9f-87cc-43647e05832e', hanbali_reading__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('3dde28ea-6b6e-4c9f-87cc-43647e05832e', hanbali_reading__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('3dde28ea-6b6e-4c9f-87cc-43647e05832e', hanbali_reading__shafii_reading, coexists_with).
narrative_ontology:cs_axiom('3dde28ea-6b6e-4c9f-87cc-43647e05832e', foundational, text_literalism_methodological_necessity).
narrative_ontology:cs_axiom_status(text_literalism_methodological_necessity, holdable).
narrative_ontology:cs_axiom_grounding('3dde28ea-6b6e-4c9f-87cc-43647e05832e', text_literalism_methodological_necessity, deontological).
narrative_ontology:cs_axiom('3dde28ea-6b6e-4c9f-87cc-43647e05832e', foundational, qiyas_restriction_drift_prevention).
narrative_ontology:cs_axiom_status(qiyas_restriction_drift_prevention, holdable).
narrative_ontology:cs_axiom_grounding('3dde28ea-6b6e-4c9f-87cc-43647e05832e', qiyas_restriction_drift_prevention, instrumental).
narrative_ontology:cs_reference_frame('3dde28ea-6b6e-4c9f-87cc-43647e05832e', hadith_authentication_as_primary_filter).
narrative_ontology:cs_drift_state('3dde28ea-6b6e-4c9f-87cc-43647e05832e', contemporary_globalized_jurisprudence, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3dde28ea-6b6e-4c9f-87cc-43647e05832e', '2026-02-26T14:22:00Z').
narrative_ontology:cs_kernel_id(hanbali_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hanbali_reading, traditionalist_schools).
narrative_ontology:constraint_beneficiary(hanbali_reading, hadith_specialists).
narrative_ontology:constraint_victim(hanbali_reading, rationalist_interpreters).
narrative_ontology:constraint_victim(hanbali_reading, juristic_reasoning_flexibility).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hanbali_reading, secondary_madhab_communities).
narrative_ontology:constraint_beneficiary(hanbali_reading, modern_usul_reform_scholars).
narrative_ontology:constraint_victim(hanbali_reading, rationalist_jurists).
narrative_ontology:constraint_victim(hanbali_reading, secondary_madhab_communities).
narrative_ontology:constraint_vindicates(hanbali_reading, textual_primacy_doctrine).
narrative_ontology:constraint_vindicates(hanbali_reading, tradition_as_authoritative_filter).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Scholars and institutions responsible for authenticating hadith sources, validating jurisprudential conclusions against textual authority, and certifying orthodox methodology. They control the gatekeeping apparatus that determines what counts as legitimate Hanbali jurisprudence. Authority flows from their role as transmitters and authenticators of textual tradition.
narrative_ontology:constraint_stakeholder(hanbali_reading, traditionalist_hadith_scholars, agenda_setter,
    institutional, generational, arbitrage, continental).

% Scholars trained in or attracted to rationalist methods (qiyas, istislah, ra'y) who adopt or are coerced into Hanbali institutional affiliation. They bear the cost of methodological restriction — reduced jurisprudential flexibility, constraints on creative legal reasoning, identity pressure to conform to traditionalist standards. Their voice in methodological disputes is devalued by gatekeepers.
narrative_ontology:constraint_stakeholder(hanbali_reading, rationalist_jurists, payer,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(hanbali_reading, rationalist_jurists, excluded).

% Communities following Hanafi, Maliki, or Shafi'i madhabs in regions where Hanbali institutional influence is strong (particularly Najd, contemporary Saudi Arabia, Gulf states). They benefit from methodological stability and legal predictability when Hanbali rulings are applied; they pay costs when Hanbali literalism overrides local jurisprudential traditions or when gatekeeping authority is wielded to suppress alternative methodologies.
narrative_ontology:constraint_stakeholder(hanbali_reading, secondary_madhab_communities, beneficiary,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(hanbali_reading, secondary_madhab_communities, payer).

% Contemporary scholars and institutions (international Islamic universities, jurisprudential institutes, modernist networks) promoting maqasid al-sharia (objectives of sacred law) methodology, comparative jurisprudence, and explicit authorization of flexible interpretation. They benefit from institutional expansion of methodological pluralism and from positioning themselves as reform voices. They can leverage transnational networks and academic institutions to bypass traditional gatekeeping.
narrative_ontology:constraint_stakeholder(hanbali_reading, modern_usul_reform_scholars, beneficiary,
    organized, generational, mobile, global).

% Institutional chains of scholarly transmission (ijaza networks) that maintain Hanbali methodological certification and authenticate new scholars. They perform continuous gatekeeping work through teaching, examination, and certification while increasingly accepting methodological flexibility in practice. Their authority persists through institutional reputation and social legitimacy rather than strict enforcement.
narrative_ontology:constraint_stakeholder(hanbali_reading, ijaza_transmission_networks, agenda_setter,
    institutional, civilizational, arbitrage, global).

% The abstract capacity for flexible juristic reasoning (qiyas, istislah, maslaha reasoning) within Islamic jurisprudence is constrained by Hanbali restrictions. This is not a real agent but a collective good — the epistemic resource of jurisprudential flexibility. It bears cost through methodological restriction and is trapped by the institutional gatekeeping that prevents its full exercise.
narrative_ontology:constraint_stakeholder(hanbali_reading, juristic_reasoning_capacity, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(hanbali_reading, juristic_reasoning_capacity).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishing rigorous, stable jurisprudential method grounded in authenticated textual sources (Quran and hadith) to prevent juristic drift, theological heterodoxy, and divergence from foundational Islamic principles. The coordination problem is: how do competing jurists maintain shared legal doctrine without reducing Islamic law to individual whim or cultural drift?
% TRANSFER_FUNCTION: Interpretive authority flows from rationalist jurists and secondary madhab practitioners toward traditionalist gatekeepers and hadith specialists. The transfer is not material wealth but institutional legitimacy, scholarly status, and methodological authority. Qiyas practitioners surrender methodological autonomy; literalist gatekeepers gain authority to certify orthodoxy.
% ABSENT_VOICES: Rationalist methodologists are institutionally present but gatekeeping-constrained; their voice in defining methodology is devalued. Lay Muslim communities (non-scholarly) are absent from the jurisprudential method dispute — the constraint operates within scholarly institutions and does not directly engage community legal preferences. Historical Mu'tazilite rationalism is absent (institutionally foreclosed centuries ago); contemporary philosophical jurisprudence is partially absent (academic rather than institutional-Islamic jurisprudence discourse).
% DISAPPEARANCE_RATIONALE: Traditionalist gatekeepers argue the constraint would leave Islamic jurisprudence without moorings — without textual limitation, jurisprudence becomes arbitrary and doctrine fragmentizes (world rearranges toward chaos). Rationalist scholars argue Islamic jurisprudence would flourish with fuller qiyas permission — the constraint's disappearance would enable more nuanced legal reasoning without doctrinal collapse (world rearranges toward better jurisprudence). The dispute is not about facts but about what institutional stability requires.
% FOUNDING_PROBLEM: Early Islamic jurisprudential diversity (particularly 2nd–3rd centuries AH) generated competing schools with incompatible rulings on identical questions. Without methodological standardization, Islamic legal authority fragmented, creating community legal uncertainty and institutional instability. Hanbali literalism was developed (formalized primarily in Ibn Hanbal's 9th-century lifetime and consolidated by 11th–13th century successors) to establish clear methodological boundaries: text-authenticated hadith would take priority over rational analogical extension, preventing speculative juristic creation and maintaining doctrinal continuity.
% FOUNDING_PROBLEM_CORROBORATION: Islamic historians and jurisprudential genealogists (e.g., Ibn al-Nadim, al-Shatibi, modern scholars like Hallaq) document the early jurisprudential diversity and the subsequent madhab consolidation. The founding problem (institutional chaos from methodological pluralism) is historically confirmed. However, the Hanbali solution's necessity is contested: Hanafi, Maliki, and Shafi'i schools maintained doctrinal stability without Hanbali literalism, suggesting the founding problem could be solved by other methods. Corroboration exists for the problem; contestation exists for whether the Hanbali solution was necessary.
narrative_ontology:disappearance_verdict(hanbali_reading, contested).
narrative_ontology:founding_problem_status(hanbali_reading, dead).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE RATIONALIST JURIST (SNARE) — A jurist trained in rationalist methods (qiyas, istislah) who cannot exit the Hanbali framework without abandoning their scholarly identity and tradition. Identity-locked: professional reputation, scholarly status, and self-conception are constituted through the jurisprudential school. Structurally constrained by institutional gatekeeping; cognitively locked by internalized methodological authority.
constraint_indexing:constraint_classification(hanbali_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: SECONDARY MADHAB COMMUNITY (TANGLED ROPE) — Communities following subsidiary schools or regional jurisprudential traditions experience genuine coordination benefits (shared legal rules, predictable judgment standards) while bearing costs through reduced methodological flexibility and potential legal outcomes less optimal for their circumstances. Constrained exit: departure requires institutional affiliation change and risks social fragmentation.
constraint_indexing:constraint_classification(hanbali_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE TRADITIONALIST SCHOOL HIERARCHY (ROPE) — Institutional beneficiary. Hanbali literalism and qiyas restriction concentrate methodological authority within traditionalist gatekeepers who control textual interpretation and transmission. Benefits from stable legal doctrine and institutional continuity. Arbitrage exit: can selectively engage or distance from the method depending on institutional pressures.
constraint_indexing:constraint_classification(hanbali_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: MODERN JURISPRUDENTIAL REFORM MOVEMENT (SCAFFOLD) — Organized scholars and institutions advocating for methodological pluralism see the literalism constraint as a temporary institutional barrier to be overcome through educational reform, canonical reinterpretation, and explicit authorization of flexible methods. Sunset logic: reform programs (revived maslaha doctrine, maqasid methodology, contemporary usul revisions) are creating alternative jurisprudential pathways. Mobile exit: modern practitioners can leverage comparative law and transnational institutional networks.
constraint_indexing:constraint_classification(hanbali_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: HISTORICAL HANBALI TRANSMISSION INSTITUTION (PITON) — The institutional apparatus for maintaining Hanbali methodological purity (ijaza chains, textual authentication, scholarly certification) persists largely through theatrical maintenance of authority rather than active enforcement. Theater ratio 0.38 reflects that much Hanbali institutional activity is performative certification of conformity rather than substantive methodological gatekeeping. The tradition maintains its own legitimacy narrative while actual methodological flexibility has substantially increased.
constraint_indexing:constraint_classification(hanbali_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (FALSE SUMMIT CANDIDATE) — From a trans-historical analytical view, text literalism appears as an inherent requirement of legal method itself: any legal system requires primary source authority and bounds on interpretive creativity. However, this naturalizes a contingent methodological choice. The structural data reveals the constraint as a constructed institutional arrangement benefiting traditionalist gatekeepers, not a law of jurisprudence. False summit detector will flag the beneficiary declarations against the mountain classification.
constraint_indexing:constraint_classification(hanbali_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hanbali_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hanbali_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hanbali_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(hanbali_reading, TR),
    TR >= 0.70.

:- end_tests(hanbali_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate extraction, reflecting that the Hanbali constraint concentrates interpretive authority but permits workaround practices and alternative madhabs. The beneficiary (traditionalist gatekeepers) extracts institutional respect and gatekeeping authority; the victim (rationalist interpreters) bears methodological constraints and reduced jurisprudential flexibility. The value is neither negligible nor total — institutional gatekeeping is real but porous. Suppression (0.45): Moderate-high, declining from historical highs. Early institutional consolidation required enforcement against alternative methodologies; modern Islamic jurisprudence permits rationalist interpretation even within Hanbali-identified frameworks, though tension remains. Barriers include institutional affiliation costs, scholarly reputation risks, and community boundary maintenance, but not impossibility of rationalist practice. Theater ratio (0.38): Moderate-low, reflecting that Hanbali institutional activity includes substantive methodological work (hadith authentication, textual exegesis) alongside performative gatekeeping. The theater ratio rises slightly over the interval as enforcement mechanisms soften and institutional legitimacy relies increasingly on reputation maintenance rather than active restriction. Claimed type (Tangled Rope): The constraint exhibits genuine coordination function (shared legal rules, methodological stability, predictable judgment standards) alongside asymmetric extraction (concentration of interpretive authority). Both elements are structurally present and analytically inseparable within the constraint.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates why kernel readings require indexed classification. From the traditionalist institutional perspective, Hanbali literalism is a Rope: it solves the coordination problem of maintaining doctrinal integrity and providing predictable legal guidance. From the rationalist jurist perspective, the same constraint is a Snare: methodological restriction is imposed without consent, cognitive identity lock prevents exit, and no real alternatives exist within the Hanbali framework. From the reform movement perspective, it is a Scaffold: temporary institutional barrier (sunset logic: modern jurisprudence is authorizing flexible interpretation) with organized agents building methodological alternatives. The institutional apparatus maintains a Piton perspective: the methodological gatekeeping has degraded while the institutional performance persists. The false summit perspective naturalizes text literalism as a law of jurisprudence itself, but beneficiary presence argues against genuine natural law. This perspectival divergence is not error — it is the kernel reading's diagnostic signal. The constraint's true structure is revealed precisely through the gap between perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality computation (d) derives from beneficiary/victim declarations and exit options. Traditionalist gatekeepers are institutional beneficiaries with arbitrage exit (d ≈ 0.15–0.25: low extraction). Rationalist jurists are victims with identity-locked exit (d ≈ 0.75–0.85: high extraction — the agent's professional identity is constituted through the methodological constraint, preventing movement despite structural mobility). Secondary madhab communities are moderate victims with constrained exit (d ≈ 0.45–0.55: moderate extraction). The reform movement is organized with mobile exit (d ≈ 0.35–0.45: moderate extraction — they face institutional resistance but control alternative pathways). The piton classification derives not from high directionality but from theater ratio: the institutional apparatus has increasingly substituted performance for substantive gatekeeping. The mountain classification (analytical perspective) is a false summit candidate: textual primacy appears as necessity when beneficiaries are examined (traditionalist schools benefit from naturalizing methodological choice as jurisprudential law), triggering FSM evaluation.
 *
 * MANDATROPHY ANALYSIS:
 *   The Hanbali constraint does not exhibit classic mandatrophy (mandate outliving function). The foundational mandate — establishing rigorous jurisprudential method through hadith authentication and literal textual reasoning — remains coherent and is still actively pursued by traditionalist scholars. However, secondary mandatrophy appears: the institutional gatekeeping apparatus (Piton perspective) has accumulated theatrical performance beyond its functional necessity. Modern jurisprudence increasingly authorizes flexible interpretation while maintaining Hanbali identity, suggesting the gatekeeping mandate is softer than historical enforcement suggested. The theater ratio trajectory models this: institutional certification activity persists while substantive gatekeeping declines. The mandatrophy is partial and peripheral, not central — the core jurisprudential method remains purposeful, but the institutional machinery has become increasingly decorative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    text_primacy_contingency,
    'Is the Hanbali insistence on text literalism a necessary feature of rigorous jurisprudence, or a contingent methodological choice that other schools demonstrate to be equally valid?',
    'Comparative outcome analysis: do rationalist methods (qiyas, istislah) produce legal incoherence or have they sustained alternative madhabs for 1400+ years with comparable internal consistency? Analysis of methodological justifications offered across schools.',
    'If text literalism is contingent: the constraint is a constructed institutional boundary (Tangled Rope, Snare, Scaffold confirmed). If necessary: the constraint approaches mountain status, though beneficiary presence argues against genuine natural law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(text_primacy_contingency, conceptual, 'Whether text literalism is necessary or contingent to jurisprudence').

omega_variable(
    weak_hadith_sufficiency_ambiguity,
    'Does the Hanbali preference for weak authenticated hadith over rational analogy (qiyas) actually produce more epistemically reliable outcomes, or does it merely substitute one form of interpretive authority (hadith authentication) for another (rationalist reconstruction)?',
    'Genealogical analysis of disputed rulings: cases where weak hadith and qiyas produce opposing conclusions; empirical examination of whether hadith-based rules or qiyas-based rules show higher stability across contexts and time periods.',
    'If weak hadith more reliable: extraction mechanism is efficiency-driven (Rope). If authority substitution: extraction mechanism is institutional gatekeeping (Snare/Tangled Rope confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(weak_hadith_sufficiency_ambiguity, empirical, 'Whether weak hadith methodology produces superior jurisprudential outcomes').

omega_variable(
    qiyas_restriction_functional_necessity,
    'Is the sharp restriction on qiyas functionally necessary to maintain doctrinal stability and prevent theological drift, or is it primarily a mechanism for concentrating interpretive authority within traditionalist gatekeepers?',
    'Institutional history: when and why did qiyas restriction intensify? Does doctrinal instability correlate with qiyas usage in other schools, or does qiyas-permissive jurisprudence show comparable stability? What does the historical record show about gatekeeping motivations versus stated methodological rationales?',
    'If functionally necessary: constraint is Rope or Tangled Rope with genuine coordination function. If primarily gatekeeping: constraint is Snare or extractive Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(qiyas_restriction_functional_necessity, empirical, 'Functional necessity of qiyas restriction versus gatekeeping mechanism').

omega_variable(
    reading_coexistence_versus_foreclosure,
    'Can the Hanbali reading (text literalism, qiyas restriction) coexist within a single unified Islamic jurisprudential framework with the rationalist readings (Hanafi, Maliki, Shafi''i emphasis on ra''y, istislah, qiyas), or do these readings logically foreclose one another when pressed to their foundations?',
    'Textual genealogy of foundational principles: do the readings rest on incompatible first premises about authority and method, or do they represent different weightings of compatible principles? Historical evidence of whether the schools have engaged as mutual legitimacy deniers or as coexisting alternatives.',
    'If coexistent: network topology is coexists_with (current assumption). If foreclosing: at least one reading''s axioms structurally eliminate the others'' core premises, requiring revision of reading_relations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_coexistence_versus_foreclosure, conceptual, 'Whether readings are logically coexistent or mutually foreclosing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hanbali_reading, 0, 750).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hanbali_tr_t0, hanbali_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(hanbali_tr_t250, hanbali_reading, theater_ratio, 250, 0.32).
narrative_ontology:measurement(hanbali_tr_t500, hanbali_reading, theater_ratio, 500, 0.36).
narrative_ontology:measurement(hanbali_tr_t750, hanbali_reading, theater_ratio, 750, 0.38).

% Extraction over time
narrative_ontology:measurement(hanbali_be_t0, hanbali_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(hanbali_be_t250, hanbali_reading, base_extractiveness, 250, 0.28).
narrative_ontology:measurement(hanbali_be_t500, hanbali_reading, base_extractiveness, 500, 0.32).
narrative_ontology:measurement(hanbali_be_t750, hanbali_reading, base_extractiveness, 750, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(hanbali_su_t0, hanbali_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(hanbali_su_t250, hanbali_reading, suppression_requirement, 250, 0.58).
narrative_ontology:measurement(hanbali_su_t500, hanbali_reading, suppression_requirement, 500, 0.48).
narrative_ontology:measurement(hanbali_su_t750, hanbali_reading, suppression_requirement, 750, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hanbali_reading, identity_coordination).
narrative_ontology:affects_constraint(hanbali_reading, hanafi_reading).
narrative_ontology:affects_constraint(hanbali_reading, maliki_reading).
narrative_ontology:affects_constraint(hanbali_reading, shafii_reading).

% DUAL FORMULATION NOTE:
% The Hanbali reading is one constraint within the contested kernel usul_al_fiqh_method. Each of the four major madhab readings is authored as a separate constraint story with its own ε-invariant extractiveness value, beneficiary structure, and perspectives. The network links are bidirectional: each reading affects the others by setting methodological boundaries and establishing alternative legitimacy claims. The constraint family is structured as a presheaf over the kernel: the kernel (contested foundational principles) admits four distinct readings, each with its own structural consequences.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hanbali_reading, moderate, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
