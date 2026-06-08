% ============================================================================
% CONSTRAINT STORY: hanafi_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hanafi_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: hanafi_reading
 *   human_readable: Hanafi Jurisprudential Method: Reason-Based Authority and Analogical Extension
 *   domain: islamic_jurisprudence/legal_theory/commitment_systems
 *
 * SUMMARY:
 *   The Hanafi jurisprudential method represents one historically dominant
 *   reading of how Islamic law adapts canonical texts to new circumstances
 *   through expansive use of qiyas (analogical reasoning) and istihsan
 *   (juristic preference based on contextual reasoning). This constraint
 *   instantiates a core tension in Islamic jurisprudence: how to maintain the
 *   authority of fixed foundational texts while acknowledging that those
 *   texts do not explicitly address most concrete legal questions that arise
 *   across centuries and variable contexts. The Hanafi reading privileges
 *   reason ('aql) as a co-equal source of juristic authority alongside
 *   textual sources, enabling judges and jurists to derive rulings by
 *   reasoning analogically from established cases and by applying general
 *   principles ('illah) even when explicit texts are silent. This creates
 *   genuine coordination benefits (adapting law to regional variation,
 *   addressing new contracts and technologies) but also generates asymmetric
 *   extraction: the tradition and institutional authorities benefit from
 *   methodological flexibility while conservative scholars,
 *   textually-oriented communities, and disempowered subjects bear the burden
 *   of justifying departures from explicit text. The constraint exhibits
 *   different types from different structural positions, reflecting the
 *   fundamental contestation about the proper balance between textual fixity
 *   and rational flexibility.
 *
 * KEY AGENTS:
 *   - Hanafi Juristic Tradition: Primary beneficiary (institutional/arbitrage) — gains authority to derive rulings beyond explicit texts; can adapt to regional variation and new circumstances while claiming Islamic legitimacy
 *   - Regional Qadi (Judicial Administrator): Mixed agent (moderate/constrained) — benefits from flexibility to adapt rulings to local conditions; bears cost of justifying departures from explicit text to conservative constituencies
 *   - Textual Literalist Communities: Primary victim (powerless/identity_locked) — their methodological framework (text-only) is systematically displaced; cannot exit without abandoning scholarly identity constituted through literal textual fidelity
 *   - Reformist Modernizers: Secondary beneficiary/organized agent (organized/mobile) — use istihsan as temporary scaffold for deriving modern rulings; see the method as transitional pending better-developed principles
 *   - Conservative Institutional Authority: Institutional actor (institutional/constrained) — maintains performative textual rigor while operating with substantive flexibility; preserves both positions through institutional theater
 *   - Sibling Jurisprudential Schools: Competing readers (institutional/arbitrage) — Maliki, Shafi'i, Hanbali traditions offer alternative methodologies with different scope and flexibility parameters; coexist with Hanafi in contested terrain
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hanafi_reading, 0.35).
domain_priors:suppression_score(hanafi_reading, 0.4).
domain_priors:theater_ratio(hanafi_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hanafi_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(hanafi_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(hanafi_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hanafi_reading, tangled_rope).
narrative_ontology:human_readable(hanafi_reading, "Hanafi Jurisprudential Method: Reason-Based Authority and Analogical Extension").
narrative_ontology:topic_domain(hanafi_reading, "islamic_jurisprudence/legal_theory/commitment_systems").

domain_priors:requires_active_enforcement(hanafi_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hanafi_reading, 'b0a8670f-8df4-4c2e-ab3b-b09d85540e13').
narrative_ontology:cs_kernel_codification('b0a8670f-8df4-4c2e-ab3b-b09d85540e13', formalized).
narrative_ontology:cs_authority_grounding('b0a8670f-8df4-4c2e-ab3b-b09d85540e13', lineage).
narrative_ontology:cs_interpretation_layer_present('b0a8670f-8df4-4c2e-ab3b-b09d85540e13').
narrative_ontology:cs_reading_relation('b0a8670f-8df4-4c2e-ab3b-b09d85540e13', hanafi_reading__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('b0a8670f-8df4-4c2e-ab3b-b09d85540e13', hanafi_reading__shafi_i_reading, coexists_with).
narrative_ontology:cs_reading_relation('b0a8670f-8df4-4c2e-ab3b-b09d85540e13', hanafi_reading__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('b0a8670f-8df4-4c2e-ab3b-b09d85540e13', foundational, reason_as_coequal_authority).
narrative_ontology:cs_axiom_status(reason_as_coequal_authority, holdable).
narrative_ontology:cs_axiom_grounding('b0a8670f-8df4-4c2e-ab3b-b09d85540e13', reason_as_coequal_authority, deontological).
narrative_ontology:cs_axiom('b0a8670f-8df4-4c2e-ab3b-b09d85540e13', foundational, istihsan_legitimate_juristic_prerogative).
narrative_ontology:cs_axiom_status(istihsan_legitimate_juristic_prerogative, holdable).
narrative_ontology:cs_axiom_grounding('b0a8670f-8df4-4c2e-ab3b-b09d85540e13', istihsan_legitimate_juristic_prerogative, conventional).
narrative_ontology:cs_reference_frame('b0a8670f-8df4-4c2e-ab3b-b09d85540e13', classical_hanafi_juristic_flexibility).
narrative_ontology:cs_drift_state('b0a8670f-8df4-4c2e-ab3b-b09d85540e13', contemporary_institutional_formalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b0a8670f-8df4-4c2e-ab3b-b09d85540e13', '2026-02-27T14:32:00Z').
narrative_ontology:cs_kernel_id(hanafi_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hanafi_reading, hanafi_juristic_tradition).
narrative_ontology:constraint_beneficiary(hanafi_reading, regional_legal_adaptation).
narrative_ontology:constraint_victim(hanafi_reading, textual_certainty_claims).
narrative_ontology:constraint_victim(hanafi_reading, non_hanafi_methodologies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hanafi_reading, regional_judges_qadis).
narrative_ontology:constraint_beneficiary(hanafi_reading, reformist_modernizers).
narrative_ontology:constraint_beneficiary(hanafi_reading, conservative_institutional_authorities).
narrative_ontology:constraint_victim(hanafi_reading, textual_literalist_communities).
narrative_ontology:constraint_victim(hanafi_reading, regional_judges_qadis).
narrative_ontology:constraint_vindicates(hanafi_reading, reason_as_interpretive_authority).
narrative_ontology:constraint_vindicates(hanafi_reading, juristic_preference_legitimacy).
narrative_ontology:constraint_vindicates(hanafi_reading, context_dependent_application).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hanafi jurisprudential authorities and institutional actors (Al-Azhar Hanafi faculty, state muftis under Hanafi tradition, religious courts deploying Hanafi method) set the agenda for what counts as legitimate istihsan and qiyas. They determine the boundaries of acceptable analogical extension and juristic preference. They benefit from the flexibility this method provides and have arbitrage options — they can emphasize textual rigor when politically convenient and invoke istihsan when circumstances demand. They maintain institutional authority over the tradition's interpretation.
narrative_ontology:constraint_stakeholder(hanafi_reading, hanafi_institutional_scholars, agenda_setter,
    institutional, generational, arbitrage, global).

% Communities and scholars who hold that explicit textual sources are the only legitimate basis for rulings bear the cost of systematic displacement of their methodological framework. They cannot exit this constraint without abandoning their scholarly and spiritual identity, which is constituted through commitment to literal textual fidelity. They experience istihsan as usurpation of textual authority by judicial preference. They can object but cannot organize effectively or leave the Islamic jurisprudential conversation.
narrative_ontology:constraint_stakeholder(hanafi_reading, textual_literalist_communities, payer,
    powerless, biographical, identity_locked, regional).

% Regional judges (qadi) benefit from the flexibility that expansive qiyas and istihsan provide for adapting rulings to local conditions and contemporary circumstances. They can derive rulings for new cases without waiting for explicit textual guidance. However, they pay the cost of justifying those departures to conservative constituencies and formal authorities who demand text-based reasoning. They are constrained by needing to maintain both practical flexibility and formal legitimacy.
narrative_ontology:constraint_stakeholder(hanafi_reading, regional_judges_qadis, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(hanafi_reading, regional_judges_qadis, payer).

% Contemporary Islamic legal reformers benefit from istihsan as a temporary mechanism for deriving modern rulings on banking, contract law, and medical ethics. They see the method as transitional scaffolding — useful now, but eventually to be superseded by explicitly developed principles and reformed foundational sources. They have mobility to shift to alternative modernizing approaches as these develop. They use istihsan strategically as a bridge toward systematic modernization.
narrative_ontology:constraint_stakeholder(hanafi_reading, reformist_modernizers, beneficiary,
    organized, generational, mobile, global).

% Conservative institutional bodies (formal Islamic authorities maintaining strict interpretive standards) maintain explicit statements restricting qiyas and istihsan to narrow circumstances, while actual judicial practice deploys these methods more expansively. They are constrained by needing to maintain both formal conservatism (to satisfy doctrinal constituencies) and practical relevance (by permitting judges to adapt rulings to contemporary circumstances). They benefit from the appearance of textual rigor even as they enable substantive flexibility.
narrative_ontology:constraint_stakeholder(hanafi_reading, conservative_institutional_authorities, agenda_setter,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(hanafi_reading, conservative_institutional_authorities, beneficiary).

% The Maliki, Shafi'i, and Hanbali jurisprudential traditions observe and compete with the Hanafi reading. They offer alternative methodologies with different boundaries on analogical extension and juristic preference. They coexist as live options in Islamic jurisprudence; each is deployed for specific doctrinal domains and institutional contexts. They have arbitrage options — they can deploy their methods strategically to claim authority in contested domains (modern banking, contract law) where Hanafi flexibility is challenged.
narrative_ontology:constraint_stakeholder(hanafi_reading, sibling_jurisprudential_schools, observer,
    institutional, civilizational, arbitrage, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: How can Islamic jurisprudence maintain the authority of canonical fixed texts while deriving rulings for new circumstances, regional variations, and cases not explicitly addressed in those texts? The Hanafi method solves this by enabling judges and jurists to reason analogically from established cases to new cases and to apply general principles even when explicit textual guidance is absent.
% TRANSFER_FUNCTION: Authority flows from juristic institutions and established schools to judges and regional authorities who deploy qiyas and istihsan. Legitimacy (the authority to pronounce binding rulings) is transferred from textual literalism to reason-based extension. Labor (justification of non-textual rulings) flows from institutions to lower-level judges. Burden (of defending departures from explicit text) flows to conservative constituencies who must accept rulings derived beyond textual authority.
% ABSENT_VOICES: Explicit textualists who reject qiyas and istihsan are absent from the institutional formation of Hanafi jurisprudential authority; they are consulted through formal doctrine but do not set agendas. Communities practicing Islamic law outside the formal jurisprudential tradition (customary Islamic practice, women's religious authority, non-elite interpretive communities) are largely absent from institutionalized jurisprudential discourse. Subjects of rulings — the people governed by qadi decisions — are not represented in the conversation about legitimate methodological boundaries.
% DISAPPEARANCE_RATIONALE: If the Hanafi method of istihsan disappeared, Islamic jurisprudence would face acute pressure: courts could not derive rulings for modern circumstances without explicit textual guidance. The world would rearrange by either (1) developing alternative methods for addressing textual gaps (explicit legislative reform, systematic rewriting of foundational sources, or adoption of alternative jurisprudential methodologies), or (2) reverting to strict textualism, which would leave many contemporary legal questions formally unresolved. Whether the world would rearrange or remain stable depends on whether one believes Islamic jurisprudence requires methods for textual extension (rearrangement) or whether one believes strict textualism plus legislative reform suffices (relative stability).
% FOUNDING_PROBLEM: Islamic jurisprudence required a method for adapting canonical texts (Qur'an and Sunnah of the Prophet) to new circumstances, regional contexts, and cases arising centuries after the foundational texts were established. The founding problem is not primarily theoretical but practical: judges must pronounce rulings on cases the texts do not explicitly address. Qiyas and istihsan were developed as jurisprudential techniques for deriving such rulings while maintaining Islamic legitimacy.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary Islamic jurisprudence universally acknowledges the need for methods addressing textual gaps — this is corroborated across all four major schools (Hanafi, Maliki, Shafi'i, Hanbali), though they differ on methodological scope and boundaries. The problem persists acutely in modern contexts: Islamic banking and finance cannot be addressed through explicit canonical texts; contemporary contract law requires derived rulings. The only parties who dispute the problem status are strict literalists who argue that an absence of textual guidance means no Islamic ruling should be pronounced at all — but this position is marginal in institutional Islamic jurisprudence, not widespread.
narrative_ontology:disappearance_verdict(hanafi_reading, contested).
narrative_ontology:founding_problem_status(hanafi_reading, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TEXTUAL LITERALIST (SNARE) — Community members or scholars who hold that explicit textual foundations are the only legitimate basis for rulings face systematic displacement of their interpretive framework. The expansion of qiyas and istihsan marginalizes their textual-priority methodology without offering organizational exit. Identity-locked: their scholarly identity is constituted through literal textual fidelity; abandoning this framework requires abandoning their scholarly self-conception. Maximum extraction from this perspective — the constraint operates against their methodological commitments while offering no coordination benefit.
constraint_indexing:constraint_classification(hanafi_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 2: REGIONAL QADI (TANGLED ROPE) — A judge in a specific locality benefits from the flexibility that qiyas and istihsan provide for adapting rulings to regional conditions and contemporary circumstances. The constraint coordinates local adaptation (genuine collective problem: how to apply fixed texts to variable conditions). Simultaneously, the qadi bears the cost of justifying departures from explicit text to conservative constituencies — the method's flexibility is exercised only under suppression constraints. Significant extraction but within a coordination framework.
constraint_indexing:constraint_classification(hanafi_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HANAFI LEGAL SCHOOL (ROPE) — The Hanafi tradition benefits from the methodological flexibility that expansive qiyas and istihsan provide. The method enables the school to claim jurisdiction over evolving circumstances (trade disputes, administrative structures, technological change) that the canonical texts do not explicitly address. The school has arbitrage options — it can emphasize textual authority when politically convenient and pragmatic adaptation when circumstances demand. Net beneficiary of this constraint. The coordination problem it solves is genuine (how to maintain legal authority across centuries and variable contexts) but the distribution is asymmetric: the school collects authority benefits while local judges and conservative scholars bear the justificatory burden.
constraint_indexing:constraint_classification(hanafi_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REFORMIST MODERNIZERS (SCAFFOLD) — Contemporary Islamic legal reformers see expansive istihsan as a temporary scaffolding for deriving modern rulings (on banking, contract law, medical ethics) that will eventually be superseded by explicit new textual foundations (rewriting of usul principles, development of new canonical sources, or explicit legislative codification). The scaffold perspective sees the extension mechanism as transitional — a bridge until Islamic jurisprudence develops formal methods for deriving rulings that match contemporary governance needs without relying on ad-hoc judicial preference. Sunset condition: when systematic principles for modern contract derivation are established, istihsan becomes unnecessary.
constraint_indexing:constraint_classification(hanafi_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: CONSERVATIVE INSTITUTIONAL AUTHORITY (PITON) — A formal Islamic authority body (Azhar institution, state mufti office, formal fatwa council) maintains explicit adherence to restrictive qiyas standards while judges operate with broader istihsan standards in practice. The performative maintenance of textual rigor at the institutional level masks substantive juristic flexibility at the application level. The constraint persists through institutional inertia — the formal statement of method differs markedly from actual practice — because abandoning either the textual performance or the practical flexibility would provoke political opposition. Theater ratio reflects the gap between proclaimed methodological conservatism and actual expansive practice.
constraint_indexing:constraint_classification(hanafi_reading, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / LOGICAL NECESSITY (MOUNTAIN) — From a civilizational/universal analytical perspective, some form of analogical extension is necessarily inherent to any legal system that maintains canonical fixed texts across time — the gap between textual specificity and new circumstances is logically irreducible. Any system claiming to apply unchanging texts to evolving conditions must develop methods for extension; qiyas and istihsan are the name Hanafi jurisprudence gives to this inevitable gap-closure. This perspective risks naturalizing what is actually a choice about WHO DECIDES when and how extension occurs and WHICH EXTENSION PRINCIPLES ARE LEGITIMATE. The engine's false summit detection will likely reclassify this as a revealed rather than natural mountain.
constraint_indexing:constraint_classification(hanafi_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hanafi_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hanafi_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hanafi_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(hanafi_reading, TR),
    TR >= 0.70.

:- end_tests(hanafi_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The Hanafi method generates genuine coordination benefits (regional adaptation, addressing new cases) but distributes these benefits asymmetrically. The tradition and institutional authorities capture the authority gains while judges and conservative communities bear the justificatory burden. The measurement reflects that substantive extraction occurs within a coordination framework — not pure extraction (Snare) but asymmetric distribution within mixed Tangled Rope. Suppression (0.40): Moderate. Conservative constituencies maintain genuine objections to expansive istihsan; judges must justify departures from explicit text; the method faces doctrinal and political resistance. Suppression has intensified historically (measurement rises from 0.35 to 0.40) as reformist and conservative critiques have formalized. But suppression is not overwhelming — the method maintains institutional authority and practical deployment. Theater ratio (0.38): Low-moderate. The Hanafi method's performative content is lower than many institutional practices because the rationale for extension is explicitly theorized and debated (qiyas and istihsan are openly discussed jurisprudential techniques, not hidden procedures). However, theater rises slightly over the interval (0.25 → 0.38) as the gap widens between formal methodological conservatism (in institutional fatwa statements) and actual expansive practice (in applied jurisprudence). Contemporary conservative institutional authorities often maintain formal restrictions on istihsan while judges deploy it substantively — this performative gap drives the rising theater measurement.
 *
 * PERSPECTIVAL GAP:
 *   The constraint classifies as six different types from six structural positions. Textual literalists experience Snare (their framework is displaced without their consent or exit). Regional judges experience Tangled Rope (benefits from flexibility, costs from justification burden). The Hanafi school experiences Rope (net beneficiary, coordination problem solved). Reformist modernizers experience Scaffold (temporary mechanism pending better development). Conservative institutions experience Piton (performative textual rigor masking substantive flexibility). The analytical observer risks seeing Mountain (logical necessity for extension) but this risks false summit — naturalizing a methodological choice as inevitable law. The perspectival gap reveals the constraint's fundamental instability: there is no single fact about whether istihsan is coordination or extraction; it depends entirely on which agent's structural position is the reference frame.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's effective extraction is computed from base extractiveness (0.35), directionality toward the constraint (d), and scope modulation. Beneficiaries of the Hanafi tradition have low d (they collect authority from the method) → low/negative effective extraction. Textual literalists have high d (the method operates against their framework) → high effective extraction experienced from their position. Regional judges have moderate d (they both benefit and bear costs) → moderate effective extraction. The Hanafi school's arbitrage exit options lower their d further despite institutional power, because they have genuine alternatives (emphasize text when convenient, invoke istihsan when necessary). Textual literalists' identity-lock prevents exit even though their structural position might permit it materially — their identity is constituted through literal textuality, so abandoning this frame requires abandoning themselves. This differentiates their trapped/identity_locked experience from mere constrained exit.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATE PERSISTENCE: The Hanafi method's founding mandate (adapt Islamic law to regional variation across the Islamic world) remains substantially live. Contemporary application of istihsan to modern contracts, banking instruments, and medical ethics shows the method continuing to serve its original function — deriving rulings for new circumstances using reason-based extension. However, the mandate's scope has contracted in some institutional domains (state courts using secular law rather than Islamic jurisprudence) and expanded in others (Islamic finance, which heavily deploys Hanafi flexibility). Mandatrophy is not fully resolved; the constraint persists both because its original problem remains and because institutional actors benefit from maintaining it. The measurement trajectory (theater rising, suppression stable, extractiveness modest but persistent) reflects a constraint neither dead nor flourishing — maintained through institutional inertia and continued utility, not through dramatic enforcement or universal acceptance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_juristic_choice,
    'Is the methodological necessity for analogical extension a natural law of legal reasoning, or is it a particular choice about institutional authority that Hanafi jurisprudence has made?',
    'Comparative jurisprudence: do non-Islamic legal systems develop parallel mechanisms for textual extension? If yes, the extension mechanism appears natural. If comparative systems use fundamentally different methods for the same problem (e.g., explicit legislative revision, constitutional amendment procedures, precedent-based evolution), then extension method is methodological choice, not natural law.',
    'If natural law: Hanafi method appears as inevitable accommodation to logical limits. If methodological choice: Hanafi method reveals as constructed authority claim with identifiable beneficiaries (the tradition, the jurists). False summit detection would flag this reading''s mountain perspective as naturalization.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_juristic_choice, conceptual, 'Whether analogical extension is natural necessity or methodological choice').

omega_variable(
    legitimate_scope_of_istihsan,
    'At what point does juristic preference (istihsan) transition from coordination mechanism (adapting text to circumstance) to extraction mechanism (judges imposing preferred outcomes under cover of juristic reasoning)?',
    'Doctrinal analysis: compare instances where istihsan was deployed to adapt rulings to new circumstances (coordination signal) vs instances where it was deployed to override established precedent for reasons of political convenience or personal preference (extraction signal). Establish patterns of istihsan use across historical periods and legal domains.',
    'If coordination dominates: Hanafi method classifies as legitimate Rope or Tangled Rope. If extraction instances exceed coordination, the reading shifts toward Snare. If the boundary is genuinely undecidable, the constraint''s classification becomes observer-relative rather than objective — a fundamental instability in the Hanafi framework itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimate_scope_of_istihsan, empirical, 'Boundary between legitimate juristic preference and illegitimate judicial overreach').

omega_variable(
    kernel_reading_contest,
    'Is the Hanafi reading of the jurisprudential method kernel a live reading that continues to hold authority in contemporary Islamic jurisprudence, or has it been formally superseded or constrained by other readings (Maliki, Shafi''i, Hanbali) in contested domains?',
    'Survey contemporary Islamic legal institutions, fatwa councils, and academic jurisprudence: which readings are deployed for modern rulings on banking, contract law, medical ethics? Document instances where Hanafi expansiveness meets institutional resistance from other methodologies. Chart the territorial and doctrinal distribution of each reading.',
    'If Hanafi remains competitive: reading relationships remain as coexists_with. If Hanafi has been formally rejected or restricted in specific domains: reading_relations may shift to influences or (in rare cases) forecloses. If one reading has achieved hegemonic status, the kernel''s pluralism may be closing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Contemporary authority status of Hanafi reading vs sibling readings').

omega_variable(
    reason_authority_grounding,
    'What grounds the legitimacy of reason (aql) as a juristic authority in Hanafi method? Is it explicit textual permission, consensus of the tradition, or independent rational necessity?',
    'Doctrinal genealogy: trace the sources cited by Hanafi jurisprudence for deploying reason as juristic authority. If grounded in explicit texts, examine whether those texts genuinely authorize independent rational extension or merely permit it within textual constraints. If grounded in consensus, document the historical formation of that consensus and whether it is disputed.',
    'If reason''s authority is textually grounded but weakly supported: Hanafi reasoning depends on interpretive extension of those texts, making the method parasitic on the very textual authority it brackets. If reason''s authority derives from tradition, the chain of transmission becomes critical — breaks in the chain (reformist rejection, institutional change) threaten the axiom. If reason''s authority is independent rational necessity: the method transcends Islamic legal boundaries and becomes comparable to non-Islamic systems.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reason_authority_grounding, conceptual, 'Epistemic grounding of reason as juristic authority').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hanafi_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hanafi_tr_t0, hanafi_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(hanafi_tr_t100, hanafi_reading, theater_ratio, 100, 0.38).
narrative_ontology:measurement(hanafi_tr_t200, hanafi_reading, theater_ratio, 200, 0.42).

% Extraction over time
narrative_ontology:measurement(hanafi_be_t0, hanafi_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(hanafi_be_t100, hanafi_reading, base_extractiveness, 100, 0.32).
narrative_ontology:measurement(hanafi_be_t200, hanafi_reading, base_extractiveness, 200, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(hanafi_su_t0, hanafi_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(hanafi_su_t100, hanafi_reading, suppression_requirement, 100, 0.4).
narrative_ontology:measurement(hanafi_su_t200, hanafi_reading, suppression_requirement, 200, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hanafi_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(hanafi_reading, 0.12).
narrative_ontology:affects_constraint(hanafi_reading, maliki_reading).
narrative_ontology:affects_constraint(hanafi_reading, shafi_i_reading).
narrative_ontology:affects_constraint(hanafi_reading, hanbali_reading).
narrative_ontology:affects_constraint(hanafi_reading, islamic_banking_fatwa_authority).
narrative_ontology:affects_constraint(hanafi_reading, consensus_ijma_modernization).

% DUAL FORMULATION NOTE:
% The Hanafi reading is part of a four-story constraint family decomposing the contested jurisprudential_method_kernel. Each sibling reading (Maliki, Shafi'i, Hanbali) has its own constraint story with different beneficiary structures, different scope limitations, and different historical trajectories. The ε values differ: Hanafi istihsan is more extractive than Hanbali textualism, less extractive than some Maliki innovations. Network links preserve the structural relationship: each reading influences and coexists with its siblings. The family is not internally ordered by truth-value (no single reading is 'correct') but by institutional distribution and contemporary deployment patterns.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hanafi_reading, moderate, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
