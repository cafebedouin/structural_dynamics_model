% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__shafii_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__shafii_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: jurisprudential_method_kernel__shafii_reading
 *   human_readable: Shafi'i Jurisprudential Method Kernel Reading
 *   domain: islamic_jurisprudence/legal_theory/institutional_pluralism
 *
 * SUMMARY:
 *   The Shafi'i jurisprudential method represents one systematized approach
 *   to resolving the foundational problem of Islamic legal authority: how to
 *   derive binding rules from multiple authoritative sources (Quran, Hadith,
 *   Consensus, Analogical Reasoning) when they conflict or remain silent on
 *   specific issues. The Shafi'i reading establishes a strict hierarchical
 *   priority — Quran first, then authenticated Hadith, then Consensus,
 *   finally Qiyas (analogical reasoning) — and explicitly rejects Urf
 *   (customary practice) and Istihsan (juristic preference) as independent
 *   authoritative sources. This reading differs structurally from competing
 *   madhab (jurisprudential schools): Hanafi methodology grants greater
 *   weight to Qiyas and Istihsan; Maliki tradition recognizes Urf as an
 *   independent source; Hanbali approach emphasizes textual literalism with
 *   minimal reasoning. The Shafi'i constraint operates as a Tangled Rope: it
 *   provides genuine coordination benefits (standardized methodology enables
 *   scholarly communication across regions, produces consistent
 *   jurisprudential conclusions, creates institutional legitimacy for fatwa
 *   issuance) while simultaneously extracting from those whose local
 *   practices and alternative reasoning methods are delegitimized by the
 *   hierarchy. The tension manifests most acutely at the victim level —
 *   communities whose customary practices diverge from authenticated hadith
 *   interpretations experience the constraint as pure extraction (Snare from
 *   their perspective), while established ulama networks benefit from
 *   institutional authority to authenticate and interpret hadith. The theater
 *   ratio has increased over the 500-year measurement interval as
 *   institutional formalism hardened the methodology into ritualized
 *   curricula and jurisprudential textbooks, while actual legal development
 *   (state legislation, constitutional interpretation, judicial reform)
 *   increasingly bypassed the hierarchy without formally disavowing it.
 *
 * KEY AGENTS:
 *   - Shafi'i Ulama Establishment (institutional/arbitrage): Primary beneficiary — control over hadith authentication, prestige in jurisprudential standardization, institutional continuity through formalized transmission
 *   - Hadith Transmission Networks (institutional/arbitrage): Secondary beneficiary — authentication authority concentrates prestige and institutional resources within established scholarly lineages
 *   - Local Customary Practice Communities (powerless/trapped): Primary victim — customary legal traditions delegitimized as non-scriptural; no exit without abandoning community identity
 *   - Regional Qadi Networks (moderate/constrained): Secondary victim/beneficiary hybrid — gain legitimacy and institutional support but lose discretion and autonomy in judgment
 *   - Islamic Legal Reform Movements (organized/constrained): Tertiary actor (analytical observer position) — view the constraint as a temporary institutional framework being superseded by independent reasoning
 *   - State Legal Systems (institutional/arbitrage): Contemporary beneficiary — use Shafi'i hierarchy to legitimate state legal codes while actually operating through secular legislative logic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__shafii_reading, 0.38).
domain_priors:suppression_score(jurisprudential_method_kernel__shafii_reading, 0.52).
domain_priors:theater_ratio(jurisprudential_method_kernel__shafii_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__shafii_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__shafii_reading, "Shafi'i Jurisprudential Method Kernel Reading").
narrative_ontology:topic_domain(jurisprudential_method_kernel__shafii_reading, "islamic_jurisprudence/legal_theory/institutional_pluralism").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__shafii_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__shafii_reading, '91d07bc7-bb72-44ed-bbe3-fc2b84a1ac1a').
narrative_ontology:cs_kernel_codification('91d07bc7-bb72-44ed-bbe3-fc2b84a1ac1a', formalized).
narrative_ontology:cs_authority_grounding('91d07bc7-bb72-44ed-bbe3-fc2b84a1ac1a', lineage).
narrative_ontology:cs_interpretation_layer_present('91d07bc7-bb72-44ed-bbe3-fc2b84a1ac1a').
narrative_ontology:cs_reading_relation('91d07bc7-bb72-44ed-bbe3-fc2b84a1ac1a', jurisprudential_method_kernel__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('91d07bc7-bb72-44ed-bbe3-fc2b84a1ac1a', jurisprudential_method_kernel__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('91d07bc7-bb72-44ed-bbe3-fc2b84a1ac1a', jurisprudential_method_kernel__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('91d07bc7-bb72-44ed-bbe3-fc2b84a1ac1a', foundational, hadith_authentication_as_boundary_constraint).
narrative_ontology:cs_axiom_status(hadith_authentication_as_boundary_constraint, holdable).
narrative_ontology:cs_axiom_grounding('91d07bc7-bb72-44ed-bbe3-fc2b84a1ac1a', hadith_authentication_as_boundary_constraint, empirically_contingent).
narrative_ontology:cs_axiom('91d07bc7-bb72-44ed-bbe3-fc2b84a1ac1a', foundational, rejection_of_urf_as_independent_source).
narrative_ontology:cs_axiom_status(rejection_of_urf_as_independent_source, holdable).
narrative_ontology:cs_axiom_grounding('91d07bc7-bb72-44ed-bbe3-fc2b84a1ac1a', rejection_of_urf_as_independent_source, deontological).
narrative_ontology:cs_reference_frame('91d07bc7-bb72-44ed-bbe3-fc2b84a1ac1a', formalized_hadith_centered_hierarchy).
narrative_ontology:cs_drift_state('91d07bc7-bb72-44ed-bbe3-fc2b84a1ac1a', contemporary_state_legal_pluralism, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('91d07bc7-bb72-44ed-bbe3-fc2b84a1ac1a', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__shafii_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__shafii_reading, shafii_ulama_establishment).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__shafii_reading, hadith_transmission_networks).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__shafii_reading, local_legal_pluralism).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__shafii_reading, regional_customary_practice).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOCAL CUSTOMARY COMMUNITIES (SNARE) — Trapped within regional jurisdictions where Shafi'i methodology forecloses local Urf as authoritative. Customary practices that served communities for generations are delegitimized as non-scriptural. Maximum extraction: communities bear the cost of standardization and lose agency over their own legal traditions. No exit without abandoning community identity.
constraint_indexing:constraint_classification(jurisprudential_method_kernel__shafii_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: REGIONAL QADI NETWORKS (TANGLED ROPE) — Constrained by the standardized methodology but also benefit from its legitimacy and institutional support. Qadis experience genuine coordination benefits (unified legal framework enables cross-jurisdictional judgments) alongside extraction (loss of discretion, subordination to authenticated hadith corpus). Can theoretically exit via Hanafi or Maliki methodology but face career and professional costs.
constraint_indexing:constraint_classification(jurisprudential_method_kernel__shafii_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SHAFI'I ULAMA ESTABLISHMENT (ROPE) — Primary beneficiary. Institutional actors who control hadith authentication, curriculum design, and jurisprudential methodology derive substantial benefits: authority to determine which hadith are 'authenticated,' prestige in establishing jurisprudential standards, and institutional continuity through formalized transmission. Experiences the constraint as coordination: standardized methodology enables scholarly communication and institutional reproduction. Net beneficiary — the constraint distributes authority toward this group.
constraint_indexing:constraint_classification(jurisprudential_method_kernel__shafii_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ISLAMIC LEGAL REFORM MOVEMENTS (SCAFFOLD) — Organized agents (modern Islamic reformers, state legal commissions, contemporary jurists) see the Shafi'i hierarchy as a temporary institutional framework that is being superseded by independent reasoning (ijtihad) and contextual reinterpretation. Reform movements view the Shafi'i constraint as a sunset clause problem: as educational institutions modernize and sources-of-law pluralism expands, the hierarchy loses prescriptive force. Theater decreases as reformers bypass the established methodology.
constraint_indexing:constraint_classification(jurisprudential_method_kernel__shafii_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CONTEMPORARY INSTITUTIONAL FORMALISM (PITON) — Traditional Shafi'i jurisprudential methodology persists as institutional ritual in formal legal education and fatwa structures, but actual judicial reasoning and legislative processes increasingly bypass the hierarchy. The formalized ranking (Quran > Hadith > Ijma > Qiyas) is maintained through inertia in curricula and formal jurisprudential texts, but substantive legal development occurs outside this framework. Theater is high because institutional actors continue to cite the hierarchy while violating its logic through legislative innovation and constitutional interpretation.
constraint_indexing:constraint_classification(jurisprudential_method_kernel__shafii_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / EPISTEMOLOGICAL NECESSITY VIEW (MOUNTAIN) — From a civilizational perspective, some standardization of legal sources is logically necessary to prevent infinite regress in jurisprudential reasoning. Any coherent legal system must establish which texts bind and which do not; some hierarchy of sources is inherent to the structure of law itself. This perspective sees the Shafi'i methodology as reflecting an epistemological necessity rather than a contingent institutional choice. However, the structural data contradicts the mountain classification — the engine will compute this as a false summit, revealing that source prioritization is historically contingent and contested.
constraint_indexing:constraint_classification(jurisprudential_method_kernel__shafii_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__shafii_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(jurisprudential_method_kernel__shafii_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(jurisprudential_method_kernel__shafii_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(jurisprudential_method_kernel__shafii_reading, TR),
    TR >= 0.70.

:- end_tests(jurisprudential_method_kernel__shafii_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The Shafi'i hierarchy coordinates legitimate jurisprudential reasoning and produces non-arbitrary conclusions from multiple sources, justifying some of the normative structure. However, the strict prioritization of authenticated hadith over local practice benefits those who control authentication (established ulama) at the expense of regional communities. The value reflects that the methodology contains both genuine coordination and asymmetric extraction. Suppression (0.52): Moderate-high. Customary practice and alternative reasoning methods (Istihsan, Urf) are explicitly delegitimized within the hierarchy, preventing communities from appealing to their own legal traditions or permitting alternative scholarly approaches. However, suppression is not total — Hanafi, Maliki, and Hanbali schools remain available as alternatives, and local judges retain some discretion in applying hadith to specific cases. Theater ratio (0.58): Moderate-high. The methodology functions with partial performativity: the stated hierarchy (Quran > Hadith > Ijma > Qiyas) guides jurisprudential discourse, but Ijma is determined by established ulama (circular), hadith authentication criteria are institutionalized preferences rather than objective standards, and Qiyas operates more flexibly in practice than the hierarchy's position suggests. Contemporary institutional contexts show high theater — the hierarchy is invoked in formal jurisprudential texts while actual legal development proceeds through state legislation and constitutional interpretation that bypass the methodology entirely.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates fundamental perspectival divergence across different structural positions within the jurisprudential landscape. The Shafi'i ulama establishment perceives the methodology as Rope (coordination mechanism that enables scholarly communication and produces legitimate jurisprudential conclusions). Local customary communities perceive it as Snare (their legal traditions are delegitimized and they bear extraction while benefiting from neither the institutional authority nor the prestige of the scholarly hierarchy). Regional qadi networks perceive Tangled Rope (genuine coordination benefits of standardized methodology alongside extraction from loss of discretion). Islamic legal reform movements perceive Scaffold (the hierarchy is a temporary institutional framework being superseded by modern legal reasoning, with a realistic sunset timeline). Contemporary institutional actors perceive Piton (the hierarchy is performative ritual maintained through inertia while actual legal development occurs outside it). The analytical observer risks perceiving Mountain (a logically necessary feature of any coherent legal system) but the structural data reveals this as a false summit — the hierarchy's necessity is contingent on accepting specific assumptions about what constitutes 'authenticity' and who has authority to determine it.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value derives from the agent's structural position relative to the constraint. Beneficiaries (Shafi'i ulama, hadith networks) derive d from institutional power + arbitrage exit options + beneficiary status = low d (approximately 0.15), producing negative effective extraction (χ negative) — these agents experience the constraint as enabling rather than constraining. Regional qadi networks derive d from moderate power + constrained exit + victim status (loss of discretion) = moderate d (approximately 0.55), producing moderate χ. Local customary communities derive d from powerless position + trapped exit (cannot abandon community) + victim status = high d (approximately 0.90), producing high χ — maximum experienced extraction. The contemporary state legal system derives d from institutional power + arbitrage exit (can formally adopt or abandon the methodology) + beneficiary status (uses hierarchy to legitimate legislation while bypassing it operationally) = low d, producing negative χ. The analytical observer derives d from analytic power + analytic exit options = canonical d (approximately 0.73), producing moderate χ.
 *
 * MANDATROPHY ANALYSIS:
 *   The Shafi'i reading resolves mandatrophy by acknowledging that the same jurisprudential methodology is both a coordination mechanism (genuinely reducing arbitrary interpretation, enabling scholarly communication) and an extraction mechanism (delegitimizing alternative legal traditions, concentrating authority in established networks). The Tangled Rope classification avoids false choice between 'pure coordination' and 'pure extraction' — the constraint performs both functions simultaneously. The perspectival gap is structural: what appears as coordination from the beneficiary's position appears as extraction from the victim's position. The false summit risk (mountain perspective) is addressed through the 'epistemological necessity' omega variable, which makes explicit that the constraint's alleged necessity depends on contingent assumptions about authenticity criteria and authority determination. The resolved mandatrophy is not 'which is it really?' but 'for whom does coordination dominate, and for whom does extraction dominate?'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hadith_authentication_criteria_contestation,
    'Do the criteria for ''authenticated hadith'' in the Shafi''i system reflect objective historical standards or institutionalized preferences that exclude competing traditions?',
    'Comparative analysis of hadith authentication criteria across Shafi''i, Hanafi, and Maliki traditions; examination of inclusion/exclusion patterns in canonical hadith collections; discourse analysis of ulama justifications for authentication boundaries',
    'If objective: Shafi''i hierarchy is epistemologically grounded (closer to Mountain). If institutionalized: the constraint is extraction dressed as methodology (closer to Snare from local communities'' perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hadith_authentication_criteria_contestation, empirical, 'Whether hadith authentication reflects objective standards or institutional preferences').

omega_variable(
    local_customary_practice_suppression_mechanism,
    'Is the suppression of Urf (local custom) structural (legal systems cannot accommodate both standardized and local sources) or extractive (the hierarchy benefits established ulama by eliminating competing legal authority)?',
    'Historical case studies of Shafi''i jurisprudence in pluralistic contexts; analysis of how Shafi''i courts actually handled local customs before standardization; comparison with Maliki and Hanafi treatment of local practice',
    'If structural: Shafi''i methodology is an attempt to solve a real coordination problem (closer to Rope/Tangled Rope). If extractive: suppression of Urf is rent-seeking by the ulama (closer to Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(local_customary_practice_suppression_mechanism, empirical, 'Whether Urf suppression is structural necessity or extractive mechanism').

omega_variable(
    ijma_boundary_determination_legitimacy,
    'Who determines whether consensus (Ijma) has been achieved, and by what criteria? Is this determination epistemically defensible or circular (those claiming authority declare consensus when it serves their position)?',
    'Historical analysis of disputed Ijma claims; examination of how Shafi''i ulama invoked Ijma to settle jurisprudential disputes; comparison with actual scholarly disagreement on core issues supposedly settled by consensus',
    'If epistemically defensible: Ijma is a genuine constraint on arbitrary interpretation (closer to Mountain epistemological necessity). If circular: Ijma invocation is theater masking continued interpretation (closer to Piton).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ijma_boundary_determination_legitimacy, empirical, 'Whether consensus determination is epistemically defensible or circular').

omega_variable(
    reading_contest_vs_methodological_hierarchy,
    'Are the four madhab (schools) — Hanafi, Maliki, Hanbali, Shafi''i — genuinely coexisting methodological frameworks (all valid for different contexts), or does the Shafi''i reading''s claim to a universally standardized hierarchy logically foreclose the others'' validity claims?',
    'Examination of classical and contemporary jurisprudential discourse on madhab pluralism; analysis of whether medieval and modern Islamic legal institutions treat the schools as interchangeable or hierarchically ordered; study of state legal code choices and whether they invoke legitimacy from methodological universality',
    'If genuinely coexisting: readings coexist_with each other (pluralism model). If Shafi''i hierarchy claims universality: this reading forecloses Hanafi flexibility and Maliki customary authority (monism model). This determines reading_relations classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_vs_methodological_hierarchy, conceptual, 'Whether madhab schools are coexisting frameworks or hierarchically ordered').

omega_variable(
    false_summit_epistemological_necessity,
    'Is the Shafi''i source hierarchy a natural law of jurisprudential reasoning (inherent to how coherent legal systems must function), or a historically specific institutional arrangement that benefits those controlling hadith transmission and ulama networks?',
    'Comparative law analysis of non-Islamic legal systems: do all coherent systems require exactly this source hierarchy, or do they achieve coherence through alternative mechanisms? Historical study of Islamic jurisprudence before Shafi''i formalization: did pre-standardization methodology create legal chaos or alternative order?',
    'If natural law: mountain classification is justified; beneficiary declaration is FSM candidate only. If historical contingency: Shafi''i reading is a Tangled Rope (coordination + extraction); false summit detection applies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_epistemological_necessity, conceptual, 'Whether source hierarchy is epistemological necessity or institutional choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__shafii_reading, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shafii_jm_tr_t0, jurisprudential_method_kernel__shafii_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(shafii_jm_tr_t250, jurisprudential_method_kernel__shafii_reading, theater_ratio, 250, 0.5).
narrative_ontology:measurement(shafii_jm_tr_t500, jurisprudential_method_kernel__shafii_reading, theater_ratio, 500, 0.58).

% Extraction over time
narrative_ontology:measurement(shafii_jm_be_t0, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(shafii_jm_be_t250, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 250, 0.35).
narrative_ontology:measurement(shafii_jm_be_t500, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 500, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__shafii_reading, identity_coordination).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__shafii_reading, hanafi_jurisprudential_kernel__hanafi_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__shafii_reading, maliki_jurisprudential_kernel__maliki_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__shafii_reading, hanbali_jurisprudential_kernel__hanbali_reading).

% DUAL FORMULATION NOTE:
% The jurisprudential_method_kernel decomposes into four structurally distinct constraints, one for each madhab reading. Each constraint has its own epsilon value, beneficiary/victim structure, and perspectival classification. The Shafi'i reading (this story) emphasizes hadith authentication as the constraining boundary and explicitly rejects Urf and Istihsan. The Hanafi reading emphasizes Qiyas and Istihsan flexibility. The Maliki reading emphasizes Urf as authoritative. The Hanbali reading emphasizes textual literalism. These are not different measurements of one constraint; they are different constraints that share a common kernel (the multi-source problem) and influence each other through institutional competition and scholarly debate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
