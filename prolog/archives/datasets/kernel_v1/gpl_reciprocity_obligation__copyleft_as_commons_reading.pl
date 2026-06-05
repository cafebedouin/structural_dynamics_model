% ============================================================================
% CONSTRAINT STORY: gpl_reciprocity_obligation__copyleft_as_commons_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_reciprocity_obligation__copyleft_as_commons_reading, []).

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
 *   constraint_id: gpl_reciprocity_obligation__copyleft_as_commons_reading
 *   human_readable: GPL Reciprocity Obligation (Commons-Preservation Reading)
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   The GPL reciprocity obligation, in the commons-preservation reading, is
 *   institutional technology designed to prevent enclosure of software
 *   commons through mandatory source-code release. When a developer
 *   distributes GPL-licensed code, they must release all derivative code
 *   under the same license — this prevents proprietary enclosure of the
 *   collective software infrastructure. From the commons-preservation
 *   perspective, this reciprocity is not primarily about individual developer
 *   freedom (that is the sibling freedom reading) nor about restricting
 *   derivative-author rights (that is the sibling restriction reading).
 *   Rather, it is a structural mechanism to solve the tragedy of the commons
 *   in software: absent reciprocity enforcement, all code eventually drifts
 *   toward proprietary capture, fragmenting the commons. The constraint
 *   exhibits Tangled Rope structure at base: genuine coordination function
 *   (preserves commons infrastructure, enables downstream developers to build
 *   freely), asymmetric extraction (proprietary derivative authors are
 *   prevented from enclosing gains), and active enforcement (legal mechanism,
 *   licensing compliance checking, community vigilance). The extractiveness
 *   has risen over 30 years (0.32 → 0.52) as more software infrastructure
 *   relies on GPL code and as enforcement mechanisms have matured. The
 *   suppression has remained elevated (0.38 → 0.48) due to legal structures,
 *   technical compliance tools, and community norms. Theater ratio has
 *   declined (0.58 → 0.38) as actual GPL enforcement has become more
 *   effective (less performative ritual, more genuine compliance). From a
 *   commons-preservation framing, the increasing extractiveness reflects
 *   intensifying conflict between proprietary distribution models and commons
 *   infrastructure — the constraint's extraction is the commons' defense
 *   against enclosure.
 *
 * KEY AGENTS:
 *   - Software Commons Institution (institutional/arbitrage): Primary beneficiary — maintains commons integrity and enables downstream use; experiences GPL reciprocity as solving core coordination problem
 *   - Proprietary Derivative Authors (powerless/trapped): Primary victims — cannot distribute closed-source derivatives without violating GPL; face maximum extraction (licensing violations, legal action, market exclusion)
 *   - Commercial Open-Source Vendors (moderate/constrained): Secondary victims and mixed participants — experience GPL as coordination mechanism (forces transparency, enables fair competition) and extraction mechanism (restricts proprietary differentiation); constrained but not trapped exit
 *   - Downstream Developers (moderate/mobile): Secondary beneficiaries — benefit from access to GPL-preserved source code and from commons infrastructure maintained by reciprocity enforcement; have exit options (non-GPL alternatives, permissive licenses)
 *   - Large Technology Companies (powerful/mobile): Mobile beneficiaries — can negotiate dual licenses, reimplement, or comply cost-effectively; experience GPL as low-extraction scaffold rather than binding constraint
 *   - Alternative Licensing Coalition (organized/constrained): Organized challengers — promote permissive licensing as substitute for GPL reciprocity; see GPL as temporary scaffolding being superseded by market norms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.48).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.52).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_commons_reading, tangled_rope).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_commons_reading, "GPL Reciprocity Obligation (Commons-Preservation Reading)").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_commons_reading, "software_licensing/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_commons_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_commons_reading, '1629df3d-fdb7-4e49-93f5-8a6729b61355').
narrative_ontology:cs_kernel_codification('1629df3d-fdb7-4e49-93f5-8a6729b61355', formalized).
narrative_ontology:cs_authority_grounding('1629df3d-fdb7-4e49-93f5-8a6729b61355', lineage).
narrative_ontology:cs_interpretation_layer_present('1629df3d-fdb7-4e49-93f5-8a6729b61355').
narrative_ontology:cs_reading_relation('1629df3d-fdb7-4e49-93f5-8a6729b61355', gpl_reciprocity_obligation__copyleft_as_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('1629df3d-fdb7-4e49-93f5-8a6729b61355', gpl_reciprocity_obligation__copyleft_as_restriction_reading, coexists_with).
narrative_ontology:cs_axiom('1629df3d-fdb7-4e49-93f5-8a6729b61355', foundational, commons_preservation_justifies_reciprocity_extraction).
narrative_ontology:cs_axiom_status(commons_preservation_justifies_reciprocity_extraction, holdable).
narrative_ontology:cs_axiom_grounding('1629df3d-fdb7-4e49-93f5-8a6729b61355', commons_preservation_justifies_reciprocity_extraction, deontological).
narrative_ontology:cs_axiom('1629df3d-fdb7-4e49-93f5-8a6729b61355', secondary, proprietary_enclosure_threat_empirically_present).
narrative_ontology:cs_axiom_status(proprietary_enclosure_threat_empirically_present, holdable).
narrative_ontology:cs_axiom_grounding('1629df3d-fdb7-4e49-93f5-8a6729b61355', proprietary_enclosure_threat_empirically_present, empirically_contingent).
narrative_ontology:cs_reference_frame('1629df3d-fdb7-4e49-93f5-8a6729b61355', reciprocity_as_commons_defense).
narrative_ontology:cs_drift_state('1629df3d-fdb7-4e49-93f5-8a6729b61355', contemporary_corporate_open_source_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1629df3d-fdb7-4e49-93f5-8a6729b61355', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_commons_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_commons_reading, software_commons_as_institution).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_commons_reading, downstream_developers).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_commons_reading, public_interest_stakeholders).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_commons_reading, proprietary_distribution_entities).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_commons_reading, exit_maximizers).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_commons_reading, closed_source_derivative_authors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROPRIETARY DERIVATIVE AUTHOR (SNARE) — Developer who wishes to extend GPL code with proprietary features and distribute closed-source derivatives. Faces maximum extraction: must either release source (violating business model), cease distribution (losing market), or pay licensor (if proprietary exception exists). No structural alternative paths. This agent experiences the GPL as pure constraint extraction — maximum d (≈0.95), maximum f(d) ≈ 1.42, yielding high χ despite moderate ε. The suppression is structural: legal enforcement (DMCA, licensing violations), market pressure (reputation damage for GPL violations), and technical barriers (automated compliance checking).
constraint_indexing:constraint_classification(gpl_reciprocity_obligation__copyleft_as_commons_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COMMERCIAL OPEN-SOURCE VENDOR (TANGLED ROPE) — Company using GPL code as foundational layer, generating revenue through services, support, or dual-licensing. Experiences GPL as hybrid coordination-extraction. The reciprocity obligation coordinates the vendor with the commons (forces transparency, encourages contribution back); but also extracts through mandatory source release (reduces proprietary differentiation margin, requires public contribution of improvements). Exit options are constrained (can dual-license if original copyright holder, can use exceptions, can switch to non-GPL alternatives) but not absent. Experienced χ moderate due to constrained exit and mixed benefit/cost.
constraint_indexing:constraint_classification(gpl_reciprocity_obligation__copyleft_as_commons_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: SOFTWARE COMMONS INSTITUTION (ROPE) — The GPL machinery itself, operating as an institutional coordination mechanism. From this perspective, reciprocity is pure coordination: enforcing source availability is the mechanism that solves the commons tragedy problem. The institution benefits from mandatory reciprocity (maintains commons integrity, prevents enclosure, enables commons growth). Institutional actors (FSF, Linux Foundation) have arbitrage options (can license under permissive alternatives, can create exceptions) and experience GPL as solving their core coordination problem with minimal coercive overhead — the extraction flow runs toward the commons, not away. Low effective extraction because beneficiary + arbitrage exit.
constraint_indexing:constraint_classification(gpl_reciprocity_obligation__copyleft_as_commons_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LARGE TECHNOLOGY COMPANY (PITON) — Fortune 500 firm with substantial GPL-licensed code embedded in products. Compliance is largely performative: automated scanning, license tracking, source code hosting (often hidden behind corporate repositories, slow release cycles). The GPL's extraction mechanism (mandatory release) is substantially theater for this actor — the company has mobility (can reimplement, can license exceptions, can switch licenses) and power (can negotiate with original authors, can establish market standards). Effective extraction χ low because powerful + mobile exits, despite moderate ε. Theater ratio high (0.6+) because compliance is largely ritual performed for legal risk management and community reputation, not because it materially constrains the actor's behavior or business model.
constraint_indexing:constraint_classification(gpl_reciprocity_obligation__copyleft_as_commons_reading, piton,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal framing, software commons enclosure is treated as a natural economic law: absent reciprocity enforcement, all code will eventually be privately captured (the tragedy of the commons as immutable). This perspective sees GPL reciprocity as the only possible solution to an inherent problem of software production under information economics. However, this naturalizes what is actually a contested institutional choice — the reading that positions GPL reciprocity as contingent (rather than necessary) treats the mountain classification as a false summit. The engine will identify this as beneficiary-motivated naturalization.
constraint_indexing:constraint_classification(gpl_reciprocity_obligation__copyleft_as_commons_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: ALTERNATIVE LICENSING COALITION (SCAFFOLD) — Organized agents promoting permissive licenses (MIT, Apache 2.0, BSD) see GPL reciprocity as a temporary coordination mechanism that is being superseded by market-driven norms and community reputation systems. They classify GPL reciprocity as a transactional-enforcement scaffolding — necessary in the 1990s-2010s to prevent enclosure, but increasingly replaced by developer norms, corporate investment in commons, and weaker-license coordination. Low effective extraction (χ ≤ 0.30) because they have agency (can adopt alternative licenses, can build competing ecosystems) and perceive a sunset (GPL's enforcement role diminishes as corporate investment in open source grows). The sunset logic: as the reputational cost of GPL violation increases and the market rewards open-source participation, the need for GPL's legal enforcement decreases.
constraint_indexing:constraint_classification(gpl_reciprocity_obligation__copyleft_as_commons_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_reciprocity_obligation__copyleft_as_commons_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gpl_reciprocity_obligation__copyleft_as_commons_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gpl_reciprocity_obligation__copyleft_as_commons_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_commons_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_commons_reading, TR),
    TR >= 0.70.

:- end_tests(gpl_reciprocity_obligation__copyleft_as_commons_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. GPL reciprocity obligation extracts from proprietary derivative authors (prevents closed-source enclosure) and from commercial vendors (forces public contribution of improvements). But extraction is not total because (a) beneficiaries gain access to derivative code and (b) open-source vendors can use alternative models (dual-licensing, service revenue, exception licensing). The trajectory 0.32 → 0.52 reflects rising enforcement capacity and expanding software infrastructure dependence on GPL code; as more critical infrastructure (Linux kernel, compiler toolchains, container runtimes) is GPL-licensed, the reciprocity obligation becomes more costly to violate, increasing effective extraction. Suppression (0.52): Moderate-high. Enforcement mechanisms include legal liability (copyright violation liability, DMCA concerns for circumvention), technical barriers (automated license scanning, compliance auditing), and institutional barriers (open-source community peer review, trademark enforcement). But suppression is not total because (a) alternatives exist (permissive licenses, proprietary reimplementation), (b) dual-licensing and exceptions can be negotiated, and (c) enforcement is imperfect (GPL violations go unpunished in many cases). The trajectory 0.38 → 0.48 reflects both strengthening enforcement infrastructure (automated tools, institutional awareness) and rising cost of non-compliance as software infrastructure dependencies increase. Theater ratio (0.58 → 0.38): Declining performativity. Early GPL enforcement (1991-2000) relied heavily on legal threats and community shaming (theater); compliance was uncertain and enforcement rare (SCO litigation era). By 2020s, GPL compliance has become routinized: automated scanning, established processes, corporate legal teams, reduced uncertainty. Lower theater reflects that the mechanism now functions primarily as deterrent and cost-allocation system (not as spectacular enforcement). The commons-preservation reading interprets declining theater as mechanism maturation, not mechanism degradation.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence. The powerless proprietary derivative author (Snare) sees pure extraction with no coordination benefit — they are prevented from commercializing their extensions. The commercial vendor (Tangled Rope) sees both coordination (forced transparency enables fair competition) and extraction (must publicly contribute improvements). The commons institution (Rope) sees coordination with minimal extraction overhead — reciprocity enforces their core function. The large technology company (Piton) sees high-theater compliance with low material extraction — they have mobility and power. The analytical observer (Mountain) risks naturalizing GPL reciprocity as the only possible solution to software enclosure. The alternative-licensing coalition (Scaffold) sees GPL reciprocity as temporary scaffolding being superseded by permissive-license norms and corporate investment. These gaps are real: the same constraint produces six different experienced extractions (from maximum Snare through minimum Rope) depending on structural position. The commons-preservation reading unifies these perspectives by positioning reciprocity as institutional technology that necessarily extracts from proprietary derivative authors in order to preserve the commons for downstream developers. This is the reading's core claim: the extraction is a feature (commons defense), not a bug (unfair restriction).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies sharply across agents. Proprietary derivative author (trapped + victim status) derives d ≈ 0.95 (full target), producing f(d) ≈ 1.42 and high χ. Commercial vendor (constrained + mixed victim/beneficiary) derives d ≈ 0.60 (net victim), producing f(d) ≈ 0.85 and moderate χ. Commons institution (arbitrage + beneficiary) derives d ≈ 0.10 (net beneficiary), producing f(d) ≈ 0.05 and low/negative χ. Large company (mobile + beneficiary) derives d ≈ 0.25 (slight beneficiary), producing f(d) ≈ 0.15 and low χ. Alternative coalition (constrained + mixed beneficiary) derives d ≈ 0.40 (net beneficiary through escape route), producing f(d) ≈ 0.40 and low-moderate χ. The commons reading positions derivative authors and vendors as victims of reciprocity extraction; the commons institution and downstream developers as beneficiaries. The large-company low-extraction experience reflects that power and mobility reduce experienced extraction even when structural extraction is present. No directionality overrides needed; all d values follow from structural derivation (beneficiary/victim + exit options + power level). The reading's normative claim is that this asymmetric extraction is justified because it preserves a public good (commons) against privately-motivated enclosure.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_contingency_commons_vs_freedom,
    'Is GPL reciprocity essential to commons preservation, or is it one contested institutional choice among alternatives?',
    'Historical and counterfactual analysis: (a) Did non-GPL projects (Apache, Linux kernel contributor patterns, npm ecosystem) successfully prevent enclosure without GPLv3-level reciprocity? (b) What is the actual rate of proprietary derivative-formation under permissive licenses vs GPL? (c) Do developer norms and corporate investment in commons now substitute for legal enforcement? (d) How much of GPL''s preservation effect is due to legal enforcement vs due to developer identity/norms?',
    'If alternatives successfully prevent enclosure: commons reading is contingent institutional choice, not necessary law. Commons-preservation argument weakens relative to freedom and restriction readings. If proprietary derivatives surge under permissive licenses: commons reading is empirically validated. If corporate investment in open source and developer norms now provide primary enclosure prevention: GPL reciprocity is historical scaffolding (supports the coalition''s sunset position).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contingency_commons_vs_freedom, empirical, 'Whether GPL reciprocity is essential or contingent for commons preservation').

omega_variable(
    definition_commons_scope,
    'What constitutes ''the commons'' that GPL reciprocity is designed to preserve? Individual agency in derivative licensing, or collective software infrastructure?',
    'Ontological clarification: Does ''commons preservation'' mean (a) preservation of derivative authors'' freedom to choose their own licensing (the freedom reading would argue this), or (b) preservation of the aggregate software commons as a public resource (the commons reading defines it this way). The two definitions are in tension: maximizing individual derivative-author freedom may enclosure the collective commons; preserving the collective commons constrains individual derivative-author freedom. Which definition is legitimate?',
    'If commons = individual freedom: the freedom reading and commons reading are incompatible (foreclose relation). If commons = collective software infrastructure: commons reading and restriction reading may coexist (coexists_with). The axis of disagreement is located in the definition of what is being preserved.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_commons_scope, conceptual, 'Definitional contest over what ''commons'' means in GPL context').

omega_variable(
    empirical_enclosure_rate_permissive,
    'Under permissive licensing (MIT, Apache 2.0, BSD), what is the actual rate of proprietary enclosure of foundational software infrastructure?',
    'Empirical audit: (a) Measure the fraction of MIT-licensed projects that are later incorporated into proprietary products without source release. (b) Track whether permissively licensed projects eventually lose developer community as proprietors fork and close-source (Xv6 derivatives, SQLite competitors). (c) Compare long-term viability and contributor diversity: GPL vs permissive ecosystems. (d) Document instances where permissive licensing enabled predatory enclosure (e.g., AWS ElastiCache absorbing open-source Redis). (e) Quantify commons contribution rates: do GPL repositories receive proportionally more upstream contributions than permissively licensed projects?',
    'High enclosure rate under permissive: validates commons reading''s empirical claim that reciprocity is necessary to prevent enclosure. Low enclosure rate under permissive: supports alternative readings (freedom or restriction) that challenge the commons reading''s necessity claim. Divergent rates by sector (infrastructure vs application layers): suggests commons reading is context-dependent rather than universal.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(empirical_enclosure_rate_permissive, empirical, 'Empirical rate of proprietary enclosure under permissive licenses').

omega_variable(
    identity_fusion_developer_norms,
    'Do developer identity norms (believing open-source contribution is ''the right thing'') substitute for GPL''s legal enforcement, or are they independent mechanisms that both operate simultaneously?',
    'Causal identification: (a) Control for project GPL status and measure contribution rates by developer type (commercial vs hobbyist, young vs established). (b) Test whether developers who violate GPL (proprietary derivatives) report lower attachment to open-source identity norms. (c) Examine whether corporations with strong open-source brand commitment (Google, Meta, Microsoft in recent years) contribute equally to GPL and permissive projects, or show differential behavior. (d) Historical analysis: Did developer norms around open-source contribution emerge before or after GPL adoption? Did norms strengthen independently of GPL, or were they constituted through GPL enforcement?',
    'If norms are identity-constitutive and independent of GPL: commons can be preserved through norm enforcement without GPL reciprocity (supports alternative readings). If norms are downstream of GPL enforcement: GPL reciprocity is causally prior to commons-preserving norms (supports commons reading''s empirical claim). If both mechanisms operate, with incomplete substitutability: commons reading and alternative readings coexist; commons preservation requires hybrid enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_developer_norms, empirical, 'Whether developer identity norms substitute for GPL legal enforcement').

omega_variable(
    kernel_reading_contingency,
    'Is this constraint one reading of a contested kernel (GPL reciprocity mechanism), or is it the constraint itself?',
    'This omega records the committer frame metadata. This story instantiates the ''copyleft_as_commons_reading'' — the framing that positions GPL reciprocity as institutional technology for preventing commons enclosure through mandatory source release. Sibling readings position the same reciprocity mechanism as (a) ''copyleft_as_freedom_reading'': mandatory source release as essential to developer freedoms; (b) ''copyleft_as_restriction_reading'': mandatory source release as restriction on derivative-author business models. All three readings describe the same kernel (GPL reciprocity obligation) but position it differently relative to institutional values (commons, freedom, restriction). The kernel itself is not disputed — ''If you distribute GPL code, you must release source'' is factually stable. The reading is the interpretive frame that assigns meaning to this requirement. The committer frame documents that the epsilon, beneficiary/victim structure, and classification are conditional on this reading being the operative institutional framing.',
    'This omega does not change the classification (Tangled Rope at base). It documents that the constraint is only stable under the commons-preservation reading. If the reading shifted to freedom or restriction, the beneficiaries and victims would flip, the epsilon might change, and the classification might shift to Snare or Scaffold. The reading contingency is NOT resolvable through empirical data — it is a normative choice about what GPL is ''for.''',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contingency, preference, 'Constraint as one reading of GPL reciprocity kernel; not resolution-dependent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl_commons_theater_1991, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 0, 0.58).
narrative_ontology:measurement(gpl_commons_theater_2006, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 15, 0.42).
narrative_ontology:measurement(gpl_commons_theater_2021, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 30, 0.38).

% Extraction over time
narrative_ontology:measurement(gpl_commons_extractiveness_1991, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(gpl_commons_extractiveness_2006, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(gpl_commons_extractiveness_2021, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 30, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(gpl_commons_suppression_1991, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(gpl_commons_suppression_2006, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 15, 0.52).
narrative_ontology:measurement(gpl_commons_suppression_2021, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 30, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_commons_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.25).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_commons_reading, gpl_reciprocity_obligation__copyleft_as_freedom_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_commons_reading, gpl_reciprocity_obligation__copyleft_as_restriction_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_commons_reading, software_patent_commons_protection).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_commons_reading, open_source_developer_identity_lock).

% DUAL FORMULATION NOTE:
% GPL reciprocity obligation is a contested kernel instantiating three structurally distinct constraints: (1) copyleft_as_commons_reading (THIS file): GPL as commons-preservation technology, ε=0.48, Tangled Rope at base. (2) copyleft_as_freedom_reading: GPL as individual-freedom protection, ε≈0.35, Rope at base. (3) copyleft_as_restriction_reading: GPL as unjust restriction on derivative-author rights, ε≈0.65, Snare at base. The three readings produce different epsilon values, beneficiary/victim structures, and classifications from the same institutional mechanism. Each reading is its own constraint story with its own ε-invariant properties. They are linked through the kernel (GPL source-release requirement) but constitute separate constraints in the classification system. Network edges document upstream dependencies: software patent commons protection depends on GPL reciprocity enforcement; developer identity lock depends on internalized commitment to GPL-style reciprocity values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpl_reciprocity_obligation__copyleft_as_commons_reading, powerful, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
