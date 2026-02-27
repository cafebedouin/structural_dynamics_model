% ============================================================================
% CONSTRAINT STORY: hhs_fetal_tissue_research_ban_2019
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hhs_fetal_tissue_research_ban_2019, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hhs_fetal_tissue_research_ban_2019
 *   human_readable: 2019 HHS Ban on Fetal Tissue Research Funding
 *   domain: political/scientific/regulatory
 *
 * SUMMARY:
 *   On June 5, 2019, the U.S. Department of Health and Human Services (HHS)
 *   announced a ban on federal funding for human fetal tissue research,
 *   effective immediately. The ban prohibited new grants for research using
 *   human fetal tissue obtained from induced abortions and terminated
 *   NIH-funded contracts with Advanced Biotherapeutic (ABT), which had been
 *   procuring fetal tissue for research. Existing grants were allowed to
 *   continue but could not be renewed. The stated rationale was ethical
 *   concern about participation in abortion; the constraint's actual
 *   structural function is extraction: it redistributes research priority,
 *   development capital, and scientific leadership away from U.S.
 *   laboratories dependent on fetal tissue models and toward international
 *   research centers (Europe, Canada, Australia, Japan, China) where no
 *   equivalent restrictions exist. The constraint exhibits snare
 *   characteristics (high suppression, trapped victims, no coordination
 *   benefit) disguised as moral principle through theater (compliance
 *   documentation, alternative-tissue justifications, ethics review
 *   expansion). Extractiveness increased over the interval (0.35→0.58) as
 *   researchers exhausted workarounds and realized the ban was durable
 *   political commitment, not temporary policy. Theater increased (0.45→0.68)
 *   as the regulatory system developed performative alternatives to
 *   substitute for lost research capacity. The constraint is a pure snare
 *   from the perspective of trapped researchers; mixed
 *   coordination-extraction from international competitors' perspective;
 *   coordination victory from the anti-abortion coalition's perspective; and
 *   degraded ritual from HHS's institutional perspective. The false summit
 *   risk is high: the civilizational observer may naturalize the ban as
 *   'America's values,' concealing that the constraint functions as
 *   extraction of scientific priority, not enforcement of universal
 *   principle.
 *
 * KEY AGENTS:
 *   - Fetal Tissue-Dependent Researchers: Primary victims (powerless/trapped) — neurodegeneration, spinal cord injury, diabetes, developmental biology researchers lose 20+ years of protocol development; cannot retrain; no substitute substrate provides equivalent developmental staging
 *   - Disease Research Institutes: Primary victims (moderate/constrained) — Parkinson's, diabetes, spinal cord injury programs lose funded research lines; can migrate internationally but at high institutional cost
 *   - Anti-Abortion Political Coalition: Primary beneficiary (organized/arbitrage) — coordinates single-issue voters and religious constituencies; gains political credibility with core supporters
 *   - HHS Regulatory Institution: Secondary actor (institutional/constrained) — enforces mandate; loses scientific autonomy; develops compliance theater to justify enforcement
 *   - International Research Centers: Secondary beneficiary (powerful/mobile) — Europe, Canada, Japan capture displaced U.S. leadership; low cost to absorb U.S. researchers
 *   - Federal Research Funding System: Tertiary actor (institutional/arbitrage) — maintains ban through inertia; develops alternative-tissue documentation theater; loses research priority without active reversal
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hhs_fetal_tissue_research_ban_2019, 0.58).
domain_priors:suppression_score(hhs_fetal_tissue_research_ban_2019, 0.72).
domain_priors:theater_ratio(hhs_fetal_tissue_research_ban_2019, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hhs_fetal_tissue_research_ban_2019, extractiveness, 0.58).
narrative_ontology:constraint_metric(hhs_fetal_tissue_research_ban_2019, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(hhs_fetal_tissue_research_ban_2019, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hhs_fetal_tissue_research_ban_2019, snare).
narrative_ontology:human_readable(hhs_fetal_tissue_research_ban_2019, "2019 HHS Ban on Fetal Tissue Research Funding").
narrative_ontology:topic_domain(hhs_fetal_tissue_research_ban_2019, "political/scientific/regulatory").

domain_priors:requires_active_enforcement(hhs_fetal_tissue_research_ban_2019).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hhs_fetal_tissue_research_ban_2019, anti_abortion_political_coalition).
narrative_ontology:constraint_victim(hhs_fetal_tissue_research_ban_2019, fetal_tissue_dependent_researchers).
narrative_ontology:constraint_victim(hhs_fetal_tissue_research_ban_2019, disease_modeling_programs).
narrative_ontology:constraint_victim(hhs_fetal_tissue_research_ban_2019, regenerative_medicine_development).
narrative_ontology:constraint_victim(hhs_fetal_tissue_research_ban_2019, translational_medicine_pipeline).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FETAL TISSUE-DEPENDENT RESEARCHERS (SNARE) — Cannot exit the constraint without abandoning lines of research with decade-long institutional investment. No alternative tissue substrate provides equivalent developmental staging or disease modeling capacity. Career-trapped: retraining costs are prohibitive; postdocs and graduate students lose funded positions. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.80.
constraint_indexing:constraint_classification(hhs_fetal_tissue_research_ban_2019, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DISEASE RESEARCH PIPELINE (SNARE) — Constrained exit: can migrate research to international centers (UK, Europe, Canada) but at high institutional cost and loss of U.S. leadership in translational medicine. Parkinson's, diabetes, spinal cord injury programs lose fetal-tissue models with no sanctioned substitutes. d≈0.78, f(d)≈1.13, σ=1.2 → χ≈0.82.
constraint_indexing:constraint_classification(hhs_fetal_tissue_research_ban_2019, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: ANTI-ABORTION POLITICAL COALITION (ROPE) — Benefits from the ban as coordination among single-issue voters and religious constituencies. Experiences the constraint as enforcement of shared moral principle. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.006. Negative effective extraction: net beneficiary.
constraint_indexing:constraint_classification(hhs_fetal_tissue_research_ban_2019, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: HHS REGULATORY INSTITUTION (TANGLED_ROPE) — Constrained by political mandate and administrative precedent. Enforcement requires ongoing compliance monitoring, grant de-obligation, and managed transition (the coordination function). But the constraint extracts institutional autonomy: HHS cannot exercise independent scientific judgment on research merit. Theater_ratio=0.68 reflects substantial compliance documentation theater. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.38.
constraint_indexing:constraint_classification(hhs_fetal_tissue_research_ban_2019, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FEDERAL RESEARCH FUNDING SYSTEM (PITON) — The ban persists partly through institutional inertia: reversing it requires active political effort despite scientific consensus on tissue utility. Theater_ratio=0.68 (compliance documentation, ethics review theater, alternative tissue justifications) indicates degradation: the funding bureaucracy maintains the ban's appearance of scientific reasonableness through regulatory theater. d≈0.10, f(d)≈-0.08, σ=1.0 → χ≈-0.005.
constraint_indexing:constraint_classification(hhs_fetal_tissue_research_ban_2019, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: INTERNATIONAL RESEARCH LEADERSHIP (TANGLED_ROPE) — Mobile exit: Europe, Canada, Australia, Japan continue fetal tissue research with no restrictions. U.S. research institutions can collaborate internationally but lose first-author positions and funding leadership. The constraint extracts U.S. scientific priority while coordinating internationally-compliant research programs. d≈0.55, f(d)≈0.75, σ=1.2 → χ≈0.52.
constraint_indexing:constraint_classification(hhs_fetal_tissue_research_ban_2019, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN?) — Risks naturalizing the ban as a permanent fixture of U.S. science policy ('America's values are incompatible with fetal tissue research'). The structural data (ε=0.58, suppression=0.72, theater=0.68) contradicts mountain classification — this is a contingent political constraint, not an immutable natural law. The false summit framing conceals the extractive mechanism behind moral language.
constraint_indexing:constraint_classification(hhs_fetal_tissue_research_ban_2019, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hhs_fetal_tissue_research_ban_2019_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hhs_fetal_tissue_research_ban_2019, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hhs_fetal_tissue_research_ban_2019, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hhs_fetal_tissue_research_ban_2019, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hhs_fetal_tissue_research_ban_2019, TR),
    TR >= 0.70.

:- end_tests(hhs_fetal_tissue_research_ban_2019_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts U.S. scientific priority through denial of access to a tissue substrate with unique developmental staging capacity. No approved substitute provides equivalent functionality (iPSCs lack gestational synchronization; organoids lack vascularization and immune cell integration; organ-on-chip lacks multi-tissue interactions). Researchers cannot simply 'switch substrates' — fetal tissue has 30+ years of protocol development. The extraction is not total: international collaboration and alternative models provide partial workarounds. Initial extractiveness (0.35) reflects optimism that alternatives would mature quickly; trajectory to 0.58 reflects realization that substitution is much slower than anticipated. Suppression (0.72): High. Barriers include: grant de-obligation precedent (researchers cannot reverse a funded project once terminated), political durability of the ban (requires active reversal, not passive expiration), lack of legislative alternative pathway (cannot access fetal tissue through international partnerships with federal funding), and career risk of advocacy (researchers publicly supporting fetal tissue research face political backlash and funding uncertainty). Theater ratio (0.68): High. Substantial performative content includes: (a) ethics review theater — HHS created new Institutional Review Board scrutiny for alternative-tissue research to justify enforcement through process rather than principle; (b) alternative-tissue justifications — researchers develop iPSC protocols not because they are equivalent but because they satisfy compliance; (c) funding mechanism theater — NIH published guidance on how to study 'developmental biology without fetal tissue,' creating the performative appearance of scientific equivalence where none exists. Theater increased over interval as regulatory system doubled down on compliance documentation.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces radically different classifications across observer positions. Trapped researchers (powerless/biographical) see pure snare: the ban is arbitrary, career-ending, and offers no coordination benefit. Constrained researchers in disease modeling (moderate/generational) see snare with some international escape: they can migrate research but lose U.S. funding and institutional base. The anti-abortion coalition (organized/immediate) sees pure rope: the ban coordinates their political base and enforces principle with no perceived extraction. HHS (institutional/biographical) sees tangled rope: forced to enforce an unpopular policy while maintaining scientific credibility. International research centers (powerful/generational) see net coordination: they capture displaced leadership and benefit from cross-border collaboration without restrictions. The analytical observer (civilizational/universal) risks seeing a mountain — 'America's ethical commitment to life' — when the structural data reveals a snare: career extraction disguised as moral principle. This perspectival collapse is the fingerprint of false summit risk.
 *
 * DIRECTIONALITY LOGIC:
 *   Anti-abortion coalition: Beneficiary + arbitrage exit → d≈0.08. They coordinate their political base and extract credibility without bearing implementation cost. Fetal tissue researchers: Victim + trapped exit → d≈0.92. Cannot exit the constraint without abandoning career investment; no alternative substrate provides equivalent functionality. Disease research pipeline: Victim + constrained exit → d≈0.78. Can migrate internationally but at high institutional cost; constrained by U.S. funding dependency. International research centers: Beneficiary + mobile exit (from U.S. perspective, but victimized from global perspective) → d≈0.25 (global scoped). They benefit from displaced leadership and lack restrictions; if anything, they have low d as beneficiaries. HHS institutional: Victim + constrained exit → d≈0.60. Forced to enforce unpopular policy; cannot independently reverse (requires political action); constrained by administrative precedent.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE VS REGULATORY COORDINATION: The mandatrophy resolves by observing that the beneficiary coalition (anti-abortion voters) experiences genuine coordination (rope), while the victim researchers experience genuine extraction (snare). These are not the same constraint viewed from different angles — they are different structural realities. The constraint's function for the coalition is coordination: binding together religious and single-issue voters around enforcement of a principle. The constraint's function for researchers is extraction: denying access to a research substrate that gives others competitive advantage. The fact that the constraint coordinates one group while extracting from another is the defining signature of a snare: it uses political power to enforce asymmetric extraction while cloaking it in moral language that appeals to a coalition. The false summit risk ('this is America's ethical principle, an immutable feature of U.S. values') must be rejected because: (1) the constraint is reversible (political choice, not natural law); (2) other democracies with similar values (Canada, Germany, France) permit fetal tissue research; (3) the theater ratio (0.68) indicates performative compliance, not principled commitment; (4) the constraint redistributes scientific priority to international competitors, which is extraction, not principle. The mandatrophy is resolved: this is snare, not mountain; snare, not rope; snare disguised as rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tissue_substitution_technical_viability,
    'Do organoid models, induced pluripotent stem cells (iPSCs), or organ-on-chip technologies provide functionally equivalent alternatives to fetal tissue for developmental staging and disease modeling?',
    'Comparative empirical validation: parallel studies using alternative substrates vs historical fetal tissue protocols for the same developmental questions; publication of equivalency studies; adoption rates in disease modeling pipelines',
    'If alternatives viable: constraint is coordination/scaffolding (temporary, with sunset as tech matures). If alternatives insufficient: constraint is pure extraction (snare, permanent structural damage to translational pipeline).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tissue_substitution_technical_viability, empirical, 'Whether alternative tissue substrates provide technical equivalence').

omega_variable(
    political_reversibility_timeline,
    'What is the structural likelihood and timeline for political reversal of the ban under different U.S. political coalitions?',
    'Historical analysis of similar politically-motivated research bans (e.g., stem cell funding restrictions 2001-2009); polling on public support for fetal tissue research; legislative session records; executive action precedents',
    'If reversible within 5-10 years: scaffold classification is legitimate (sunset is real). If reversible only after generational political shift (30+ years): constraint is effectively snare for current research generation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_reversibility_timeline, preference, 'Timeline for political reversal of the ban').

omega_variable(
    international_competitive_displacement,
    'How much U.S. translational medicine leadership is structurally displaced to international centers by the ban, and is this displacement reversible?',
    'Bibliometric analysis: first-author publication share, grant leadership, clinical trial initiation rates for fetal-tissue-dependent diseases (Parkinson''s, spinal cord injury, diabetes); tracking of U.S. researcher migration to international labs; institutional partnership data',
    'If displacement is large and irreversible: snare classification confirmed (extraction of scientific priority). If displacement is marginal or reversed by international collaboration: tangled_rope classification confirmed (mixed coordination-extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(international_competitive_displacement, empirical, 'Magnitude and reversibility of U.S. scientific leadership displacement').

omega_variable(
    moral_framing_necessity,
    'Is the anti-abortion moral framing of the ban a core structural feature or a contingent political wrapper around economic/ideological extraction mechanisms?',
    'Textual analysis of policy justifications; comparison with similar bans in other countries with different moral frameworks; analysis of who benefits from the ban independent of stated moral rationale',
    'If moral framing is core: beneficiary perspective (organized coalition) is genuine. If framing is contingent: snare classification is strengthened (extraction disguised as principle).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_framing_necessity, conceptual, 'Whether moral framing is essential or incidental to the ban').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hhs_fetal_tissue_research_ban_2019, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hhs_fetal_tr_t0, hhs_fetal_tissue_research_ban_2019, theater_ratio, 0, 0.45).
narrative_ontology:measurement(hhs_fetal_tr_t3, hhs_fetal_tissue_research_ban_2019, theater_ratio, 3, 0.58).
narrative_ontology:measurement(hhs_fetal_tr_t6, hhs_fetal_tissue_research_ban_2019, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(hhs_fetal_be_t0, hhs_fetal_tissue_research_ban_2019, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hhs_fetal_be_t3, hhs_fetal_tissue_research_ban_2019, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(hhs_fetal_be_t6, hhs_fetal_tissue_research_ban_2019, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hhs_fetal_tissue_research_ban_2019, enforcement_mechanism).
narrative_ontology:affects_constraint(hhs_fetal_tissue_research_ban_2019, stem_cell_research_funding_restrictions).
narrative_ontology:affects_constraint(hhs_fetal_tissue_research_ban_2019, international_research_brain_drain).
narrative_ontology:affects_constraint(hhs_fetal_tissue_research_ban_2019, regenerative_medicine_development_delays).

% DUAL FORMULATION NOTE:
% The 2019 HHS ban is a descendant of the 2001-2009 stem cell funding restrictions (different tissue substrate, same coordination-of-extraction mechanism). The upstream constraint (stem cell restrictions) created precedent for politically-motivated research bans; the 2019 ban demonstrates institutional durability of that precedent. Both are snares disguised as moral principle. Downstream constraints (brain drain to international centers, regenerative medicine development delays) are consequences of the ban's extraction of scientific priority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hhs_fetal_tissue_research_ban_2019, organized, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
