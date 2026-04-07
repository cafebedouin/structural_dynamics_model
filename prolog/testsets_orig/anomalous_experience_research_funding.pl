% ============================================================================
% CONSTRAINT STORY: anomalous_experience_research_funding
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_anomalous_experience_research_funding, []).

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
 *   constraint_id: anomalous_experience_research_funding
 *   human_readable: Anomalous Experience Research Funding Constraint
 *   domain: research_administration/anomalous_phenomena
 *
 * SUMMARY:
 *   Anomalous experience research funding represents a structural constraint
 *   on which phenomena receive institutional validation, professional
 *   legitimacy, and resource allocation. The constraint operates through a
 *   coordination mechanism (mainstream science maintaining methodological
 *   consensus) combined with extractive gatekeeping (funding access
 *   conditional on conformity to mainstream frameworks). Researchers
 *   investigating anomalous experiences—unusual perceptual phenomena, rare
 *   subjective states, apparent violations of conventional physical models,
 *   or experiences that existing frameworks classify as impossible—face
 *   systematic exclusion from mainstream funding sources, publishing venues,
 *   and institutional affiliation. This creates a market in anomalous
 *   research conducted through alternative channels (private foundations,
 *   international networks, heterodox venues), which reduces suppression for
 *   some researchers but preserves the core extraction mechanism:
 *   institutional prestige and career advancement remain concentrated in
 *   mainstream channels, while exploratory anomalous research remains
 *   perpetually marginalized. The constraint exhibits properties of both
 *   tangled_rope (genuine coordination function + asymmetric extraction) and
 *   snare (severe suppression, limited exit options). The theater ratio
 *   (0.65) reflects that peer review for anomalous claims applies
 *   methodological skepticism standards higher than those for mainstream
 *   research—rejection is often framed as epistemological necessity when it
 *   serves institutional boundary maintenance.
 *
 * KEY AGENTS:
 *   - Anomalous Experience Researchers: Primary victims (powerless/trapped) — pursue novel phenomena with limited funding access; face career risk from association with anomalous research; trapped between pursuing their intellectual interests and maintaining professional viability
 *   - Mainstream Scientific Establishment: Primary beneficiary (institutional/arbitrage) — benefits from methodological consensus and epistemic coherence; excludes anomalous phenomena from research agenda; maintains institutional prestige through alignment with established frameworks
 *   - Funding Gatekeepers: Primary beneficiary (powerful/mobile) — control resource allocation; actively enforce exclusion through funding criteria and review standards; choose to maintain mainstream portfolio alignment
 *   - Heterodox Research Community: Secondary actor (moderate/constrained) — constrained by limited funding and publishing barriers; benefits from alternative research pathways; can exit mainstream funding at career cost
 *   - Alternative Funding Ecosystem: Emerging organized actor (organized/constrained) — building parallel funding and publishing pathways; developing exit routes for anomalous researchers; constrained by smaller resource base
 *   - Peer Review System: Institutional mechanism (institutional/arbitrage) — applies differential skepticism standards; legitimizes exclusion through extraordinary evidence standard; increasingly theatrical in application
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing institutional exclusion as epistemological necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(anomalous_experience_research_funding, 0.58).
domain_priors:suppression_score(anomalous_experience_research_funding, 0.68).
domain_priors:theater_ratio(anomalous_experience_research_funding, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(anomalous_experience_research_funding, extractiveness, 0.58).
narrative_ontology:constraint_metric(anomalous_experience_research_funding, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(anomalous_experience_research_funding, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(anomalous_experience_research_funding, tangled_rope).
narrative_ontology:human_readable(anomalous_experience_research_funding, "Anomalous Experience Research Funding Constraint").
narrative_ontology:topic_domain(anomalous_experience_research_funding, "research_administration/anomalous_phenomena").

domain_priors:requires_active_enforcement(anomalous_experience_research_funding).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(anomalous_experience_research_funding, mainstream_institutional_science).
narrative_ontology:constraint_beneficiary(anomalous_experience_research_funding, funding_gatekeepers).
narrative_ontology:constraint_victim(anomalous_experience_research_funding, anomalous_experience_researchers).
narrative_ontology:constraint_victim(anomalous_experience_research_funding, field_epistemic_development).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ANOMALOUS EXPERIENCE RESEARCHER (SNARE) — Career trapped within funding structure. Researcher cannot pursue anomalous experience investigation without institutional affiliation; cannot secure institutional affiliation while pursuing anomalous investigation; cannot publish anomalous findings in high-impact venues without damaging career credibility. High suppression through reputational risk and funding unavailability. No meaningful exit — the researcher must either abandon the research or accept permanent marginalization.
constraint_indexing:constraint_classification(anomalous_experience_research_funding, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: HETERODOX RESEARCH COMMUNITY (TANGLED ROPE) — Constrained by funding scarcity and publishing barriers, but also benefits from alternative funding sources (private foundations, non-traditional venues, international networks). The constraint coordinates discussion of anomalous phenomena across institutional boundaries while simultaneously extracting compliance with silence in mainstream channels. Moderate exit cost — researchers can pursue work through alternative institutions or funding, but at significant career opportunity cost.
constraint_indexing:constraint_classification(anomalous_experience_research_funding, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MAINSTREAM SCIENTIFIC ESTABLISHMENT (ROPE) — Benefits from coordination: anomalous experience research is cordoned off, allowing mainstream institutional science to maintain methodological consensus and epistemological coherence without confronting disconfirming data classes. The constraint solves a genuine coordination problem (how to maintain disciplinary boundaries) while delivering clear benefits to this agent. Low experienced extraction — the beneficiary experiences this as pure coordination.
constraint_indexing:constraint_classification(anomalous_experience_research_funding, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FUNDING GATEKEEPER INSTITUTION (TANGLED ROPE) — Powerful agent with mobile exit options (can choose what research to fund). Benefits from the constraint by maintaining research portfolio coherence and institutional prestige through alignment with mainstream consensus. Also enforces extraction from researchers by controlling access to resources. Mobile exit options mean this agent could theoretically reallocate funding but chooses not to — extraction is active, not passive.
constraint_indexing:constraint_classification(anomalous_experience_research_funding, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: PEER REVIEW RITUAL (PITON) — The formal review process for anomalous experience papers has become substantially performative. Reviewers apply extraordinary skepticism standards that differ markedly from those applied to mainstream research. Rejection rationales often invoke methodological impossibility rather than empirical disproof. The ritual persists through institutional inertia despite degraded function — peer review legitimizes rejection decisions through theatrical rigor assessment rather than genuine evaluation. Theater ratio is high because the functional verification has largely been displaced by reputation defense.
constraint_indexing:constraint_classification(anomalous_experience_research_funding, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: EMERGING ALTERNATIVE FUNDING ECOSYSTEM (SCAFFOLD) — Organized agents (independent foundations, international research networks, citizen science platforms, preprint servers) are building parallel funding and publishing pathways for anomalous experience research. These create temporary coordination structures that reduce dependence on mainstream gatekeepers. The constraint is being bypassed through alternative infrastructure — not eliminated, but increasingly supplemented. Sunset logic applies: as alternative funding matures and produces credible findings, the exclusion mechanism loses force.
constraint_indexing:constraint_classification(anomalous_experience_research_funding, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW FRAMING (MOUNTAIN) — From a civilizational frame, anomalous experience research may be intrinsically marginalizable because extraordinary claims require extraordinary evidence — a principle that appears as natural law to scientific practice. Under this reading, the funding exclusion is not a constraint but a reflection of epistemological necessity. However, this perspective risks naturalizing what is actually a contingent institutional arrangement. The false summit detector should flag this classification: the 'law of evidence' is being used to justify exclusion that serves institutional interests.
constraint_indexing:constraint_classification(anomalous_experience_research_funding, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(anomalous_experience_research_funding_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(anomalous_experience_research_funding, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(anomalous_experience_research_funding, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(anomalous_experience_research_funding, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(anomalous_experience_research_funding, TR),
    TR >= 0.70.

:- end_tests(anomalous_experience_research_funding_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Researchers pursuing anomalous phenomena face significant extraction through funding unavailability and career risk. However, the extraction is not total—alternative funding sources exist, and some institutional flexibility is increasing. The value reflects barriers that are real and painful but not absolute. The measurement trajectory shows extractiveness increasing over the 30-year interval as mainstream science has become more methodologically restrictive and anomalous research has become more explicitly excluded from priority research areas. Suppression (0.68): High. Multiple suppression mechanisms operate simultaneously: explicit funding exclusion, differential peer review standards, publishing barriers, institutional affiliation barriers, and career risk. Researchers cannot easily escape this suppression without abandoning either their research interests or their mainstream career. Theater ratio (0.65): Moderately high. The peer review process for anomalous claims has become increasingly ritualistic—reviewers apply extraordinary evidence standards that differ from those used for mainstream research, framing exclusion as epistemological rigor rather than gatekeeping. The ritual has strengthened over time as mainstream institutions have become more defensive about their boundaries. The increase in theater_ratio from 0.45 to 0.65 over the interval reflects growing performativity: exclusion is increasingly justified through methodological critique rather than engaged empirical evaluation.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The same funding mechanism appears as coordination (Rope) to mainstream gatekeepers, extraction (Snare) to trapped researchers, mixed coordination-extraction (Tangled Rope) to researchers with alternative exit routes, institutional theater (Piton) to reviewers whose standards have become decoupled from function, and temporary institutional failure (Scaffold) to the organized coalition building alternative pathways. The analytical observer faces a false summit risk—the 'extraordinary evidence standard' appears as epistemological law but serves institutional boundary maintenance.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values for each perspective are derived from structural position and exit options. Mainstream scientists experience low d (0.15–0.25) because they are beneficiaries with arbitrage exit options—they can easily move resources into or away from anomalous phenomena and experience no extraction. Anomalous experience researchers experience high d (0.90–0.95) because they are victims with trapped exit options—they cannot escape the funding constraint without abandoning their research interests. Heterodox researchers experience moderate d (0.60–0.70) because they are victims with constrained (but not trapped) exit options—they can pursue research through alternative channels at significant career cost. Gatekeepers experience low d (0.30–0.40) because they are beneficiaries with mobile exit options—they actively choose to maintain exclusion, so the extraction flows away from them. The alternative funding ecosystem experiences moderate d (0.50–0.60) because it is a growing actor with emerging exit capacity—as alternative infrastructure develops, the constraint's force diminishes for actors who can access these pathways.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE CLASSIFICATION CONFIRMED: The constraint exhibits both genuine coordination and asymmetric extraction. COORDINATION FUNCTION: Mainstream science requires some mechanism for maintaining methodological consensus and disciplinary coherence—anomalous experience research poses genuine epistemic challenges that threaten interpretability of findings within established frameworks. Excluding poorly-specified anomalous claims allows researchers to focus on well-characterized phenomena with interpretable results. This is a real coordination problem. ASYMMETRIC EXTRACTION: The constraint systematically benefits institutional science (career prestige, funding concentration, methodological consensus) while extracting from anomalous researchers (career risk, funding unavailability, publishing barriers). The extraction flows from powerless researchers toward powerful institutions. The constraint requires active enforcement—gatekeepers must continuously apply differential review standards and funding exclusion to maintain the boundary. WITHOUT the active enforcement, anomalous research would receive mainstream attention and resources, indicating the extraction is not self-maintaining. MANDATROPHY RISK: The constraint risks being mislabeled as pure coordination (Rope) because it genuinely solves a coordination problem. However, the scope of coordination is asymmetric—it coordinates mainstream science at the cost of extracting from anomalous researchers. The gatekeeper institution's perspective (Rope) would mislabel the constraint if taken as the system's classification. The engine should flag this asymmetry and correctly classify the constraint as Tangled Rope despite the genuine coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    anomalous_claim_verifiability,
    'Are anomalous experience claims genuinely irreproducible under controlled conditions, or does exclusion from mainstream research infrastructure prevent the resource investment needed for rigorous testing?',
    'Well-funded, pre-registered, peer-designed replication studies in collaborative institutions with adequate resources and measurement apparatus. Comparison of anomalous claim replication rates under high-funding vs low-funding conditions.',
    'If inherently irreproducible: funding exclusion reflects genuine epistemological necessity; reclassify toward natural law. If reproducible under proper conditions: funding exclusion is pure institutional gatekeeping; reclassify toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(anomalous_claim_verifiability, empirical, 'Whether anomalous claims are intrinsically irreproducible or reproducible given adequate resources').

omega_variable(
    gatekeeper_institutional_interest,
    'To what extent does mainstream institutional exclusion of anomalous research serve epistemological rigor vs. institutional self-protection (maintaining consensus, defending disciplinary boundaries, protecting funding concentration)?',
    'Comparative analysis: review standards applied to anomalous research vs. to mainstream research with equivalent empirical status and methodological rigor. Examination of reviewer identity, institutional affiliation, and citation patterns. Structural analysis of funding flows and career incentives.',
    'If primarily epistemological: funding constraint is justified; classify as rope. If primarily institutional self-protection: funding constraint is extractive; classify as snare or tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeper_institutional_interest, empirical, 'Whether gatekeeper exclusion serves epistemic rigor or institutional self-interest').

omega_variable(
    alternative_pathways_sufficiency,
    'Do alternative funding sources (private foundations, international networks, preprint infrastructure) constitute genuine exit routes for anomalous experience researchers, or merely ghetto solutions that preserve institutional exclusion?',
    'Longitudinal career tracking of researchers who exit mainstream funding channels. Citation analysis and empirical validity assessment of research produced through alternative pathways. Resource comparison: funding levels in alternative vs. mainstream channels.',
    'If exit routes are genuine: scaffold perspective is structural; expect sunset as alternative funding matures. If ghetto solutions: alternative pathways do not change the constraint''s extraction mechanism; reclassify toward pure snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_pathways_sufficiency, empirical, 'Whether alternative research pathways are functional exits or institutional ghettoes').

omega_variable(
    anomalous_experience_ontology,
    'What counts as ''anomalous experience research''? Does the funding constraint apply to phenomenological investigation of rare experiences, to causal claims about mechanism, to specific anomaly classes, or to all phenomena outside mainstream frameworks?',
    'Systematic review of funding rejections with coded analysis of rejection rationales. Mapping of the boundary between accepted heterodox research and excluded anomalous investigation. Linguistic analysis of gatekeeper framing.',
    'If boundary is empirical (research on already-anomalous phenomena vs. research positing new mechanisms): constraint is coordination mechanism (rope). If boundary is political (anyone questioning mainstream consensus): constraint is pure gatekeeping (snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(anomalous_experience_ontology, conceptual, 'Definitional clarity of what constitutes anomalous experience research for funding purposes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(anomalous_experience_research_funding, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anomex_tr_t0, anomalous_experience_research_funding, theater_ratio, 0, 0.45).
narrative_ontology:measurement(anomex_tr_t10, anomalous_experience_research_funding, theater_ratio, 10, 0.58).
narrative_ontology:measurement(anomex_tr_t20, anomalous_experience_research_funding, theater_ratio, 20, 0.65).
narrative_ontology:measurement(anomex_tr_t30, anomalous_experience_research_funding, theater_ratio, 30, 0.62).

% Extraction over time
narrative_ontology:measurement(anomex_be_t0, anomalous_experience_research_funding, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(anomex_be_t10, anomalous_experience_research_funding, base_extractiveness, 10, 0.51).
narrative_ontology:measurement(anomex_be_t20, anomalous_experience_research_funding, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(anomex_be_t30, anomalous_experience_research_funding, base_extractiveness, 30, 0.56).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(anomalous_experience_research_funding, information_standard).
narrative_ontology:affects_constraint(anomalous_experience_research_funding, extraordinary_evidence_standard).
narrative_ontology:affects_constraint(anomalous_experience_research_funding, institutional_reputation_risk).
narrative_ontology:affects_constraint(anomalous_experience_research_funding, alternative_research_infrastructure).

% DUAL FORMULATION NOTE:
% Anomalous experience research funding decomposes into three structurally distinct constraints: (1) extraordinary_evidence_standard—the methodological requirement for anomalous claims, which may be epistemologically justified; (2) institutional_reputation_risk—the career consequences of pursuing anomalous research, which is institutional rather than epistemic; (3) alternative_research_infrastructure—the emerging parallel funding and publishing ecosystem that reduces dependence on mainstream gatekeepers. The funding constraint story integrates all three but represents them as a single composite mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(anomalous_experience_research_funding, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
