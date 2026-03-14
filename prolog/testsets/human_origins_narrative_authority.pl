% ============================================================================
% CONSTRAINT STORY: human_origins_narrative_authority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_human_origins_narrative_authority, []).

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
 *   constraint_id: human_origins_narrative_authority
 *   human_readable: Human Origins Narrative Authority
 *   domain: epistemology/cultural_authority/science_studies
 *
 * SUMMARY:
 *   The constraint of human origins narrative authority governs which
 *   frameworks for understanding human emergence are recognized as legitimate
 *   knowledge. This is not merely an epistemic question but a structural
 *   extraction mechanism: the concentration of authority to define the human
 *   past generates asymmetric benefits for institutional gatekeepers and
 *   costs for alternative knowledge systems. The constraint exhibits tangled
 *   coordination and extraction: genuine coordination function exists
 *   (complex prehistory does require expert interpretation), but this
 *   coordination is achieved through suppression of alternatives rather than
 *   through inclusive integration. The extractiveness has increased over the
 *   measured interval (0.35 → 0.58) as the Modern Evolutionary Synthesis has
 *   calcified into institutional orthodoxy, while theater_ratio has also
 *   risen (0.48 → 0.64) indicating that narrative maintenance is increasingly
 *   performative relative to genuine explanatory advancement. The constraint
 *   demonstrates all six DR types depending on perspective, making it a
 *   diagnostic exemplar for how epistemological authority concentrates.
 *
 * KEY AGENTS:
 *   - Alternative Origin Frameworks: Primary victim (powerless/trapped) — indigenous cosmologies, non-Western scientific traditions, heterodox accounts systematically excluded from legitimate knowledge production
 *   - Marginalized Knowledge Systems: Primary victim (powerless/trapped) — communities whose origin narratives are subordinated, delegitimized, or appropriated without credit
 *   - Scientific Evidence Integrators: Secondary victim (moderate/constrained) — paleontologists, geneticists, archaeologists who generate findings that destabilize dominant narratives face career penalties and funding asymmetries
 *   - Institutional Gatekeepers: Primary beneficiary (institutional/arbitrage) — academic institutions, textbook publishers, museums, media maintain monopoly on legitimate narrative authority and derive prestige, curriculum control, and publication advantage
 *   - Narrative Authority Custodians: Primary beneficiary (institutional/arbitrage) — university departments, journal editorial boards, disciplinary associations that enforce orthodoxy through hiring, publication, and credentialing
 *   - Plural Knowledge Community: Organized victim (organized/constrained) — epistemic pluralism movements, decolonial science initiatives, indigenous scholarship programs building alternative institutional footing
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing institutional authority concentration as structural inevitability of expert knowledge
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(human_origins_narrative_authority, 0.58).
domain_priors:suppression_score(human_origins_narrative_authority, 0.68).
domain_priors:theater_ratio(human_origins_narrative_authority, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(human_origins_narrative_authority, extractiveness, 0.58).
narrative_ontology:constraint_metric(human_origins_narrative_authority, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(human_origins_narrative_authority, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(human_origins_narrative_authority, tangled_rope).
narrative_ontology:human_readable(human_origins_narrative_authority, "Human Origins Narrative Authority").
narrative_ontology:topic_domain(human_origins_narrative_authority, "epistemology/cultural_authority/science_studies").

domain_priors:requires_active_enforcement(human_origins_narrative_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(human_origins_narrative_authority, dominant_origin_narrative_custodians).
narrative_ontology:constraint_beneficiary(human_origins_narrative_authority, institutional_knowledge_gatekeepers).
narrative_ontology:constraint_victim(human_origins_narrative_authority, alternative_origin_frameworks).
narrative_ontology:constraint_victim(human_origins_narrative_authority, marginalized_knowledge_systems).
narrative_ontology:constraint_victim(human_origins_narrative_authority, scientific_evidence_integration).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ALTERNATIVE ORIGIN FRAMEWORKS (SNARE) — Indigenous cosmologies, non-Western scientific traditions, and heterodox accounts face structural exclusion from legitimate knowledge production. Trapped by institutional gatekeeping, publication barriers, and the dominance of narrative authority concentrated in Western academic institutions. No path to parity; extraction is maximal as these frameworks are systematically suppressed.
constraint_indexing:constraint_classification(human_origins_narrative_authority, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SCIENTIFIC EVIDENCE INTEGRATORS (TANGLED ROPE) — Paleontologists, geneticists, and archaeologists generate evidence that both constrains and occasionally destabilizes dominant narratives. Experience tangled coordination (the scientific method itself is coordination) alongside extraction (career penalties for findings that challenge establishment narratives, funding asymmetries favoring narratives that maintain institutional stability). Constrained by career risk but not completely trapped.
constraint_indexing:constraint_classification(human_origins_narrative_authority, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: NARRATIVE AUTHORITY CUSTODIANS (ROPE) — Academic institutions, textbook publishers, museum curators, and media gatekeepers benefit from stabilized narrative dominance. Experience the constraint as coordination: maintaining coherent public understanding of human origins. The extraction they derive (institutional prestige, publication monopoly, curriculum control) appears legitimate because they frame it as coordination rather than as asymmetric authority capture.
constraint_indexing:constraint_classification(human_origins_narrative_authority, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PLURAL KNOWLEDGE COMMUNITY (SCAFFOLD) — Organized movements toward epistemic pluralism (indigenous scholarship initiatives, decolonial science movements, collaborative anthropology) view narrative authority as a temporary institutional monopoly with a sunset clause. As these alternatives mature and gain institutional footing, the extractive mechanisms of singular narrative authority lose force. Organized agents see constrained but shrinking exit costs.
constraint_indexing:constraint_classification(human_origins_narrative_authority, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: EVOLUTIONARY SYNTHESIS ORTHODOXY (PITON) — The Modern Evolutionary Synthesis, once a genuinely coordinating framework for understanding human origins, has calcified into theater. The core mechanisms (natural selection, genetic inheritance, population dynamics) remain scientifically productive, but much of the institutional apparatus around 'THE' human origin story has become performative: textbook standardization, museum exhibitions, popular science that presents settled consensus where genuine uncertainty persists. Theater ratio high because institutional maintenance exceeds genuine explanatory function.
constraint_indexing:constraint_classification(human_origins_narrative_authority, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some narrative authority consolidation is structurally inevitable: complex claims about deep human history always require expert interpretation, and interpretation always involves authority relations. This perspective risks naturalizing what is actually a contingent institutional choice to concentrate authority rather than distribute it. The engine will flag this as a false summit.
constraint_indexing:constraint_classification(human_origins_narrative_authority, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(human_origins_narrative_authority_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(human_origins_narrative_authority, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(human_origins_narrative_authority, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(human_origins_narrative_authority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(human_origins_narrative_authority, TR),
    TR >= 0.70.

:- end_tests(human_origins_narrative_authority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint generates significant asymmetric benefits: narrative authority custodians monopolize legitimate knowledge production (textbook adoption, museum exhibitions, media authority, grant funding), while alternative frameworks receive fraction of resources and institutional recognition. The extraction is not as severe as pure rent-seeking (some genuine scientific coordination exists) but exceeds what pure coordination would justify. The measurement trajectory (0.35 → 0.58) reflects increasing institutionalization of narrative orthodoxy and decreasing integration of heterodox findings. Suppression (0.68): High. Structural barriers include publication gatekeeping (peer review filters favoring orthodox frameworks), funding asymmetries (mainstream research receives disproportionate grants), curriculum standardization (textbooks enforce single narrative), institutional hiring (departments reproduce orthodox scholars), and media monopoly (popular science presents consensus where uncertainty exists). Alternative frameworks face tacit knowledge exclusion (oral traditions not recognized as publishable), credentialing barriers (non-Western scholars lack Western credentials), and appropriation risk (findings absorbed into dominant narrative without credit). Theater ratio (0.64): Moderate-high. Institutional apparatus for maintaining narrative authority is increasingly performative relative to explanatory function. Museum exhibitions present settled human origin story despite genuine scientific uncertainty. Textbooks standardize orthodoxy despite ongoing paradigm shifts (Homo floresiensis, Denisovan interbreeding, extended childhood hypothesis challenged prior certainty). Popular science emphasizes narrative coherence over evidential ambiguity. Academic tenure patterns reward paradigm defenders and penalize paradigm challengers.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows the full range of classification from a single structural arrangement. Alternative frameworks see extraction with no exit (Snare) — their accounts are systematically suppressed and cannot compete equally. Scientific evidence integrators see mixed coordination-extraction (Tangled Rope) — the method enables their work but results are suppressed. Institutional gatekeepers see pure coordination (Rope) — they frame narrative authority as legitimate knowledge governance. Plural knowledge movements see a temporary extractive monopoly with sunset (Scaffold) — alternative institutions and decolonial frameworks are building exit pathways. The evolutionary synthesis establishment sees its own degraded apparatus (Piton) — narrative maintenance is increasingly theater. The civilizational analytical observer risks seeing immutable authority structure (Mountain) — expert interpretation always requires gatekeeping — but structural data reveals this as false naturalization: the choice to concentrate authority rather than distribute it is contingent, not inevitable.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (institutional gatekeepers, narrative authority custodians) occupy positions of concentrated power with arbitrage exit options — they can shift to alternative narratives only if institutional authority requires it, but institutional inertia is high. Their directionality d is low (≈0.15-0.25), producing negative or low effective extraction χ from their perspective. They experience the constraint as coordination: legitimate knowledge governance. Victims (alternative frameworks, marginalized knowledge systems) occupy powerless positions with trapped exit options — they cannot transition to orthodox frameworks without abandoning their epistemic commitments, and orthodox frameworks will not integrate them without radical restructuring. Their directionality d is high (≈0.85-0.95), producing high effective extraction χ. Scientific evidence integrators occupy moderate power with constrained exit options — they can generate alternative evidence but face career penalties (publication rejection, funding denial, hiring discrimination) for challenging orthodoxy. Their d is moderate (≈0.55-0.65), producing tangled experience of coordination (scientific method enables their work) and extraction (orthodoxy suppresses their findings).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by revealing how 'Human Origins' naturalizes a choice about knowledge authority as a fact about the world. The mountain perspective claims narrative consolidation is structurally inevitable: complex deep-time claims require interpretation, interpretation requires authority, authority requires gatekeeping. But this conflates three separable claims: (1) interpretation is necessary, (2) gatekeeping is necessary, (3) concentrated gatekeeping is necessary. Only (1) is structurally required. Plural knowledge systems demonstrate that (2) can be achieved through distributed validation (peer review across traditions, collaborative verification), and (3) is an institutional choice optimizing for institutional stability, not epistemic validity. The constraint's genuine coordination function (integrating complex paleontological, genetic, and archaeological evidence) is distinct from its extractive function (monopolizing narrative authority). Decomposing these would produce two stories: human_origins_evidence_coordination (ε≈0.25, Rope) and human_origins_narrative_monopoly (ε≈0.68, Tangled Rope). They are linked by network dependency: monopoly on narrative authority shapes which evidence questions get funded. The analytical observer's mountain perspective is a false summit that prevents seeing the actual structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    narrative_authority_vs_epistemic_validity,
    'Can narrative authority be distributed without fragmenting empirical validity standards? Or does epistemic reliability require some degree of gatekeeping?',
    'Comparative analysis of distributed knowledge systems (indigenous knowledge governance, citizen science, open-source scientific collaboration) against centralized systems; measurement of error rates, reproducibility, and knowledge integration across systems',
    'If distributed systems maintain validity: narrative authority consolidation is extractive choice, not structural necessity (Snare from more perspectives). If distributed systems show higher error rates: some gatekeeping is coordinate cost (Rope from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(narrative_authority_vs_epistemic_validity, empirical, 'Whether distributed narrative authority can maintain epistemic validity').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of alternative frameworks primarily structural (institutional barriers, resource control) or internalized (marginalized scholars have internalized dismissal of their frameworks)?',
    'Post-institutional-reform suppression trajectory: if suppression declines after gatekeeping mechanisms are removed, suppression is primarily structural. If it persists despite open platforms, suppression is partially internalized.',
    'If structural: correctable through institutional design. If internalized: requires longer epistemological transition and identity reconstruction for suppressed knowledge communities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural vs internalized suppression of alternative frameworks').

omega_variable(
    evidence_vs_narrative_directionality,
    'Does narrative authority shape which evidence is generated (funding, researcher hiring, research questions), or do scientific findings drive narrative revision?',
    'Historical analysis of paradigm shifts (e.g., acceptance of African origins, recognition of Homo floresiensis, Denisovan interbreeding, extended childhood hypothesis); correlation between funding flows and evidence production; tracking of publication lag between finding and institutional incorporation',
    'If narrative shapes evidence: extractiveness is higher (Snare closer to target). If evidence drives narrative: extractiveness is lower (Rope closer to equilibrium).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(evidence_vs_narrative_directionality, empirical, 'Whether narrative authority drives or responds to scientific evidence').

omega_variable(
    identity_lock_in_excluded_scholars,
    'Are scholars from marginalized traditions trapped in excluded positions by identity lock-in (their identity as scholars constituted through that tradition) or by structural barriers (no institutional pathways)?',
    'Interview and career trajectory analysis; measurement of schema shift in scholars who transition between excluded and mainstream institutions; analysis of whether exit from marginalized framework requires identity transformation',
    'If identity lock-in dominant: constraint appears as mobile from structural view but rope from identity perspective (diagnostic signal of cognitive capture). If structural barriers dominant: constraint is mountain-like immobility.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_in_excluded_scholars, conceptual, 'Identity lock-in vs structural barriers in excluded scholars').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(human_origins_narrative_authority, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hona_tr_t0, human_origins_narrative_authority, theater_ratio, 0, 0.48).
narrative_ontology:measurement(hona_tr_t30, human_origins_narrative_authority, theater_ratio, 30, 0.58).
narrative_ontology:measurement(hona_tr_t60, human_origins_narrative_authority, theater_ratio, 60, 0.64).

% Extraction over time
narrative_ontology:measurement(hona_be_t0, human_origins_narrative_authority, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hona_be_t30, human_origins_narrative_authority, base_extractiveness, 30, 0.48).
narrative_ontology:measurement(hona_be_t60, human_origins_narrative_authority, base_extractiveness, 60, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(human_origins_narrative_authority, identity_coordination).
narrative_ontology:affects_constraint(human_origins_narrative_authority, epistemic_pluralism_barriers).
narrative_ontology:affects_constraint(human_origins_narrative_authority, indigenous_knowledge_appropriation).
narrative_ontology:affects_constraint(human_origins_narrative_authority, academic_publication_gatekeeping).

% DUAL FORMULATION NOTE:
% Human origins narrative authority decomposes along the knowledge production pipeline. The constraint family includes: (1) evidence_generation_bias (which research questions get funded), (2) publication_gatekeeping (which findings reach legitimate channels), (3) narrative_authority_concentration (which frameworks define 'the' human story). Each has different ε and different beneficiary/victim dynamics. This story addresses the integrated constraint across all three; specialized stories address each pipeline stage.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(human_origins_narrative_authority, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
