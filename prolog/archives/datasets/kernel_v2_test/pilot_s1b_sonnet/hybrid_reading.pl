% ============================================================================
% CONSTRAINT STORY: hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hybrid_reading, []).

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
 *   constraint_id: hybrid_reading
 *   human_readable: Hybrid Latin Reconstruction (Pragmatic Accommodation Reading)
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   The humanist Latin reconstruction of the 15th-16th centuries created a
 *   hybrid linguistic regime: Classical Latin (Cicero, Virgil) was
 *   reconstructed for prestige literary and rhetorical domains, while
 *   medieval Latin substrate was retained for technical, legal, and
 *   administrative functions. This reading treats the hybrid system as
 *   pragmatic scaffold — a transitional arrangement coordinating dual needs
 *   (symbolic prestige + functional continuity) during the gradual
 *   displacement of Latin by vernacular languages. The constraint exhibits
 *   low extraction because domain separation allows both registers to serve
 *   their functions without requiring wholesale replacement of working Latin.
 *   Theater ratio rises gradually (0.20→0.48 over 250 years) as Latin itself
 *   becomes increasingly performative: by 1700, Latin persists primarily for
 *   symbolic/credentialing purposes rather than as living administrative
 *   language, but the hybrid domain structure remains stable throughout the
 *   interval. This is ONE READING of the contested kernel 'correct_latin'.
 *   Sibling readings (continuity_reading: medieval forms are legitimate
 *   development; rupture_reading: Classical purity requires rejection of
 *   medieval substrate) would assign different victim sets and extraction
 *   mechanisms.
 *
 * KEY AGENTS:
 *   - Humanist Scholars: Primary beneficiaries (institutional/mobile) — capture prestige of Classical reconstruction while retaining medieval forms for practical work; define 'correct' Latin for literary domains
 *   - Administrative Scribes: Beneficiaries (moderate/constrained) — medieval technical vocabulary and syntactic patterns preserved for contracts, records, official correspondence; working Latin largely unaffected by humanist revival
 *   - Technical Professionals: Beneficiaries (moderate/constrained) — legal, medical, philosophical Latin retains medieval terminology and structures; domain expertise insulated from Classical purism
 *   - University Curriculum Committees: Organized agents (organized/mobile) — manage transition between scholastic and humanist models; see domain separation as temporary solution during vernacular encroachment
 *   - Papal Chancery: Institutional authority (powerful/arbitrage) — maintains hybrid practice (Ciceronian bulls, medieval canon law); benefits from symbolic prestige and functional continuity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hybrid_reading, 0.18).
domain_priors:suppression_score(hybrid_reading, 0.22).
domain_priors:theater_ratio(hybrid_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hybrid_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(hybrid_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(hybrid_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hybrid_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(hybrid_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hybrid_reading, scaffold).
narrative_ontology:human_readable(hybrid_reading, "Hybrid Latin Reconstruction (Pragmatic Accommodation Reading)").
narrative_ontology:topic_domain(hybrid_reading, "historical_linguistics/philology/intellectual_history").

narrative_ontology:has_sunset_clause(hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hybrid_reading, '36f826eb-255d-45e3-9efd-827326e1d0f2').
narrative_ontology:cs_kernel_codification('36f826eb-255d-45e3-9efd-827326e1d0f2', distributed).
narrative_ontology:cs_authority_grounding('36f826eb-255d-45e3-9efd-827326e1d0f2', distributed).
narrative_ontology:cs_reading_relation('36f826eb-255d-45e3-9efd-827326e1d0f2', hybrid_reading__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('36f826eb-255d-45e3-9efd-827326e1d0f2', hybrid_reading__rupture_reading, coexists_with).
narrative_ontology:cs_axiom('36f826eb-255d-45e3-9efd-827326e1d0f2', foundational, domain_specificity_legitimates_variation).
narrative_ontology:cs_axiom_status(domain_specificity_legitimates_variation, holdable).
narrative_ontology:cs_axiom_grounding('36f826eb-255d-45e3-9efd-827326e1d0f2', domain_specificity_legitimates_variation, conventional).
narrative_ontology:cs_axiom('36f826eb-255d-45e3-9efd-827326e1d0f2', secondary, functional_continuity_preserves_technical_substrate).
narrative_ontology:cs_axiom_status(functional_continuity_preserves_technical_substrate, holdable).
narrative_ontology:cs_axiom_grounding('36f826eb-255d-45e3-9efd-827326e1d0f2', functional_continuity_preserves_technical_substrate, instrumental).
narrative_ontology:cs_reference_frame('36f826eb-255d-45e3-9efd-827326e1d0f2', pragmatic_domain_separation).
narrative_ontology:cs_drift_state('36f826eb-255d-45e3-9efd-827326e1d0f2', vernacular_encroachment_peak, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('36f826eb-255d-45e3-9efd-827326e1d0f2', '').
narrative_ontology:cs_kernel_id(hybrid_reading, correct_latin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hybrid_reading, humanist_scholars).
narrative_ontology:constraint_beneficiary(hybrid_reading, administrative_scribes).
narrative_ontology:constraint_beneficiary(hybrid_reading, technical_professionals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hybrid_reading, university_curriculum_committees).
narrative_ontology:constraint_beneficiary(hybrid_reading, papal_chancery).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define 'correct' Latin for prestige literary domains through recovery of Classical models (Cicero, Virgil, Livy). Benefit from symbolic capital of antiquarian expertise while retaining medieval forms for administrative correspondence and technical writing. Can exit to vernacular literary production if Latin becomes non-viable, but institutional position (university chairs, patronage) rewards Classical mastery.
narrative_ontology:constraint_stakeholder(hybrid_reading, humanist_scholars, agenda_setter,
    institutional, generational, mobile, continental).

% Produce contracts, land records, legal documents, municipal correspondence in medieval Latin — technical vocabulary and syntactic patterns unchanged by humanist revival. Hybrid system allows continuation of established practice without retraining. Exit constrained by career investment in Latin literacy, but vernacular administrative writing is emerging alternative in some regions by 1600.
narrative_ontology:constraint_stakeholder(hybrid_reading, administrative_scribes, beneficiary,
    moderate, biographical, constrained, regional).

% Medical, legal, and philosophical professionals use Latin with medieval technical terminology. Domain-specific Latin insulated from Classical purism — medical texts retain medieval anatomical vocabulary, legal briefs retain medieval procedural formulas, philosophical disputations retain scholastic terminology. Benefit from functional continuity while prestige domains undergo reconstruction.
narrative_ontology:constraint_stakeholder(hybrid_reading, technical_professionals, beneficiary,
    moderate, biographical, constrained, regional).

% Manage transition from scholastic to humanist pedagogy. See domain separation as temporary: Classical Latin for literary studies, medieval Latin for professional training (law, medicine, theology). Benefit from managing gradual transition without rupture to either constituency (humanists demand Classical standards, professional faculties demand functional continuity). Organized collective can coordinate policy across universities.
narrative_ontology:constraint_stakeholder(hybrid_reading, university_curriculum_committees, agenda_setter,
    organized, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(hybrid_reading, university_curriculum_committees, beneficiary).

% Institutional authority maintaining hybrid practice: Ciceronian periodic structure and vocabulary for papal bulls and encyclicals (symbolic prestige), medieval technical terminology for canon law and administrative correspondence (functional continuity). Benefits from both registers and has authority to define correctness for different documentary genres. Arbitrage-level exit because can set own standards without external constraint.
narrative_ontology:constraint_stakeholder(hybrid_reading, papal_chancery, agenda_setter,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(hybrid_reading, papal_chancery, beneficiary).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Domain-specific Latin standards solve the coordination problem of maintaining both symbolic continuity with classical antiquity (for prestige literary culture) and functional continuity with medieval administrative/technical practice. Without domain separation, either Classical purism forces costly retraining of scribes/professionals, or medieval substrate preservation forfeits humanist prestige.
% TRANSFER_FUNCTION: Symbolic capital flows to humanist scholars (Classical expertise confers prestige, patronage, university positions). Functional continuity flows to administrative scribes and technical professionals (medieval substrate preserved in working documents). No substantial monetary transfer; primary transfer is status and career security across domains.
% ABSENT_VOICES: Medieval scholastic philosophers and theologians who see Classical Latin as pagan literary register inappropriate for Christian truth. Present in universities but marginalized by humanist curriculum reforms. Also absent: vernacular writers (Dante, Petrarch successors) who see Latin itself as extractive regardless of register — not in this conversation because constraint is internal to Latin-writing community.
% DISAPPEARANCE_RATIONALE: If hybrid domain separation disappeared overnight, either (1) Classical purism extends to all domains, forcing retraining of scribes and disrupting administrative continuity, or (2) medieval substrate extends to prestige domains, forfeiting humanist antiquarian project. Actual arrangement coordinates coexistence of both registers by separating their domains. Disappearance creates conflict that the hybrid system was designed to avoid.
% FOUNDING_PROBLEM: After 1453 (fall of Constantinople), influx of Greek manuscripts and refugee scholars intensified humanist recovery of Classical Latin models. Founding problem: how to reconcile humanist drive for Classical purity (Ciceronian syntax, Augustan vocabulary) with entrenched medieval Latin in legal, medical, administrative, and theological domains. Wholesale replacement would disrupt functional continuity; ignoring Classical models would forfeit Renaissance antiquarian project.
% FOUNDING_PROBLEM_CORROBORATION: By 1700, founding problem is dead: Latin is no longer primary administrative language (vernacular has displaced it), and Classical vs medieval distinction matters only for scholarly/ecclesiastical domains where Latin persists. Corroboration: historiography of Latin (Waquet 'Latin or the Empire of a Sign', Ijsewijn 'Companion to Neo-Latin Studies') documents 17th-century vernacularization of administration and restriction of Latin to prestige/ritual functions. University records show vernacular lectures displacing Latin in technical faculties 1650-1750. The hybrid system outlives its founding problem — domain separation persists as performance after the functional coordination need has dissolved.
narrative_ontology:disappearance_verdict(hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(hybrid_reading, dead).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HUMANIST SCHOLAR (SCAFFOLD) — Benefits from reconstructed Classical prestige register while retaining functional medieval forms for administrative work. Experiences the constraint as temporary solution: domain-specific standards coordinate literary production without requiring wholesale replacement of working Latin. Low extraction because the arrangement serves genuine dual needs.
constraint_indexing:constraint_classification(hybrid_reading, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 2: ADMINISTRATIVE SCRIBE (ROPE) — Benefits from preservation of medieval technical vocabulary and syntactic patterns for contracts, records, official correspondence. Humanist reconstruction affects prestige domains but leaves working Latin largely intact. Coordination without significant extraction — can continue established practice while Classical revival proceeds in parallel.
constraint_indexing:constraint_classification(hybrid_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: UNIVERSITY CURRICULUM (SCAFFOLD) — Organized agents managing transition between medieval scholastic Latin and humanist Classical models. Sees domain separation as transitional: eventually vernacular will displace Latin for technical/administrative use, while Classical Latin becomes exclusively literary-scholarly. Sunset visible within generational timeframe as vernacular encroachment accelerates.
constraint_indexing:constraint_classification(hybrid_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 4: PAPAL CHANCERY (ROPE) — Institutional authority maintaining hybrid practice: Ciceronian periodic structure for bulls and encyclicals, medieval technical terminology for canon law. Benefits from symbolic prestige of Classical reconstruction while preserving functional continuity. Arbitrage-level exit because authority to define 'correct' Latin for different domains.
constraint_indexing:constraint_classification(hybrid_reading, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (SCAFFOLD) — From civilizational perspective, domain-specific reconstruction is pragmatic accommodation during language transition. Humanist revival reconstructs what is symbolically valuable (prestige literature) while retaining what is functionally necessary (technical/legal vocabulary). Sunset clause clear: Latin losing ground to vernacular in all domains; hybrid system is transitional arrangement managing decline, not permanent equilibrium.
constraint_indexing:constraint_classification(hybrid_reading, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hybrid_reading_tests).
:- end_tests(hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The hybrid system involves minimal extraction because domain separation allows both Classical and medieval registers to coexist without forcing wholesale replacement. Humanists gain prestige from Classical reconstruction without bearing cost of retraining administrative/technical professionals; scribes retain functional vocabulary without resisting literary reform. Slight extraction accumulates over time (0.12→0.25) as Classical standard expands into borderline domains (philosophy, theology) and as vernacular displacement increases career risk for Latin-dependent professionals. Suppression (0.22): Low. Alternatives are not strongly suppressed — agents can choose domain-appropriate register, and vernacular is available exit option. Mild suppression from institutional inertia (universities, church requiring Latin for credentials) and from prestige asymmetry (Classical Latin increasingly gatekeeps elite literary culture). Theater ratio (0.35 at endpoint): Moderate-low but rising. Early in interval (1450), Latin is functional administrative language — theater is minimal. By 1700, Latin persists primarily for symbolic purposes (credentialing, prestige, continuity with tradition) while vernacular has displaced it for most technical/administrative functions. The performance is the constraint's gradual transformation into ritual rather than its initial state. Accessibility collapse (0.15) and resistance (0.20) are low, consistent with scaffold rather than mountain: alternatives (vernacular, pure Classical, pure medieval) remain accessible, and the hybrid system faces mild resistance from purists on both sides but is not deeply contested.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates convergent classification across perspectives — all five perspectives see scaffold or rope, none see snare or tangled_rope. The convergence reflects the structural fact that hybrid reconstruction is low-extraction coordination during language transition. The humanist scholar sees scaffold (temporary dual standard during transition to vernacular + Classical literary canon). The administrative scribe sees rope (functional preservation of working Latin). The university curriculum sees scaffold (transitional pedagogy). The papal chancery sees rope (stable hybrid serving dual institutional needs). The analytical observer sees scaffold (pragmatic accommodation during Latin's decline as administrative language). The lack of perspectival gap is itself diagnostic: when a constraint's beneficiary structure is genuinely symmetric and extraction is low, agent perspectives converge. Contrast with sibling readings: rupture_reading would show perspectival gap (humanists see rope, medieval scribes see snare); continuity_reading would show opposite gap (medievalists see rope, humanists see snare as imposing archaism).
 *
 * DIRECTIONALITY LOGIC:
 *   All identified agents are beneficiaries — no victims declared. Humanist scholars benefit from prestige of Classical reconstruction. Administrative scribes benefit from preservation of working medieval forms. Technical professionals benefit from domain insulation. University committees and papal chancery benefit from managing gradual transition without rupture. Directionality derivation: beneficiaries with mobile/arbitrage exit options experience low or negative effective extraction (χ near zero or negative for humanists, papal chancery). Beneficiaries with constrained exit options experience slightly higher χ but still low (scribes, technical professionals moderately bound by career path but benefiting from arrangement). No powerless/trapped agents because domain separation prevents forced adoption of either standard. The constraint coordinates dual needs rather than extracting from one group to benefit another.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold classification resolves mandatrophy by identifying the sunset mechanism: Latin is being displaced by vernacular for technical/administrative functions, and the hybrid system is transitional arrangement managing that displacement. The constraint is NOT permanent equilibrium (which would be rope) because vernacular encroachment is unidirectional and accelerating. By 1700, Latin persists primarily in ecclesiastical and scholarly domains; by 1900, Latin is almost entirely performative (church ritual, academic credentials, legal boilerplate). The has_sunset_clause declaration reflects this trajectory: the hybrid domain separation exists precisely because Latin is losing ground, and the separation manages the transition rather than preserving Latin indefinitely. Theater ratio rising from 0.20 to 0.48 over 250 years traces the constraint's transformation from functional coordination to symbolic performance. If Latin had remained functional administrative language, theater ratio would stay low; the rise signals that the scaffold is working as designed (managing exit from Latin while preserving prestige register for those who value it).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_framing_ambiguity,
    'Is the hybrid reconstruction (Classical for prestige, medieval for technical) the primary structural fact, or is the existence of competing readings (continuity vs rupture) the primary fact?',
    'Committer-frame analysis: if sibling readings (continuity_reading: medieval forms are legitimate development; rupture_reading: Classical purity requires wholesale rejection of medieval substrate) produce different beneficiary structures or victim sets, the kernel itself is contested. If siblings differ only in evaluation (good/bad) but not structure, hybrid_reading is the shared constraint.',
    'If kernel is contested: beneficiaries of hybrid arrangement (humanists, scribes, technical professionals) appear in this reading but not in rupture_reading (which would name medieval scribes as obstacles). If kernel is stable: all three readings describe same beneficiary structure with different normative framings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Whether domain-specific reconstruction is the kernel or one reading of a contested kernel about correct Latin').

omega_variable(
    sunset_mechanism_ambiguity,
    'Does the hybrid system sunset because vernacular displaces Latin entirely, or because one standard (Classical or medieval) eventually wins?',
    'Historical trajectory analysis: if Latin persists into modernity only in ecclesiastical/scholarly domains with uniform Classical standard, sunset via standardization. If Latin disappears from technical/administrative use while persisting in literature, sunset via vernacular displacement. If both registers persist indefinitely in separate domains, no sunset (rope misclassified as scaffold).',
    'Determines whether scaffold classification is correct or whether the constraint is stable coordination (rope). Affects predictions about Latin standardization movements in 17th-19th centuries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_mechanism_ambiguity, empirical, 'Mechanism driving the sunset of hybrid Latin domain separation').

omega_variable(
    prestige_domain_boundary,
    'Where exactly is the boundary between ''prestige'' domains requiring Classical reconstruction and ''technical'' domains retaining medieval substrate?',
    'Domain-by-domain analysis of actual Latin usage in 15th-17th centuries: legal briefs, medical texts, philosophical treatises, theological disputations, diplomatic correspondence, commercial contracts. Identify which domains adopt Ciceronian syntax/vocabulary and which retain medieval patterns.',
    'If boundary is sharp and stable: coordination is real (domains are genuinely separate speech communities). If boundary is contested or shifts over time: extraction mechanism (prestige domains expanding at expense of technical domains, or vice versa).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prestige_domain_boundary, empirical, 'Stability and location of the prestige/technical domain boundary').

omega_variable(
    alternative_reading_structural_delta,
    'What structural elements would change if continuity_reading (medieval forms are legitimate development) or rupture_reading (wholesale Classical purity) were adopted instead?',
    'Committer-axis cross-reading analysis: continuity_reading would treat humanist reconstruction as extraction (delegitimizing functional working Latin); rupture_reading would treat medieval substrate retention as extraction (contaminating pure Classical model). Hybrid_reading treats both as coordination.',
    'Identifies this constraint as one reading of kernel ''correct_latin''. Different readings assign different victim sets: continuity_reading victims = humanist scholars (imposing artificial archaism); rupture_reading victims = administrative scribes (preserving corrupt forms); hybrid_reading victims = none (both registers serve legitimate functions).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_reading_structural_delta, conceptual, 'Structural changes under alternative kernel readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hybrid_reading, 0, 250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hybrid_lat_theater_1450, hybrid_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(hybrid_lat_theater_1500, hybrid_reading, theater_ratio, 50, 0.3).
narrative_ontology:measurement(hybrid_lat_theater_1550, hybrid_reading, theater_ratio, 100, 0.35).
narrative_ontology:measurement(hybrid_lat_theater_1600, hybrid_reading, theater_ratio, 150, 0.38).
narrative_ontology:measurement(hybrid_lat_theater_1650, hybrid_reading, theater_ratio, 200, 0.42).
narrative_ontology:measurement(hybrid_lat_theater_1700, hybrid_reading, theater_ratio, 250, 0.48).

% Extraction over time
narrative_ontology:measurement(hybrid_lat_extract_1450, hybrid_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(hybrid_lat_extract_1500, hybrid_reading, base_extractiveness, 50, 0.14).
narrative_ontology:measurement(hybrid_lat_extract_1550, hybrid_reading, base_extractiveness, 100, 0.18).
narrative_ontology:measurement(hybrid_lat_extract_1600, hybrid_reading, base_extractiveness, 150, 0.2).
narrative_ontology:measurement(hybrid_lat_extract_1650, hybrid_reading, base_extractiveness, 200, 0.22).
narrative_ontology:measurement(hybrid_lat_extract_1700, hybrid_reading, base_extractiveness, 250, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hybrid_reading, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is part of the 'correct_latin' kernel family. Sibling constraints (continuity_reading, rupture_reading) model alternative readings with different beneficiary/victim structures. Network edges would link all three readings to the kernel 'correct_latin' once kernel-level modeling is implemented. Current implementation: each reading is independent constraint story with kernel_context in commentary.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
