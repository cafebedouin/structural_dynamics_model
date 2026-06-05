% ============================================================================
% CONSTRAINT STORY: maliki_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maliki_reading, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: maliki_reading
 *   human_readable: Maliki School Authority: Medinan Practice as Living Sunnah
 *   domain: islamic_jurisprudence/legal_philosophy/commitment_systems
 *
 * SUMMARY:
 *   The Maliki school's elevation of Medinan practice ('amal ahl al-Madina)
 *   as authoritative embodiment of Sunnah and recognition of local custom
 *   ('urf) as a valid law source instantiate a distinctive jurisprudential
 *   reading within Islamic legal philosophy. This reading privileges
 *   geographic location and living tradition over isolated textual authority,
 *   creating a structural constraint that benefits the Medinan scholarly
 *   lineage while extracting deference from non-Medinan jurists. The
 *   constraint exhibits genuine coordination function (coherent methodology
 *   for resolving disputes across dispersed Islamic communities) layered with
 *   asymmetric extraction (geographic privilege that advantages Medinan
 *   authorities). The key distinguishing feature from sibling jurisprudential
 *   readings (Hanafi, Shafi'i, Hanbali) is the relative weight given to
 *   Medinan consensus and the willingness to treat evolving community
 *   practice as authoritative law source. This reading resolves into a
 *   tangled rope: coordination of Islamic jurisprudence across time and space
 *   requires some methodological standard, and the Maliki choice of Medinan
 *   practice provides that standard; simultaneously, the choice privileges
 *   one geographic location's reasoning and constrains other jurists'
 *   interpretive authority. The beneficiary is the Medinan scholarly lineage
 *   and, by extension, the institutional Maliki school; the victim is any
 *   non-Medinan jurist whose reasoning, no matter how rigorous, lacks the
 *   geographic authority that Medina confers.
 *
 * KEY AGENTS:
 *   - Medinan Scholarly Lineage: Primary beneficiary (institutional/arbitrage) — their juridical reasoning carries geographic authority; their interpretation of Medinan practice becomes binding precedent
 *   - Non-Medinan Jurists: Primary victim (powerless/identity_locked or moderate/constrained depending on time horizon) — their reasoning must defer to Medinan authority or be classified as inferior methodology
 *   - Maliki School as Institutional Actor: Institutional beneficiary (institutional/constrained) — coordinates jurisprudence across regions while maintaining dependence on Medinan privilege for legitimacy
 *   - Islamic Jurisprudential Communities (Maghreb, al-Andalus, Egypt): Moderate agents (moderate/constrained) — benefit from Maliki institutional structure and scholarly authority while bearing extraction through methodological constraints
 *   - Contemporary Reform Movements: Organized agents (organized/mobile) — recognize Maliki method as historically functional but see it as inert in contemporary contexts; exercising exit via maqasid-based reasoning and historical contextualization
 *   - Analytical Observer: Universal perspective (analytical/analytical) — sees the reading as a historically-contingent institutional design that solved medieval coordination problems while structurally privileging one geographic center
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maliki_reading, 0.38).
domain_priors:suppression_score(maliki_reading, 0.42).
domain_priors:theater_ratio(maliki_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maliki_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(maliki_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(maliki_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maliki_reading, tangled_rope).
narrative_ontology:human_readable(maliki_reading, "Maliki School Authority: Medinan Practice as Living Sunnah").
narrative_ontology:topic_domain(maliki_reading, "islamic_jurisprudence/legal_philosophy/commitment_systems").

domain_priors:requires_active_enforcement(maliki_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maliki_reading, '64958c2e-84b2-43eb-b1b0-4a7d34554bad').
narrative_ontology:cs_created_at('64958c2e-84b2-43eb-b1b0-4a7d34554bad', '').
narrative_ontology:cs_kernel_codification('64958c2e-84b2-43eb-b1b0-4a7d34554bad', fixed_text).
narrative_ontology:cs_authority_grounding('64958c2e-84b2-43eb-b1b0-4a7d34554bad', lineage).
narrative_ontology:cs_interpretation_layer_present('64958c2e-84b2-43eb-b1b0-4a7d34554bad').
narrative_ontology:cs_kernel_id(maliki_reading, jurisprudential_method_kernel).
narrative_ontology:cs_reading_relation('64958c2e-84b2-43eb-b1b0-4a7d34554bad', hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('64958c2e-84b2-43eb-b1b0-4a7d34554bad', shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('64958c2e-84b2-43eb-b1b0-4a7d34554bad', hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('64958c2e-84b2-43eb-b1b0-4a7d34554bad', foundational, medinan_practice_epistemically_privileged).
narrative_ontology:cs_axiom_status(medinan_practice_epistemically_privileged, holdable).
narrative_ontology:cs_axiom('64958c2e-84b2-43eb-b1b0-4a7d34554bad', foundational, living_tradition_as_law_source).
narrative_ontology:cs_axiom_status(living_tradition_as_law_source, holdable).
narrative_ontology:cs_reference_frame('64958c2e-84b2-43eb-b1b0-4a7d34554bad', medinan_scholarly_consensus).
narrative_ontology:cs_drift_state('64958c2e-84b2-43eb-b1b0-4a7d34554bad', contemporary_legal_pluralism, gap(authority_erosion, substantial, true)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maliki_reading, medinan_scholarly_lineage).
narrative_ontology:constraint_victim(maliki_reading, non_medinan_jurists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-MEDINAN JURIST (SNARE) — Structurally mobile in principle (could adopt alternative methodologies from other schools) but identity-locked within the Maliki framework. Their professional identity, teaching lineage, and epistemic authority are constituted through acceptance of Maliki axioms. Exit would require abandoning not just a legal methodology but a scholarly tradition and communal belonging. The constraint extracts deference to Medinan precedent while offering no countervailing authority for non-Medinan juristic reasoning.
constraint_indexing:constraint_classification(maliki_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 2: REGIONAL JURIST IN MAGHREB OR AL-ANDALUS (TANGLED ROPE) — Benefits from Maliki dominance as the de facto jurisprudential standard across these territories: career legitimacy, judicial appointments, scholarly authority, and institutional backing flow through Maliki credentials. Simultaneously bears extraction through methodological constraints that privilege Medinan precedent over locally-grounded reasoning. Exit is costly (loss of institutional position, scholarly standing) but possible (adoption of Hanafi or Hanbali methods exists as an alternative, though rare).
constraint_indexing:constraint_classification(maliki_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: MEDINAN SCHOLARLY LINEAGE (ROPE) — Primary beneficiary. Experiences the constraint as a coordination mechanism: Maliki methodology legitimates their interpretive authority, attracts students seeking authoritative legal reasoning, and establishes their precedent as binding across geographically dispersed communities. The geographic privilege creates arbitrage: they can exit Maliki authority entirely and their standing persists; non-Medinan jurists cannot. Net beneficiary — deference flows toward them.
constraint_indexing:constraint_classification(maliki_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: MALIKI SCHOOL AS INSTITUTION (TANGLED ROPE) — The school itself coordinates legal reasoning across diverse territories and time horizons (genuine coordination function) while extracting conformity to Medinan interpretive precedent (asymmetric constraint). The school's authority depends on maintaining the fiction of Medinan consensus; this dependence constrains its own evolution and limits its ability to incorporate new juridical reasoning without threatening its legitimacy. Enforcement mechanisms (sectarian scholarly consensus, judicial appointment gatekeeping) are active and necessary.
constraint_indexing:constraint_classification(maliki_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CONTEMPORARY ISLAMIC REFORM MOVEMENT (PITON) — Organized actors (modernist scholars, reformers, comparative jurisprudence movements) recognize the Maliki method as historically functional but largely inert in contemporary contexts. Theater ratio (0.35) reflects that the constraint persists through institutional inertia and legitimacy claims about Medinan authenticity rather than because the Medinan-privilege mechanism solves real contemporary coordination problems. Reform movements have exit options (comparative jurisprudence, maqasid-based reasoning, historical contextualization) and are actively exercising them. The constraint persists not from function but from authority structure and scholarly tradition.
constraint_indexing:constraint_classification(maliki_reading, piton,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From the civilizational/universal perspective, the Maliki reading instantiates a genuine coordination solution (how do dispersed communities maintain interpretive coherence and legal authority?) layered with an extraction mechanism (privileging one geographic location's practice as epistemically superior). The reading is neither a natural law nor pure extraction but a historically-contingent institutional design that coordinated medieval Islamic jurisprudence while extracting deference from non-Medinan authorities.
constraint_indexing:constraint_classification(maliki_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maliki_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(maliki_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(maliki_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(maliki_reading, TR),
    TR >= 0.70.

:- end_tests(maliki_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The Maliki reading coordinates Islamic jurisprudence across dispersed communities — a genuine coordination function that reduces uncertainty about legal authority and provides coherent dispute resolution. The extraction component comes from privileging Medinan practice: this choice benefits Medinan authorities disproportionately and constrains non-Medinan jurists' ability to claim equal interpretive standing. The value reflects the balance between the real coordination benefit and the real but not total extraction of deference. Temporal trend (0.28 → 0.38 → 0.42): Extractiveness has increased over the interval as the Maliki school solidified institutional dominance and later as contemporary reform pressures created tension with tradition. The increase reflects accumulating institutional investment in maintaining Medinan privilege and rising resistance from non-Medinan jurisdictions seeking interpretive autonomy. Suppression (0.42): Moderate. Barriers to exit include institutional gatekeeping (judicial appointments, scholarly credentialing), epistemic isolation within Maliki intellectual frameworks, and identity fusion (scholars' professional identity constituted through Maliki training). Suppression is not total — non-Medinan schools exist as live alternatives, and some contemporary scholars are adopting comparative or maqasid-based methods. Theater ratio (0.35): Low-moderate. The Maliki reading retains genuine functional content — it solves coordination problems and provides coherent methodology — but over time an increasing fraction of the constraint operates through the ritual of citing Medinan precedent rather than through the substance of Medinan reasoning. Contemporary scholars often reinterpret Medinan practice to align with contemporary reasoning, suggesting the constraint functions increasingly as a form to be satisfied rather than as a methodological principle to be applied.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is extreme and revealing. The Medinan scholarly lineage sees the constraint as pure coordination (Rope) — their geographic position legitimates their reasoning and attracts followers and institutional backing. Non-Medinan jurists at the powerless/identity-locked level see extraction with no exit (Snare) — they cannot coherently abandon the Maliki framework without ceasing to be Maliki jurists, yet the framework structurally privileges Medinan reasoning. Non-Medinan jurists at the moderate/constrained level see mixed coordination and extraction (Tangled Rope) — the system provides real authority and institutional position while constraining their independent reasoning. The Maliki school as institutional actor sees itself as coordinator (Tangled Rope) — it must maintain Medinan privilege to sustain its legitimacy while managing tension with non-Medinan territories seeking interpretive autonomy. Contemporary reform movements see institutional inertia (Piton) — the theater has increased (0.35) as the constraint operates more through ritual invocation of Medinan authority than through substantive Medinan reasoning. The analytical observer sees a historically-contingent institutional design (Tangled Rope) — real coordination benefit mixed with real extraction asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality computation for this reading works as follows: The Medinan scholarly lineage, as primary beneficiary with arbitrage exit options, derives low d (approximately 0.10-0.15), producing negative or near-zero χ — they experience the constraint as net benefit without extraction cost. Non-Medinan jurists operate at two distinct directionality levels depending on time horizon and identity status. At the biographical horizon with identity_locked exit, they derive high d (approximately 0.80-0.85), producing high χ — they experience maximum extraction because exit would require identity abandonment. At the generational horizon with constrained exit, they derive moderate d (approximately 0.55-0.65), producing moderate χ — exit is possible but costly. The Maliki school as institutional actor with constrained exit (cannot fully abandon Medinan privilege without losing legitimacy) derives moderate d (approximately 0.45-0.55). These directionality differences are not noise — they reflect real structural differences in how different agents experience the constraint. An agent with arbitrage exit experiences a benefit flow; an agent with identity_locked exit experiences maximal extraction; an agent with constrained exit experiences mixed extraction and benefit. The perspectival gap IS the directionality gap.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves mandatrophy by clarifying what the constraint actually coordinates versus what it extracts. Classical Maliki theory claims that the constraint ONLY coordinates: Medinan practice establishes reliable knowledge of Sunnah, and adherence to it ensures fidelity to prophetic tradition. The mandatrophy appears when we examine who benefits and who bears costs. If Medinan practice truly is the sole reliable source of Sunnah, non-Medinan jurists should expect no extraction cost — following superior authority should feel like access to better information, not like deference to geographic privilege. But the structural data shows extraction: non-Medinan jurists experience the constraint as limiting their reasoning authority, and the bifurcated perspectives (Snare for identity-locked agents, Tangled Rope for constrained agents) reveal the asymmetry. The reading survives mandatrophy analysis as a genuine Tangled Rope: the coordination function is real, but so is the extraction. The geographic privilege that enables coordination also enables extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    medinan_consensus_empirical_status,
    'Did Medinan jurisprudential practice in the second and third Islamic centuries actually constitute a unified, documentable consensus, or is the ''amal ahl al-Madina'' a retrospective scholarly construction?',
    'Comparative analysis of earliest hadith collections, Medinan legal treatises (Ibn Sahnun, Sahnun), and Malik''s Muwatta against later compilations; investigation of which practices are actually attested in early sources vs. which are attributed to Medina by later jurists',
    'If empirically unified: Maliki authority rests on documented communal practice (coordinate legitimacy, reduces extraction component). If retrospectively constructed: the geographic privilege is narrativized authority without evidence base (increases extraction component, suggests reclassification toward snare or piton).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medinan_consensus_empirical_status, empirical, 'Whether Medinan scholarly consensus was historically unified or retrospectively constructed').

omega_variable(
    alternative_jurisprudential_coherence,
    'Could non-Medinan juristic reasoning (Hanafi, Shafi''i, Hanbali methods) have achieved equivalent or superior coordination of Islamic law across dispersed communities without geographic privilege?',
    'Historical counterfactual analysis: examination of how Hanafi jurisprudence achieved coherence across Anatolia, Central Asia, and the Ottoman Empire WITHOUT privileging a single geographic center; comparison of dispute resolution effectiveness, territorial coverage, institutional stability across schools',
    'If alternative methods achieved equivalent coordination: Medinan privilege appears as artificial extraction rather than necessary coordination (reclassification toward snare). If Medinan privilege uniquely enabled coordination: extraction component is justified as necessary cost of coordination mechanism (remains tangled_rope or escalates to rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_jurisprudential_coherence, empirical, 'Whether alternative jurisprudential methods could achieve coordination without geographic privilege').

omega_variable(
    reading_identity_and_lineage_fusion,
    'To what extent is the Maliki reading instantiated because the Maliki school genuinely endorses it versus because scholars derive their professional identity and authority from Maliki credentials and cannot coherently abandon the reading without becoming non-Maliki?',
    'Ethnographic and historiographic analysis: examination of Maliki-trained scholars who adopt maqasid-based reasoning or historical contextualization while claiming Maliki identity; documentation of whether they reframe the Medinan privilege (reinterpret it to be consistent with contemporary reasoning) or explicitly abandon it; assessment of institutional and social costs they face',
    'If reading is actively endorsed across generations: institutional commitment is genuine. If reading persists primarily through identity fusion: the constraint operates largely through identity_locked mechanisms; this reading is closer to a snare for non-Medinan jurists at biographical horizon and closer to a piton at civilizational horizon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_and_lineage_fusion, empirical, 'Whether Maliki reading is actively endorsed or persists through identity fusion').

omega_variable(
    sibling_reading_mutual_exclusion,
    'At the kernel level (jurisprudential method), do the Maliki, Hanafi, Shafi''i, and Hanbali readings logically foreclose one another, or can a single framework accommodate multiple methods as equally valid readings of the same jurisprudential kernel?',
    'Analysis of classical and contemporary jurisprudential theory: examination of claims by each school about whether other schools'' methods are valid vs. invalid; investigation of whether Orthodox Islam permits talfiq (mixing methods) or demands methodological fidelity; assessment of whether the kernel is ''what is valid jurisprudential method?'' (readings would coexist) or ''what is THE valid method?'' (readings would foreclose)',
    'If kernel admits multiple methods as equally valid: readings coexist_with one another; no foreclosure. If each reading claims exclusive methodological validity: readings foreclose one another; this reading logically rules out others within a single institutional framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_mutual_exclusion, conceptual, 'Whether jurisprudential methods logically foreclose one another or can coexist').

omega_variable(
    axiom_geographic_privilege_normativity,
    'Is the normative claim ''Medinan practice is more authoritative than non-Medinan practice because of geographic origin'' itself contested within Islamic legal philosophy, or is it a foundational premise all schools accept (disagreeing only on extent)?',
    'Comparative jurisprudential analysis: examination of whether Hanafi, Shafi''i, and Hanbali sources explicitly reject geographic privilege as a valid criterion (axiom foreclosed) or accept it but weight it differently (axiom holdable but contested), or ignore it entirely (axiom inoperative in those frameworks)',
    'If rejected by sibling schools: axiom is foreclosed by those traditions; this reading''s foundational claim is distinctive but not universally held. If accepted but weighted differently: axiom is holdable across schools but with different magnitudes of deference. If ignored: axiom is inoperative in alternative frameworks (suggests the readings instantiate genuinely different kernels, not different readings of the same kernel).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(axiom_geographic_privilege_normativity, conceptual, 'Whether geographic privilege is a contested or foundational axiom').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maliki_reading, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(malik_tr_t0, maliki_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(malik_tr_t200, maliki_reading, theater_ratio, 200, 0.28).
narrative_ontology:measurement(malik_tr_t400, maliki_reading, theater_ratio, 400, 0.35).

% Extraction over time
narrative_ontology:measurement(malik_be_t0, maliki_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(malik_be_t200, maliki_reading, base_extractiveness, 200, 0.38).
narrative_ontology:measurement(malik_be_t400, maliki_reading, base_extractiveness, 400, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maliki_reading, identity_coordination).
narrative_ontology:affects_constraint(maliki_reading, hanafi_reading).
narrative_ontology:affects_constraint(maliki_reading, shafii_reading).
narrative_ontology:affects_constraint(maliki_reading, hanbali_reading).

% DUAL FORMULATION NOTE:
% The jurisprudential_method_kernel is instantiated as four separate constraint stories, one for each major school's reading. Each story has its own epsilon value reflecting the school's actual empirical status and institutional dominance at specific times and places. The Maliki reading (this story) has ε=0.38 reflecting moderate extraction in regions where Maliki authority is dominant. The Hanafi reading would have different ε reflecting its different geographic and institutional distribution. The stories are linked via affects_constraints to model how jurisprudential competition shapes each school's institutional position.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(maliki_reading, institutional, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
