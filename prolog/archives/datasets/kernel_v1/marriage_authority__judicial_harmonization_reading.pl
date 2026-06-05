% ============================================================================
% CONSTRAINT STORY: marriage_authority__judicial_harmonization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__judicial_harmonization_reading, []).

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
 *   constraint_id: marriage_authority__judicial_harmonization_reading
 *   human_readable: Marriage Authority: Judicial Harmonization via Constitutional Floor (Without Formal UCC)
 *   domain: legal_pluralism/constitutional_law/family_law
 *
 * SUMMARY:
 *   In legal pluralism contexts where multiple personal law codes (religious,
 *   customary, civil) govern marriage simultaneously, constitutional courts
 *   face a structural problem: how to enforce fundamental rights across
 *   jurisdictions without formal legislative harmonization? The judicial
 *   harmonization reading describes one institutional pathway where superior
 *   courts impose a constitutional floor on marriage law through case-by-case
 *   review, gradually establishing binding principles without legislative
 *   codification. This constraint exhibits the tangled_rope structure:
 *   genuine coordination function (courts resolve conflicts, establish
 *   baseline protections, prevent sub-constitutional abuse) coexists with
 *   asymmetric extraction (judiciary consolidates authority to define
 *   marriage law boundaries, personal law communities lose autonomy,
 *   legislative capacity for democratic reform is preempted or redirected).
 *   The theater_ratio (0.62) reflects that judicial review maintains formal
 *   personal law structures while functionally constraining them — the codes
 *   remain nominally operative but courts determine their actual application.
 *   The extractiveness trajectory (0.22 → 0.38 across 20 time periods) models
 *   the gradual accumulation of constitutional precedent, progressively
 *   narrowing community matrimonial discretion. The suppression metric (0.48)
 *   captures both structural barriers (personal law subjects cannot exit the
 *   system without abandoning community status) and institutional barriers
 *   (legislative reform pathways become channeled through judicial
 *   constitutional interpretation rather than democratic deliberation). This
 *   is ONE reading of the marriage_authority kernel contest — it describes
 *   the judicial mechanism rather than endorsing any of the competing
 *   normative frameworks (communal autonomy, secularist UCC, gender rights,
 *   federalist millet, or harmonization itself). The sibling readings make
 *   distinct normative claims about whether this mechanism is legitimate,
 *   desirable, or inevitable.
 *
 * KEY AGENTS:
 *   - Personal Law Communities: Primary victims (powerless/trapped) — face dual obligation to community norms and constitutional floor, with no exit available. Experience maximum extraction as courts reinterpret marriage law boundaries.
 *   - Superior Judiciary: Primary beneficiary (institutional/arbitrage) — consolidates constitutional interpretation authority over marriage law. Experiences constraint as legitimate coordination mechanism (solving conflicts) while capturing authority to define constitutional floor.
 *   - Reform-Oriented Community Members: Secondary victims-beneficiaries (moderate/constrained) — can use judicial review to advance gender equality within communities, but doing so undermines community autonomy. Face constrained exit due to identity stakes in both community membership and rights protection.
 *   - Legislative Reform Coalition: Organized actors (organized/constrained) — gender rights groups, secular civil society, reformist jurists pursuing UCC. Perceive judicial harmonization as temporary scaffold with sunset clause; experience constraint as coordination tool pending legislative codification.
 *   - Personal Law System Structure: Institutional actor (institutional/arbitrage) — multi-code marriage authority persists through inertia despite judicial erosion of its autonomy premise. Nominally operative but functionally constrained; high theater ratio reflects performative persistence.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing judicial authority consolidation as inherent constitutional necessity, missing the false summit status and identifiable beneficiaries.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__judicial_harmonization_reading, 0.38).
domain_priors:suppression_score(marriage_authority__judicial_harmonization_reading, 0.48).
domain_priors:theater_ratio(marriage_authority__judicial_harmonization_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__judicial_harmonization_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority__judicial_harmonization_reading, "Marriage Authority: Judicial Harmonization via Constitutional Floor (Without Formal UCC)").
narrative_ontology:topic_domain(marriage_authority__judicial_harmonization_reading, "legal_pluralism/constitutional_law/family_law").

domain_priors:requires_active_enforcement(marriage_authority__judicial_harmonization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__judicial_harmonization_reading, '1da31a4d-36cc-4971-b5b9-71c6cb9d1dfc').
narrative_ontology:cs_kernel_codification('1da31a4d-36cc-4971-b5b9-71c6cb9d1dfc', fixed_text).
narrative_ontology:cs_authority_grounding('1da31a4d-36cc-4971-b5b9-71c6cb9d1dfc', lineage).
narrative_ontology:cs_interpretation_layer_present('1da31a4d-36cc-4971-b5b9-71c6cb9d1dfc').
narrative_ontology:cs_reading_relation('1da31a4d-36cc-4971-b5b9-71c6cb9d1dfc', marriage_authority__communal_autonomy_reading, influences).
narrative_ontology:cs_reading_relation('1da31a4d-36cc-4971-b5b9-71c6cb9d1dfc', marriage_authority__secularist_reading, influences).
narrative_ontology:cs_reading_relation('1da31a4d-36cc-4971-b5b9-71c6cb9d1dfc', marriage_authority__gender_rights_reading, influences).
narrative_ontology:cs_reading_relation('1da31a4d-36cc-4971-b5b9-71c6cb9d1dfc', marriage_authority__federalist_millet_reading, influences).
narrative_ontology:cs_axiom('1da31a4d-36cc-4971-b5b9-71c6cb9d1dfc', foundational, constitutional_floor_inevitability).
narrative_ontology:cs_axiom_status(constitutional_floor_inevitability, holdable).
narrative_ontology:cs_axiom_grounding('1da31a4d-36cc-4971-b5b9-71c6cb9d1dfc', constitutional_floor_inevitability, instrumental).
narrative_ontology:cs_axiom('1da31a4d-36cc-4971-b5b9-71c6cb9d1dfc', foundational, uncodified_harmonization_pathway).
narrative_ontology:cs_axiom_status(uncodified_harmonization_pathway, holdable).
narrative_ontology:cs_axiom_grounding('1da31a4d-36cc-4971-b5b9-71c6cb9d1dfc', uncodified_harmonization_pathway, empirically_contingent).
narrative_ontology:cs_reference_frame('1da31a4d-36cc-4971-b5b9-71c6cb9d1dfc', plural_personal_law_codes_with_constitutional_floor).
narrative_ontology:cs_drift_state('1da31a4d-36cc-4971-b5b9-71c6cb9d1dfc', contemporary_post_constitutional_justice_reforms, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1da31a4d-36cc-4971-b5b9-71c6cb9d1dfc', '').
narrative_ontology:cs_kernel_id(marriage_authority__judicial_harmonization_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__judicial_harmonization_reading, superior_judiciary).
narrative_ontology:constraint_beneficiary(marriage_authority__judicial_harmonization_reading, constitutional_interpretation_authority).
narrative_ontology:constraint_victim(marriage_authority__judicial_harmonization_reading, personal_law_community_autonomy).
narrative_ontology:constraint_victim(marriage_authority__judicial_harmonization_reading, legislative_capacity_for_reform).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PERSONAL LAW COMMUNITIES (SNARE) — Trapped within dual obligation: must follow community marriage norms to maintain identity and social standing, while simultaneously exposed to Supreme Court constitutional reinterpretation of marriage authority. No exit pathway available. Court-imposed constitutional floor gradually eliminates community matrimonial autonomy without consent or legislative process. Full extraction experience — subject to constraint architecture they cannot modify.
constraint_indexing:constraint_classification(marriage_authority__judicial_harmonization_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REFORM-ORIENTED COMMUNITY MEMBERS (TANGLED ROPE) — Face constrained exit: can appeal to judicial harmonization for gender-equality grounds, but doing so undermines community autonomy they also value. Genuine coordination function exists (courts enforce baseline equality protections), but asymmetric extraction occurs (courts set agenda, not communities). Constrained by both tradition and litigation risk.
constraint_indexing:constraint_classification(marriage_authority__judicial_harmonization_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SUPERIOR JUDICIARY (ROPE) — Experiences constraint as coordination mechanism. Judicial review authority enables judiciary to resolve marriage law conflicts across jurisdictions, establish uniform constitutional floor, and navigate between community autonomy and individual rights. Judiciary benefits from expanded constitutional interpretation authority. Legitimate coordination function (courts solve conflicts), but also institutional capture of authority to define marriage law boundaries. Arbitrage option: can defer to legislatures or exercise expansive interpretation.
constraint_indexing:constraint_classification(marriage_authority__judicial_harmonization_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LEGISLATIVE REFORM COALITION (SCAFFOLD) — Organized actors (gender rights groups, secular civil society, reformist jurists) perceive judicial harmonization as temporary constraint with sunset clause. Strategy: accumulate constitutional precedents until legislated Uniform Civil Code becomes inevitable as codification of judiciary-established principles. Theater_ratio reflects performative aspects of incremental case-by-case review compared to direct legislation. Sunset logic: judicial harmonization pathway is explicitly temporary — destination is democratic legislative UCC.
constraint_indexing:constraint_classification(marriage_authority__judicial_harmonization_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PERSONAL LAW SYSTEM ITSELF (PITON) — The multi-code marriage authority structure persists through institutional inertia despite judicial harmonization eroding its foundational premise (community autonomy). The system is degraded — courts apply personal law codes while simultaneously imposing constitutional floors that override them. Theater_ratio high: codes remain formally operative but functionally constrained. Maintenance through habit, constitutional formalism, and political difficulty of formal abolition. The piton status reflects that judicial review is increasingly performative relative to the outcome (constitutional floor will ultimately determine marriage law).
constraint_indexing:constraint_classification(marriage_authority__judicial_harmonization_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational view, judicial review of fundamental rights (including marriage equality and consent) is an inherent institutional necessity in any modern constitutional order. The constitutional floor is presented as immutable architectural requirement, not contingent institutional choice. However, the structural data contradicts this — specific beneficiaries (judiciary), identifiable victims (community autonomy), and measurable extraction reveal this as a false summit naturalizing institutional power consolidation as constitutional inevitability.
constraint_indexing:constraint_classification(marriage_authority__judicial_harmonization_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__judicial_harmonization_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(marriage_authority__judicial_harmonization_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(marriage_authority__judicial_harmonization_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(marriage_authority__judicial_harmonization_reading, TR),
    TR >= 0.70.

:- end_tests(marriage_authority__judicial_harmonization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. Judicial harmonization does extract authority from personal law communities and legislative processes, but the extraction is partial and justified by genuine coordination problems (rights protection, conflict resolution across multiple codes, preventing abuse). The trajectory from 0.22 to 0.38 reflects gradual accumulation of precedent that narrows community discretion without formal legal change. Suppression (0.48): Moderate-high. Personal law subjects face substantial barriers to exit (community identity loss, social sanctions, religious/cultural marginalization) and cannot collectively resist court-imposed constitutional reinterpretation. Legislative actors face barriers to direct democratic reform (judicial preemption, constitutional constraints on legislative scope). Theater ratio (0.62): Moderate-high. Personal law codes formally remain operative, but constitutional review progressively determines their actual application. The performative element increases over time (0.48 → 0.62) as courts establish precedent while maintaining the fiction of community code authority. The constraint approaches Piton-like behavior at the system level — the personal law structure persists through institutional inertia despite being functionally eroded by judicial authority.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival divergence is extreme: from Snare (powerless community) to Rope (institutional judiciary) to Tangled Rope (moderate reformer) to Scaffold (organized coalition) to Piton (system-level) to Mountain (analytical false summit). This range reflects that the same institutional mechanism produces radically different constraint experiences depending on position. The gap reveals that neither single-perspective analysis nor averaging perspectives captures the structure — the presheaf over observation sites IS the analytical object.
 *
 * DIRECTIONALITY LOGIC:
 *   The judicial harmonization reading positions courts as beneficiaries (institutional/arbitrage exit: they can choose whether to exercise expansive review or defer to legislatures) and personal law communities as victims (trapped exit: they cannot exit the dual-obligation structure without abandoning community status). The derived directionality d for communities is high (0.85+), producing high experienced extractiveness chi; the derived d for judiciary is low (0.15–0.25), producing low or negative chi. Reform-oriented members occupy intermediate position (constrained exit: high personal cost to voice rights claims but possible), producing mid-range d and mid-range chi. The override values do not require adjustment — the structural derivation captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves the mandatrophy by distinguishing institutional mechanism from normative position. The reading describes HOW marriage authority evolves (case-by-case judicial harmonization imposing constitutional floor) WITHOUT claiming WHETHER this is legitimate, inevitable, or desirable. The sibling readings each make distinct normative claims: communal autonomy argues courts should not override community norms; secularist argues this pathway is transitional toward UCC; gender rights argues it enables equality protection; federalist argues pluralism is deliberately anti-majoritarian; this reading describes the mechanism all must navigate. The mandatrophy dissolves because the question 'which normative reading is correct?' is decoupled from 'how does the institutional mechanism actually operate?' The reading avoids the trap of naturalizing (mountain false summit) or dismissing (piton reductionism) by mapping the actual perspectival landscape: the mechanism produces coordination benefits (judiciary solves conflicts) alongside extraction (judiciary captures authority) in proportions that depend on position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_floor_scope_ambiguity,
    'How far does the constitutional floor extend? Minimal floor (consent, basic equality) vs. maximal floor (secular civil procedure, inheritance equality, spousal property rights)?',
    'Longitudinal analysis of Supreme Court marriage decisions; identification of explicit vs. implicit scope boundaries; comparison with legislated UCC proposals',
    'Minimal floor: personal law communities retain significant autonomy, constraint approaches Rope classification. Maximal floor: personal law structure is functionally dissolved, constraint approaches Snare from community perspective. Theater increases as scope expands without formal legal change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_floor_scope_ambiguity, conceptual, 'Ambiguity about how expansively constitutional floor applies to marriage law').

omega_variable(
    judicial_vs_legislative_temporality,
    'Is case-by-case judicial harmonization actually a temporary scaffold toward UCC legislation, or a structural substitution for legislation that will never occur?',
    'Historical tracking: legislative progress toward formal UCC; analysis of political economy blocking formal codification; comparative study of similar legal pluralism contexts (whether they terminate in codification or perpetual judicial harmonization)',
    'If temporary scaffold: this reading''s sunset clause is real, constraint approaches Rope as consensus on constitutional principles emerges. If permanent substitution: judicial harmonization is an extractive mechanism disguised as temporary coordination, constraint drifts toward Snare. Theater ratio trajectory is diagnostic — if theater increases indefinitely, sunset was illusory.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_vs_legislative_temporality, empirical, 'Whether judicial harmonization is temporary or permanent substitute for legislation').

omega_variable(
    community_capture_vs_exit,
    'Can communities effectively preserve internal autonomy by adapting personal law codes to meet constitutional floor requirements? Or does constitutional review eliminate meaningful community discretion even when formal codes remain?',
    'Ethnographic and doctrinal analysis of community marriage norm adaptation following major constitutional decisions; measurement of actual (vs. nominal) discretion remaining to communities post-decision',
    'If communities can adapt: constraint is coordination (Rope from community perspective). If constitutional floor fully determines outcomes: constraint is extraction (Snare) disguised by nominal community code authority. Suppression metric rises if communities perceive decisions as imposed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(community_capture_vs_exit, empirical, 'Whether communities retain meaningful autonomy within constitutional floor constraints').

omega_variable(
    reading_identity_ambiguity,
    'Is this reading describing a normative position about how marriage authority SHOULD be governed (like the sibling readings), or is it describing an institutional mechanism that is independent of normative commitments?',
    'Analysis of the reading''s grounding: does it claim that judicial harmonization is legitimate BECAUSE it achieves constitutional principles (normative), or does it describe judicial harmonization as an institutional FACT regardless of normative legitimacy (descriptive)? The source material indicates mechanism rather than normative stance.',
    'If normative: this reading forecloses or coexists-with sibling readings depending on their axioms. If descriptive: this reading influences all siblings by describing the institutional pathway none can avoid. This ambiguity routes to the committer structure: whether this reading is a distinct normative claim or a structural description of how any normative claim must be instantiated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_identity_ambiguity, conceptual, 'Whether judicial harmonization is a normative position or institutional mechanism independent of normativity').

omega_variable(
    false_summit_naturalness,
    'Is the constitutional floor an inherent feature of rights-protecting constitutional order (mountain), or a contingent institutional power consolidation by the judiciary (snare/tangled_rope)?',
    'Comparative constitutional law: do all modern democracies require judicial review of marriage law? Do alternative institutional arrangements (direct legislative codification, community-legislature partnerships, federal-subfederal negotiation) produce worse rights protection outcomes? Historical analysis of whether the present judicial review pathway was inevitable or chosen.',
    'If inherent: mountain classification is correct. If contingent: false summit triggered, constraint reclassified via signature_detection.pl to tangled_rope or snare. This omega documents the FSM candidate status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_naturalness, empirical, 'Whether constitutional review of marriage is inherent necessity or contingent institutional choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__judicial_harmonization_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority__judicial_harmonization_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(marr_tr_t10, marriage_authority__judicial_harmonization_reading, theater_ratio, 10, 0.58).
narrative_ontology:measurement(marr_tr_t20, marriage_authority__judicial_harmonization_reading, theater_ratio, 20, 0.62).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority__judicial_harmonization_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(marr_be_t10, marriage_authority__judicial_harmonization_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(marr_be_t20, marriage_authority__judicial_harmonization_reading, base_extractiveness, 20, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority__judicial_harmonization_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(marr_su_t10, marriage_authority__judicial_harmonization_reading, suppression_requirement, 10, 0.44).
narrative_ontology:measurement(marr_su_t20, marriage_authority__judicial_harmonization_reading, suppression_requirement, 20, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__judicial_harmonization_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, marriage_authority__communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, marriage_authority__secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, marriage_authority__gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, marriage_authority__federalist_millet_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a contested kernel (marriage_authority kernel). All five readings are instantiated as separate constraints, each with their own ε values, beneficiary/victim declarations, and perspective landscapes. The readings share a kernel (the persistent marriage authority problem across legal pluralism) but diverge on normative claims and institutional mechanisms. The judicial_harmonization_reading is unique because it describes mechanism rather than endorsing a normative position — it thus influences all other readings by establishing the institutional pathway all must navigate. Network edges link all five readings; the judicial_harmonization_reading is upstream in that it describes how any normative reading (communal, secularist, gender_rights, federalist) must be institutionally implemented or resisted.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
