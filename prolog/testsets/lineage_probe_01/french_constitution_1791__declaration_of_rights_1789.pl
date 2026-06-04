% ============================================================================
% CONSTRAINT STORY: french_constitution_1791__declaration_of_rights_1789
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_french_constitution_1791__declaration_of_rights_1789, []).

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
 *   constraint_id: french_constitution_1791__declaration_of_rights_1789
 *   human_readable: Declaration of Rights (1789) Prefixed to the 1791 Constitution: Universal Rights vs. Feudal Extraction
 *   domain: political/legal/historical
 *
 * SUMMARY:
 *   The 1791 Constitution of France opens with the Declaration of the Rights
 *   of Man and of the Citizen (proclaimed August 26, 1789) prefixed as its
 *   foundational justification. This reading instantiates the Declaration as
 *   the Constitutional kernel's core claim: that universal rights of man and
 *   citizen are the reason for the Constitution's existence and the measure
 *   of its legitimacy. The Declaration suppresses the legal premises of the
 *   ancien régime wholesale — feudal privilege, hereditary order, arbitrary
 *   royal authority — by asserting that all men are born free and equal in
 *   rights. The structural delta this reading produces: suppression of
 *   feudalism's legal framework (feudal dues declared void, hereditary
 *   privilege nullified); beneficiary is the abstract rights-bearing citizen
 *   and concretely the bourgeoisie whose property rights and commercial
 *   freedom the Constitution protects; victim set is feudal privilege holders
 *   (nobles) and peasantry (whose extraction shifts from feudal lords to the
 *   state through new taxation and revolutionary expropriation). The
 *   extractiveness reflects the constraint's hybrid nature: genuine
 *   coordination function (creation of uniform law, abolition of feudal
 *   fragmentation, property protection enabling commerce) coexists with
 *   asymmetric extraction (state consolidates fiscal power, property owners
 *   dominate legal protections, state expropriation of ecclesiastical and
 *   emigré property). This reading differs from sibling readings: the
 *   active/passive citizenship reading emphasizes that universality is
 *   means-tested at the ballot; the monarchical reading emphasizes that the
 *   king retains power despite the Declaration; the failure reading
 *   emphasizes that the Constitution collapses within a year; the suspension
 *   reading emphasizes the veto's limitation on popular sovereignty. This
 *   reading isolates the Declaration's claim as foundational — the
 *   rights-universality assertion upon which the Constitution claims to rest.
 *
 * KEY AGENTS:
 *   - Rights-bearing citizen (abstract): Nominal beneficiary invoked by the Declaration as reason for Constitution's existence; the universal subject whose rights are declared inherent
 *   - Peasantry (feudal dues payers): Primary victim in ancien régime; nominally freed by Declaration but extraction continues through landlord resistance and state expropriation
 *   - Feudal nobility (privilege holders): Structural victim of Declaration's suppression of hereditary order and feudal dues; loses extraction mechanism though retains property
 *   - Bourgeoisie (property owners): Primary concrete beneficiary; benefits from property protection, uniform law, and commercial freedom; controls interpretation of 'universal rights'
 *   - Revolutionary government (Constituent Assembly): Authority enforcer; maintains Declaration's universalism while coordinating transition, suppressing counter-revolution, and consolidating state extraction
 *   - Royalist opposition: Institutional voice of ancien régime; reads Declaration as performative theatre masking revolutionary seizure of legitimate authority
 *   - Analytical observer: Civilizational perspective that risks naturalizing the Declaration as discovery of immutable law rather than constructed political arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(french_constitution_1791__declaration_of_rights_1789, 0.58).
domain_priors:suppression_score(french_constitution_1791__declaration_of_rights_1789, 0.72).
domain_priors:theater_ratio(french_constitution_1791__declaration_of_rights_1789, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(french_constitution_1791__declaration_of_rights_1789, extractiveness, 0.58).
narrative_ontology:constraint_metric(french_constitution_1791__declaration_of_rights_1789, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(french_constitution_1791__declaration_of_rights_1789, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(french_constitution_1791__declaration_of_rights_1789, tangled_rope).
narrative_ontology:human_readable(french_constitution_1791__declaration_of_rights_1789, "Declaration of Rights (1789) Prefixed to the 1791 Constitution: Universal Rights vs. Feudal Extraction").
narrative_ontology:topic_domain(french_constitution_1791__declaration_of_rights_1789, "political/legal/historical").

domain_priors:requires_active_enforcement(french_constitution_1791__declaration_of_rights_1789).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(french_constitution_1791__declaration_of_rights_1789, '3176f23f-7f91-4a6a-b2cb-21a746700430').
narrative_ontology:cs_kernel_codification('3176f23f-7f91-4a6a-b2cb-21a746700430', formalized).
narrative_ontology:cs_authority_grounding('3176f23f-7f91-4a6a-b2cb-21a746700430', lineage).
narrative_ontology:cs_interpretation_layer_present('3176f23f-7f91-4a6a-b2cb-21a746700430').
narrative_ontology:cs_reading_relation('3176f23f-7f91-4a6a-b2cb-21a746700430', french_constitution_1791__active_passive_citizenship, coexists_with).
narrative_ontology:cs_reading_relation('3176f23f-7f91-4a6a-b2cb-21a746700430', french_constitution_1791__suspensive_veto_monarchy, coexists_with).
narrative_ontology:cs_reading_relation('3176f23f-7f91-4a6a-b2cb-21a746700430', french_constitution_1791__failure_and_succession, influences).
narrative_ontology:cs_axiom('3176f23f-7f91-4a6a-b2cb-21a746700430', foundational, universal_rights_inherent_to_humanity).
narrative_ontology:cs_axiom_status(universal_rights_inherent_to_humanity, holdable).
narrative_ontology:cs_axiom_grounding('3176f23f-7f91-4a6a-b2cb-21a746700430', universal_rights_inherent_to_humanity, deontological).
narrative_ontology:cs_axiom('3176f23f-7f91-4a6a-b2cb-21a746700430', foundational, feudal_extraction_mechanism_incompatible_with_universal_rights).
narrative_ontology:cs_axiom_status(feudal_extraction_mechanism_incompatible_with_universal_rights, holdable).
narrative_ontology:cs_axiom_grounding('3176f23f-7f91-4a6a-b2cb-21a746700430', feudal_extraction_mechanism_incompatible_with_universal_rights, deontological).
narrative_ontology:cs_reference_frame('3176f23f-7f91-4a6a-b2cb-21a746700430', universal_rights_foundation_state).
narrative_ontology:cs_drift_state('3176f23f-7f91-4a6a-b2cb-21a746700430', constitutional_implementation_crisis, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3176f23f-7f91-4a6a-b2cb-21a746700430', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(french_constitution_1791__declaration_of_rights_1789, french_constitution_1791).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(french_constitution_1791__declaration_of_rights_1789, rights_bearing_citizen_abstract).
narrative_ontology:constraint_beneficiary(french_constitution_1791__declaration_of_rights_1789, bourgeoisie_property_owners).
narrative_ontology:constraint_victim(french_constitution_1791__declaration_of_rights_1789, feudal_privilege_order).
narrative_ontology:constraint_victim(french_constitution_1791__declaration_of_rights_1789, peasantry_labor_extraction).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PEASANTRY UNDER FEUDAL REGIME (SNARE) — Trapped by hereditary obligation to pay feudal dues and corvée labor. The Declaration nominally abolishes this extraction but enforcement is incomplete; landlords resist and some lords continue extraction through violence and institutional inertia. The peasant experiences the constraint as high suppression (can neither exit feudal obligation nor enforce the Declaration's promise) and high extractiveness (dues and labor flows continue despite nominal abolition).
constraint_indexing:constraint_classification(french_constitution_1791__declaration_of_rights_1789, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LANDED ARISTOCRACY (TANGLED ROPE) — Constrained by legal suppression of feudal rights (Declaration nominally voids feudal extraction) but benefits from coordination function: the Constitution preserves property ownership, inheritance rights, and legal personhood. The aristocracy experiences genuine loss (feudal dues revenue eliminated) but also genuine coordination benefit (constitutional property protections, rule of law replacing arbitrary royal seizure). Mixed extraction and coordination — suppression is high because feudal extraction is legally prohibited, but the constraint also provides institutional stability benefiting property owners.
constraint_indexing:constraint_classification(french_constitution_1791__declaration_of_rights_1789, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: BOURGEOISIE / PROPERTY-OWNING CLASS (ROPE) — Mobile and powerful; experiences the constraint primarily as coordination. The Declaration and Constitution establish property rights, enforce contracts, create uniform law replacing feudal fragmentation, and guarantee personal liberty essential to commerce and manufacture. The bourgeoisie sees this as a coordination mechanism solving collective action problems (fragmented feudal jurisdictions, arbitrary noble seizure, lack of contract enforcement). Net beneficiary through institutional stability, not extraction.
constraint_indexing:constraint_classification(french_constitution_1791__declaration_of_rights_1789, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: REVOLUTIONARY GOVERNMENT (TANGLED ROPE) — Constrained by competing legitimacy claims: the Declaration as normative foundation vs. the need to maintain institutional order, property protection, and fiscal stability. The government coordinates the transition from feudalism to constitutional law (genuine coordination function) but also extracts through: land confiscation of ecclesiastical property, new taxation replacing feudal dues, and suppression of counter-revolutionary resistance. High suppression (revolutionary terror, enforcement machinery) and high extractiveness (fiscal extraction continues, now consolidated in the state) alongside genuine coordination of the new legal order.
constraint_indexing:constraint_classification(french_constitution_1791__declaration_of_rights_1789, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ROYALIST OPPOSITION / ANCIEN RÉGIME LOYALISTS (PITON) — Views the Declaration as a performative theatre masking the Revolution's destruction of legitimate authority. The Royalist reading treats the Declaration's universalism as propaganda whose functional effect is to dismantle the old order's legal premises without providing stable replacement. Theater ratio high: the Declaration performs universality while the Constitution retains property privilege, capital-qualified citizenship, and monarchy (albeit weakened). The constraint appears to Royalists as a degraded institutional form — it claims to be universal law but functions as revolutionary seizure of power.
constraint_indexing:constraint_classification(french_constitution_1791__declaration_of_rights_1789, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational distance, the Declaration appears as the discovery or instantiation of immutable human rights — a natural law that feudalism had merely suppressed but could not fundamentally alter. This perspective reads the Declaration as emergent from the logic of reason itself, not as a constructed institutional choice. Rights are treated as inherent to the human condition, accessible through rational analysis, and thus impossible to truly suppress (though they can be temporarily denied). However, this reading naturalizes what is actually a contingent political-economic arrangement: the Declaration is a constructed document whose beneficiaries and victims are identifiable, whose enforceability depends on political power, and whose extraction mechanisms are visible in the property regime and citizenship restrictions it enshrines.
constraint_indexing:constraint_classification(french_constitution_1791__declaration_of_rights_1789, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(french_constitution_1791__declaration_of_rights_1789_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(french_constitution_1791__declaration_of_rights_1789, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(french_constitution_1791__declaration_of_rights_1789, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(french_constitution_1791__declaration_of_rights_1789, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(french_constitution_1791__declaration_of_rights_1789, TR),
    TR >= 0.70.

:- end_tests(french_constitution_1791__declaration_of_rights_1789_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint exhibits genuine tangled-rope structure: the Declaration abolishes one extraction regime (feudal dues) and nominally establishes universal rights, but simultaneously enables new extraction mechanisms (state consolidates fiscal power, property regime restricts rights-enforcement to property owners, new taxation replaces feudal dues). The bourgeoisie experiences the constraint as primarily coordinative (property protection, uniform law solving feudal fragmentation). The peasantry and nobility experience it as extractive (suppression of old privileges, new state extraction). Measurement trajectory shows initial euphoria (0.68→0.55) as feudal dues are nominally abolished, followed by realization that state extraction and enforcement resistance sustain or exceed previous levels (0.55→0.58). Suppression (0.72): High. The Declaration requires active enforcement against: (a) landlords who resist dues abolition and use violence to maintain extraction; (b) counter-revolutionary forces using the ancien régime's legitimacy to delegitimize the Declaration; (c) institutional complexity requiring new courts, new administrative structures, and new force to replace feudal authority. Suppression rises sharply from t0 (0.48) to t2 (0.72) as enforcement machinery is built and revolutionary terror intensifies. By t5 (0.81), suppression peaks as the Terror consolidates control. Theater ratio (0.48): Moderate. The Declaration performs universality while the Constitution retains property qualification of rights, restricted citizenship, and monarchy. The performance gap is real but not extreme — the Declaration genuinely abolishes some extraction mechanisms (feudal dues) and genuinely establishes new protections (property, personal liberty, legal equality), but the universality claim is mediated by property ownership. Theater rises from ancien régime baseline (0.35) as the Declaration's performative claims accumulate, but remains moderate because the Constitution's institutional changes are substantive, not purely theatrical.
 *
 * PERSPECTIVAL GAP:
 *   The Declaration produces maximal perspectival divergence. The nominal beneficiary (rights-bearing citizen in abstract) sees universal liberation. The concrete victim (peasantry) sees feudal extraction nominally abolished but continuing through resistance and replaced by state extraction. The concrete beneficiary (bourgeoisie) sees coordination function and property protection. The enforcement authority (Revolutionary government) sees coordination challenged by suppression requirement. The Royalist opposition sees performative theatre masking illegitimate seizure. The analytical observer risks seeing natural law discovery when examining constructed political power. This gap between nominal universalism and structural asymmetry is the reading's diagnostic signature — it reveals how a constraint can claim universality while producing extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies sharply by perspective. Peasantry (powerless + trapped): d ≈ 0.95, experiences maximum extraction — trapped in enforcement of old extraction while nominally freed. Nobility (powerful + constrained): d ≈ 0.65, high extraction burden (feudal rights suppressed) but not total (property retained, legal personhood protected). Bourgeoisie (powerful + mobile): d ≈ 0.15, benefits from coordination; can exit (though at cost of counter-revolution). Revolutionary government (institutional + constrained): d ≈ 0.72, high extraction responsibility (must enforce against resistance, consolidate power, manage transition) alongside coordination benefit (creates new legal order). Royalist opposition (institutional + constrained): d ≈ 0.55, moderate extraction (suppressed as illegitimate authority) alongside constrained response (no clear alternative institutional form). Analytical observer (analytical + analytical): d ≈ 0.73, derived as detached observer unable to fully escape the naturalization of the Declaration as immutable law. The engine's directionality derivation produces these values from beneficiary/victim declarations and exit capacity. The perspectival gap is diagnostic: peasantry and nobility see maximal extraction despite beneficiary rhetoric; bourgeoisie sees coordination; government sees mixed; observer risks naturalizing contingent arrangements.
 *
 * MANDATROPHY ANALYSIS:
 *   The Declaration reading resolves mandatrophy by showing that the constraint's classification as Tangled Rope (not Snare) depends on acknowledging genuine coordination function (property protection, uniform law replacing feudal fragmentation) alongside genuine extraction (suppression of feudal privileges, state consolidation of fiscal power, property-mediated access to rights). If the reading emphasized only the Declaration's nominally universal rights, the classification would collapse toward pure coordination (Rope) and mask the extraction mechanism. If it emphasized only state consolidation and property asymmetry, it would collapse toward Snare and miss the genuine institutional benefits of law replacing feudal chaos. The Tangled Rope classification forces acknowledgment of both: the Declaration is not a natural law discovery (Mountain false summit), not pure coordination (Rope would mask extraction), and not pure extraction (Snare would erase genuine institutional benefits). It is a constructed arrangement that produces both coordination and extraction, with beneficiaries and victims that are structurally identifiable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    universality_vs_property_regime,
    'Are the Declaration''s ''universal rights'' genuinely universal, or does the Constitution''s protection of property ownership create two classes of effective rights-holders (property owners and others)?',
    'Historical analysis of post-1791 legal disputes: do courts enforce Declaration rights equally for propertyless and property-owning citizens? Comparison of material outcomes by class.',
    'If genuinely universal: Declaration overcomes property-based extraction (ε lower, more Rope). If property-qualified: Declaration is window-dressing on continued extraction (ε higher, more Snare). The reading''s core claim depends on this resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(universality_vs_property_regime, empirical, 'Whether Declaration rights are truly universal or mediated by property ownership').

omega_variable(
    feudal_extraction_continuation_mechanism,
    'After the Declaration nominally abolishes feudal dues, what proportion of extraction continues through: (a) violent landlord resistance, (b) bureaucratic resistance by local officials, (c) legal loopholes the Constitution leaves open, (d) economic coercion (ability to evict, deny land access)?',
    'Estate records, court documents, peasant petitions post-1791; comparison of dues paid pre-1789 vs post-1791; regional variation in enforcement patterns.',
    'If continuation is minimal: Declaration genuinely suppresses old extraction (supports Rope reading). If substantial: extraction persists despite Declaration (supports Snare/Tangled Rope reading). Suppression metric depends on this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feudal_extraction_continuation_mechanism, empirical, 'Extent of feudal extraction continuation despite Declaration abolition').

omega_variable(
    natural_law_vs_constructed_declaration,
    'Is the Declaration a discovery of immutable natural rights that exist independent of the Constitution, or a constructed political document whose authority depends entirely on the Revolutionary government''s institutional power?',
    'Genealogical analysis: trace the Declaration''s intellectual lineage (Locke, Rousseau, medieval charters, Roman law); examine the Declaration''s actual enforcement mechanisms (legislative, executive, judicial); compare to pre-Declaration rights traditions in France and Europe.',
    'If natural law discovery: Mountain classification is appropriate (ε ≤ 0.25, inherent to reason). If constructed: false summit — the Declaration naturalizes contingent power arrangements (ε properly ≥ 0.46, Tangled Rope or Snare). This omega is the engine''s diagnostic gate for false summit detection.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_declaration, conceptual, 'Whether Declaration is natural law discovery or constructed political arrangement').

omega_variable(
    reading_contest_authorization,
    'This reading (Declaration as universal rights prefixed to Constitution) competes with sibling readings that emphasize active/passive citizenship split, monarchical retention, and constitutional failure. Which reading is authorized by the Constitution''s own text and institutional structure, and can more than one reading be simultaneously valid within a single framework?',
    'Close reading of the 1791 Constitution text; analysis of competing contemporary interpretations by revolutionary actors (Girondins, Montagnards, Royalists, Feuillants); structural comparison of which reading is enforced by which institutions.',
    'If one reading forecloses others: the Constitutional framework is logically coherent but contested interpretations compete for authority (forecloses or influences relations apply). If multiple readings coexist: the Constitution itself is under-specified and different readings operate in parallel institutional domains (coexists_with applies). This determines the cs_structure.reading_relations declarations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_authorization, conceptual, 'Logical and institutional status of competing readings within single Constitutional framework').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(french_constitution_1791__declaration_of_rights_1789, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fr1791_decl_theater_t0_ancien_regime, french_constitution_1791__declaration_of_rights_1789, theater_ratio, 0, 0.35).
narrative_ontology:measurement(fr1791_decl_theater_t2_declaration_preface, french_constitution_1791__declaration_of_rights_1789, theater_ratio, 2, 0.48).
narrative_ontology:measurement(fr1791_decl_theater_t5_revolutionary_courts, french_constitution_1791__declaration_of_rights_1789, theater_ratio, 5, 0.52).

% Extraction over time
narrative_ontology:measurement(fr1791_decl_extractiveness_t0_prereform, french_constitution_1791__declaration_of_rights_1789, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(fr1791_decl_extractiveness_t2_reform_momentum, french_constitution_1791__declaration_of_rights_1789, base_extractiveness, 2, 0.55).
narrative_ontology:measurement(fr1791_decl_extractiveness_t5_resistance, french_constitution_1791__declaration_of_rights_1789, base_extractiveness, 5, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fr1791_decl_suppression_t0_feudal_stability, french_constitution_1791__declaration_of_rights_1789, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(fr1791_decl_suppression_t2_enforcement_ratchet, french_constitution_1791__declaration_of_rights_1789, suppression_requirement, 2, 0.72).
narrative_ontology:measurement(fr1791_decl_suppression_t5_terror_intensification, french_constitution_1791__declaration_of_rights_1789, suppression_requirement, 5, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(french_constitution_1791__declaration_of_rights_1789, identity_coordination).
narrative_ontology:affects_constraint(french_constitution_1791__declaration_of_rights_1789, french_constitution_1791__active_passive_citizenship).
narrative_ontology:affects_constraint(french_constitution_1791__declaration_of_rights_1789, french_constitution_1791__suspensive_veto_monarchy).
narrative_ontology:affects_constraint(french_constitution_1791__declaration_of_rights_1789, french_constitution_1791__failure_and_succession).

% DUAL FORMULATION NOTE:
% The 1791 Constitution is decomposed into four readings sharing a kernel: declaration_of_rights_1789 (this story, emphasizing universality claim), active_passive_citizenship (emphasizing means-tested access), suspensive_veto_monarchy (emphasizing monarchical retention), and failure_and_succession (emphasizing constitutional collapse). Each reading has its own ε value, its own beneficiary/victim structure, and its own institutional dynamics. They are not observables of a single constraint but structurally distinct claims about how the Constitution functions. All share the same kernel text but interpret it differently, producing different constraint classifications. The Declaration reading shows how a constraint can claim universality while producing extraction asymmetry — diagnostic for false-summit detection and institutional naturalization analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(french_constitution_1791__declaration_of_rights_1789, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
