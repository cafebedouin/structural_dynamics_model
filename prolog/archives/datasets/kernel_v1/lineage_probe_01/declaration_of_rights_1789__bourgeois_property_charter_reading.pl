% ============================================================================
% CONSTRAINT STORY: declaration_of_rights_1789__bourgeois_property_charter_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_declaration_of_rights_1789__bourgeois_property_charter_reading, []).

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
 *   constraint_id: declaration_of_rights_1789__bourgeois_property_charter_reading
 *   human_readable: The Declaration as Bourgeois Property Charter (1789 Reading)
 *   domain: legal/doctrinal/constitutional_founding
 *
 * SUMMARY:
 *   The Declaration of the Rights of Man and Citizen (26 August 1789) emerges
 *   from the propertied wing of the revolutionary coalition as a strategic
 *   document that frames narrow property protection as universal principle.
 *   Article 17 declares property 'sacred and inviolable' — the only right
 *   defined with this intensifier. The document's genius is linguistic: it
 *   deploys universalist language ('All men are born free and equal in
 *   rights') while the founding's institutional structure immediately
 *   restricts these rights to property owners (franchise tied to contribution
 *   levels, no redistribution mechanism, no court to enforce Article 1
 *   against property holders). This reading examines the constraint as a
 *   hybrid coordination-extraction mechanism: the Declaration solves the
 *   propertied revolutionaries' core problem (securing new wealth against
 *   feudal reversions and popular seizure) while suppressing the propertyless
 *   reading of Article 1 through the performative invocation of universal
 *   principle. The suppression is not explicit prohibition but rather the
 *   structural impossibility of reading 'all men equal' alongside a
 *   property-restricted franchise while remaining coherent within the
 *   Declaration's own terms. The constraint's extractiveness (0.62) reflects
 *   that the propertyless and the organized Assembly both bear costs of
 *   active enforcement — suppressing redistribution claims, managing the gap
 *   between universal text and restrictive practice — while the propertied
 *   beneficiary experiences low friction and arbitrage capacity.
 *
 * KEY AGENTS:
 *   - Acquiring Third-Estate Wealth: Primary beneficiary (institutional/arbitrage) — benefits from property protection, confiscation of Church estates, market opening. Experiences the Declaration as coordination (Rope perspective).
 *   - Propertyless Reading Article 1 Literally: Primary victim (powerless/trapped) — the universalist framing promises equality and dignity but the institutional structure forecloses it. Experiences maximum extraction (Snare perspective).
 *   - Non-Propertied Third Estate: Secondary victim (moderate/constrained) — participated in revolution, contributed to universal framing, constrained to non-voting status and no property acquisition mechanism. Tangled rope perspective.
 *   - Revolutionary Legislative Assembly: Institutional actor with dual mandate (institutional/constrained) — coordinate property protection while managing the gap between universal text and restrictive practice. Active suppression required. Tangled rope perspective.
 *   - The Declaration as Living Constitutional Text: Institutional mechanism degrading over generations (institutional/constrained) — shifts from operative legal constraint to performative ritual. Piton perspective.
 *   - Transnational Revolutionary Movements: Organized agents (organized/mobile) — the Declaration's universalism coordinates property-protecting revolutions across European borders; sunset as competing constitutional frameworks mature. Scaffold perspective.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the contingent property choice as universal natural law. Mountain (false summit) perspective.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(declaration_of_rights_1789__bourgeois_property_charter_reading, 0.62).
domain_priors:suppression_score(declaration_of_rights_1789__bourgeois_property_charter_reading, 0.68).
domain_priors:theater_ratio(declaration_of_rights_1789__bourgeois_property_charter_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(declaration_of_rights_1789__bourgeois_property_charter_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(declaration_of_rights_1789__bourgeois_property_charter_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(declaration_of_rights_1789__bourgeois_property_charter_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(declaration_of_rights_1789__bourgeois_property_charter_reading, tangled_rope).
narrative_ontology:human_readable(declaration_of_rights_1789__bourgeois_property_charter_reading, "The Declaration as Bourgeois Property Charter (1789 Reading)").
narrative_ontology:topic_domain(declaration_of_rights_1789__bourgeois_property_charter_reading, "legal/doctrinal/constitutional_founding").

domain_priors:requires_active_enforcement(declaration_of_rights_1789__bourgeois_property_charter_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(declaration_of_rights_1789__bourgeois_property_charter_reading, 'de28db5f-cff0-4701-bd32-5ed90e6478ec').
narrative_ontology:cs_kernel_codification('de28db5f-cff0-4701-bd32-5ed90e6478ec', fixed_text).
narrative_ontology:cs_authority_grounding('de28db5f-cff0-4701-bd32-5ed90e6478ec', extraction).
narrative_ontology:cs_interpretation_layer_present('de28db5f-cff0-4701-bd32-5ed90e6478ec').
narrative_ontology:cs_reading_relation('de28db5f-cff0-4701-bd32-5ed90e6478ec', declaration_of_rights_1789__universal_charter_reading, coexists_with).
narrative_ontology:cs_reading_relation('de28db5f-cff0-4701-bd32-5ed90e6478ec', declaration_of_rights_1789__declaratory_unenforceable_reading, influences).
narrative_ontology:cs_axiom('de28db5f-cff0-4701-bd32-5ed90e6478ec', foundational, property_protection_prerequisite_for_market_society).
narrative_ontology:cs_axiom_status(property_protection_prerequisite_for_market_society, holdable).
narrative_ontology:cs_axiom_grounding('de28db5f-cff0-4701-bd32-5ed90e6478ec', property_protection_prerequisite_for_market_society, instrumental).
narrative_ontology:cs_axiom('de28db5f-cff0-4701-bd32-5ed90e6478ec', foundational, universal_language_necessary_for_revolutionary_legitimacy).
narrative_ontology:cs_axiom_status(universal_language_necessary_for_revolutionary_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('de28db5f-cff0-4701-bd32-5ed90e6478ec', universal_language_necessary_for_revolutionary_legitimacy, conventional).
narrative_ontology:cs_reference_frame('de28db5f-cff0-4701-bd32-5ed90e6478ec', property_based_market_legitimacy).
narrative_ontology:cs_drift_state('de28db5f-cff0-4701-bd32-5ed90e6478ec', contemporary_human_rights_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('de28db5f-cff0-4701-bd32-5ed90e6478ec', '').
narrative_ontology:cs_kernel_id(declaration_of_rights_1789__bourgeois_property_charter_reading, declaration_of_rights_1789).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(declaration_of_rights_1789__bourgeois_property_charter_reading, acquiring_third_estate_wealth).
narrative_ontology:constraint_beneficiary(declaration_of_rights_1789__bourgeois_property_charter_reading, propertied_revolutionary_faction).
narrative_ontology:constraint_victim(declaration_of_rights_1789__bourgeois_property_charter_reading, propertyless_reading_article_1_literally).
narrative_ontology:constraint_victim(declaration_of_rights_1789__bourgeois_property_charter_reading, redistribution_claims_suppressed).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROPERTYLESS CLAIM (SNARE) — The Declaration's Article 1 ('All men are born free and equal in rights') read literally promises property protection and dignity to all. But the founding's institutional structure immediately forecloses this reading: franchise tied to property ownership, no redistribution mechanism, no remedy for those without property. Trapped by the document's promise and the institutional silence on redistribution. Maximum extraction: the propertyless bear the cost of the universalist framing while receiving none of its guarantees.
constraint_indexing:constraint_classification(declaration_of_rights_1789__bourgeois_property_charter_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: NON-PROPERTIED THIRD ESTATE (TANGLED ROPE) — Participated in the revolution, contributed to the universal framing, but constrained to non-voting status and no mechanism for property acquisition. Benefits from the social peace (the revolution's coordination function prevented feudal re-establishment) but bears extraction as property concentration accelerates during the revolutionary window. Exit options limited by legal/military barriers to armed resistance and by the document's rhetoric (universal claims preclude revolution against universal principles).
constraint_indexing:constraint_classification(declaration_of_rights_1789__bourgeois_property_charter_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ACQUIRING THIRD-ESTATE WEALTH (ROPE) — Primary beneficiary. The Declaration secures property rights at the founding, enabling confiscation of Church and émigré estates, consolidation of peasant holdings into marketable plots, and commercial expansion. No extraction experienced — the constraint solves their core coordination problem: how to protect new property acquisitions against feudal reversions or popular seizure. Arbitrage exit available: can move capital across borders, invest in colonial ventures, shift to other property forms. The document's universalism is their genius — it frames narrow property protection as universal principle.
constraint_indexing:constraint_classification(declaration_of_rights_1789__bourgeois_property_charter_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REVOLUTIONARY LEGISLATIVE ASSEMBLY (TANGLED ROPE) — Faces dual mandate: coordinate the revolution's gains (property protection, market opening, feudal abolition) while managing the gap between Article 1's universalism and the founding's property restrictions. Constrained by the need to suppress redistribution claims without openly violating the universalist text. The Assembly benefits from the Declaration (it provides legitimacy) but bears extraction as it must actively police the boundary between universal principle and restrictive practice. Requires sustained enforcement to prevent propertyless groups from reading Article 1 literally.
constraint_indexing:constraint_classification(declaration_of_rights_1789__bourgeois_property_charter_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: DECLARATION AS LIVING CONSTITUTIONAL TEXT (PITON) — Over generations, the Declaration becomes a civic ritual and foundational myth rather than an operative constraint. Courts invoke it ceremonially but do not derive substantive property doctrine from it (that comes from Code Napoleon, commercial law, market mechanisms). The theater ratio (0.55) reflects that the Declaration's actual work is symbolic — it legitimates the property system without doing the detailed legal work of property definition. The generational view sees the constraint as substantially degraded: no longer solving the 1789 coordination problem, but maintained as a performative founding gesture.
constraint_indexing:constraint_classification(declaration_of_rights_1789__bourgeois_property_charter_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: TRANSNATIONAL REVOLUTIONARY MOVEMENTS (SCAFFOLD) — The Declaration's universalist language creates a temporary coordination function for exporting revolution and property protection across European borders. The scaffold's sunset: as each nation creates its own founding text (US Constitution, Spanish liberalism, 1848 constitutional movements), the Declaration's specific legal force diminishes even as its rhetorical power increases. The constraint is temporary and functional — it coordinates transnational property-protecting revolutions for roughly 30-50 years (1789-1848), then degrades into a symbolic reference as competing constitutional frameworks mature. Mobile exit: revolutionary movements can adopt the Declaration's language selectively or abandon it for domestic constitutional framing.
constraint_indexing:constraint_classification(declaration_of_rights_1789__bourgeois_property_charter_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW READING (MOUNTAIN) — From a civilizational scale, the Declaration can appear to articulate universal and immutable natural law: human beings possess inherent rights that no legislature can violate. This view treats Article 17 (property is sacred and inviolable) as an expression of natural justice rather than a contingent historical choice. But the structural data reveals this as a false summit: the naturalness of the property protection masks the contingent choice to restrict its scope to propertied citizens and to suppress redistribution claims. The constraint has identifiable beneficiaries (acquiring wealth, propertied faction) — the engine flags this for FSM evaluation.
constraint_indexing:constraint_classification(declaration_of_rights_1789__bourgeois_property_charter_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(declaration_of_rights_1789__bourgeois_property_charter_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(declaration_of_rights_1789__bourgeois_property_charter_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(declaration_of_rights_1789__bourgeois_property_charter_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(declaration_of_rights_1789__bourgeois_property_charter_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(declaration_of_rights_1789__bourgeois_property_charter_reading, TR),
    TR >= 0.70.

:- end_tests(declaration_of_rights_1789__bourgeois_property_charter_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): The constraint's base extraction is moderate-high, reflecting that the Declaration creates genuine beneficiaries (acquiring wealth) whose gains exceed the costs of coordination. However, extractiveness is not maximal (not 0.72+) because the constraint does solve a coordination problem — the revolution genuinely needed property protection to prevent feudal reversions and popular seizure. The measurable gap between universal principle and restrictive practice constitutes extractive overhead, but the underlying coordination function is real. Suppression (0.68): High. The suppression mechanism operates at multiple levels: (1) Explicit, through franchise restrictions and absence of redistribution mechanisms; (2) Structural, through the impossibility of coherently reading 'all men equal' within the property-restrictive institutional framework; (3) Doctrinal, through the subsequent reframing of property as a natural right rather than a founding choice. The suppression requirement increases over the interval (0.55→0.68) as the gap between universal text and restrictive practice becomes more visible and requires more active policing. Theater ratio (0.55): Moderate. The Declaration's actual work splits: in the immediate founding moment (t=0, theater=0.40), the document does real legal work (defining property rights, abolishing feudalism, establishing market mechanisms). Over generations, as the Declaration becomes a civic ritual and foundational myth, the performative content rises (theater=0.55 by t=4). The document is neither purely functional nor purely ceremonial — it is a hybrid that shifts toward performance over time. This trajectory matches the piton classification at the generational scale.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exemplifies the deepest perspectival gap in the corpus. The same linguistic formulation — 'All men are born free and equal in rights' coupled with 'Property is sacred and inviolable' — produces opposite classifications depending on whether the observer can exercise the property right. For the propertied observer, the Declaration is rope (solves the coordination problem of property security). For the propertyless, it is snare (universal framing masks property restriction). For the Legislative Assembly, it is tangled rope (genuine coordination + extraction asymmetry). For the transnational movement, it is scaffold (temporary until each nation writes its own constitution). For the institutional document itself, it is piton (ceremonial over generations). For the natural-law reader, it appears mountain (universal principle) — but the presence of identifiable beneficiaries triggers the false-summit evaluation. The perspectival gap reveals that the constraint's entire structure is contingent on the observer's structural position. No single classification is 'wrong' — each captures a real aspect of how the Declaration functions at that position.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is derived from the agent's structural position relative to the constraint. (1) Acquiring wealth: beneficiary + arbitrage → d ≈ 0.05 → f(d) ≈ -0.12 → negative effective extraction (they experience the constraint as beneficial). (2) Propertyless reading literally: victim + trapped → d ≈ 0.95 → f(d) ≈ 1.42 → maximum effective extraction. (3) Non-propertied third estate: victim + constrained (moderate power, significant barriers) → d ≈ 0.70 → f(d) ≈ 1.08 → high effective extraction. (4) Legislative Assembly: institutional actor, benefits from legitimacy but constrained by dual mandate → d ≈ 0.52 → f(d) ≈ 0.68 → moderate effective extraction (they benefit from coordination but bear enforcement costs). The perspectival gap is maximal: the beneficiary sees coordination (rope), the powerless sees extraction (snare), the organized sees a temporary solution (scaffold), the institutional degraded sees ritual (piton), and the civilizational analytical observer risks seeing natural law (false summit). These gaps reveal that the classification is not determined by the constraint's intrinsic properties but by the observer's structural relationship to the extraction flow.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by recognizing that this reading is ONE reading of a contested kernel, not a claim about the Declaration's unique true nature. The bourgeois_property_charter_reading acknowledges that the universal_charter_reading and the declaratory_unenforceable_reading are also structurally defensible — they emerge from different agent positions and different historical moments. The mandatrophy resolves because: (1) The constraint's ε is stable for this reading (0.62 base extractiveness reflects the coordination-extraction hybrid). (2) The seven perspectives show how a single structural arrangement (universal text + restrictive practice) produces different classifications depending on position. (3) The false-summit identification of the analytical mountain perspective reveals the natural-law framing as a perspectival choice (analytically privileged but not uniquely true). The sibling readings (universal_charter, declaratory_unenforceable) have their own ε values and their own false-summit risks — they are not alternatives within this story but separate constraints linked via network.affects_constraints.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_contingent_choice,
    'Is Article 17''s property protection grounded in universal natural law, or is it a contingent institutional choice designed to lock in the propertied revolution''s gains?',
    'Comparative constitutional analysis: do all property-founding documents converge on Article 17''s specific formulation, or do they vary by the class interests of the founding coalition? Historical documents from 1789-1791 debates: do participants frame property as natural or as a strategic founding choice?',
    'If natural law: mountain classification confirmed (false summit gate may not fire). If contingent: mountain classification is invalid; constraint reclassifies as tangled_rope or snare depending on enforcement visibility. This reading''s core claim depends on the outcome.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_contingent_choice, conceptual, 'Whether property protection is natural law or contingent institutional choice').

omega_variable(
    article_1_literal_reading_foreclosure,
    'Does the Declaration''s institutional structure (property-based franchise, no redistribution mechanism) logically foreclose the propertyless reading of Article 1 (''All men are born free and equal''), or merely suppress it?',
    'Doctrinal analysis: can a court or legislature coherently hold that Article 1 applies universally while Article 17 restricts property rights to propertied citizens? If coherent: constraints coexist (this reading coexists_with the universal reading). If incoherent: this reading forecloses the universal reading (the founding''s structure makes literal Article 1 impossible).',
    'If foreclosed: reading_relations should declare ''forecloses'' with the universal_charter_reading. If coexist: declare ''coexists_with''. The outcome determines whether the contradiction is logical or merely political.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_1_literal_reading_foreclosure, conceptual, 'Whether institutional structure logically forecloses literal Article 1 or merely suppresses it politically').

omega_variable(
    suppression_mechanism_enforcement_visibility,
    'Is the suppression of redistribution claims maintained by explicit legal prohibition (high enforcement visibility) or by the subtler mechanism of universal language that precludes redistribution as incompatible with individual rights doctrine?',
    'Historical and doctrinal evidence: 1790s legal records show whether courts explicitly reject redistribution claims, or whether redistribution arguments simply cannot be formulated within the Declaration''s property-rights framework. The 1793 Montagne proposals and their reception provide test cases.',
    'If explicit prohibition: suppression measurement should rise (0.68 is accurate). If structural via language: suppression might be lower (the constraint works through framing, not force), but theater_ratio should rise. The decomposition affects measurement trajectory over generations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_enforcement_visibility, empirical, 'Whether suppression is explicit legal prohibition or structural framing').

omega_variable(
    universal_vs_particularist_kernel_reading,
    'The Declaration itself is the kernel under contest. Is the kernel legitimately readable as a universal human-rights charter (universal_charter_reading), or does the historical founding context (property restriction, franchise limitation, suppressed redistribution) make the universal reading a misreading of the founder''s intent?',
    'Textual analysis of preparatory materials and legislative debates (Mirabeau, Talleyrand, Mounier, Robespierre). If the founders consciously chose universal language to advance property interests: the bourgeois_property_charter_reading captures intent, and universal_charter_reading is a later reframing. If founders intended universal scope but subordinated it to property: both readings remain legitimate (coexists_with relation holds). If the text''s universalism was an accidental consequence: oracle gap in the founding itself (no single intent settles the reading).',
    'This omega determines the reading_relations field: forecloses (this reading makes universal reading impossible in the founding context), coexists_with (both held simultaneously by different factions), or neither (the founding itself is indeterminate). The kernel''s ambiguity structure depends on this outcome.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_vs_particularist_kernel_reading, conceptual, 'Whether the Declaration''s universal language represents founder intent or later reframing').

omega_variable(
    identity_fusion_and_legitimacy_claim,
    'Does the bourgeois reading''s legitimacy depend on claiming natural-law status for property (fusing contingent choice with universal principle), or can the reading stand on the explicit recognition that the Declaration is a contingent founding choice by propertied revolutionaries?',
    'Doctrinal evolution: do later jurists defend the Declaration''s property doctrine by invoking natural law, or by invoking the historical founding as an authoritative political choice binding on the nation? The Restoration''s attempt to supersede the Declaration tests this: if property doctrine depends on natural law, it survives. If it depends on founding authority, it may not.',
    'If identity-fused with natural law: the mountain false-summit classification applies (the reading naturalizes contingency). If standing on founding authority alone: the tangled_rope classification is stable (the reading is transparent about its basis). The outcome shapes the engine''s FSM evaluation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_and_legitimacy_claim, conceptual, 'Whether the bourgeois reading''s legitimacy depends on natural law fusion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(declaration_of_rights_1789__bourgeois_property_charter_reading, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(decl_bourgeois_theater_t0, declaration_of_rights_1789__bourgeois_property_charter_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(decl_bourgeois_theater_t2, declaration_of_rights_1789__bourgeois_property_charter_reading, theater_ratio, 2, 0.48).
narrative_ontology:measurement(decl_bourgeois_theater_t4, declaration_of_rights_1789__bourgeois_property_charter_reading, theater_ratio, 4, 0.55).

% Extraction over time
narrative_ontology:measurement(decl_bourgeois_extract_t0, declaration_of_rights_1789__bourgeois_property_charter_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(decl_bourgeois_extract_t2, declaration_of_rights_1789__bourgeois_property_charter_reading, base_extractiveness, 2, 0.58).
narrative_ontology:measurement(decl_bourgeois_extract_t4, declaration_of_rights_1789__bourgeois_property_charter_reading, base_extractiveness, 4, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(decl_bourgeois_supp_t0, declaration_of_rights_1789__bourgeois_property_charter_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(decl_bourgeois_supp_t2, declaration_of_rights_1789__bourgeois_property_charter_reading, suppression_requirement, 2, 0.65).
narrative_ontology:measurement(decl_bourgeois_supp_t4, declaration_of_rights_1789__bourgeois_property_charter_reading, suppression_requirement, 4, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(declaration_of_rights_1789__bourgeois_property_charter_reading, identity_coordination).
narrative_ontology:affects_constraint(declaration_of_rights_1789__bourgeois_property_charter_reading, declaration_of_rights_1789__universal_charter_reading).
narrative_ontology:affects_constraint(declaration_of_rights_1789__bourgeois_property_charter_reading, declaration_of_rights_1789__declaratory_unenforceable_reading).

% DUAL FORMULATION NOTE:
% The Declaration of 1789 is the kernel under contest. This story instantiates the bourgeois_property_charter_reading: the Declaration as a strategic document securing third-estate wealth through universalist framing. Two sibling readings exist as separate constraints: (1) declaration_of_rights_1789__universal_charter_reading (constraint_id TBD) — the Declaration as addressing all mankind, not just Frenchmen; ε lower, coordination function higher, victim set narrower. (2) declaration_of_rights_1789__declaratory_unenforceable_reading (constraint_id TBD) — the Declaration as declaring principles that the founding constitution immediately violated; ε reflects the enforcement gap, theater_ratio higher (all declaration, no teeth). Each reading has its own ε, beneficiary/victim structure, and measurement trajectory. The three stories together form the Declaration constraint family, linked via network.affects_constraints. This reading (bourgeois_property_charter) influences the other two: the universal reading must contend with the property restriction, and the unenforceable reading emerges in response to the enforcement gap between universal text and restrictive practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
