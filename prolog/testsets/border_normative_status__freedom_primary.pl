% ============================================================================
% CONSTRAINT STORY: border_normative_status__freedom_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_normative_status__freedom_primary, []).

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
 *   constraint_id: border_normative_status__freedom_primary
 *   human_readable: Freedom of Movement as Fundamental Right (Border Restriction Critique)
 *   domain: political_philosophy/international_law/migration
 *
 * SUMMARY:
 *   The 'freedom_primary' reading positions freedom of movement as a
 *   fundamental human right that borders impermissibly restrict unless the
 *   restricting state can justify the restriction by reference to
 *   extraordinary interests. This is ONE READING of a contested kernel
 *   (border_normative_status) that permits at least three structurally
 *   distinct interpretations: freedom_primary (this reading),
 *   qualified_sovereignty (states retain authority but must exercise it
 *   proportionately and consistently with human rights), and
 *   sovereignty_primary (states have foundational authority to exclude). The
 *   freedom_primary reading instantiates an inversion of the burden of proof:
 *   migrants are presumptively rights-bearers entitled to move, and states
 *   are presumptively in violation unless they can clear an extraordinary
 *   justification bar. This reading benefits international human rights
 *   institutions and mobile skilled workers; it extracts from displaced
 *   domestic workers (whose labor-market position is structured by managed
 *   migration) and from excluded migrants (who bear the immediate cost of
 *   enforcement). The measurement series track the rise of this reading's
 *   institutional authority and enforcement intensity from 1950 (post-WWII
 *   codification) to 2026 (contemporary enforcement crisis).
 *
 * KEY AGENTS:
 *   - Excluded migrants: powerless, trapped, positioned under this reading as rights-violation victims (no legitimate grounds for exclusion exist)
 *   - Asylum seekers: powerless, trapped, positioned as holders of fundamental movement rights
 *   - Displaced domestic workers: moderate power, constrained exit, positioned as collateral victims of the border constraint's labor-market effects
 *   - Nation-state executives: institutional power, positioned as presumptively in violation unless they can justify extraordinary circumstances
 *   - International human rights bodies: institutional power, beneficiary of the reading's vindication of universal rights as supreme framework
 *   - Mobile skilled workers: powerful, arbitrage exit, asymmetric beneficiary (freedom-of-movement norm rarely restricts their access)
 *   - Sovereignty doctrine defenders: excluded from this reading's authority structure, positioned as the reading's intellectual opponents
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__freedom_primary, 0.68).
domain_priors:suppression_score(border_normative_status__freedom_primary, 0.72).
domain_priors:theater_ratio(border_normative_status__freedom_primary, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__freedom_primary, tangled_rope).
narrative_ontology:human_readable(border_normative_status__freedom_primary, "Freedom of Movement as Fundamental Right (Border Restriction Critique)").
narrative_ontology:topic_domain(border_normative_status__freedom_primary, "political_philosophy/international_law/migration").

domain_priors:requires_active_enforcement(border_normative_status__freedom_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__freedom_primary, '4ac0bc7c-4cbc-459a-a4e3-a665b53add03').
narrative_ontology:cs_kernel_codification('4ac0bc7c-4cbc-459a-a4e3-a665b53add03', formalized).
narrative_ontology:cs_authority_grounding('4ac0bc7c-4cbc-459a-a4e3-a665b53add03', lineage).
narrative_ontology:cs_interpretation_layer_present('4ac0bc7c-4cbc-459a-a4e3-a665b53add03').
narrative_ontology:cs_reading_relation('4ac0bc7c-4cbc-459a-a4e3-a665b53add03', border_normative_status__sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('4ac0bc7c-4cbc-459a-a4e3-a665b53add03', border_normative_status__qualified_sovereignty, influences).
narrative_ontology:cs_axiom('4ac0bc7c-4cbc-459a-a4e3-a665b53add03', foundational, freedom_of_movement_fundamental).
narrative_ontology:cs_axiom_status(freedom_of_movement_fundamental, holdable).
narrative_ontology:cs_axiom_grounding('4ac0bc7c-4cbc-459a-a4e3-a665b53add03', freedom_of_movement_fundamental, deontological).
narrative_ontology:cs_axiom('4ac0bc7c-4cbc-459a-a4e3-a665b53add03', foundational, exclusion_requires_extraordinary_justification).
narrative_ontology:cs_axiom_status(exclusion_requires_extraordinary_justification, holdable).
narrative_ontology:cs_axiom_grounding('4ac0bc7c-4cbc-459a-a4e3-a665b53add03', exclusion_requires_extraordinary_justification, deontological).
narrative_ontology:cs_reference_frame('4ac0bc7c-4cbc-459a-a4e3-a665b53add03', post_wwii_universal_human_rights).
narrative_ontology:cs_drift_state('4ac0bc7c-4cbc-459a-a4e3-a665b53add03', contemporary_enforcement_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4ac0bc7c-4cbc-459a-a4e3-a665b53add03', '').
narrative_ontology:cs_kernel_id(border_normative_status__freedom_primary, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__freedom_primary, international_human_rights_regime).
narrative_ontology:constraint_beneficiary(border_normative_status__freedom_primary, mobile_skilled_workers).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, excluded_migrants).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, displaced_domestic_workers).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, asylum_seekers).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__freedom_primary, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(border_normative_status__freedom_primary, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_normative_status__freedom_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_normative_status__freedom_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_normative_status__freedom_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.15 (1950, immediately post-UDHR codification, not yet institutionalized) to 0.68 (2026, full enforcement machinery operational). The measurement series reveal extraction accumulation — the reading started with high coordination intent (preventing atrocity) and low extraction (states had not yet hardened enforcement). Over 76 years, as states responded to the reading with increasingly sophisticated border enforcement, the extraction function became dominant: states use the 'human rights' framing to legitimate ever-more-intrusive monitoring, detention, and deportation machinery. Theater ratio rises from 0.08 (early era: genuine humanitarian motivation) to 0.41 (contemporary: substantial share of enforcement energy is procedural theater — appeals, reviews, due-process compliance — while exclusion rates remain high). This signals Goodhart drift: the constraint's stated function (protecting movement rights) has been instrumentalized into a legitimacy performance while the actual operation (excluding and expelling non-members) continues unchanged. Suppression rises from 0.35 to 0.72 because enforcement intensity has hardened: early asylum processing was sparse and often sympathetic; contemporary enforcement is a vast carceral apparatus (detention centers, fast-track deportations, algorithmic screening). The accessibility_collapse of 0.62 reflects that excluded migrants face nearly-total barrier closure (once excluded, re-entry is extremely difficult, legally and practically) but that the reading itself preserves some alternative path (legal asylum claims, family reunification, humanitarian exception) — genuine natural-law closure would be higher. Resistance of 0.58 reflects significant countermovement (migrant solidarity organizing, asylum advocacy, legal challenges) but not organized successful resistance to the reading's enforcement regime. This is a tangled_rope because it coordinates real humanitarian protection (asylum adjudication, international refugee regime) AND extracts labor-market control for states (managed migration, wage suppression for domestic workers). The extraction is active and requires continuous enforcement (border police, deportation machinery, visa systems).
 *
 * PERSPECTIVAL GAP:
 *   The nation-state executive seat and the international human rights body seat should compute radically different types. From the state's position, the freedom-of-movement reading is an external constraint that strips state discretion and imposes procedural costs (asylum adjudication, legal defense, international oversight). The state seat experiences this as snare-like — the human rights framing legitimates constraint while the state actually continues to exclude at high rates, just with more procedure. From the human rights body's position, the reading is a genuine coordination achievement — it elevated asylum rights to the international level and created a legal architecture that saves lives. Both seats experience the same constraint; the engine should compute different types for each because directionality differs: the state is a target (d near 1.0, direction: forced to run costly procedures to exclude), while the human rights body is a beneficiary (d near 0.0, the constraint's enforcement expands institutional authority). This perspectival divergence is the essence of the tangled_rope: genuine coordination (asylum protection) that is asymmetrically distributed (benefits human rights institutions more than it protects migrants; extracts from states through enforcement costs and from workers through labor-market suppression).
 *
 * DIRECTIONALITY LOGIC:
 *   Excluded migrants and asylum seekers carry high directionality (d near 1.0 → target end) because they bear the primary cost of enforcement and have no exit (trapped, powerless, identity_locked). Displaced domestic workers carry moderate-to-high directionality (d around 0.65-0.75) because they experience depressed wages and constrained labor-market access through managed migration, but they retain some geographic and occupational mobility (exit is constrained, not trapped). Nation-state executives carry high directionality (d near 0.8) because the reading strips their a priori legitimacy to exclude and requires them to justify exclusions through costly procedures; however, they retain the practical power to exclude (they do so massively) and the reading has become a legitimacy performance for their exclusions (so d is not maximal — they capture extraction benefit through the theater). International human rights bodies carry very low directionality (d near 0.15 → beneficiary end) because the reading vindicates their institutional authority and expands their remit without requiring them to bear extraction costs. Mobile skilled workers carry low directionality (d near 0.25) because the reading's freedom-of-movement norm rarely restricts THEIR access (they move through legal channels, professional sponsorship, skilled-migrant visas) — they benefit from the rhetoric while rarely bearing its suppression. Sovereignty defenders are excluded from the directionality calculation entirely (they are not a stakeholder in this reading's operation; they are the intellectual opponents). No directionality overrides are needed; the structural derivation from beneficiary/victim declarations and exit options produces accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is clear: mid-20th-century states weaponized border control to commit genocide and create mass statelessness (Holocaust, partition, ethnic cleansing). The founding solution was to establish freedom of movement as a human right anterior to state authority — membership in political community should not be the prerequisite for protection from atrocity. However, the founding problem status is CONTESTED (not dead, not fully live, but disputed): human rights bodies attest the problem is live because authoritarian states still weaponize borders and the risk of state-driven atrocity remains high. Realist scholars and states attest the problem is substantially managed through deterrence, domestic legal systems, and international scrutiny — the reading has become a tool of unelected NGOs and courts imposing migration agendas on democratically-elected governments. Labor advocates attest a DIFFERENT founding problem: the reading was designed to prevent atrocity but has become a mechanism for suppressing domestic labor standards and real wages for precarious workers. The disappearance_verdict is world_rearranges: if the reading disappeared, asylum law would collapse, and the entire international human rights order would lose its foundational justification. This is NOT mandatrophy in the classical sense (a constraint whose mandate has died but whose enforcement persists) because the reading's mandate is still CONTESTED — it is simultaneously live (for human rights bodies), dead (for realist states), and misdirected (for labor advocates). This is a constraint whose mandate has become POLYPHONIC — different audiences hear different founding problems and different solutions. The theater_ratio rising to 0.41 signals one form of drift: the constraint's stated coordination function (saving lives through asylum protection) is increasingly decoupled from its actual operation (states exclude at high rates while performing compliance with human rights procedures). This is not classical mandatrophy but it is mandate-drift: the constraint persists because multiple audiences have stake in its existence, but the stakes are increasingly divergent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    universality_vs_asymmetric_beneficiary,
    'Does the freedom-of-movement reading function as a universal human right, or does it systematically benefit mobile, skilled, and wealthy migrants while extracting costs from less-mobile domestic workers and excluded asylum seekers?',
    'Comparative labor-market analysis tracking wage effects on low-skill citizens vs. high-skill migrant inflows in freedom-of-movement zones (EU, Schengen) vs. restricted-sovereignty zones; ethnographic study of how asylum seekers and economic migrants experience the reading''s protections vs. its enforcement machinery.',
    'If asymmetric: the constraint is snare masquerading as rope (universal rights claim covering extraction). Reclassification would shift from tangled_rope (coordination + extraction) to snare (extraction disguised as principle). The sovereignty_primary reading would gain explanatory power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universality_vs_asymmetric_beneficiary, empirical, 'Whether the reading instantiates universal rights or disguises asymmetric extraction.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the freedom-of-movement reading''s core axiom (movement is a fundamental right anterior to state authority) logically foreclose the sovereignty_primary reading (states have foundational authority to exclude), or do the readings merely coexist as live political positions held by different institutional factions?',
    'Formal logical analysis of whether ''fundamental inalienable right'' and ''foundational state authority'' can be held in the same commitment framework (they likely cannot — one asserts rights-to-override-state, the other asserts state-grounds-all-rights). Examine whether any major institutional voice attempts to hold both simultaneously or whether they are held by fully separate factions.',
    'If foreclosure: relations should be ''forecloses'' not ''coexists_with''. If coexistence: the kernel_context should document that the readings are not logically incompatible but politically incompatible — different authorities have different legitimate starting points.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether the readings are logically incompatible or politically divided.').

omega_variable(
    suppression_mechanism_internalization,
    'To what extent is the suppression of excluded migrants'' mobility internalized (they accept exclusion as legitimate) vs. structural (they are materially barred by enforcement machinery)? What happens to suppression post-exit — does it persist in the form of trauma and identity damage, or does it dissipate once the physical barrier is removed?',
    'Longitudinal study of forced-return migrants and asylum rejectees: do rejected migrants, when able to settle elsewhere, report persistent suppression-like symptoms (fear of borders, identity damage) or does suppression tracking disappear once the structural barrier is gone? Interviews with long-term excluded populations on whether they internalize the exclusion as legitimate.',
    'If substantially internalized: the true suppression is higher than measured because it travels with the migrant post-exclusion. The constraint''s effective extraction is understated. If structural only: suppression metric is accurate and reflects enforcement intensity, not psychological capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of migrant mobility is structural or internalized.').

omega_variable(
    coordination_vs_extraction_separation,
    'Is there a genuine coordination function (asylum protection, humanitarian response) that is separable from the extraction function (states retaining power to exclude workers and regulate labor markets), or are the two inseparable — the protection only possible through state enforcement of exclusivity?',
    'Natural experiment from zones that adopt open movement (EU free movement) while retaining asylum protections: if protection and extraction remain separable (asylum claims still adjudicated fairly in open zones), the functions are separable and extraction is revealed as additional overlay. If open movement destabilizes asylum systems, they are inseparable.',
    'If separable: the tangled_rope reading is accurate — genuine coordination (asylum, humanitarian protection) bundled with pure extraction (labor-market control). If inseparable: the extraction is part of the coordination cost and the constraint drifts toward rope (less extractive than measured).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_extraction_separation, empirical, 'Whether border coordination and extraction can be separated.').

omega_variable(
    reading_authority_grounding,
    'What grounds the authority of this reading — the Universal Declaration of Human Rights (lineage to post-WWII commitment), the empirical fact that states have committed atrocities (extraction of legitimacy from state failure), or the coherence of the rights framework itself (expertise in moral reasoning)? Which grounding is most fragile?',
    'Historical analysis of how the reading is invoked and defended in practice: do authorities cite the UDHR as binding text (lineage), cite state atrocities as evidence states cannot be trusted (extraction), or cite the internal coherence of human-rights doctrine (expertise)? Track which grounding breaks first under pressure.',
    'If lineage is primary: the reading''s authority rests on continued acceptance of UDHR as binding; rejecting the post-WWII order destabilizes the reading. If extraction: the reading survives only as long as state atrocity risk is salient. If expertise: the reading survives as long as the professional human-rights community maintains consensus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_authority_grounding, conceptual, 'What grounds the authority of the freedom-of-movement reading.').

omega_variable(
    displacement_vs_exclusion_victim_set,
    'Are displaced domestic workers genuine victims of the constraint (their labor-market position is structured by the border and managed migration), or are they victims of a different constraint (labor-market deregulation, inequality)? Does including them in the victim set reveal mandatrophy (a constraint whose founding problem no longer applies to them), or does it reveal an unseen founding problem (the reading creates collateral damage beyond its stated scope)?',
    'Counterfactual analysis: in a world with identical labor-market deregulation and inequality but without freedom-of-movement reading (i.e., with sovereignty_primary), would displaced domestic workers'' labor-market position be substantially different? If yes, they are victims of THIS constraint. If no, they are victims of labor-market deregulation and the reading is orthogonal.',
    'If displaced workers are victims of THIS constraint: the constraint''s true victim set is wider than the reading''s authors acknowledge, and the reading is extracting from an unintended target set. If they are victims of a different constraint: the inclusion is a category error and the constraint''s classification may shift. Either way, the analysis reveals either hidden extraction or analytical confusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displacement_vs_exclusion_victim_set, empirical, 'Whether displaced domestic workers are victims of the reading or of separate labor-market dynamics.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__freedom_primary, 1950, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1950, border_normative_status__freedom_primary, theater_ratio, 1950, 0.08).
narrative_ontology:measurement(bord_tr_t1975, border_normative_status__freedom_primary, theater_ratio, 1975, 0.16).
narrative_ontology:measurement(bord_tr_t1990, border_normative_status__freedom_primary, theater_ratio, 1990, 0.24).
narrative_ontology:measurement(bord_tr_t2005, border_normative_status__freedom_primary, theater_ratio, 2005, 0.32).
narrative_ontology:measurement(bord_tr_t2015, border_normative_status__freedom_primary, theater_ratio, 2015, 0.38).
narrative_ontology:measurement(bord_tr_t2026, border_normative_status__freedom_primary, theater_ratio, 2026, 0.41).

% Extraction over time
narrative_ontology:measurement(bord_be_t1950, border_normative_status__freedom_primary, base_extractiveness, 1950, 0.15).
narrative_ontology:measurement(bord_be_t1975, border_normative_status__freedom_primary, base_extractiveness, 1975, 0.32).
narrative_ontology:measurement(bord_be_t1990, border_normative_status__freedom_primary, base_extractiveness, 1990, 0.48).
narrative_ontology:measurement(bord_be_t2005, border_normative_status__freedom_primary, base_extractiveness, 2005, 0.58).
narrative_ontology:measurement(bord_be_t2015, border_normative_status__freedom_primary, base_extractiveness, 2015, 0.64).
narrative_ontology:measurement(bord_be_t2026, border_normative_status__freedom_primary, base_extractiveness, 2026, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1950, border_normative_status__freedom_primary, suppression_requirement, 1950, 0.35).
narrative_ontology:measurement(bord_su_t1975, border_normative_status__freedom_primary, suppression_requirement, 1975, 0.45).
narrative_ontology:measurement(bord_su_t1990, border_normative_status__freedom_primary, suppression_requirement, 1990, 0.54).
narrative_ontology:measurement(bord_su_t2005, border_normative_status__freedom_primary, suppression_requirement, 2005, 0.62).
narrative_ontology:measurement(bord_su_t2015, border_normative_status__freedom_primary, suppression_requirement, 2015, 0.68).
narrative_ontology:measurement(bord_su_t2026, border_normative_status__freedom_primary, suppression_requirement, 2026, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_normative_status__freedom_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(border_normative_status__freedom_primary, 0.12).
narrative_ontology:affects_constraint(border_normative_status__freedom_primary, border_normative_status__qualified_sovereignty).
narrative_ontology:affects_constraint(border_normative_status__freedom_primary, border_normative_status__sovereignty_primary).

% DUAL FORMULATION NOTE:
% The border_normative_status kernel admits at least three readings, each instantiating a different constraint. The freedom_primary reading (this constraint) treats freedom of movement as fundamental and positions state exclusion as presumptively illegitimate. The qualified_sovereignty reading (sibling) treats state authority as legitimate but requiring proportionality and rights consistency. The sovereignty_primary reading (sibling) treats state exclusion authority as foundational and anterior to human rights. Each reading has different ε, different beneficiary/victim structures, and different computed types per seat. They are not three angles on one constraint — they are three constraints from one contested kernel. The network links document the family relationship and enable contamination analysis (if freedom_primary's institutional authority erodes, how does that pressure qualified_sovereignty and sovereignty_primary?).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(border_normative_status__freedom_primary, institutional, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
