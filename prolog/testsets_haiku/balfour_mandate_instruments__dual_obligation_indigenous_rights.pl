% ============================================================================
% CONSTRAINT STORY: balfour_mandate_instruments__dual_obligation_indigenous_rights
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_balfour_mandate_instruments__dual_obligation_indigenous_rights, []).

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
 *   constraint_id: balfour_mandate_instruments__dual_obligation_indigenous_rights
 *   human_readable: Balfour Mandate Dual Obligation: Indigenous Rights Protection Over National Home Primacy
 *   domain: international_law/colonial_administration/state_formation
 *
 * SUMMARY:
 *   The Balfour Declaration (1917) and subsequent mandate instruments (League
 *   of Nations, 1922) contain two contradictory commitments: facilitate a
 *   Jewish national home in Palestine, and protect the rights and status of
 *   existing Arab communities. This story instantiates ONE READING of the
 *   contested kernel: the dual-obligation reading, in which 'national home'
 *   is subordinated to the protection of Arab civil, political, and property
 *   rights, and in which Arab majority status grounds a claim to
 *   representative government and an eventual path to sovereignty. This
 *   reading interprets the mandate as a coordination mechanism that resolves
 *   the Balfour tension by treating Arab rights as superior in priority and
 *   Arab demographic status as the primary constraint on Jewish institutional
 *   development. The alternative readings—jewish_national_home_primacy
 *   (national home as proto-state requiring unrestricted land access and
 *   demographic transformation) and mandatory_interpretive_discretion
 *   (British administrative discretion overrides any substantive reading)—are
 *   separate constraints in separate stories, linked via
 *   network.affects_constraints. This story's ε is deliberately independent
 *   of those readings: the dual-obligation reading instantiates a high
 *   tangled_rope constraint (coordination function + asymmetric extraction
 *   from zionist_organizations and british_mandatory_administrators)
 *   regardless of whether alternative readings were operationally dominant or
 *   historically marginalized. The measurement series tracks the constraint's
 *   extractiveness and theater as the dual-obligation reading was nominally
 *   in force under the mandate system (1922–1948).
 *
 * KEY AGENTS:
 *   - palestinian_arab_elites — Organized actors claiming representative authority and benefit from dual-obligation protection of Arab rights, land tenure, and majority status; constrained exit (cannot abandon the territory); generational time horizon; regional scope.
 *   - arab_communities — Powerless, trapped stakeholders holding property and place under existing tenure; benefit from land-transfer restrictions and immigration quotas; biographical horizon; regional scope.
 *   - zionist_organizations — Powerful, globally-connected; pay through constrained land acquisition and immigration limits; seek unrestricted access to establish Jewish dominance; generational horizon; global scope.
 *   - british_mandatory_administrators — Institutional payer and agenda-setter; constrained by the dual-obligation reading from satisfying Zionist demands or extracting maximum surplus; sit between incompatible benefit paths; generational horizon.
 *   - league_of_nations_oversight_body — Analytical observer; tasked with ensuring mandate compliance with the dual-obligation principle; external verification point for whether the constraint is actually enforced or performatively invoked.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.68).
domain_priors:suppression_score(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.72).
domain_priors:theater_ratio(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, extractiveness, 0.68).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, resistance, 0.76).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__dual_obligation_indigenous_rights, tangled_rope).
narrative_ontology:human_readable(balfour_mandate_instruments__dual_obligation_indigenous_rights, "Balfour Mandate Dual Obligation: Indigenous Rights Protection Over National Home Primacy").
narrative_ontology:topic_domain(balfour_mandate_instruments__dual_obligation_indigenous_rights, "international_law/colonial_administration/state_formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__dual_obligation_indigenous_rights).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__dual_obligation_indigenous_rights, 'c76500a0-0e27-4a27-9c6e-d6ce961e4666').
narrative_ontology:cs_kernel_codification('c76500a0-0e27-4a27-9c6e-d6ce961e4666', fixed_text).
narrative_ontology:cs_authority_grounding('c76500a0-0e27-4a27-9c6e-d6ce961e4666', lineage).
narrative_ontology:cs_interpretation_layer_present('c76500a0-0e27-4a27-9c6e-d6ce961e4666').
narrative_ontology:cs_reading_relation('c76500a0-0e27-4a27-9c6e-d6ce961e4666', balfour_mandate_instruments__jewish_national_home_primacy, forecloses).
narrative_ontology:cs_reading_relation('c76500a0-0e27-4a27-9c6e-d6ce961e4666', balfour_mandate_instruments__mandatory_interpretive_discretion, influences).
narrative_ontology:cs_axiom('c76500a0-0e27-4a27-9c6e-d6ce961e4666', foundational, arab_rights_superior_to_national_home).
narrative_ontology:cs_axiom_status(arab_rights_superior_to_national_home, holdable).
narrative_ontology:cs_axiom_grounding('c76500a0-0e27-4a27-9c6e-d6ce961e4666', arab_rights_superior_to_national_home, deontological).
narrative_ontology:cs_axiom('c76500a0-0e27-4a27-9c6e-d6ce961e4666', foundational, arab_majority_grounds_self_determination).
narrative_ontology:cs_axiom_status(arab_majority_grounds_self_determination, holdable).
narrative_ontology:cs_axiom_grounding('c76500a0-0e27-4a27-9c6e-d6ce961e4666', arab_majority_grounds_self_determination, deontological).
narrative_ontology:cs_reference_frame('c76500a0-0e27-4a27-9c6e-d6ce961e4666', mandate_dual_obligation_regime).
narrative_ontology:cs_drift_state('c76500a0-0e27-4a27-9c6e-d6ce961e4666', id_1948_mandate_end, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c76500a0-0e27-4a27-9c6e-d6ce961e4666', '').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__dual_obligation_indigenous_rights, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_elites).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__dual_obligation_indigenous_rights, arab_communities).
narrative_ontology:constraint_victim(balfour_mandate_instruments__dual_obligation_indigenous_rights, zionist_organizations).
narrative_ontology:constraint_victim(balfour_mandate_instruments__dual_obligation_indigenous_rights, british_mandatory_administrators).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__dual_obligation_indigenous_rights, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(balfour_mandate_instruments__dual_obligation_indigenous_rights, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(balfour_mandate_instruments__dual_obligation_indigenous_rights_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(balfour_mandate_instruments__dual_obligation_indigenous_rights, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(balfour_mandate_instruments__dual_obligation_indigenous_rights_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.68) because the dual-obligation reading constrains two major parties (zionist_organizations and british_mandatory_administrators) in ways that impose substantive costs: Zionist land acquisition is restricted, immigration is capped, and demographic transformation is slowed; British administrators face constraints on extracting maximum value and satisfying Zionist demands. The beneficiaries (arab_communities and palestinian_arab_elites) receive protection that is real but not extraction-free—the constraint itself requires enforcement overhead and the protection depends on external (League) oversight. Suppression is higher than extractiveness (0.72) because the constraint's persistence depends on actively suppressing alternative readings (jewish_national_home_primacy would reverse the priority entirely) and suppressing Zionist efforts to acquire land and accelerate immigration outside quota. Theater-ratio is moderate (0.41) because the dual-obligation reading is genuinely codified in the mandate text and has real enforcement mechanisms (land-transfer law, immigration quotas, League review), but the measurement trajectory shows rising theater over the interval—as the constraint became harder to maintain in practice, more enforcement energy went to performative compliance (public statements of adherence to the dual-obligation principle) while administrative practice subtly shifted toward the jewish_national_home_primacy reading (discretionary waivers, land transfers tolerated, immigration loosened). The rising theater ratio over the 26-year interval is the key signal: the constraint is drifting from enforced coordination toward Piton (maintained mainly by performative commitment while the actual reading operationally dominant shifts). The accessibility_collapse (0.58) reflects that alternatives to the dual-obligation reading exist (the sibling readings are intellectually available and advocated by organized parties) and are not fully suppressed; resistance is high (0.76) because the Zionist organizational establishment and some British officials actively resist the reading and seek to overthrow it in favor of jewish_national_home_primacy.
 *
 * PERSPECTIVAL GAP:
 *   From the Palestinian Arab elites and communities' seats, the dual-obligation reading is the structural backbone protecting their land tenure and majority status; from their perspective, the constraint is a real protective regime that must be actively enforced and defended. From the British mandatory administrators' seat, the reading is a binding legal framework they are charged with implementing, but it conflicts with Zionist expectations and British imperial interests in maintaining Jewish-Arab cooperation; they experience the constraint as simultaneously binding and uncomfortable. From the zionist_organizations' seat, the dual-obligation reading is an obstacle that must be circumvented through discretionary waivers, negotiation, and shifting the operational reading toward jewish_national_home_primacy; they experience it as a constraint on their expansionist project. From the League of Nations' seat, the reading is a mandate obligation they must verify is being honored, but they lack enforcement power and depend on British truthful reporting. The engine computes per-seat classifications from this structural asymmetry: Arab elites compute as beneficiaries (low directionality, low/negative extraction); Zionist organizations as targets (high directionality, high extraction); British administrators as caught between (symmetric or slightly-toward-target); League as analytical observer (no extraction). The claimed_type is tangled_rope because the reading genuinely coordinates (resolves the Balfour tension through a priority structure) AND extracts asymmetrically (constrains two parties to benefit two others) AND requires active enforcement (suppression to block alternative readings and alternative land transfers).
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality of each stakeholder toward this constraint is derived from whether they benefit or bear costs: Palestinian Arab communities (beneficiaries, majority-protected) have low d (near 0.1–0.2); palestinian_arab_elites (beneficiaries, governance-claim-protected) have low-to-moderate d (near 0.2–0.3); zionist_organizations (victims, expansion-constrained) have high d (near 0.75–0.85); british_mandatory_administrators (constrained from satisfying Zionist demands, but empowered to implement the dual-obligation reading) have moderate d (near 0.45–0.55, symmetric). The exit-options modulate these base directionalities: Arab communities are identity_locked and trapped (cannot exit the territory; exit options are theoretically 'mobile' within the region but practically 'trapped' because they are defending their existing place). Zionist organizations are constrained (cannot acquire land outside quota, cannot immigrate above limit) but have arbitrage-grade exits available (alternative territories, secondary markets, negotiation). British administrators are institutional with generational time horizons (they can be replaced, but the institution persists; exit is constrained by the mandate system itself). The derived directionality feeds the engine's effective-extraction computation, which amplifies extraction for trapped high-d agents and dampens it for beneficiaries with exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is genuine: the Balfour Declaration and mandate are structurally contradictory, and the dual-obligation reading offers a coherent resolution (subordinate 'national home' to Arab rights). However, the measurement trajectory shows rising theater_ratio and rising suppression_requirement over the interval—signals that the constraint's functional mandate may be decaying. By year 18–26, the dual-obligation reading is maintained more by performative commitment (League reports, British public statements) than by actual enforcement (land transfers are increasingly tolerated, immigration restrictions are loosened via discretionary waivers, Arab majority status is demographically eroding). The constraint is not yet a Piton (it still carries some functional force), but it is drifting toward Piton if the theater-ratio continued to rise and the actual operational reading shifted decisively to jewish_national_home_primacy. The mandatrophy question: does the dual-obligation reading remain a live constraint, or has it become a theatrical cover story for the jewish_national_home_primacy reading operating de facto? The measurement trajectory suggests the latter is occurring by 1940s (interval end), which would support a reclassification to Piton if this story were extended to later periods.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_kernel_contest,
    'Is this constraint one reading of the Balfour mandate kernel, or is the kernel itself multiple irreducible obligations without a coherent reading?',
    'Comparative study of mandate-text interpretation across League of Nations jurisprudence, parallel mandate documents (Class A mandates), and the principle of self-determination as applied elsewhere in the post-WWI settlement. If the dual-obligation structure is consistent with League precedent, the reading is one stable interpretation; if the kernel itself is genuinely irreconcilable, the constraint is mislabeled and two separate constraint stories (one per incompatible obligation) would be more accurate.',
    'If this reading is coherent and precedent-grounded, the engine computes it as a tangled_rope with substantive enforcement requirements. If the kernel is irreconcilable, the constraint might be better modeled as a Piton (contradictory obligations maintained performatively) or split into two snare stories (one for each subordinated party).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_contest, conceptual, 'Whether the mandate kernel admits coherent dual-obligation readings or is fundamentally over-constrained.').

omega_variable(
    enforcement_discretion_collapse,
    'Did the British mandatory power treat the dual-obligation reading as binding, or did it invoke mandatory administrative discretion to privilege jewish_national_home_primacy in practice while maintaining verbal commitment to dual obligation?',
    'Archival analysis of British administrative practice: land-transfer enforcement records, immigration quotas actually implemented, Arab petition responses, internal policy memos. If the dual-obligation reading was structurally enforced (land transfers blocked, immigration capped to prevent demographic displacement), the constraint''s extraction is moderate. If British officials systematized exceptions and discretionary waivers that de facto implemented the jewish_national_home_primacy reading while performatively honoring dual obligation, the constraint''s theater_ratio and suppression_requirement were higher than measured—the enforcement was theatrical.',
    'High enforcement fidelity = tangled_rope (genuine coordination with real cost to zionist_organizations); systematic discretionary waiver = Piton or snare (mandate as rhetorical cover for de facto privilege to one party). The direction of this finding determines whether the constraint extracted from zionist organizations or from palestinian_arab_communities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_discretion_collapse, empirical, 'Whether the dual-obligation reading was actually enforced or performatively invoked while discretion privileged the alternative reading.').

omega_variable(
    sibling_reading_logical_structure,
    'Do the sibling readings (jewish_national_home_primacy and mandatory_interpretive_discretion) logically foreclose this reading, coexist with it, or influence it?',
    'Formal analysis of the mandate text''s logical structure: can one party hold jewish_national_home_primacy (national home as proto-state requiring demographic/territorial dominance) while another party holds dual_obligation_indigenous_rights (national home subordinated to Arab rights) within the SAME mandate framework? If no—if adopting one reading logically entails rejecting the other—they foreclose each other. If yes—if the text is elastic enough to support both readings held by different parties simultaneously—they coexist. If mandatory_interpretive_discretion (British power chooses freely) is adopted, does it eliminate the possibility of either substantive reading being binding, or does it create structural pressure on how substantive readings must accommodate discretion?',
    'Forecloses = this reading is mutually exclusive with siblings; one reading wins and the others are false. Coexists_with = all readings remain live, held by different parties in an unresolved contest (the historical record). Influences = the discretion reading creates pressure on how substantive readings are formalized, but does not eliminate them. This structural classification feeds the engine''s contamination analysis: if readings foreclose, a corpus documenting all three readings would be internally contradictory; if they coexist, the corpus documents a genuine historical contest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_logical_structure, conceptual, 'The logical structure of the sibling readings'' relationship to this reading within the mandate kernel.').

omega_variable(
    arab_agency_and_resistance,
    'To what extent did Palestinian Arab elites and communities actively enforce or demand enforcement of the dual-obligation reading, versus passively benefiting from British administrative constraints on zionist_organizations?',
    'Archive of Arab organizational statements, petitions to the League, negotiation records, and armed/political resistance. If Arab actors systematically demanded dual-obligation enforcement as a substantive principle (not merely accepting it as a brake on Zionist expansion), the reading is an active constraint they maintained. If Arab resistance emerged only when Zionist pressure mounted and Arab organizations were reactive, the constraint may be better characterized as a byproduct of British administrative preference or League oversight rather than a coordinated dual-obligation regime.',
    'High Arab agency in maintaining the reading strengthens the tangled_rope classification (the reading is actively enforced, not merely administratively convenient). Low agency suggests the constraint may be more accurately modeled as a Piton (the dual-obligation language persists but Arab power to enforce it is limited, and British administrators hold the actual discretion).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(arab_agency_and_resistance, empirical, 'Whether Arab communities actively maintained the dual-obligation reading or benefited passively from constraints imposed by British administrative structure or League oversight.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0, 26).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balf_tr_t0, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 0, 0.18).
narrative_ontology:measurement(balf_tr_t4, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 4, 0.24).
narrative_ontology:measurement(balf_tr_t8, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 8, 0.3).
narrative_ontology:measurement(balf_tr_t12, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 12, 0.35).
narrative_ontology:measurement(balf_tr_t18, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 18, 0.39).
narrative_ontology:measurement(balf_tr_t26, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 26, 0.41).

% Extraction over time
narrative_ontology:measurement(balf_be_t0, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(balf_be_t4, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 4, 0.51).
narrative_ontology:measurement(balf_be_t8, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(balf_be_t12, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 12, 0.63).
narrative_ontology:measurement(balf_be_t18, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 18, 0.66).
narrative_ontology:measurement(balf_be_t26, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 26, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(balf_su_t0, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 0, 0.54).
narrative_ontology:measurement(balf_su_t4, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 4, 0.6).
narrative_ontology:measurement(balf_su_t8, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 8, 0.64).
narrative_ontology:measurement(balf_su_t12, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 12, 0.68).
narrative_ontology:measurement(balf_su_t18, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 18, 0.71).
narrative_ontology:measurement(balf_su_t26, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 26, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__dual_obligation_indigenous_rights, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.12).
narrative_ontology:affects_constraint(balfour_mandate_instruments__dual_obligation_indigenous_rights, balfour_mandate_instruments__jewish_national_home_primacy).
narrative_ontology:affects_constraint(balfour_mandate_instruments__dual_obligation_indigenous_rights, balfour_mandate_instruments__mandatory_interpretive_discretion).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested balfour_mandate_instruments kernel. The kernel—the Balfour Declaration and League mandate instruments—contains two logically incompatible commitments: facilitate a Jewish national home AND protect existing Arab rights. Three structurally distinct constraints instantiate the three competing readings. dual_obligation_indigenous_rights (this story) treats Arab rights as superior and Arab majority status as grounding sovereignty claims. jewish_national_home_primacy reverses the priority, subordinating Arab rights to Jewish demographic and territorial transformation. mandatory_interpretive_discretion denies that any substantive reading is binding—only British administrative discretion is the operational constraint. Each reading has its own ε-invariant structure, stakeholder asymmetries, and classification. They are linked via network.affects_constraints because a shift in any one reading's operational dominance structurally affects the others (as mandatory_interpretive_discretion gains operational ground, substantive readings lose binding force). All three stories carry omegas documenting the kernel contest and the reading-specificity of their ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
