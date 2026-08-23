% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__overdetermined_composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dueling_disappearance_mechanism__overdetermined_composite_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dueling_disappearance_mechanism__overdetermined_composite_reading
 *   human_readable: Overdetermined Composite Mechanism of Dueling's Decline
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   Dueling disappeared from the Atlantic world between 1800 and 1900. This
 *   constraint story models the overdetermined-composite reading: four
 *   independent sufficient conditions — legal prohibition (statutes
 *   criminalizing dueling), institutional modernization (courts, credit
 *   systems, professional bodies), cultural shift (dignity culture displacing
 *   honor culture), and Civil War trauma (mass death delegitimizing
 *   ritualized killing) — acted simultaneously. No single cause was
 *   necessary; each was sufficient. The constraint is the historical
 *   convergence itself, which operated as a tangled rope: it coordinated a
 *   new social order (public dispute resolution, state monopoly on violence)
 *   while extracting from honor-culture adherents whose identity and status
 *   system was dismantled. The claim/metric gap is deliberate: the constraint
 *   is CLAIMED as tangled_rope (multiple beneficiaries, active enforcement,
 *   asymmetric extraction) while the authored metrics describe a historically
 *   emergent convergence with high extraction from identity-locked victims
 *   and rising theater as cultural performance replaced functional necessity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.68).
domain_priors:suppression_score(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.72).
domain_priors:theater_ratio(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__overdetermined_composite_reading, tangled_rope).
narrative_ontology:human_readable(dueling_disappearance_mechanism__overdetermined_composite_reading, "Overdetermined Composite Mechanism of Dueling's Decline").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__overdetermined_composite_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:requires_active_enforcement(dueling_disappearance_mechanism__overdetermined_composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__overdetermined_composite_reading, '6017cef9-ae1a-4b6f-af32-5eb145d4719d').
narrative_ontology:cs_kernel_codification('6017cef9-ae1a-4b6f-af32-5eb145d4719d', implicit).
narrative_ontology:cs_authority_grounding('6017cef9-ae1a-4b6f-af32-5eb145d4719d', distributed).
narrative_ontology:cs_reading_relation('6017cef9-ae1a-4b6f-af32-5eb145d4719d', dueling_disappearance_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('6017cef9-ae1a-4b6f-af32-5eb145d4719d', dueling_disappearance_mechanism__institutional_displacement_reading, coexists_with).
narrative_ontology:cs_axiom('6017cef9-ae1a-4b6f-af32-5eb145d4719d', foundational, multiple_independent_sufficient_conditions).
narrative_ontology:cs_axiom_status(multiple_independent_sufficient_conditions, holdable).
narrative_ontology:cs_axiom_grounding('6017cef9-ae1a-4b6f-af32-5eb145d4719d', multiple_independent_sufficient_conditions, empirically_contingent).
narrative_ontology:cs_axiom('6017cef9-ae1a-4b6f-af32-5eb145d4719d', foundational, causal_pathways_non_separable).
narrative_ontology:cs_axiom_status(causal_pathways_non_separable, holdable).
narrative_ontology:cs_axiom_grounding('6017cef9-ae1a-4b6f-af32-5eb145d4719d', causal_pathways_non_separable, empirically_contingent).
narrative_ontology:cs_reference_frame('6017cef9-ae1a-4b6f-af32-5eb145d4719d', overdetermined_historical_causality).
narrative_ontology:cs_drift_state('6017cef9-ae1a-4b6f-af32-5eb145d4719d', contemporary_historiography, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('6017cef9-ae1a-4b6f-af32-5eb145d4719d', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__overdetermined_composite_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, legal_authorities).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, modern_institutions).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, cultural_reformers).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, post_war_reconstructionists).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__overdetermined_composite_reading, honor_culture_adherents).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__overdetermined_composite_reading, traditional_elites).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__overdetermined_composite_reading, duelists).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__overdetermined_composite_reading, historical_overdetermination_thesis).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__overdetermined_composite_reading, multi_causal_explanatory_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacted and enforced anti-dueling statutes; gained monopoly on legitimate violence and dispute resolution through courts. The legal prohibition was one sufficient condition for dueling's end, and legal authorities benefited from the expanded jurisdiction and state-building it enabled.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, legal_authorities, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(dueling_disappearance_mechanism__overdetermined_composite_reading, legal_authorities, beneficiary).

% Courts, banking systems, professional licensing bodies, and commercial credit networks provided alternative dispute-resolution and reputation-management mechanisms. They outcompeted dueling as the default for settling conflicts among elites and merchants, capturing the trust and coordination functions dueling once served.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, modern_institutions, beneficiary,
    organized, generational, mobile, national).

% Dignity-culture advocates (clergy, editors, educators, women's reform societies) campaigned against dueling as barbaric and irrational. Their cultural shift displaced honor-culture axioms, making dueling socially illegible. They gained moral authority and cultural capital from the transition.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, cultural_reformers, beneficiary,
    organized, biographical, mobile, regional).

% Post-Civil War political and military elites viewed dueling as a remnant of the aristocratic violence that had helped cause the war. The trauma of mass death made ritualized killing culturally toxic. Reconstruction governments and veterans' organizations benefited from a social order that rejected private violence.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, post_war_reconstructionists, beneficiary,
    institutional, biographical, constrained, national).

% Southern planters, military officers, and political elites whose status and conflict resolution depended on honor-culture logic. They bore the costs of legal prohibition, cultural marginalization, and institutional displacement. Exit was identity-locked: abandoning dueling meant abandoning the self-concept and social grammar that defined their class.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, honor_culture_adherents, payer,
    moderate, biographical, identity_locked, regional).

% Northern and European aristocrats and gentlemen who used dueling to regulate access to leadership, credit, and marriage markets. They lost a low-friction, class-exclusive coordination mechanism and were forced into slower, more public, and more inclusive institutional channels.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, traditional_elites, payer,
    powerful, biographical, constrained, national).

% Individual men who would have fought duels to defend reputation or settle disputes. They faced criminal prosecution, social ostracism, or the loss of honor without recourse. No exit: the practice was legally banned and culturally erased around them.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, duelists, payer,
    powerless, immediate, trapped, local).

% Historians and sociologists who study dueling's decline. They observe the structural convergence of legal, institutional, cultural, and traumatic forces without participating in the historical conflict. Their analyses produce the competing readings of the kernel.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, historical_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a convergent historical process that replaced private violence (dueling) with public, institutionalized, and culturally legitimate mechanisms for dispute resolution, status allocation, and social ordering — solving the coordination problem of how elites manage conflict without killing each other.
% TRANSFER_FUNCTION: Transferred the monopoly on legitimate violence and status arbitration from honor-culture participants (duelists, traditional elites) to legal authorities, modern institutions, and cultural reformers. The transfer moved authority, revenue (court fees, professional licensing), and moral capital from the honor system to the state and civil society.
% ABSENT_VOICES: Enslaved people and free Black communities in the antebellum South — dueling was a white elite practice that reinforced the racial hierarchy; their perspectives on the honor system's collapse are structurally excluded from the historical record. Also, working-class men who resolved disputes through rough-and-tumble fighting rather than dueling — their parallel tradition is erased by the elite-focused narrative.
% DISAPPEARANCE_RATIONALE: If the overdetermined convergence of legal prohibition, institutional modernization, cultural shift, and Civil War trauma had not occurred simultaneously, dueling would likely have persisted in at least some regions or classes. The world rearranges because each mechanism alone was sufficient; their convergence made the outcome robust and the post-dueling order structurally dependent on all four.
% FOUNDING_PROBLEM: The problem was not founded — it emerged. The overdetermined decline was not a designed arrangement but a historical convergence. The 'founding problem' each mechanism solved was distinct: legal authorities sought state monopoly on violence; modern institutions sought efficient dispute resolution; cultural reformers sought moral progress; post-war elites sought social stability after mass trauma.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the early republic (e.g., Freeman, 'Affairs of Honor'; Greenberg, 'Honor and Slavery') document the multi-causal convergence from archives outside the beneficiary institutions. Legal historians (e.g., Novick, 'Honor's Law') confirm anti-dueling statutes were enforced unevenly and often symbolically, supporting the claim that law alone was insufficient. Cultural historians (e.g., Rotundo, 'American Manhood') trace the dignity-culture shift independently of legal records. The convergence is corroborated by the failure of any single-cause account to explain the timing and universality of dueling's end across the Atlantic world.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__overdetermined_composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__overdetermined_composite_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__overdetermined_composite_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dueling_disappearance_mechanism__overdetermined_composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__overdetermined_composite_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dueling_disappearance_mechanism__overdetermined_composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dueling_disappearance_mechanism__overdetermined_composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the convergence imposed massive costs on honor-culture adherents (legal penalties, status loss, identity dissolution) while beneficiaries captured authority and resources. Suppression (0.72) is high because the constraint's persistence depended on active enforcement: statutes, police, cultural shaming, and veterans' organizations all policed the boundary. Theater ratio (0.38) is moderate: early anti-dueling laws were often symbolic (theater), but post-war enforcement became functional; later, 'civility' became a performative marker of class distinction. Accessibility collapse (0.75) is high: once the new institutional and cultural order was established, returning to dueling became structurally unimaginable. Resistance (0.55) is moderate: honor-culture adherents resisted through evasion, coded challenges, and political opposition, but resistance fragmented across the four mechanisms.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (honor_culture_adherents, traditional_elites, duelists) experience the convergence as extraction and identity dissolution — their exit options are identity_locked, constrained, or trapped. The agenda_setter/beneficiary seats (legal_authorities, modern_institutions, cultural_reformers, post_war_reconstructionists) experience it as coordination and legitimation — their exit options are arbitrage or mobile. The engine computes this divergence from the structural data: honor-culture adherents are identity_locked (abandoning dueling = abandoning self), making their effective extraction near-maximal; legal authorities have arbitrage-grade exit (they write the rules), making their effective extraction negative (subsidy). The analytical observer sees the full overdetermined structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Four beneficiary groups map to four mechanisms: legal_authorities (legal prohibition), modern_institutions (institutional modernization), cultural_reformers (cultural shift), post_war_reconstructionists (Civil War trauma). Each mechanism independently sufficed to end dueling; their convergence made the outcome robust. Victims are honor_culture_adherents (identity_locked — honor was constitutive), traditional_elites (constrained — could adapt but at high status cost), duelists (trapped — no exit). Directionality derives from this: beneficiaries have low d (arbitrage/mobile exit), victims have high d (identity_locked/constrained/trapped). No directionality overrides needed — the structural derivation captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint (the overdetermined convergence) has no mandate — it is not a designed arrangement but a historical emergent. Mandatrophy does not apply in the standard sense. However, each beneficiary mechanism had its own mandate: legal prohibition's mandate (state monopoly on violence) remains live; institutional modernization's mandate (efficient dispute resolution) remains live; cultural reform's mandate (moral progress) is contested; post-war reconstruction's mandate (social stability) is dead (Reconstruction ended). The convergence itself persists as historical fact, not as an enforced arrangement. The engine's mandatrophy detector should register 'not_applicable' or 'resolved' for the composite, while individual mechanisms show mixed status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_non_separability,
    'Are the four causal pathways (legal, institutional, cultural, traumatic) genuinely non-separable in their effects, or can historical counterfactuals isolate their individual contributions?',
    'Comparative historical analysis of regions where only a subset of mechanisms operated (e.g., European states with legal prohibition but no Civil War trauma; US regions with cultural shift but delayed institutional modernization). If dueling persisted where any single mechanism was absent, non-separability is falsified.',
    'If pathways are separable, the constraint decomposes into four distinct constraints (one per mechanism), each with its own ε and victim set. If non-separable, the tangled_rope classification stands as a genuine composite with irreducible extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_non_separability, empirical, 'Whether the overdetermination claim holds under counterfactual scrutiny.').

omega_variable(
    victim_set_indeterminacy,
    'Does the victim set depend on which mechanism dominated in a given region/class, making the composite constraint''s victim set contextually variable rather than fixed?',
    'Micro-historical analysis of dueling''s end in specific localities: where legal prohibition led, victims are duelists (trapped); where cultural shift led, victims are honor_culture_adherents (identity_locked); where institutional displacement led, victims are traditional_elites (constrained). Compare victim profiles across cases.',
    'If victim set varies systematically by dominant mechanism, the composite constraint may be a family of constraints rather than a single constraint. This affects whether the engine computes one classification or multiple.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_set_indeterminacy, conceptual, 'Whether the composite constraint has a stable victim set or a mechanism-dependent one.').

omega_variable(
    committer_frame_location,
    'Where in the structural data does this reading''s disagreement with sibling readings locate?',
    'Map each reading''s causal claim to the beneficiary/victim declarations and extractiveness metric: contraction_reading locates disagreement in cultural_reformers as sole beneficiary and honor_culture_adherents as sole victim; institutional_displacement_reading locates it in modern_institutions as sole beneficiary and traditional_elites as sole victim; this reading locates it in the conjunction of all four beneficiary groups and the indeterminacy of the victim set.',
    'Clarifies that the kernel contest is not about metrics but about causal ontology: single-cause vs. multi-cause vs. overdetermined. The engine''s per-seat classification will differ across readings because beneficiary/victim declarations differ.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_location, conceptual, 'Structural location of the kernel contest for this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__overdetermined_composite_reading, 1800, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dueling_disappearance_overdetermined_tr_t1800, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1800, 0.1).
narrative_ontology:measurement(dueling_disappearance_overdetermined_tr_t1825, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1825, 0.18).
narrative_ontology:measurement(dueling_disappearance_overdetermined_tr_t1850, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1850, 0.28).
narrative_ontology:measurement(dueling_disappearance_overdetermined_tr_t1865, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1865, 0.35).
narrative_ontology:measurement(dueling_disappearance_overdetermined_tr_t1875, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1875, 0.42).
narrative_ontology:measurement(dueling_disappearance_overdetermined_tr_t1900, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1900, 0.38).

% Extraction over time
narrative_ontology:measurement(dueling_disappearance_overdetermined_be_t1800, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1800, 0.15).
narrative_ontology:measurement(dueling_disappearance_overdetermined_be_t1825, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1825, 0.28).
narrative_ontology:measurement(dueling_disappearance_overdetermined_be_t1850, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1850, 0.45).
narrative_ontology:measurement(dueling_disappearance_overdetermined_be_t1865, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1865, 0.62).
narrative_ontology:measurement(dueling_disappearance_overdetermined_be_t1875, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1875, 0.71).
narrative_ontology:measurement(dueling_disappearance_overdetermined_be_t1900, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1900, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(dueling_disappearance_overdetermined_su_t1800, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1800, 0.2).
narrative_ontology:measurement(dueling_disappearance_overdetermined_su_t1825, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1825, 0.35).
narrative_ontology:measurement(dueling_disappearance_overdetermined_su_t1850, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1850, 0.55).
narrative_ontology:measurement(dueling_disappearance_overdetermined_su_t1865, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1865, 0.75).
narrative_ontology:measurement(dueling_disappearance_overdetermined_su_t1875, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1875, 0.8).
narrative_ontology:measurement(dueling_disappearance_overdetermined_su_t1900, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1900, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dueling_disappearance_mechanism__overdetermined_composite_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.12).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__overdetermined_composite_reading, dueling_disappearance_mechanism__contraction_reading).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__overdetermined_composite_reading, dueling_disappearance_mechanism__institutional_displacement_reading).

% DUAL FORMULATION NOTE:
% This constraint family (dueling_disappearance_mechanism) decomposes the single historical outcome (dueling's end) into three structurally distinct explanatory constraints. The overdetermined_composite_reading claims the mechanisms were simultaneously sufficient and non-separable (tangled_rope, high ε). The contraction_reading claims cultural shift was necessary and sufficient (rope or mountain, low ε). The institutional_displacement_reading claims institutional substitution was necessary and sufficient (rope, low ε). The ε values differ because each reading identifies different beneficiary/victim structures and different causal sufficiency claims. They are linked as a family because they share the same historical referent (dueling's disappearance) but instantiate different constraints from it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
