% ============================================================================
% CONSTRAINT STORY: versailles_reparations_clauses__punitive_liability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_versailles_reparations_clauses__punitive_liability_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: versailles_reparations_clauses__punitive_liability_reading
 *   human_readable: Versailles Reparations — Punitive Liability Reading
 *   domain: international_relations/legal_history/political_economy
 *
 * SUMMARY:
 *   The Versailles Treaty's Article 231 ('war guilt clause') and Article 232
 *   (reparations obligation) established Germany's liability for 'all loss
 *   and damage' suffered by the Allies. The punitive liability reading —
 *   championed by French Premier Clemenceau and hardliners in the Reparations
 *   Commission — treats this as a quasi-unlimited moral and financial claim:
 *   Germany bears unique responsibility for the war and must pay whatever the
 *   Allies determine, with no binding reference to German capacity. This
 *   reading governed the 1921 London Schedule of Payments (132 billion gold
 *   marks), the 1923 Ruhr occupation, and the enforcement machinery until the
 *   Lausanne Conference (1932) effectively ended reparations. The constraint
 *   is a snare: high extraction from a trapped population, active enforcement
 *   (occupation, financial control), and no genuine coordination function.
 *   The coordination story ('reparations stabilize Europe') is cover; the
 *   function is transfer.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(versailles_reparations_clauses__punitive_liability_reading, 0.78).
domain_priors:suppression_score(versailles_reparations_clauses__punitive_liability_reading, 0.82).
domain_priors:theater_ratio(versailles_reparations_clauses__punitive_liability_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__punitive_liability_reading, snare).
narrative_ontology:human_readable(versailles_reparations_clauses__punitive_liability_reading, "Versailles Reparations — Punitive Liability Reading").
narrative_ontology:topic_domain(versailles_reparations_clauses__punitive_liability_reading, "international_relations/legal_history/political_economy").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__punitive_liability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__punitive_liability_reading, '2d4884a7-6766-4769-8f77-cebdd12cd52f').
narrative_ontology:cs_kernel_codification('2d4884a7-6766-4769-8f77-cebdd12cd52f', formalized).
narrative_ontology:cs_authority_grounding('2d4884a7-6766-4769-8f77-cebdd12cd52f', lineage).
narrative_ontology:cs_interpretation_layer_present('2d4884a7-6766-4769-8f77-cebdd12cd52f').
narrative_ontology:cs_reading_relation('2d4884a7-6766-4769-8f77-cebdd12cd52f', versailles_reparations_clauses__limited_responsibility_reading, forecloses).
narrative_ontology:cs_reading_relation('2d4884a7-6766-4769-8f77-cebdd12cd52f', versailles_reparations_clauses__repudiation_reading, coexists_with).
narrative_ontology:cs_axiom('2d4884a7-6766-4769-8f77-cebdd12cd52f', foundational, germany_unique_moral_culpability).
narrative_ontology:cs_axiom_status(germany_unique_moral_culpability, holdable).
narrative_ontology:cs_axiom_grounding('2d4884a7-6766-4769-8f77-cebdd12cd52f', germany_unique_moral_culpability, deontological).
narrative_ontology:cs_axiom('2d4884a7-6766-4769-8f77-cebdd12cd52f', foundational, reparations_unbounded_by_capacity).
narrative_ontology:cs_axiom_status(reparations_unbounded_by_capacity, overridden).
narrative_ontology:cs_axiom_grounding('2d4884a7-6766-4769-8f77-cebdd12cd52f', reparations_unbounded_by_capacity, conventional).
narrative_ontology:cs_reference_frame('2d4884a7-6766-4769-8f77-cebdd12cd52f', versailles_treaty_1919).
narrative_ontology:cs_drift_state('2d4884a7-6766-4769-8f77-cebdd12cd52f', lausanne_conference_1932, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('2d4884a7-6766-4769-8f77-cebdd12cd52f', '').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__punitive_liability_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__punitive_liability_reading, allied_creditor_states).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__punitive_liability_reading, french_occupation_authorities).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__punitive_liability_reading, british_treasury_officials).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, german_workers).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, german_taxpayers).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, weimar_republic_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__punitive_liability_reading, american_bankers).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__punitive_liability_reading, german_industrialists).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, german_industrialists).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__punitive_liability_reading, war_guilt_doctrine).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__punitive_liability_reading, collective_national_responsibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive reparations payments that offset their own war debts and reconstruction costs. France uses payments to fund occupation of the Rhineland and industrial recovery; Britain uses them to service war loans from the United States. They control the Reparations Commission and can impose sanctions (occupation, trade restrictions) for non-compliance. Exit is arbitrage-grade: they can reschedule, reduce, or enforce at will.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, allied_creditor_states, beneficiary,
    institutional, generational, arbitrage, continental).

% Administer the Ruhr occupation (1923-1925) and the Inter-Allied Control Commissions that monitor German compliance. They set enforcement policy on the ground: seizing coal, railways, and customs revenue when payments fall short. Their authority derives from the Treaty and the Reparations Commission. They bear some political cost from international criticism but hold coercive leverage.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, french_occupation_authorities, agenda_setter,
    institutional, biographical, mobile, regional).

% Bear the burden through hyperinflation (1923), wage suppression, unemployment, and social spending cuts. Real wages collapse as the Reichsmark is printed to buy foreign currency for reparations. Exit is constrained: emigration is possible but costly; political resistance (strikes, Kapp Putsch, communist uprisings) is met with state and paramilitary violence. No structural voice in reparations negotiations.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, german_workers, payer,
    powerless, biographical, constrained, national).

% Finance reparations through taxation and inflation tax. Property owners lose savings; middle class is decimated by 1923 hyperinflation. Tax compliance is enforced by the Weimar state under Allied supervision. Exit is constrained: capital flight is restricted; tax resistance risks Allied sanctions. The burden falls disproportionately on those without foreign assets.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, german_taxpayers, payer,
    moderate, biographical, constrained, national).

% Caught between Allied demands and domestic legitimacy. Must administer payments they cannot afford while maintaining enough sovereignty to govern. The reparations constraint structures their entire fiscal policy: foreign loans (Dawes, Young Plans) become the only way to meet obligations, creating a debt trap. They set domestic policy (taxation, borrowing) but under external duress. Exit is trapped: repudiation triggers occupation and loss of legitimacy; compliance destroys domestic support.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, weimar_republic_institutions, payer,
    organized, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(versailles_reparations_clauses__punitive_liability_reading, weimar_republic_institutions, agenda_setter).

% Provide the loans (Dawes Plan 1924, Young Plan 1929) that enable Germany to pay reparations, which then flow to Allies, who pay war debts to the US. They profit from underwriting fees and interest spreads. They are not party to the Treaty but their capital makes the system function. Exit is arbitrage: they can withdraw lending (as in 1928-1929) forcing renegotiation.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, american_bankers, beneficiary,
    powerful, biographical, arbitrage, global).

% Heavy industry (Stinnes, Thyssen, Krupp) gains from inflation: debts denominated in marks are wiped out, exports become competitive, and they acquire foreign assets. They also fund right-wing parties that oppose reparations. Some pay reparations in kind (coal, steel) but pass costs to workers. Exit is mobile: they can move capital abroad, restructure, or profit from the chaos. Dual position: beneficiaries of inflation, payers of in-kind deliveries.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, german_industrialists, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(versailles_reparations_clauses__punitive_liability_reading, german_industrialists, payer).

% The technical body (representatives of France, Britain, Italy, Belgium, Japan, later US observer) that determines amounts, schedules, and compliance. It interprets Article 231 and 232, issues bonds, authorizes sanctions. Its decisions are binding on Germany. It is the institutional embodiment of the constraint's enforcement mechanism.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, reparations_commission, agenda_setter,
    institutional, generational, analytical, continental).

% Denounce the Treaty as a 'Diktat' and reparations as enslavement. They are excluded from the negotiation table (Treaty imposed, not negotiated) and from the Reparations Commission. Their opposition is structural: they reject the constraint's legitimacy entirely. Identity-locked: their political identity is constituted by rejection of Versailles; exit would mean abandoning their core constituency and narrative.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, german_nationalist_movements, excluded,
    organized, biographical, identity_locked, national).

% Argue (Keynes, The Economic Consequences of the Peace, 1919) that the reparations burden exceeds Germany's capacity to pay, will destabilize Europe, and serves no one's long-term interest. Their analysis is structural: they identify the transfer problem and the impossibility of extracting real resources without destroying the German economy. They have no enforcement power but their diagnosis shapes later renegotiations (Dawes, Young) and post-WWII settlement design.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, keynesian_economists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: No genuine coordination function. The reparations regime does not solve a collective action problem among the parties; it imposes a unilateral transfer from a defeated power to victors. The Reparations Commission coordinates enforcement among Allies, but this is coordination OF extraction, not coordination FOR mutual benefit.
% TRANSFER_FUNCTION: Moves real resources (coal, timber, livestock, merchant ships, foreign currency, gold) and fiscal capacity from the German economy (workers, taxpayers, firms, state) to Allied creditor states (France, Britain, Belgium, Italy) and their creditors (US government, American bankers). The transfer is enforced by occupation, trade sanctions, and financial supervision.
% ABSENT_VOICES: German workers and taxpayers had no representation at Versailles or in the Reparations Commission. Colonial subjects of the Allied powers (whose resources were also mobilized for the war) were entirely absent. The Soviet government (excluded from Versailles) denounced the Treaty as imperialist but had no seat. The 'reparations' concept itself excluded the possibility of German counter-claims for Allied blockade deaths (1918-1919).
% DISAPPEARANCE_RATIONALE: If the punitive liability reading vanished overnight, the fiscal transfer from Germany to Allies would cease, the Ruhr occupation would lose its legal basis, the Dawes/Young loan cycles would unwind, and the Weimar Republic's existential fiscal crisis would lift — though the political damage (hyperinflation trauma, nationalist radicalization) would persist. The European financial architecture of the 1920s was built around this constraint; its removal rearranges the world.
% FOUNDING_PROBLEM: The Allied powers needed to finance their war effort and reconstruction without raising taxes on their own populations to politically unsustainable levels. Article 231 and the reparations mechanism were built to shift the cost of the war onto the defeated power, justified by the war guilt doctrine.
% FOUNDING_PROBLEM_CORROBORATION: Keynes (1919) and contemporary Allied economists (e.g., John Foster Dulles later acknowledged the sum was unpayable) attested the capacity problem was known at founding. The Dawes Committee (1924) and Young Committee (1929) both implicitly acknowledged the original problem (how to make Germany pay) was unsolvable by restructuring the payment schedule. No credible non-beneficiary source maintains the founding problem (Allied war financing) was still live after 1924; the constraint persisted as a political tool, not a financial necessity.
narrative_ontology:disappearance_verdict(versailles_reparations_clauses__punitive_liability_reading, world_rearranges).
narrative_ontology:founding_problem_status(versailles_reparations_clauses__punitive_liability_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(versailles_reparations_clauses__punitive_liability_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(versailles_reparations_clauses__punitive_liability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(versailles_reparations_clauses__punitive_liability_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(versailles_reparations_clauses__punitive_liability_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(versailles_reparations_clauses__punitive_liability_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(versailles_reparations_clauses__punitive_liability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78 peak) because the claim exceeds Germany's capacity to pay without catastrophic social cost — the 1921 figure was 3-4x Germany's annual GDP. Suppression is very high (0.82) because alternatives are structurally suppressed: Germany cannot exit the Treaty (defeat), cannot negotiate (Diktat), and resistance triggers occupation. Theater ratio is moderate (0.28): the Reparations Commission and later plans (Dawes, Young) create a veneer of technical administration, but the core operation is coercive transfer. Accessibility collapse (0.72) reflects that once the war guilt doctrine is accepted, the reparations obligation follows logically — alternatives (limited liability, capacity-based payments) are foreclosed by the reading's own premise. Resistance (0.55) is substantial but fragmented: hyperinflation resistance, Ruhr passive resistance, nationalist opposition — all ultimately fail to change the constraint until the Great Depression forces systemic collapse.
 *
 * PERSPECTIVAL GAP:
 *   From the Allied creditor seat, the constraint appears as a legitimate debt enforcement mechanism (coordination of inter-Allied war debt settlement). From the German worker seat, it is a starvation mechanism. From the Weimar institution seat, it is a sovereignty trap. The engine computes these divergences from the declared power/exit/role structure — the reading does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Allied creditor states are structural beneficiaries (d ~ 0.1): they collect transfers, control enforcement, have arbitrage-grade exit. French occupation authorities are agenda setters (d ~ 0.2): they administer enforcement, bear some political cost, but hold coercive power. German workers and taxpayers are full targets (d ~ 0.9-1.0): they bear the costs, have constrained exit, no voice. Weimar institutions are trapped payers who also set domestic policy under duress (d ~ 0.7). American bankers are beneficiaries with arbitrage exit (d ~ 0.1). German industrialists are dual: they benefit from inflation (d ~ 0.2) but pay in-kind (d ~ 0.6). Nationalist movements are identity-locked excluded (d ~ 0.8): they reject the constraint but their identity is fused to that rejection. The engine will compute per-seat effective extraction from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Allied war financing) was dead by 1924 — the Dawes Plan acknowledged Germany could not pay the original sum. Yet the constraint persisted for eight more years through successive plans (Dawes, Young, Hoover Moratorium, Lausanne) because it had become a political instrument: France used it to maintain security against Germany; Britain used it to balance US war debts; German nationalists used it to delegitimize the Republic. The mandate atrophied into a zombie constraint — the extraction continued (though reduced) after the justification vanished. This is a classic mandatrophy case: the arrangement outlived its function and persisted by inertia and political utility for agenda setters.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    war_guilt_naturalness,
    'Is Article 231''s war guilt attribution a genuine legal/moral fact (Mountain-like) or a constructed political instrument (Snare-like)?',
    'Compare the drafting history (Allied deliberations on Article 231) with the legal doctrine of state responsibility in 1919. If the clause was drafted specifically to ground unlimited financial liability rather than reflect pre-existing law, it is constructed.',
    'If constructed, the snare classification is reinforced: the ''natural law'' framing (Germany''s unique guilt) is a cover story for extraction. If genuine, the constraint has a Mountain-like core (legal fact) with Snare-like enforcement (excessive quantum).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(war_guilt_naturalness, conceptual, 'Whether the war guilt doctrine is a natural legal fact or a constructed liability instrument.').

omega_variable(
    capacity_vs_liability_boundary,
    'Where is the structural boundary between ''liability'' (what is owed) and ''capacity'' (what can be paid) in this reading?',
    'Analyze the Reparations Commission''s own deliberations (1921-1932): did they ever treat capacity as a binding limit on liability, or only as a scheduling convenience? The Young Plan (1929) formally separated ''unconditional'' and ''conditional'' portions — the boundary is there.',
    'If capacity is never a binding limit, the reading is purely extractive (snare). If capacity becomes a structural limit (Dawes/Young), the reading mutates toward tangled_rope (coordination of payment capacity + extraction of surplus).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capacity_vs_liability_boundary, empirical, 'Whether the punitive reading internally recognizes a capacity constraint on extraction.').

omega_variable(
    reparations_war_debt_circularity,
    'Is the reparations-debt circularity (Germany pays Allies, Allies pay US, US lends to Germany) a genuine coordination mechanism or a Ponzi structure that masks extraction?',
    'Trace the net resource flows: did American capital actually increase German productive capacity, or did it only service the circular transfer? Schuker (1988) shows net capital inflow to Germany 1924-1931 was ~2% of GDP — the circularity primarily recycled Allied payments.',
    'If Ponzi, the coordination function is illusory and the snare classification holds. If genuine coordination, the later period (1924-1929) has a rope-like component (Dawes Plan as resource_allocation coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reparations_war_debt_circularity, empirical, 'Whether the financial circularity of the 1920s was functional coordination or extractive recycling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__punitive_liability_reading, 1919, 1932).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vers_tr_t1919, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1919, 0.15).
narrative_ontology:measurement(vers_tr_t1921, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1921, 0.22).
narrative_ontology:measurement(vers_tr_t1923, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1923, 0.35).
narrative_ontology:measurement(vers_tr_t1924, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1924, 0.3).
narrative_ontology:measurement(vers_tr_t1929, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1929, 0.28).
narrative_ontology:measurement(vers_tr_t1931, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1931, 0.35).
narrative_ontology:measurement(vers_tr_t1932, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1932, 0.4).

% Extraction over time
narrative_ontology:measurement(vers_be_t1919, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1919, 0.65).
narrative_ontology:measurement(vers_be_t1921, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1921, 0.78).
narrative_ontology:measurement(vers_be_t1923, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1923, 0.85).
narrative_ontology:measurement(vers_be_t1924, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1924, 0.72).
narrative_ontology:measurement(vers_be_t1929, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1929, 0.68).
narrative_ontology:measurement(vers_be_t1931, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1931, 0.55).
narrative_ontology:measurement(vers_be_t1932, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1932, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(vers_su_t1919, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1919, 0.6).
narrative_ontology:measurement(vers_su_t1921, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1921, 0.75).
narrative_ontology:measurement(vers_su_t1923, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1923, 0.9).
narrative_ontology:measurement(vers_su_t1924, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1924, 0.8).
narrative_ontology:measurement(vers_su_t1929, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1929, 0.7).
narrative_ontology:measurement(vers_su_t1931, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1931, 0.6).
narrative_ontology:measurement(vers_su_t1932, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1932, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(versailles_reparations_clauses__punitive_liability_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(versailles_reparations_clauses__punitive_liability_reading, 0.15).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, versailles_reparations_clauses__limited_responsibility_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, versailles_reparations_clauses__repudiation_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, dawes_plan_1924).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, young_plan_1929).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, weimar_hyperinflation_1923).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, ruhr_occupation_1923).

% DUAL FORMULATION NOTE:
% The versailles_reparations_clauses kernel decomposes into three readings: punitive_liability (this story, high extraction, snare), limited_responsibility (capacity-bounded, tangled_rope/rope), and repudiation (no obligation, mountain/snare depending on seat). This reading's structural dominance (1919-1924) creates the conditions for the limited reading's adoption (Dawes/Young). The repudiation reading is the nationalist counter-position that eventually captures the German state (1933). All three are linked in the network.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(versailles_reparations_clauses__punitive_liability_reading, organized, 0.7).
constraint_indexing:directionality_override(versailles_reparations_clauses__punitive_liability_reading, powerful, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
