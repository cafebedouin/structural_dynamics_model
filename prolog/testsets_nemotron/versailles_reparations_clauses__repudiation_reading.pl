% ============================================================================
% CONSTRAINT STORY: versailles_reparations_clauses__repudiation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_versailles_reparations_clauses__repudiation_reading, []).

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
 *   constraint_id: versailles_reparations_clauses__repudiation_reading
 *   human_readable: Versailles Reparations — Repudiation Reading (Total Rejection of Payment Obligations)
 *   domain: international_relations/legal_history/political_economy
 *
 * SUMMARY:
 *   The Versailles reparations clauses (Articles 231-247) imposed a
 *   quasi-unlimited liability on Germany for all Allied war costs. The
 *   repudiation reading — articulated by the German Foreign Office from 1919
 *   onward, adopted by every Weimar government in practice, and weaponized by
 *   the nationalist right — holds that a treaty signed under threat of
 *   renewed blockade and occupation is legally void, that Article 231 ('war
 *   guilt') is a moral fiction, and that Germany owes nothing beyond
 *   voluntary token payments. This reading does not merely contest the
 *   amount; it rejects the obligation's existence. Its operation suppresses
 *   Allied creditor claims completely (high ε, high suppression) while
 *   enabling German rearmament space. The constraint is a snare: the
 *   coordination story (reconstruction + binding) is cover; persistence
 *   depends on German refusal to pay and Allied inability to enforce without
 *   war.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(versailles_reparations_clauses__repudiation_reading, 0.88).
domain_priors:suppression_score(versailles_reparations_clauses__repudiation_reading, 0.92).
domain_priors:theater_ratio(versailles_reparations_clauses__repudiation_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__repudiation_reading, snare).
narrative_ontology:human_readable(versailles_reparations_clauses__repudiation_reading, "Versailles Reparations — Repudiation Reading (Total Rejection of Payment Obligations)").
narrative_ontology:topic_domain(versailles_reparations_clauses__repudiation_reading, "international_relations/legal_history/political_economy").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__repudiation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__repudiation_reading, '62d4edf0-b9f3-40a1-85fd-a9f1cb3a85cb').
narrative_ontology:cs_kernel_codification('62d4edf0-b9f3-40a1-85fd-a9f1cb3a85cb', formalized).
narrative_ontology:cs_authority_grounding('62d4edf0-b9f3-40a1-85fd-a9f1cb3a85cb', extraction).
narrative_ontology:cs_interpretation_layer_present('62d4edf0-b9f3-40a1-85fd-a9f1cb3a85cb').
narrative_ontology:cs_reading_relation('62d4edf0-b9f3-40a1-85fd-a9f1cb3a85cb', versailles_reparations_clauses__limited_responsibility_reading, forecloses).
narrative_ontology:cs_reading_relation('62d4edf0-b9f3-40a1-85fd-a9f1cb3a85cb', versailles_reparations_clauses__punitive_liability_reading, forecloses).
narrative_ontology:cs_axiom('62d4edf0-b9f3-40a1-85fd-a9f1cb3a85cb', foundational, treaty_signed_under_duress_is_legally_void).
narrative_ontology:cs_axiom_status(treaty_signed_under_duress_is_legally_void, holdable).
narrative_ontology:cs_axiom_grounding('62d4edf0-b9f3-40a1-85fd-a9f1cb3a85cb', treaty_signed_under_duress_is_legally_void, deontological).
narrative_ontology:cs_axiom('62d4edf0-b9f3-40a1-85fd-a9f1cb3a85cb', foundational, war_guilt_clause_is_moral_fiction_with_no_legal_force).
narrative_ontology:cs_axiom_status(war_guilt_clause_is_moral_fiction_with_no_legal_force, holdable).
narrative_ontology:cs_axiom_grounding('62d4edf0-b9f3-40a1-85fd-a9f1cb3a85cb', war_guilt_clause_is_moral_fiction_with_no_legal_force, deontological).
narrative_ontology:cs_axiom('62d4edf0-b9f3-40a1-85fd-a9f1cb3a85cb', secondary, german_sovereignty_requires_unrestricted_rearmament_capacity).
narrative_ontology:cs_axiom_status(german_sovereignty_requires_unrestricted_rearmament_capacity, holdable).
narrative_ontology:cs_axiom_grounding('62d4edf0-b9f3-40a1-85fd-a9f1cb3a85cb', german_sovereignty_requires_unrestricted_rearmament_capacity, instrumental).
narrative_ontology:cs_reference_frame('62d4edf0-b9f3-40a1-85fd-a9f1cb3a85cb', classical_westphalian_sovereignty).
narrative_ontology:cs_drift_state('62d4edf0-b9f3-40a1-85fd-a9f1cb3a85cb', weimar_revisionist_peak_1930, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('62d4edf0-b9f3-40a1-85fd-a9f1cb3a85cb', '').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__repudiation_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__repudiation_reading, weimar_revisionist_elites).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__repudiation_reading, nationalist_militarist_factions).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__repudiation_reading, german_industrial_cartels).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, french_belgian_creditor_states).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, british_treasury_bondholders).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, allied_veterans_pension_systems).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, reparations_commission_bureaucracy).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__repudiation_reading, treaty_under_duress_is_void).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__repudiation_reading, war_guilt_clause_is_fictional).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__repudiation_reading, german_sovereignty_absolute).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Weimar foreign ministers and chancellors (Stresemann, Brüning, von Papen) who publicly repudiate reparations while negotiating reductions. Their political survival depends on performing rejection to domestic audiences while extracting concessions from Allies. Identity-locked: their legitimacy derives from the 'stab-in-the-back' narrative that makes Versailles illegitimate; accepting full payment would destroy their political identity.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, weimar_revisionist_elites, agenda_setter,
    institutional, biographical, identity_locked, national).

% DNVP, Stahlhelm, early NSDAP — use reparations rejection as recruiting tool and justification for secret rearmament. Benefit politically from the constraint's extraction (Allied resources denied to Germany become the grievance that fuels their rise). Exit constrained: their ideology requires the constraint's existence as enemy; they cannot 'exit' the grievance without dissolving their purpose.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, nationalist_militarist_factions, beneficiary,
    organized, generational, constrained, national).

% Heavy industry (Krupp, IG Farben, United Steel Works) that evades reparations deliveries in kind (coal, timber, chemicals) while retaining productive capacity for future rearmament. Mobile exit: they can shift production, hide assets, or relocate capital; their benefit is the preservation of industrial base that would otherwise be shipped to France/Belgium.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, german_industrial_cartels, beneficiary,
    powerful, biographical, mobile, global).

% France and Belgium bear the direct cost of non-payment: ruined reconstruction budgets, occupied Ruhr yields nothing, war debts to US/UK go unpaid because German reparations don't arrive. Constrained exit: they cannot 'walk away' from the border security problem; the constraint extracts their reconstruction capacity while they remain exposed to German revanchism.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, french_belgian_creditor_states, payer,
    institutional, generational, constrained, continental).

% British investors holding German external bonds and reparations-linked securities. Pay through defaulted coupons and collapsed bond values. Constrained exit: bonds are illiquid; selling crystallizes losses; British government blocks write-offs to preserve City credibility.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, british_treasury_bondholders, payer,
    organized, biographical, constrained, global).

% Pension funds for disabled veterans and widows in France, UK, Belgium — funded partly by expected reparations. When Germany stops paying, domestic taxpayers absorb the shortfall or benefits are cut. Trapped exit: no alternative funding source; the constraint extracts from the most vulnerable Allied populations.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, allied_veterans_pension_systems, payer,
    moderate, biographical, trapped, national).

% Inter-Allied Reparations Commission staff and technical experts whose institutional mandate evaporates when Germany refuses delivery schedules. They bear the institutional cost of enforcing a dead letter. Constrained exit: civil service careers tied to a function that the political principals have abandoned.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, reparations_commission_bureaucracy, payer,
    moderate, immediate, constrained, continental).

% League secretariat and minority-protection commissions that watch the reparations dispute as a test case for treaty enforcement. Analytical exit: they observe the structural collapse of the Versailles system without power to alter it.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, league_of_nations_observers, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The reparations clauses were meant to coordinate post-war reconstruction: transfer resources from the defeated power to repair the physical and human damage of the war, while binding Germany into a supervised economic relationship that prevented rapid rearmament.
% TRANSFER_FUNCTION: Moves real resources (coal, timber, chemicals, ships, gold marks) and financial claims from German economy to Allied reconstruction budgets and war-debt service. The repudiation reading reverses this flow: Germany retains all resources; Allies absorb the loss.
% ABSENT_VOICES: German working-class households who bear the inflationary cost of passive resistance (Ruhr 1923) and later the rearmament economy; ordinary French/Belgian civilians in occupied zones whose reconstruction is delayed; colonial subjects whose labor and resources financed the war but who have no claim on reparations. These voices are excluded from the diplomatic negotiation and the domestic German political arena alike.
% DISAPPEARANCE_RATIONALE: If the repudiation reading vanished overnight — i.e., if Germany accepted full reparations liability — the Weimar fiscal crisis would deepen catastrophically, French/Belgian reconstruction would accelerate, the Ruhr occupation would end cooperatively, and the nationalist grievance engine that powered Hitler's rise would lose its primary fuel. The European political economy of the 1920s-30s would reorganize around a solvent Germany paying tribute rather than a bankrupt Germany arming.
% FOUNDING_PROBLEM: How to make the loser of a total industrial war pay for the damage it caused, without destroying the loser's capacity to function as a state — and how to bind the loser into a durable peace settlement.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is dead: the physical damage of 1914-18 was largely repaired by the late 1920s (French reconstruction complete, Belgian industry restored) — not by German payments but by Allied domestic investment and US loans. The binding function failed: Germany rearmed regardless. Corroboration from outside beneficiaries: Keynes (The Economic Consequences of the Peace) predicted the founding problem was mis-specified; French economic historians (Boemeke, Feldman, Glaser) confirm reparations never funded more than a fraction of reconstruction; the Dawes/Young Plan architects admitted the binding function was theatrical by 1929.
narrative_ontology:disappearance_verdict(versailles_reparations_clauses__repudiation_reading, world_rearranges).
narrative_ontology:founding_problem_status(versailles_reparations_clauses__repudiation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(versailles_reparations_clauses__repudiation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(versailles_reparations_clauses__repudiation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(versailles_reparations_clauses__repudiation_reading, 0.88, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(versailles_reparations_clauses__repudiation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(versailles_reparations_clauses__repudiation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(versailles_reparations_clauses__repudiation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.88 at interval end: by 1933 Germany has paid ~21 billion GM of ~132 billion demanded — but the extraction is not what Germany paid; it is what Allies LOST (reconstruction delayed, war debts defaulted, security guarantees evaporated) while Germany retained industrial capacity for rearmament. The repudiation reading extracts from Allied populations to subsidize German revisionism. Suppression 0.92: enforcement requires military occupation (Ruhr 1923) which fails; the constraint persists only because Germany refuses and Allies cannot compel without war. Theater 0.25: the Dawes/Young Plans and League supervision create an elaborate performative framework that masks the underlying extraction. Accessibility collapse 0.78: once the 'treaty under duress' frame is accepted, no negotiated settlement can restore legitimacy — the only alternatives are enforcement by force or total abandonment. Resistance 0.85: Allied populations resist the extraction (French taxpayers, British bondholders, Belgian veterans) but their resistance is structurally ineffective.
 *
 * PERSPECTIVAL GAP:
 *   From the Weimar elite seat, the constraint is a necessary defense of sovereignty against an unjust diktat — the extraction they experience is the *threat* of Allied enforcement, not the reparations themselves. From the French veteran seat, the same constraint is a snare that extracts their pension to fund German rearmament. The engine computes this divergence: the agenda_setter seat (Weimar elites) will show low effective extraction (they benefit), while the payer seats show extreme effective extraction. The claimed_type 'snare' reflects the payer-seat reality; the agenda_setter seat would claim 'mountain' (treaty is void by natural law of sovereignty).
 *
 * DIRECTIONALITY LOGIC:
 *   Weimar elites and nationalists are beneficiaries (d ~0.15-0.25): they collect political capital and rearmament space from the constraint's operation. Industrial cartels are mobile beneficiaries (d ~0.20): they preserve assets. French/Belgian states and Allied veterans are trapped/constrained payers (d ~0.85-0.95): they bear the full cost of non-payment with no exit. British bondholders are constrained payers (d ~0.75): they hold defaulted paper. The reparations bureaucracy is a constrained payer (d ~0.70): its institutional mandate is extracted. League observers are analytical (d = 0.5). Identity-lock is decisive for Weimar elites: their political identity IS the repudiation; they cannot exit the frame without ceasing to be who they claim to be.
 *
 * MANDATROPHY ANALYSIS:
 *   The reparations mandate was built for a founding problem (reconstruction + binding) that died by 1929 — reconstruction complete without German money, binding failed. Yet the constraint persisted and intensified (Young Plan, Hoover Moratorium, Lausanne Conference) because the repudiation reading had become the *only* legitimate German political position. Any German leader accepting payment was a 'traitor.' The mandatrophy is resolved: the arrangement persists as pure extraction (Allied resources → German rearmament space) with no coordination function remaining. The theater of negotiation (Lausanne 1932) is the corpse's last movement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_framing_underdetermination,
    'Is the Versailles reparations kernel a single contested text, or do the three readings instantiate fundamentally different kernels (one legal, one economic, one moral)?',
    'Trace whether the three readings share any common epistemic referent — do they argue about the SAME Articles 231-247, or does each reading treat a different subset of provisions as the ''real'' kernel? If the latter, the kernel_id is a false unity.',
    'If the readings operate on different kernels, the cs_structure linking is invalid — they are not sibling readings of one kernel but separate constraints falsely grouped by label. This would require decomposing the family into independent stories without reading_relations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether ''Versailles reparations'' names one kernel or three.').

omega_variable(
    duress_legitimacy_boundary,
    'Where does the ''treaty under duress is illegitimate'' principle stop? If Versailles is void, are Locarno, Kellogg-Briand, and the Young Plan also void — all signed under threat of continued occupation or economic strangulation?',
    'Examine whether the repudiation reading''s proponents applied the duress principle consistently across all Weimar-era treaties, or only to Versailles. Inconsistency would reveal the principle as a targeted weapon, not a general normative axiom.',
    'If the duress axiom is selectively applied, its status as a foundational normative claim (axiom) is undermined — it becomes an instrumental position, not a deontological commitment. This changes the grounding_type from ''deontological'' to ''instrumental''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(duress_legitimacy_boundary, conceptual, 'Consistency of the duress illegitimacy axiom across the treaty system.').

omega_variable(
    reparations_vs_reconstruction_causality,
    'Did German non-payment *cause* Allied reconstruction failure, or did Allied reconstruction succeed despite non-payment (via US loans and domestic investment), making the extraction claim overstated?',
    'Quantify: French reconstruction spending 1919-1939 vs. actual German deliveries received. If French spending >> German deliveries, the extraction from French taxpayers is domestic, not German — the snare''s victim is the French state choosing to spend, not the German state refusing.',
    'If Allied reconstruction was domestically funded, the repudiation reading''s extraction from Allies is indirect (political choice to maintain war-debt service) rather than direct (resources physically transferred). This lowers ε for the payer seats and may shift classification toward tangled_rope (coordination failure + asymmetric political cost).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reparations_vs_reconstruction_causality, empirical, 'Causal attribution of Allied reconstruction shortfalls to German non-payment vs. domestic policy choices.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__repudiation_reading, 1919, 1933).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vrcrr_tr_t1919, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1919, 0.15).
narrative_ontology:measurement(vrcrr_tr_t1921, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1921, 0.22).
narrative_ontology:measurement(vrcrr_tr_t1923, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1923, 0.35).
narrative_ontology:measurement(vrcrr_tr_t1924, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1924, 0.28).
narrative_ontology:measurement(vrcrr_tr_t1929, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1929, 0.25).
narrative_ontology:measurement(vrcrr_tr_t1931, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1931, 0.23).
narrative_ontology:measurement(vrcrr_tr_t1933, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1933, 0.25).

% Extraction over time
narrative_ontology:measurement(vrcrr_be_t1919, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 1919, 0.45).
narrative_ontology:measurement(vrcrr_be_t1921, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 1921, 0.62).
narrative_ontology:measurement(vrcrr_be_t1923, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 1923, 0.85).
narrative_ontology:measurement(vrcrr_be_t1924, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 1924, 0.55).
narrative_ontology:measurement(vrcrr_be_t1929, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 1929, 0.72).
narrative_ontology:measurement(vrcrr_be_t1931, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 1931, 0.81).
narrative_ontology:measurement(vrcrr_be_t1933, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 1933, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(vrcrr_su_t1919, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 1919, 0.7).
narrative_ontology:measurement(vrcrr_su_t1921, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 1921, 0.82).
narrative_ontology:measurement(vrcrr_su_t1923, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 1923, 0.95).
narrative_ontology:measurement(vrcrr_su_t1924, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 1924, 0.85).
narrative_ontology:measurement(vrcrr_su_t1929, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 1929, 0.88).
narrative_ontology:measurement(vrcrr_su_t1931, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 1931, 0.9).
narrative_ontology:measurement(vrcrr_su_t1933, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 1933, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(versailles_reparations_clauses__repudiation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(versailles_reparations_clauses__repudiation_reading, 0.12).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, versailles_reparations_clauses__limited_responsibility_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, versailles_reparations_clauses__punitive_liability_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, locarno_treaties__german_revisionist_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, young_plan__final_settlement_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, hitler_rearmament_program__versailles_nullification).

% DUAL FORMULATION NOTE:
% This is the repudiation_reading of the versailles_reparations_clauses kernel. The limited_responsibility_reading treats reparations as an economic coordination problem (capacity-to-pay); the punitive_liability_reading treats them as a moral/legal liability (war guilt). This reading treats them as a void treaty (duress). The three readings have fundamentally different ε values (this reading: 0.88; limited: ~0.45; punitive: ~0.35 from Allied seat) because they describe different structural arrangements — not different measurements of the same arrangement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(versailles_reparations_clauses__repudiation_reading, institutional, 0.2).
constraint_indexing:directionality_override(versailles_reparations_clauses__repudiation_reading, organized, 0.25).
constraint_indexing:directionality_override(versailles_reparations_clauses__repudiation_reading, powerful, 0.2).
constraint_indexing:directionality_override(versailles_reparations_clauses__repudiation_reading, moderate, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
