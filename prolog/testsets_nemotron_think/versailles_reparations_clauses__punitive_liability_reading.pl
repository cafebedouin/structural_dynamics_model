% ============================================================================
% CONSTRAINT STORY: versailles_reparations_clauses__punitive_liability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   human_readable: Versailles Article 231 Punitive Liability Reading
 *   domain: international_relations/legal_history/political_economy
 *
 * SUMMARY:
 *   The Versailles Treaty's Article 231 ('war guilt clause') and the
 *   reparations machinery built upon it instantiate the punitive liability
 *   reading: Germany accepts sole moral responsibility for 'all loss and
 *   damage' and faces quasi-unlimited financial liability (132 billion gold
 *   marks, 1921 London Schedule). This reading was authored by the Allied
 *   victors (primarily France, with UK and US concurrence) and imposed on a
 *   defeated Germany that had no negotiating position. The constraint
 *   operated through the Reparations Commission (1920-1930), the Ruhr
 *   occupation (1923-1925), the Dawes Plan (1924), the Young Plan (1929), and
 *   ended at Lausanne (1932). The claimed_type 'tangled_rope' reflects the
 *   genuine coordination function (war cost allocation, inter-Allied debt
 *   settlement, military constraint) fused with asymmetric extraction
 *   (capacity-unbounded liability, private bank profit extraction via
 *   recycling loans). The engine will compute per-seat classifications from
 *   the structural data below.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(versailles_reparations_clauses__punitive_liability_reading, 0.82).
domain_priors:suppression_score(versailles_reparations_clauses__punitive_liability_reading, 0.74).
domain_priors:theater_ratio(versailles_reparations_clauses__punitive_liability_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, accessibility_collapse, 0.81).
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__punitive_liability_reading, tangled_rope).
narrative_ontology:human_readable(versailles_reparations_clauses__punitive_liability_reading, "Versailles Article 231 Punitive Liability Reading").
narrative_ontology:topic_domain(versailles_reparations_clauses__punitive_liability_reading, "international_relations/legal_history/political_economy").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__punitive_liability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__punitive_liability_reading, 'a2d460f5-1007-462c-9a5e-86a362b2e4fb').
narrative_ontology:cs_kernel_codification('a2d460f5-1007-462c-9a5e-86a362b2e4fb', formalized).
narrative_ontology:cs_authority_grounding('a2d460f5-1007-462c-9a5e-86a362b2e4fb', extraction).
narrative_ontology:cs_interpretation_layer_present('a2d460f5-1007-462c-9a5e-86a362b2e4fb').
narrative_ontology:cs_reading_relation('a2d460f5-1007-462c-9a5e-86a362b2e4fb', versailles_reparations_clauses__limited_responsibility_reading, influences).
narrative_ontology:cs_reading_relation('a2d460f5-1007-462c-9a5e-86a362b2e4fb', versailles_reparations_clauses__repudiation_reading, coexists_with).
narrative_ontology:cs_axiom('a2d460f5-1007-462c-9a5e-86a362b2e4fb', foundational, german_unique_war_guilt).
narrative_ontology:cs_axiom_status(german_unique_war_guilt, holdable).
narrative_ontology:cs_axiom_grounding('a2d460f5-1007-462c-9a5e-86a362b2e4fb', german_unique_war_guilt, deontological).
narrative_ontology:cs_axiom('a2d460f5-1007-462c-9a5e-86a362b2e4fb', foundational, reparations_unbounded_by_capacity).
narrative_ontology:cs_axiom_status(reparations_unbounded_by_capacity, holdable).
narrative_ontology:cs_axiom_grounding('a2d460f5-1007-462c-9a5e-86a362b2e4fb', reparations_unbounded_by_capacity, empirically_contingent).
narrative_ontology:cs_reference_frame('a2d460f5-1007-462c-9a5e-86a362b2e4fb', versailles_treaty_order).
narrative_ontology:cs_drift_state('a2d460f5-1007-462c-9a5e-86a362b2e4fb', post_dawes_young_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a2d460f5-1007-462c-9a5e-86a362b2e4fb', '').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__punitive_liability_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__punitive_liability_reading, allied_creditor_states).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__punitive_liability_reading, international_financial_interests).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, german_workers_taxpayers).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, german_fiscal_sovereignty).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, weimar_democratic_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__punitive_liability_reading, german_industrialists).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, german_government).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, german_industrialists).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__punitive_liability_reading, victor_justice_doctrine).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__punitive_liability_reading, collective_war_guilt_principle).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__punitive_liability_reading, inter_allied_debt_enforcement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% France, UK, Belgium, Italy and smaller Allies set reparations policy through the Reparations Commission. They collect payments to service their own war debts to the US and fund reconstruction. France occupies the Ruhr in 1923 to enforce compliance. Their exit options are maximal — they control the enforcement machinery and can shift between occupation, sanctions, and financial pressure.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, allied_creditor_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(versailles_reparations_clauses__punitive_liability_reading, allied_creditor_states, beneficiary).

% Private banks (J.P. Morgan, Schroder, etc.) underwrite German loans (Dawes, Young) that recycle reparations payments back to Germany as credit, earning fees and interest. They benefit from the reparations framework's existence regardless of whether Germany can pay. They can move capital globally and shift to other sovereign lending.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, international_financial_interests, beneficiary,
    organized, biographical, mobile, global).

% The Weimar government formally accepts Treaty obligations (signing 1919, London Schedule 1921) but negotiates revisions (Dawes 1924, Young 1929) and periodically defaults. It bears the political cost of compliance (hyperinflation 1923, austerity) and non-compliance (Ruhr occupation). Its sovereignty is subordinated to the Reparations Commission's oversight of budgets, taxes, and railway revenues. Exit is constrained — repudiation risks occupation, loss of credit access, and territorial loss; compliance destroys domestic legitimacy.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, german_government, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(versailles_reparations_clauses__punitive_liability_reading, german_government, agenda_setter).

% Bear the actual incidence of reparations through inflation (1919-1923), taxation, wage suppression, and unemployment. The 1923 hyperinflation wiped out savings and fixed incomes. The 1930-32 austerity (Bruning) cut wages and benefits to generate export surpluses for reparations. No meaningful exit — emigration is costly, political voice is fragmented, and the constraint is enforced by both foreign occupation and their own government's compliance.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, german_workers_taxpayers, payer,
    powerless, biographical, trapped, national).

% Heavy industry (Stinnes, Thyssen, Krupp) initially benefits from inflation (debts erased, export competitiveness) and resists currency stabilization. Later, under Dawes/Young, they face higher taxes and reparations-linked levies but gain access to foreign loans and stabilized markets. Some fund anti-reparations parties (DNVP, Nazis) while profiting from the system. Exit is constrained — assets are immobile, but political influence lets them shape compliance terms.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, german_industrialists, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(versailles_reparations_clauses__punitive_liability_reading, german_industrialists, beneficiary).

% The Republic's legitimacy is bound to Treaty acceptance (Ebert's 'stem the poison' speech). Reparations fuel the 'stab-in-the-back' myth and destabilize parliamentary coalitions. The constraint makes democratic governance nearly impossible — every government falls over reparations policy. Identity-locked: the Republic's self-conception as a lawful successor state requires Treaty compliance, but compliance destroys its political base. Exit (repudiation) would dissolve the Republic's founding identity.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, weimar_democratic_institutions, payer,
    moderate, generational, identity_locked, national).

% The League hosts the Reparations Commission and Dawes/Young negotiations. Its officials (Norman, Schacht, Morgan) produce the technical fixes that keep the system running. They observe the constraint's operation without bearing its costs or collecting its gains. Their analytical seat is the only one with genuine freedom to assess the constraint's viability — and their reports (Dawes Report, Young Report) document its structural impossibility.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, league_of_nations_observers, observer,
    institutional, generational, analytical, global).

% The US is not a Treaty signatory but is the ultimate creditor (Allied war debts to US). Washington refuses to link reparations to war debts officially, but the Dawes/Young loans are Wall Street products that require US tacit approval. The US could restructure the entire system by forgiving Allied debts but chooses not to. Excluded from the Reparations Commission but structurally decisive.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, us_creditor_position, excluded,
    institutional, generational, arbitrage, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates the unprecedented costs of total industrial war among the combatants; provides a legal framework for inter-Allied debt settlement; attempts to prevent German military resurgence through financial constraint.
% TRANSFER_FUNCTION: Moves real resources — gold reserves, coal deliveries, railway revenues, industrial output, fiscal surpluses — from the German economy to Allied treasuries, which onward-transfer to US creditors. The Dawes/Young loans recycle private capital to Germany to enable payments, creating a circular flow: US banks → Germany → Allies → US Treasury.
% ABSENT_VOICES: The German population (no referendum on Treaty), neutral states (Netherlands, Switzerland, Scandinavia) whose trade was disrupted, colonial subjects whose resources were pledged as collateral, and future generations who inherited the destabilized order. The Reparations Commission had no German voting members until 1924 (Dawes).
% DISAPPEARANCE_RATIONALE: If the punitive liability reading vanished in 1919, the London Schedule (1921) would not have set 132 billion gold marks; the Ruhr occupation (1923) would not have occurred; hyperinflation would not have wiped the middle class; Dawes/Young Plans would not have been needed; the Weimar Republic might have stabilized; the Nazi rise would lose its primary grievance; the global financial crisis of 1931 (Creditanstalt, German banking collapse) would have a different trigger. The interwar order was built around this constraint.
% FOUNDING_PROBLEM: How to assign financial responsibility for a war that destroyed four empires, killed 17 million, and left the victors indebted to the US — while preventing the defeated power from reconstituting a military threat. The Treaty's answer: unique German guilt (Art. 231) justifying unbounded liability.
% FOUNDING_PROBLEM_CORROBORATION: Keynes (The Economic Consequences of the Peace, 1919) — outside the beneficiary set — warned the sum exceeded German capacity and would destroy Europe's economy. The Dawes Committee (1924), chaired by a US banker, concluded 'Germany cannot pay' the London Schedule. The Young Committee (1929) reduced the total but still assumed capacity that collapsed in 1931. The Lausanne Conference (1932) ended reparations de facto, confirming the founding problem (German capacity) was dead but the arrangement persisted 13 years past viability.
narrative_ontology:disappearance_verdict(versailles_reparations_clauses__punitive_liability_reading, world_rearranges).
narrative_ontology:founding_problem_status(versailles_reparations_clauses__punitive_liability_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(versailles_reparations_clauses__punitive_liability_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(versailles_reparations_clauses__punitive_liability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(versailles_reparations_clauses__punitive_liability_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.82) is high because the liability was deliberately decoupled from German capacity — the 1921 Schedule assumed 2.5bn GM/year when Germany's total export surplus never exceeded 1.5bn. Suppression (0.74) is high because alternatives (repudiation, bankruptcy, currency collapse) were actively blocked by occupation, sanctions, and credit denial. Theater (0.38) rises over time: early enforcement is genuine (Ruhr occupation), but Dawes/Young become performative — complex financial engineering that masks the fundamental impossibility. Accessibility_collapse (0.81) is high because once Article 231 was signed, the legal framework closed off alternatives: Germany could not legally contest the principle, only the schedule. Resistance (0.68) is substantial: passive resistance in Ruhr (1923), government falls (Cuno, Stresemann, Muller, Bruning), and the eventual political repudiation (Hitler 1933).
 *
 * PERSPECTIVAL GAP:
 *   The Allied seat reads the constraint as genuine coordination (war settlement, debt justice) with necessary enforcement. The German worker seat reads it as pure extraction (survival threat, no voice). The German government seat reads it as a trap it must administer. The banker seat reads it as a profitable recycling mechanism. The League observer seat reads it as a technical problem of capacity. These divergent readings are not perceptual errors — they follow from the structural positions the engine computes from power/exit/beneficiary declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   Allied creditor states are structural beneficiaries (collect payments, control enforcement, d near 0.0). International finance benefits from the recycling mechanism (d ~0.1). German government is a dual-positioned payer/agenda_setter: it administers compliance but bears the political cost (d ~0.7, overridden from derived ~0.4 because the government's formal Treaty acceptance masks its structural victimhood). German workers/taxpayers are full targets (trapped, no exit, d ~0.95). Industrialists are constrained payers with secondary beneficiary access (d ~0.6). Weimar institutions are identity-locked payers (d ~0.85 — their founding identity requires compliance that destroys them). US position is excluded but structurally decisive (arbitrage exit, d not computed).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (allocating total war costs, preventing German resurgence) was live in 1919 but dead by 1924 (Dawes Report: 'Germany cannot pay'). The constraint persisted 8 years past viability through performative financial engineering (Dawes, Young) that served as theater. The mandatrophy is resolved in the sense that the constraint's original justification vanished, but the machinery continued extracting until the system collapsed. This is not a scaffold (no sunset clause) and not a piton (active enforcement continued to the end). It is a tangled_rope whose coordination function atrophied while extraction intensified.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the punitive_liability_reading a distinct constraint from the limited_responsibility_reading and repudiation_reading, or are they interpretations of a single constraint?',
    'Structural decomposition: if the three readings have different beneficiary/victim structures, different ε values, and different enforcement logics, they are separate constraints per the ε-invariance principle. This story asserts they are separate.',
    'If separate, each reading gets its own classification (this one: tangled_rope; limited_responsibility: likely rope or scaffold; repudiation: mountain or snare depending on framing). If one constraint, the classification must average across readings, losing the structural divergence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel''s readings are structurally distinct constraints (ε-invariance) or observer perspectives on one constraint.').

omega_variable(
    war_guilt_natural_vs_constructed,
    'Is Article 231''s ''war guilt'' a genuine legal/moral principle discovered by the victors, or a constructed justification for extraction?',
    'Compare the Treaty''s drafting record (Allied deliberations on Art. 231 as a compromise between US ''no indemnity'' and French ''full liability'') with the subsequent reparations practice. If the clause was drafted to enable unbounded claims, it is constructed.',
    'If constructed, the constraint is a false summit candidate (mountain claim masking extraction). If genuine principle, the extraction is a consequence of a real moral/legal fact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(war_guilt_natural_vs_constructed, conceptual, 'Natural-law vs. constructed ambiguity in the war guilt clause — core to FSM detection if this reading were claimed as mountain.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of German alternatives structural (occupation, sanctions, credit denial) or internalized (German acceptance of war guilt, democratic identity-lock)?',
    'Post-1932 trajectory: when the constraint was removed (Lausanne, then Hitler''s repudiation), did German political culture immediately normalize, or did the ''guilt'' narrative persist? The persistence of ''stab-in-the-back'' and victimhood narratives after 1932 suggests internalized suppression.',
    'If internalized, effective suppression is higher than structural measures suggest — the constraint installed a self-sustaining cognitive cage. This would amplify χ for identity-locked seats (Weimar institutions, workers).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the German population and institutions.').

omega_variable(
    allied_unity_vs_divergence,
    'Did the Allied creditor states share a unified punitive liability reading, or did France (maximalist) and UK/US (moderate) diverge structurally?',
    'Analyze the Reparations Commission voting records, the Ruhr occupation (French-Belgian only, UK opposed), and the Dawes/Young negotiations (US-led). If Allies diverged, the ''agenda_setter'' stakeholder should be split.',
    'If Allies diverged, the coordination function is weaker (less genuine coordination among beneficiaries) and extraction is more nakedly French. This would shift classification toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(allied_unity_vs_divergence, empirical, 'Whether the beneficiary coalition was unified or fractured — affects coordination/extraction balance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__punitive_liability_reading, 1919, 1932).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(versailles_punitive_tr_t1919, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1919, 0.15).
narrative_ontology:measurement(versailles_punitive_tr_t1921, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1921, 0.25).
narrative_ontology:measurement(versailles_punitive_tr_t1923, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1923, 0.35).
narrative_ontology:measurement(versailles_punitive_tr_t1924, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1924, 0.45).
narrative_ontology:measurement(versailles_punitive_tr_t1929, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1929, 0.52).
narrative_ontology:measurement(versailles_punitive_tr_t1931, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1931, 0.68).
narrative_ontology:measurement(versailles_punitive_tr_t1932, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1932, 0.85).

% Extraction over time
narrative_ontology:measurement(versailles_punitive_be_t1919, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1919, 0.75).
narrative_ontology:measurement(versailles_punitive_be_t1921, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1921, 0.85).
narrative_ontology:measurement(versailles_punitive_be_t1923, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1923, 0.92).
narrative_ontology:measurement(versailles_punitive_be_t1924, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1924, 0.68).
narrative_ontology:measurement(versailles_punitive_be_t1929, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1929, 0.62).
narrative_ontology:measurement(versailles_punitive_be_t1931, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1931, 0.58).
narrative_ontology:measurement(versailles_punitive_be_t1932, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1932, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(versailles_punitive_su_t1919, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1919, 0.6).
narrative_ontology:measurement(versailles_punitive_su_t1921, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1921, 0.75).
narrative_ontology:measurement(versailles_punitive_su_t1923, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1923, 0.9).
narrative_ontology:measurement(versailles_punitive_su_t1924, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1924, 0.55).
narrative_ontology:measurement(versailles_punitive_su_t1929, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1929, 0.5).
narrative_ontology:measurement(versailles_punitive_su_t1931, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1931, 0.45).
narrative_ontology:measurement(versailles_punitive_su_t1932, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1932, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(versailles_reparations_clauses__punitive_liability_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(versailles_reparations_clauses__punitive_liability_reading, 0.12).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, versailles_reparations_clauses__limited_responsibility_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, versailles_reparations_clauses__repudiation_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, dawes_plan).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, young_plan).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, weimar_hyperinflation_1923).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, inter_allied_war_debts).

% DUAL FORMULATION NOTE:
% This constraint family (versailles_reparations_clauses) decomposes the Treaty's reparations clauses into three readings with distinct ε values and beneficiary/victim structures. The punitive_liability_reading has the highest ε (0.82) and most asymmetric extraction. The limited_responsibility_reading (Dawes/Young operational reading) has lower ε (~0.45) and genuine coordination. The repudiation_reading (German nationalist/Weimar resistance reading) has near-zero ε from the German seat but high suppression from the Allied seat. They are linked because each reading cites the others as its counterfactual.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(versailles_reparations_clauses__punitive_liability_reading, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
