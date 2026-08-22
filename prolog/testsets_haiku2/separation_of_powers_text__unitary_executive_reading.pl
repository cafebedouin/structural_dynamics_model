% ============================================================================
% CONSTRAINT STORY: separation_of_powers_text__unitary_executive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_separation_of_powers_text__unitary_executive_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: separation_of_powers_text__unitary_executive_reading
 *   human_readable: Unitary Executive Doctrine: Presidential Control of All Executive Authority
 *   domain: constitutional_law/political_theory/administrative_law
 *
 * SUMMARY:
 *   The unitary executive reading asserts that the Constitution's Vesting
 *   Clause grants the President exclusive and comprehensive authority over
 *   all executive-branch officials and actions. From this interpretive seat,
 *   independent agencies—entities with statutory insulation from at-will
 *   presidential removal and delegated discretion in technical regulatory
 *   domains—are constitutionally anomalous. The reading has two beneficiary
 *   seats: the executive presidency (gains consolidated authority) and
 *   regulated industries aligned with deregulation (gain access to political
 *   override of expert constraints). Two victim seats bear costs: the
 *   independent agencies themselves (face subordination or invalidation) and
 *   constituencies dependent on agency independence (labor, consumers,
 *   financial stability). Congress occupies a dual seat: as payer of its own
 *   diminished delegation authority, as beneficiary of executive vigor when
 *   aligned with presidential goals. The kernel contest frames this reading
 *   against two siblings: the formalist reading (strict boundaries permit
 *   Congress to insulate agencies via the Necessary and Proper Clause) and
 *   the functionalist reading (flexible overlap is constitutional if agencies
 *   cannot prevent branches from performing essential functions). This story
 *   instantiates ONLY the unitary executive reading as a structurally
 *   coherent constraint—with its own ε, its own beneficiary/victim
 *   configuration, and its own persistence mechanisms. The sibling readings
 *   are OTHER constraints, not alternatives within this one.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__unitary_executive_reading, 0.68).
domain_priors:suppression_score(separation_of_powers_text__unitary_executive_reading, 0.72).
domain_priors:theater_ratio(separation_of_powers_text__unitary_executive_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__unitary_executive_reading, tangled_rope).
narrative_ontology:human_readable(separation_of_powers_text__unitary_executive_reading, "Unitary Executive Doctrine: Presidential Control of All Executive Authority").
narrative_ontology:topic_domain(separation_of_powers_text__unitary_executive_reading, "constitutional_law/political_theory/administrative_law").

domain_priors:requires_active_enforcement(separation_of_powers_text__unitary_executive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__unitary_executive_reading, '05cfe1fb-1a34-4dc6-9b90-f472544f504c').
narrative_ontology:cs_kernel_codification('05cfe1fb-1a34-4dc6-9b90-f472544f504c', fixed_text).
narrative_ontology:cs_authority_grounding('05cfe1fb-1a34-4dc6-9b90-f472544f504c', lineage).
narrative_ontology:cs_interpretation_layer_present('05cfe1fb-1a34-4dc6-9b90-f472544f504c').
narrative_ontology:cs_reading_relation('05cfe1fb-1a34-4dc6-9b90-f472544f504c', separation_of_powers_text__formalist_reading, forecloses).
narrative_ontology:cs_reading_relation('05cfe1fb-1a34-4dc6-9b90-f472544f504c', separation_of_powers_text__functionalist_reading, coexists_with).
narrative_ontology:cs_axiom('05cfe1fb-1a34-4dc6-9b90-f472544f504c', foundational, vesting_clause_grants_exclusive_presidential_authority).
narrative_ontology:cs_axiom_status(vesting_clause_grants_exclusive_presidential_authority, holdable).
narrative_ontology:cs_axiom_grounding('05cfe1fb-1a34-4dc6-9b90-f472544f504c', vesting_clause_grants_exclusive_presidential_authority, deontological).
narrative_ontology:cs_axiom('05cfe1fb-1a34-4dc6-9b90-f472544f504c', foundational, presidential_removal_power_must_be_absolute).
narrative_ontology:cs_axiom_status(presidential_removal_power_must_be_absolute, holdable).
narrative_ontology:cs_axiom_grounding('05cfe1fb-1a34-4dc6-9b90-f472544f504c', presidential_removal_power_must_be_absolute, deontological).
narrative_ontology:cs_reference_frame('05cfe1fb-1a34-4dc6-9b90-f472544f504c', exclusive_presidential_executive_authority).
narrative_ontology:cs_drift_state('05cfe1fb-1a34-4dc6-9b90-f472544f504c', contemporary_post_seila_law_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('05cfe1fb-1a34-4dc6-9b90-f472544f504c', '').
narrative_ontology:cs_kernel_id(separation_of_powers_text__unitary_executive_reading, separation_of_powers_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers_text__unitary_executive_reading, executive_presidency).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, independent_agencies).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, congressional_authority_over_executive_branch).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__unitary_executive_reading, congress).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__unitary_executive_reading, federal_judiciary).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__unitary_executive_reading, regulated_industries_and_market_actors).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, congress).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, public_interest_constituencies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces the unitary executive doctrine through appointments of constitutionally committed judges, removal decisions that test agency insulation, and executive orders asserting unitary authority. Claims exclusive constitutional authority over all executive-branch functions via the Vesting Clause. Collects the benefit of consolidated authority: can direct agency action without statutory constraint, redirect regulatory policy without legislative change, and subordinate independent expertise to political preference. Exit option: can reinterpret the doctrine through successive appointments without repealing it (arbitrage—maintaining authority while shifting its justification).
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, executive_presidency, agenda_setter,
    institutional, generational, arbitrage, national).

% Federal Reserve, FTC, NLRB, and similar entities chartered with statutory independence and removal insulation. Under the unitary executive reading, their independence is unconstitutional and subject to invalidation or forced subordination. They bear the cost of defending their constitutionality in court, complying with presidential directives despite statutory mandate to the contrary, and operating under persistent constitutional threat. Cannot exit: their statutory existence and regulatory mission define them; departure from the regulatory domain requires congressional action Congress has not undertaken. Identity-locked: the regulatory function is their constitutive purpose.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, independent_agencies, payer,
    organized, generational, identity_locked, national).

% Created the independent agencies via statute and established their removal insulation and mandate independence. Under the unitary executive reading, Congress's delegation of executive authority and insulation provisions are ultra vires (beyond its constitutional power). Bears the cost of defending the constitutionality of its own statutory framework through litigation and continued legislative assertion. Retains formal authority to pass new statutes but faces the constraint that any delegation of independent executive authority is constitutionally suspect. Dual positioned: as payer (losing delegation authority) and beneficiary (if unified executive serves congressional partisan goals, Congress can leverage executive vigor). Constrained exit: must legislate to change the framework but cannot override constitutional invalidation without amendment.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, congress, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__unitary_executive_reading, congress, beneficiary).

% Becomes the final arbiter of the Vesting Clause's meaning through constitutional review. Gains authority and legitimacy (especially a judiciary philosophically aligned with unitary executive doctrine) as the ultimate interpreter of separation of powers. Observes and adjudicates the constraint through successive cases (Bowsher, Morrison, Free Enterprise Fund, Seila Law, et al.). Bears the cost of resolving recurrent constitutional challenges to agency independence without complete consensus. Analytical seat: not bound by the constraint itself but by the duty to interpret the Constitution.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, federal_judiciary, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__unitary_executive_reading, federal_judiciary, observer).

% Industries regulated by independent agencies (finance, labor, consumer protection, competition) benefit from reduction in agency independence—regulatory decision-making becomes responsive to presidential policy preference, which often aligns with industry preferences for deregulation or limited enforcement. Gain access to political override of expert regulatory constraints. Can mobilize to support presidential deregulation campaigns and oppose agency resistance. Mobile: can shift focus to congressional lobbying, industry association advocacy, or international markets if domestic regulation tightens.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, regulated_industries_and_market_actors, beneficiary,
    powerful, generational, mobile, national).

% Labor unions relying on NLRB neutral arbitration, consumers depending on FTC enforcement, environmental groups relying on independent EPA authority, financial stability stakeholders relying on Federal Reserve technical expertise. Extract value from agency independence: their regulatory voice is heard in independent proceedings without political override. Under unitary executive doctrine, lose access to neutral agency forums; regulation becomes responsive to presidential preference, which may not align with their interests. Recourse is through presidential responsiveness, which is unreliable. Cannot exit the regulatory system: they are identity-locked into dependence on these institutions; no alternative regulatory path exists. Diffuse interest: hard to mobilize for litigation or political action.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, public_interest_constituencies, payer,
    powerless, biographical, identity_locked, national).

% Constitutional scholars and judges who argue for strict separation of powers with explicit Congressional delegation authority would defend the constitutionality of agency independence via the Necessary and Proper Clause. Excluded from the unitary executive frame: their core axiom (Congress CAN delegate) is treated as non-starter by the reading. Cannot participate as legitimate constitutional readers within the unitary framework; their arguments are framed as incorrect, not merely alternative. Trapped: no exit from the constitutional interpretation discourse; must engage within the reading's frame or be dismissed.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, formalist_reading_defenders, excluded,
    institutional, generational, trapped, national).

% Constitutional scholars and judges who argue for flexible separation of powers (overlap is constitutional if no branch is prevented from performing essential functions) would defend agency independence by showing that independent agencies do not prevent presidential performance of core executive duties. Excluded from the unitary executive frame: their test (functionality) is treated as inadequate by the reading's axiom (unity itself is required, not just functionality). Cannot participate as legitimate constitutional readers; their arguments are framed as misunderstanding the Vesting Clause's exclusive grant, not merely applying different standards. Trapped: must engage within the reading's frame or be dismissed as misreading the text.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, functionalist_reading_defenders, excluded,
    institutional, generational, trapped, national).

% Acts as the ultimate arbiter of constitutional meaning regarding executive power and agency constitutionality. Has issued mixed decisions over decades: upheld some independent structures (Morrison v. Olson, 1988), struck down others (Free Enterprise Fund v. PCAOB, 2010; Seila Law v. CFPB, 2020), without a complete unified doctrine. Currently faces pressure to complete the doctrine's logical arc by invalidating remaining statutory removal insulations or to retreat and preserve a space for Congressional agency design. Analytical seat: observes and adjudicates, but not bound by the constraint itself.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, supreme_court, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(separation_of_powers_text__unitary_executive_reading, executive_presidency).
narrative_ontology:fixing_cost_class(separation_of_powers_text__unitary_executive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Consolidates decision-making about executive-branch action under a single hierarchical authority (the President) rather than diffusing it across expert agencies with conflicting mandates. Solves a claimed coordination problem: competing agency authorities create contradictory directives, delayed action, and diffused accountability.
% TRANSFER_FUNCTION: Transfers constitutional authority from independent agencies to the President; shifts regulatory decision-making from expert-neutral proceedings to presidential policy preferences; extracts statutory removal insulation (payers: agencies and Congress) and delivers it to presidential prerogative (beneficiary: executive presidency and politically aligned regulated industries).
% ABSENT_VOICES: Agencies themselves cannot speak in constitutional-law discourse—they are objects of regulation, not subjects defending their own constitutionality. Labor unions, consumer advocates, and financial stability interests would object to loss of neutral agency expertise, but they are downstream of the constitutional dispute and rarely participate in litigation. Foreign governments and actors dependent on stable U.S. regulatory regimes (e.g., trading partners relying on predictable competition enforcement) are absent from the domestic constitutional frame.
% DISAPPEARANCE_RATIONALE: If the unitary executive constraint were repudiated (e.g., via constitutional amendment or Supreme Court reversal), independent agency authority would be restored, regulated industries would face renewed technical expertise insulation from political pressure, and executive consolidation would fracture. The regulatory landscape would reorganize around agency independence rather than presidential direction.
% FOUNDING_PROBLEM: The Constitution's allocation of executive power via the Vesting Clause ('The executive power shall be vested in a President') is ambiguous: does it grant the President exclusive executive authority, or merely identify the President as the branch holder of whatever executive authority exists? The founding problem is the question of whether structural coherence and unified accountability require all executive action to flow from the President, or whether Congress can constitutionally partition executive authority.
% FOUNDING_PROBLEM_CORROBORATION: The unitary executive reading invokes the Vesting Clause's text and early executive practice (Washington's removal of cabinet officers, Lincoln's war powers assertions) as corroboration. Functionalist and formalist readings cite the same historical sources differently, invoking the Necessary and Proper Clause and the Framers' delegation of agency-like functions (e.g., Secretary of War) to argue the Framers did not view executive unity as absolute. Academic constitutionalists (Yale Law School Youngstown framework, OLC opinions under different administrations) corroborate the contest itself but not the verdict. The Supreme Court has issued split decisions: *Morrison v. Olson* (1988) upheld independent counsel insulation; *Free Enterprise Fund v. PCAOB* (2010) struck down double-insulated board structure; *Seila Law v. CFPB* (2020) invalidated single-director CFPB insulation but left multi-member boards open. No consensus external corroboration exists for either the founding problem's continuation or resolution.
narrative_ontology:disappearance_verdict(separation_of_powers_text__unitary_executive_reading, world_rearranges).
narrative_ontology:founding_problem_status(separation_of_powers_text__unitary_executive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(separation_of_powers_text__unitary_executive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(separation_of_powers_text__unitary_executive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(separation_of_powers_text__unitary_executive_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(separation_of_powers_text__unitary_executive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(separation_of_powers_text__unitary_executive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(separation_of_powers_text__unitary_executive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.48→0.68 over the interval) because the reading concentrates authority in one seat and extracts the capacity for independent action from agencies and congressional delegation authority. The temporal drift reflects the doctrine's incremental entrenchment through Supreme Court decisions (Bowsher, Morrison, Free Enterprise Fund, Seila Law). Suppression is high (0.72 at end) because the reading is enforced through the threat of constitutional invalidation—agencies conform not through shared preference but through structural coercion (the constitutional threat). Theater is moderate (0.41) because there is a real coordination problem the reading addresses (unified executive direction) alongside the extraction of agency independence; some enforcement activity defends genuine hierarchical clarity, some defends only presidential prerogative over expertise. The measurement series track extraction and suppression rising steeply to t=24 (the Supreme Court decisions) then plateauing as the doctrine stabilized without triggering full agency invalidation—a characteristic of a constraint enforced via constitutional threat without complete enforcement. Accessibility collapse is high (0.78): once the unitary executive axiom is accepted, alternatives (agency independence, functionalism, formalism) become logically closed off within that framework. Resistance is high (0.71) because the constraint meets sustained opposition from agency defenders, functionalist scholars, and courts that have not fully endorsed the doctrine.
 *
 * PERSPECTIVAL GAP:
 *   The seat divergence is structural and sharp. From the executive presidency's seat, the constraint is coordination (unify authority, clarify lines, enable coherent policy). From an independent agency's seat, the constraint is extraction (subordination via constitutional threat, removal of statutory insulation, loss of functional autonomy). From a functionalist judge's seat, the constraint is either a false doctrine (the Vesting Clause does not require executive unity) or a tangled rope (coordination function offset by democratic deficit). From a public-interest constituency's seat, the constraint is a snare (extraction of neutral regulatory access, no alternatives, suppression via constitutional threat). The engine computes all four classifications from the directional and power data; the perspectival gap is WHERE THE TYPE DIVERGENCE APPEARS—the constraint cannot be assigned a single seat-independent type because the structural relationship to executive consolidation differs radically across seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The executive presidency sits at d≈0.0–0.2 (beneficiary end): the doctrine concentrates authority, requires no exit, and gains from every constraint application. Independent agencies sit at d≈0.8–0.95 (full target end): they are identity-locked into the regulatory system, have no practical exit, and face subordination via constitutional threat. Congress sits at d≈0.5–0.7: it loses authority to delegate but retains formal statutory authority; the tension between its institutional power and the doctrine's constraint on it produces the dual role. Regulated industries sit at d≈0.0–0.3 (beneficiary end): they gain deregulation access through political channels. The judiciary sits at d≈0.2–0.4 (beneficiary lean): it gains interpretive authority and legitimacy as constitutional arbiter but also bears the cost of repeatedly adjudicating agency constitutionality. Public-interest constituencies sit at d≈0.85–0.95 (full target end): powerless, identity-locked, extracting via loss of regulatory neutral ground. These directionalities derive from the beneficiary/victim structure and power/exit data: no override needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (constitutional interpretation of the Vesting Clause) is contested (founding_problem_status = contested). The disappearance verdict is world_rearranges: if the unitary executive constraint were repealed (via constitutional amendment or Supreme Court reversal), the regulatory landscape would reorganize around independent agency authority rather than presidential consolidation. These two facts together—contested origin, world_rearranges verdict—pass the mandatrophy test (an arrangement whose founding purpose is contested AND whose disappearance would require reorganization is not yet mandatrophic). However, the measurement series suggest a plateau: extraction and suppression rise steeply to t=24 (Supreme Court consolidation of doctrine) then stabilize at t=50. This suggests the doctrine has solidified WITHOUT triggering full agency invalidation—agencies still exist but are subordinated and operate under the threat of constitutional challenge. This is a STABLE EXTRACTION scenario: the doctrine persists because it redistributes authority without destroying agencies entirely; the payers (agencies and constituencies) cannot exit but also cannot force a reckoning because the constraint is enforced through constitutional interpretation, not legislative mandate that can be repealed. A piton candidate is emerging: the doctrine persists via theater (agencies perform independence while accepting presidential override), not via shared preference. The constraint is NOT yet mandatrophic (the founding problem is still contested and the world WOULD reorganize on disappearance), but it shows piton-like dynamics (theater rising, enforcement plateau, no party benefiting enough to maintain it except the presidency).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vesting_clause_interpretation_ambiguity,
    'Does the Vesting Clause (''The executive power shall be vested in a President'') grant the President EXCLUSIVE executive authority, or merely identify the President as the branch-holder of whatever executive authority Congress defines?',
    'Historical analysis of Constitutional Convention records and ratification debates; comparative analysis with Article II''s explicit exclusivity language elsewhere (e.g., President is sole Commander-in-Chief) vs. silence on agency insulation; Supreme Court crystallization of doctrine through subsequent cases.',
    'If the clause grants exclusive authority, the unitary executive reading is the correct structural reading and independent agencies are constitutionally suspect. If the clause only names the President as the executive branch, Congress retains authority to partition executive functions, and the reading forecloses to formalism. This is the irreducible kernel ambiguity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vesting_clause_interpretation_ambiguity, conceptual, 'What the Vesting Clause grants: exclusive authority or branch-naming?').

omega_variable(
    necessity_of_unified_executive_for_accountability,
    'Is unified executive command (all executive officials report to President) NECESSARY for democratic accountability, or can Congress constitute independent agencies with statutory mandates and preserve accountability through legislative oversight and judicial review?',
    'Empirical comparison: accountability outcomes (responsiveness to public preferences, rule of law compliance, expert-neutral administration) across jurisdictions with unified executives vs. independent agencies; case-law doctrine development distinguishing accountability paths.',
    'If unified command is necessary for accountability, the doctrine serves a genuine coordination function and is tangled rope (coordination + extraction). If Congress''s statutory mandate + oversight + judicial review suffice for accountability, the coordination claim is false and the constraint is pure snare (extraction dressed as coordination). This shifts the type from tangled rope to snare depending on empirical outcome.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_of_unified_executive_for_accountability, empirical, 'Whether unity of command is necessary or sufficient for executive accountability.').

omega_variable(
    constitutional_threat_suppression_mechanism,
    'Is the measured suppression of agency independence maintained via external constitutional threat (the doctrine can be repealed), or is it internalized (agencies and defending institutions have accepted the axiom as legitimate)?',
    'Post-invalidation trajectory: if the Supreme Court reversed the doctrine (or an amendment superseded it), would agencies immediately reassert independence, or would suppression persist due to internalized deference?',
    'If suppression is purely structural/constitutional-threat-based, the constraint''s hold is conditional on the doctrine''s persistence in constitutional interpretation. If internalized, the constraint has embedded itself in institutional culture and would persist even after the legal threat is lifted. This determines whether the constraint is a contingent extraction (reversible) or a deep institutional trap (harder to escape).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(constitutional_threat_suppression_mechanism, empirical, 'Is suppression structural or internalized in the regulatory agencies?').

omega_variable(
    kernel_reader_axiom_foreclosure,
    'From within the unitary executive reading''s own epistemic frame, can the formalist reading (Congress can constitutionally insulate agencies) remain logically tenable, or does the reading''s core axiom (exclusive executive power via Vesting Clause) foreclose formalism?',
    'Close analysis of the reading relations specified in cs_structure: if the unitary executive axiom (exclusive presidential authority) directly contradicts the formalist axiom (Congress has delegation authority), they foreclose each other. If they can coexist (e.g., one holds about the text and one about legislative history), they coexist rather than foreclose.',
    'Foreclusion relation in cs_structure.reading_relations affects how the engine models the kernel contest: foreclosed readings are dead inside unitary-executive frames, while coexisting readings remain live. This determines whether the reading is committed to eventual invalidation of agencies or merely to subordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reader_axiom_foreclosure, conceptual, 'Do the unitary executive and formalist axioms logically foreclose each other or coexist?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__unitary_executive_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sepa_tr_t0, separation_of_powers_text__unitary_executive_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(sepa_tr_t8, separation_of_powers_text__unitary_executive_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(sepa_tr_t16, separation_of_powers_text__unitary_executive_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement(sepa_tr_t24, separation_of_powers_text__unitary_executive_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement(sepa_tr_t35, separation_of_powers_text__unitary_executive_reading, theater_ratio, 35, 0.41).
narrative_ontology:measurement(sepa_tr_t50, separation_of_powers_text__unitary_executive_reading, theater_ratio, 50, 0.41).

% Extraction over time
narrative_ontology:measurement(sepa_be_t0, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(sepa_be_t8, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 8, 0.54).
narrative_ontology:measurement(sepa_be_t16, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(sepa_be_t24, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 24, 0.65).
narrative_ontology:measurement(sepa_be_t35, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement(sepa_be_t50, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(sepa_su_t0, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(sepa_su_t8, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(sepa_su_t16, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 16, 0.65).
narrative_ontology:measurement(sepa_su_t24, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(sepa_su_t35, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 35, 0.72).
narrative_ontology:measurement(sepa_su_t50, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(separation_of_powers_text__unitary_executive_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(separation_of_powers_text__unitary_executive_reading, 0.12).
narrative_ontology:affects_constraint(separation_of_powers_text__unitary_executive_reading, separation_of_powers_text__formalist_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__unitary_executive_reading, separation_of_powers_text__functionalist_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__unitary_executive_reading, presidential_removal_power_absolute).
narrative_ontology:affects_constraint(separation_of_powers_text__unitary_executive_reading, congressional_delegation_authority_boundaries).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the separation_of_powers_text kernel. The sibling readings—formalist_reading and functionalist_reading—are distinct constraints with different ε values, different beneficiary/victim structures, and different types. They are linked by network.affects_constraints. The constraint family models the kernel contest: each reading instantiates a different constraint with a different structural relationship to executive consolidation and agency independence. Do not merge the readings into a single constraint with measurement-parameter dependence; each reading is a distinct story with a distinct ε. The unitary_executive_reading forecloses the formalist_reading's core axiom but coexists with the functionalist_reading's test-based approach. See cs_structure.reading_relations for the formal relationship declarations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
