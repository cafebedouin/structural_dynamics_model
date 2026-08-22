% ============================================================================
% CONSTRAINT STORY: provincial_sovereignty_boundary__constitutional_subordination
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_provincial_sovereignty_boundary__constitutional_subordination, []).

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
 *   constraint_id: provincial_sovereignty_boundary__constitutional_subordination
 *   human_readable: Constitutional Subordination of Provinces — Federal Veto over Exit (constitutional_subordination reading)
 *   domain: political economy/federalism/resource governance
 *
 * SUMMARY:
 *   The constitutional subordination reading of Canadian federalism:
 *   provinces hold no sovereignty prior to or independent of the Constitution
 *   Acts — they are creatures of the federal framework — and territorial exit
 *   requires federal consent; unilaterally, secession is constitutional
 *   nullity (Secession Reference 1998; Clarity Act 2000). This story
 *   instantiates ONE reading of the contested kernel
 *   provincial_sovereignty_boundary. The compact_federalism reading
 *   (provinces as sovereign compact parties retaining residual sovereignty)
 *   and the resource_sovereignty_primacy reading (s.92A resource ownership
 *   grounds absolute sovereignty) are separate constraints authored from
 *   other seats; they are not folded into this file, per the
 *   epsilon-invariance discipline. The epsilon referent is the standing
 *   subordination arrangement itself, assessed by this reading's own lights:
 *   the reading sees the arrangement as legitimate constitutional operation,
 *   so its authored epsilon is moderate — the flows and foreclosures are
 *   real, but this seat deems them constitutionally justified. The
 *   claim/metric independence rule holds: claimed_type tangled_rope is my
 *   structural judgment of the arrangement; the metrics are my descriptive
 *   judgment; the engine computes per-seat classifications from the
 *   structural data, and divergence between claim and computed type is
 *   signal, not error.
 *
 * KEY AGENTS:
 *   - federal_government: agenda-setter and chief beneficiary (institutional/arbitrage) — holds the amendment gate, enforces the exit veto, collects the arrangement's structural gains
 *   - have_not_provinces: beneficiary (organized/trapped) — equalization recipients whose fiscal models depend on the federal spending power this reading legitimizes
 *   - resource_rich_provinces: primary payer (powerful/constrained) — net contributors whose resource authority is bounded by federal climate policy; exit only through the arrangement's own negotiation procedure
 *   - quebec_sovereignty_movement: payer (organized/identity_locked) — the foreclosed exit project itself; two referendums, one lost by half a point
 *   - supreme_court_of_canada: enforcing interpreter (institutional/analytical) — polices the boundary; neither collects nor pays
 *   - indigenous_nations: excluded cost-bearer (organized/trapped) — prior sovereignty claims overridden by the constitution-sourcing logic; no seat at the constitutional table
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_sovereignty_boundary__constitutional_subordination, 0.5).
domain_priors:suppression_score(provincial_sovereignty_boundary__constitutional_subordination, 0.55).
domain_priors:theater_ratio(provincial_sovereignty_boundary__constitutional_subordination, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, extractiveness, 0.5).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__constitutional_subordination, tangled_rope).
narrative_ontology:human_readable(provincial_sovereignty_boundary__constitutional_subordination, "Constitutional Subordination of Provinces — Federal Veto over Exit (constitutional_subordination reading)").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__constitutional_subordination, "political economy/federalism/resource governance").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__constitutional_subordination).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__constitutional_subordination, 'a32e0f3e-7822-414e-8cfb-7aecb874dd99').
narrative_ontology:cs_kernel_codification('a32e0f3e-7822-414e-8cfb-7aecb874dd99', fixed_text).
narrative_ontology:cs_authority_grounding('a32e0f3e-7822-414e-8cfb-7aecb874dd99', extraction).
narrative_ontology:cs_interpretation_layer_present('a32e0f3e-7822-414e-8cfb-7aecb874dd99').
narrative_ontology:cs_reading_relation('a32e0f3e-7822-414e-8cfb-7aecb874dd99', provincial_sovereignty_boundary__compact_federalism, forecloses).
narrative_ontology:cs_reading_relation('a32e0f3e-7822-414e-8cfb-7aecb874dd99', provincial_sovereignty_boundary__resource_sovereignty_primacy, forecloses).
narrative_ontology:cs_axiom('a32e0f3e-7822-414e-8cfb-7aecb874dd99', foundational, provinces_are_constitutional_creatures).
narrative_ontology:cs_axiom_status(provinces_are_constitutional_creatures, holdable).
narrative_ontology:cs_axiom_grounding('a32e0f3e-7822-414e-8cfb-7aecb874dd99', provinces_are_constitutional_creatures, conventional).
narrative_ontology:cs_axiom('a32e0f3e-7822-414e-8cfb-7aecb874dd99', secondary, unilateral_secession_constitutionally_null).
narrative_ontology:cs_axiom_status(unilateral_secession_constitutionally_null, holdable).
narrative_ontology:cs_axiom_grounding('a32e0f3e-7822-414e-8cfb-7aecb874dd99', unilateral_secession_constitutionally_null, conventional).
narrative_ontology:cs_reference_frame('a32e0f3e-7822-414e-8cfb-7aecb874dd99', one_and_indivisible_federal_framework).
narrative_ontology:cs_drift_state('a32e0f3e-7822-414e-8cfb-7aecb874dd99', post_secession_reference_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a32e0f3e-7822-414e-8cfb-7aecb874dd99', '').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__constitutional_subordination, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, federal_government).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, have_not_provinces).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__constitutional_subordination, resource_rich_provinces).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__constitutional_subordination, quebec_sovereignty_movement).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__constitutional_subordination, indigenous_nations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, resource_rich_provinces).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__constitutional_subordination, federal_spending_power_doctrine).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__constitutional_subordination, pogg_national_concern_doctrine).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__constitutional_subordination, unilateral_secession_unconstitutionality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the amendment gate over any change to the provincial-federal boundary, appoints the judges who adjudicate it, and wrote the Clarity Act's terms for any secession negotiation. Collects the arrangement's structural gains: territorial integrity without provincial consent rights, fiscal leverage through conditional transfers, and policy authority in climate and environmental assessment. Exit from the arrangement is meaningless from its seat — it is the arrangement's author and residual sovereign, able to restructure its own instruments at will.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Receive equalization transfers that close the gap between their fiscal capacity and the national average; several operate on annual budgets that depend on the flows. The subordination reading legitimizes the federal spending power that sustains them. Leaving the federation would mean forfeiting the transfers their fiscal models rest on, so exit is not a live option from where they stand regardless of its legal status.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, have_not_provinces, beneficiary,
    organized, biographical, trapped, regional).

% Net fiscal contributors whose resource revenues are drawn into the federal tax base that funds equalization, and whose resource development is now bounded by federal climate policy upheld as a matter of national concern. They also draw real benefit from the common market, pooled debt, and defense the union provides. Their only exit path runs through the negotiation procedure the arrangement itself defines — unilateral departure is foreclosed, so leverage must be exercised politically from inside, through provincial statutes, referenda, and litigation.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, resource_rich_provinces, payer,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__constitutional_subordination, resource_rich_provinces, beneficiary).

% A mass movement spanning parties and generations whose project — provincial exit — is the precise act the arrangement forecloses. It has twice taken the question to a referendum (1980, 1995, the latter losing by half a point) and forced the reference case that now defines the terms of any exit. The movement cannot abandon the exit project without ceasing to be itself; it contests from inside a framework that holds its goal constitutionally null without federal consent.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, quebec_sovereignty_movement, payer,
    organized, generational, identity_locked, regional).

% Adjudicates the boundary through reference jurisdiction: it authored the Secession Reference's terms (unilateral exit unconstitutional; negotiation obligatory on a clear vote and clear question) and struck down the federal Impact Assessment Act's designated-projects scheme as beyond federal competence. It neither collects the arrangement's gains nor bears its costs; its own authority is constituted by the framework it polices, and its interpretive moves absorb drift without formal amendment.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, supreme_court_of_canada, agenda_setter,
    institutional, generational, analytical, national).

% Hold prior occupancy and treaty relationships across the territory both levels of government claim to govern. Section 35 recognizes Aboriginal and treaty rights, but the sovereignty contest proceeds between federal and provincial governments with no Indigenous seat at the constitutional table, and the constitution-sourcing logic locates all governing authority in 1867/1982 rather than in prior Indigenous sovereignty. They cannot exit the framework that overrides their governance authority, and much of the landmass was never ceded by treaty.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, indigenous_nations, excluded,
    organized, generational, trapped, continental).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__constitutional_subordination, indigenous_nations, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(provincial_sovereignty_boundary__constitutional_subordination, federal_government).
narrative_ontology:fixing_cost_class(provincial_sovereignty_boundary__constitutional_subordination, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Holds together a continental federation of regionally asymmetric provinces: a single currency and common market, pooled defense and debt, interprovincial redistribution that stabilizes the union, and a supreme adjudicator for cross-boundary disputes. Prevents destructive unilateral fragmentation and preserves a common economic space across regions of very unequal fiscal capacity.
% TRANSFER_FUNCTION: Moves fiscal capacity from high-capacity provinces through the federal tax base to lower-capacity provinces (equalization); moves policy authority over climate and environmental assessment from provinces to the federation; and moves the exit decision itself from provinces to the federal gate — territory cannot leave without federal consent.
% ABSENT_VOICES: Indigenous nations would object that the arrangement sources all governing authority in the 1867/1982 constitution while much of the territory was never ceded — the sovereignty conversation excludes the peoples with the longest-standing claim to it. Municipal governments, creatures of provinces under the same logic, have no seat either. Both are absent from the table where the boundary is set, and the arrangement's cost structure falls partly on them.
% DISAPPEARANCE_RATIONALE: Overnight removal would leave provinces free to exit unilaterally: the Quebec sovereignty question reopens immediately, resource provinces would contest federal climate authority with no adjudicated boundary, and the equalization architecture loses its constitutional footing — the union would face fragmentation pressure within a single political cycle.
% FOUNDING_PROBLEM: The 1860s problem of uniting fragmented British North American colonies into a polity able to finance a continental railway, defend itself, and avoid absorption into the United States — solved by designing a strong central government with residual powers and provinces as subordinate jurisdictions rather than sovereign compact parties.
% FOUNDING_PROBLEM_CORROBORATION: The specific founding problems (colonial defense, railway finance) are dead — any post-1945 political history attests that from outside the benefiting parties. Constitutional historians working from the London Resolutions and Macdonald's correspondence attest the centralist founding design this reading claims. But Quebec's dissenting tradition, documented from the 1865 Confederation debates onward, attests that the founding terms were contested as a compact from the start, and Indigenous legal scholarship attests the founding excluded prior sovereignty entirely. No outside source attests this reading's framing of the problem as settled; the status is contested, not dead and not cleanly live.
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__constitutional_subordination, world_rearranges).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__constitutional_subordination, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__constitutional_subordination, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(provincial_sovereignty_boundary__constitutional_subordination, 'none', 1).
narrative_ontology:epsilon_provenance(provincial_sovereignty_boundary__constitutional_subordination, 0.5, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(provincial_sovereignty_boundary__constitutional_subordination_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(provincial_sovereignty_boundary__constitutional_subordination, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(provincial_sovereignty_boundary__constitutional_subordination_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.50 at interval end, reading-indexed): equalization moves tens of billions annually through the federal tax base, federal climate policy bounds resource development worth billions, and the exit option is foreclosed — but this reading assesses those flows as legitimate constitutional operation, and the Secession Reference's negotiation obligation keeps exit procedurally open. Suppression (0.55) is structural rather than violent: the veto operates through the amending formula, judicial enforcement, and the Clarity Act, while referendums and provincial sovereignty statutes remain legal. Theater is low-moderate (0.25): the machinery genuinely adjudicates and transfers; the performative share is symbolic union maintenance and the Clarity Act's question-certification ritual. Accessibility collapse is 0.5: the unilateral-exit alternative collapses almost completely, but renegotiation, asymmetric federalism, and judicial contest remain live. Resistance is 0.65: sustained, institutionalized, and cyclical. The measurement series documents an accommodation-assertion cycle rather than monotonic drift: crisis (1995 referendum) -> formalization (1998 Secession Reference, 2000 Clarity Act) -> accommodation (open federalism circa 2007) -> renewed assertion (carbon pricing upheld 2021; provincial sovereignty statutes) -> judicial recalibration (Impact Assessment Act struck down in part, 2023). The oscillation is partly the arrangement's hold mechanism itself: intermittent concession resets resistance while the underlying foreclosure never moves. All three tracked metrics share one time grid. Suppression_requirement is tracked because enforcement capacity genuinely changed over the interval (Clarity Act hardening, climate backstop enforcement, then partial judicial constraint) — not merely extraction.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats and the payer seats compute different types from the same structure. From the federal government's seat the arrangement is the coordination it administers: union maintenance, solidarity transfers, national policy capacity. From the resource provinces' seat the same structure is a fiscal drain and a policy ceiling with the exit door welded shut. From the sovereignty movement's seat it is the nullification of a democratic project that twice came to a vote. The have-not provinces' seat sees the subsidy side of the very flows the resource provinces see as extraction. The engine computes this divergence from power, exit options, and declared position; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: federal_government and have_not_provinces sit near the beneficiary end (low d — the veto and the transfers subsidize them); resource_rich_provinces and quebec_sovereignty_movement sit near the target end (high d — they pay the transfers and bear the foreclosure, the movement compounded by identity lock); indigenous_nations derive high d as victims whose governance authority the constitution-sourcing logic overrides, compounded by trapped exit and continental scope. The Supreme Court is deliberately left to the canonical fallback: it is declared neither beneficiary nor victim because it collects no rents and bears no costs — its authority is constituted by the framework it polices, a near-symmetric enforcement seat. No directionality overrides are used: the derivation from declared structure captures every seat, and an override keyed to the institutional power atom would misplace the federal government alongside the Court.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards both failure modes. Reading the arrangement as pure extraction would erase the genuine coordination function — a continental common market, pooled defense and debt, redistribution no province could replicate alone — and would misread the Secession Reference's negotiation obligation as mere cover. Reading it as pure coordination would erase the asymmetry: the same structure that stabilizes the union forecloses the sovereignty movement's project, draws resource revenues into a federal base their earners do not control, and overrides Indigenous governance authority without a seat at the table. Tangled rope holds both halves. On mandate: the 1867 founding mandate (colonial defense, railway finance) is dead, but the arrangement's operative mandate — holding a regionally asymmetric federation together — is live; the constraint transformed its function rather than outliving it, so no mandatrophy resolution is declared. The R5 fields record the founding problem as contested rather than dead: the mismatch consumer should find no dead-mandate-plus-world_rearranges flag here, and that absence is itself the honest measurement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This story instantiates the constitutional_subordination reading of the contested kernel provincial_sovereignty_boundary. Would the sibling readings reclassify the same standing arrangement — compact_federalism (Confederation as compact among sovereign provinces; residual sovereignty; exit negotiable under duress) computes the equalization flows and the exit veto as extraction from sovereign parties, and resource_sovereignty_primacy (s.92A resource ownership grounds absolute sovereignty; resource control = territorial sovereignty) computes federal climate policy over provincial resources as expropriation? The disagreement is located in a single structural element: the source of provincial authority — constitutional grant versus founding compact versus resource ownership.',
    'Author sibling stories from each reading''s seat over the identical referent (the standing subordination arrangement), each with its own epsilon, victim set, and claimed type; compare the three computed classifications. The divergence across readings is the measurement; no observable flow can adjudicate it because the readings dispute what the flows ARE.',
    'This story''s epsilon (0.50) is reading-indexed: the constitutional_subordination seat assesses the flows as legitimate constitutional operation and authors moderate extraction. The same referent assessed from the compact_federalism seat would author epsilon near the snare range; from the resource_sovereignty_primacy seat, high with a different victim set. Classification of the arrangement is therefore kernel-contested by construction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one of three readings of the provincial sovereignty kernel; epsilon is a property of this reading, not of the topic.').

omega_variable(
    union_necessity_contingency,
    'Is the federal veto over provincial exit a structural necessity of governing a continental, regionally asymmetric economy, or a contingent constitutional choice that alternative federal designs (confederal, treaty-based, secession-permissive) could replace?',
    'Comparative federal analysis: stability and redistribution records of exit-locked federations versus exit-permissive unions (devolutionary arrangements, treaty systems with withdrawal clauses), controlling for regional asymmetry.',
    'If necessity, part of the measured suppression is the price of the coordination itself and the rope-side reading strengthens; if contingent, the veto is a constructed closure and the extraction-side reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(union_necessity_contingency, conceptual, 'Whether the exit veto is coordination cost or constructed foreclosure.').

omega_variable(
    indigenous_sovereignty_exclusion,
    'Does the arrangement''s cost-bearing structure extend to Indigenous nations — whose prior sovereignty the constitution-sourcing logic locates nowhere, on territory much of which was never ceded — and how much of the measured extraction is borne by seats absent from the constitutional table where the boundary is contested?',
    'Section 35 jurisprudence trajectory and treaty-federalism litigation: if courts come to recognize prior Indigenous sovereignty as limiting both federal and provincial authority, the victim set widens and epsilon rises; the current story measures only seats present in the contest.',
    'Recognition would reclassify the arrangement''s victim structure and raise effective extraction beyond what this reading''s own lights can see; non-recognition leaves the current victim set as measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_sovereignty_exclusion, empirical, 'Extraction borne by a structurally excluded seat, invisible to the contest''s participants.').

omega_variable(
    exit_foreclosure_depth,
    'How deep is the exit foreclosure in fact: absolute (the federal veto means no exit under any circumstances) or procedural (a clear majority on a clear question obliges negotiation, so exit remains possible through federal consent)?',
    'An actual post-clear-vote secession negotiation — the trigger the Clarity Act defines. The 1995 referendum (50.6% No) approached it; only a negotiation attempt would reveal whether consent is withholdable at will or genuinely obliged.',
    'Procedural foreclosure lowers the effective suppression and supports the coordination reading; absolute foreclosure raises suppression and supports the extraction reading. The Secession Reference''s text supports the procedural reading; federal behavior under duress is untested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_foreclosure_depth, empirical, 'Whether the veto is absolute or a negotiation gate.').

omega_variable(
    provincial_compliance_mechanism,
    'Is provincial acquiescence in the subordination arrangement maintained by structural enforcement (the amending formula, fiscal dependency, judicial enforcement) or by internalized identity (provincial political cultures — Quebec sovereignty identity, western grievance identity — that reproduce the contest as constitutive of the province''s self-conception)?',
    'Post-accommodation trajectory: after each concession episode (open federalism circa 2007, fiscal-balance adjustments), does contest intensity resume at prior levels within a political cycle (identity-reproduced) or decay durably (structurally maintained)?',
    'If internalized, suppression persists through accommodation cycles and the oscillating measurement series is itself part of the arrangement''s hold — intermittent concession reproduces consent while the underlying foreclosure never moves; if structural, suppression tracks enforcement capacity and accommodation should durably lower it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(provincial_compliance_mechanism, empirical, 'Structural versus internalized maintenance of provincial compliance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__constitutional_subordination, 1982, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prov_tr_t1982, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 1982, 0.2).
narrative_ontology:measurement(prov_tr_t1990, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 1990, 0.28).
narrative_ontology:measurement(prov_tr_t1995, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 1995, 0.24).
narrative_ontology:measurement(prov_tr_t1998, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 1998, 0.22).
narrative_ontology:measurement(prov_tr_t2007, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 2007, 0.26).
narrative_ontology:measurement(prov_tr_t2015, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 2015, 0.28).
narrative_ontology:measurement(prov_tr_t2021, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 2021, 0.3).
narrative_ontology:measurement(prov_tr_t2025, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 2025, 0.25).

% Extraction over time
narrative_ontology:measurement(prov_be_t1982, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 1982, 0.38).
narrative_ontology:measurement(prov_be_t1990, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 1990, 0.43).
narrative_ontology:measurement(prov_be_t1995, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 1995, 0.49).
narrative_ontology:measurement(prov_be_t1998, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 1998, 0.52).
narrative_ontology:measurement(prov_be_t2007, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 2007, 0.47).
narrative_ontology:measurement(prov_be_t2015, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 2015, 0.49).
narrative_ontology:measurement(prov_be_t2021, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 2021, 0.53).
narrative_ontology:measurement(prov_be_t2025, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 2025, 0.5).

% Suppression requirement over time
narrative_ontology:measurement(prov_su_t1982, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 1982, 0.45).
narrative_ontology:measurement(prov_su_t1990, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 1990, 0.48).
narrative_ontology:measurement(prov_su_t1995, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 1995, 0.55).
narrative_ontology:measurement(prov_su_t1998, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 1998, 0.58).
narrative_ontology:measurement(prov_su_t2007, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 2007, 0.52).
narrative_ontology:measurement(prov_su_t2015, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 2015, 0.54).
narrative_ontology:measurement(prov_su_t2021, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 2021, 0.6).
narrative_ontology:measurement(prov_su_t2025, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_sovereignty_boundary__constitutional_subordination, enforcement_mechanism).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, provincial_sovereignty_boundary__compact_federalism).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, provincial_sovereignty_boundary__resource_sovereignty_primacy).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, equalization_fiscal_transfer_regime).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, federal_carbon_pricing_backstop).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, clarity_act_secession_gatekeeping).

% DUAL FORMULATION NOTE:
% The colloquial label 'provincial sovereignty' decomposes into three structurally distinct constraints — one per reading of the provincial_sovereignty_boundary kernel — each with its own epsilon, victim set, and classification, per the epsilon-invariance principle. This story is the constitutional_subordination member. The constitutional text is upstream in empirical confidence and is cited as evidence by all three readings for incompatible conclusions; the sibling stories should link back via affects_constraints so the family is closed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
