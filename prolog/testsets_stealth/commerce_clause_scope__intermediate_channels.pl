% ============================================================================
% CONSTRAINT STORY: commerce_clause_scope__intermediate_channels
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_scope__intermediate_channels, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: commerce_clause_scope__intermediate_channels
 *   human_readable: Commerce Clause Scope — Intermediate Channels Reading (Tripartite Framework with Limiting Principles)
 *   domain: constitutional/legal/political
 *
 * SUMMARY:
 *   The intermediate_channels reading holds that federal commerce power
 *   extends to three things — the channels of interstate commerce,
 *   instrumentalities and persons/things in interstate commerce, and
 *   activities substantially affecting interstate commerce — while insisting
 *   the third category is fenced by categorical limiting principles:
 *   non-economic local conduct is beyond reach absent a jurisdictional
 *   element, aggregation counts only economic activity, and Congress may not
 *   regulate through attenuated causal chains. The arrangement coordinates
 *   genuinely (a continental economy needs national commercial governance,
 *   and the categories tell Congress which problems are national) and
 *   extracts asymmetrically through the same structure (comprehensive federal
 *   schemes reach purely local producers via cumulative-effect accounting,
 *   and regulatory jurisdiction migrates from states to Washington). The
 *   claim/metric gap is deliberate: the reading CLAIMS a bounded, principled
 *   allocation while the authored metrics describe a framework whose limits
 *   are increasingly maintained by recital rather than adjudication — the
 *   engine measures that divergence; do not reconcile the claim to the
 *   metrics. Temporal note: the interval runs from the school-zone decision
 *   (t0) to the present (t30), tracing the limiting principles from fresh
 *   enforcement through aggregation-revival to settled rhetorical status.
 *
 * KEY AGENTS:
 *   - - federal_government: Agenda-setter and principal collector (institutional/constrained) — enacts and defends comprehensive schemes; gains jurisdiction with each surviving nexus recital
 *   - - supreme_court: Enforcement organ (institutional/constrained) — draws and polices the categorical lines; owns the boundary, cannot decline it
 *   - - state_governments: Dual-positioned (organized/constrained) — beneficiaries of the reserved spheres (family law, criminal law, education), payers of preemption in the economic sphere
 *   - - intrastate_economic_actors: Primary target (moderate/trapped) — local producers reached by cumulative-effect accounting; market withdrawal does not remove them from the tally
 *   - - local_noncommercial_conduct_subjects: Target at the margin (powerless/trapped) — non-commercial conduct exposed where a jurisdictional allegation is pleaded around the categorical exclusion
 *   - - national_market_participants: Beneficiary with leverage (powerful/arbitrage) — uniform federal rules lower national operating costs; forum-shop between the two levels
 *   - - local_criminal_justice_systems: Excluded voice (organized/trapped) — caseload reallocated to federal dockets without a seat in the doctrinal conversation
 *   - - legal_academy: Analytical observer (moderate/analytical) — maps where the categories absorb conduct and keeps the doctrinal ledger
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_scope__intermediate_channels, 0.55).
domain_priors:suppression_score(commerce_clause_scope__intermediate_channels, 0.45).
domain_priors:theater_ratio(commerce_clause_scope__intermediate_channels, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, extractiveness, 0.55).
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__intermediate_channels, tangled_rope).
narrative_ontology:human_readable(commerce_clause_scope__intermediate_channels, "Commerce Clause Scope — Intermediate Channels Reading (Tripartite Framework with Limiting Principles)").
narrative_ontology:topic_domain(commerce_clause_scope__intermediate_channels, "constitutional/legal/political").

domain_priors:requires_active_enforcement(commerce_clause_scope__intermediate_channels).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__intermediate_channels, 'ef4b9448-2b82-47b2-805f-ebe0781cfb6e').
narrative_ontology:cs_kernel_codification('ef4b9448-2b82-47b2-805f-ebe0781cfb6e', fixed_text).
narrative_ontology:cs_authority_grounding('ef4b9448-2b82-47b2-805f-ebe0781cfb6e', lineage).
narrative_ontology:cs_interpretation_layer_present('ef4b9448-2b82-47b2-805f-ebe0781cfb6e').
narrative_ontology:cs_reading_relation('ef4b9448-2b82-47b2-805f-ebe0781cfb6e', commerce_clause_scope__narrow_originalist, forecloses).
narrative_ontology:cs_reading_relation('ef4b9448-2b82-47b2-805f-ebe0781cfb6e', commerce_clause_scope__broad_effects_test, coexists_with).
narrative_ontology:cs_axiom('ef4b9448-2b82-47b2-805f-ebe0781cfb6e', foundational, categorical_limits_check_federal_power).
narrative_ontology:cs_axiom_status(categorical_limits_check_federal_power, holdable).
narrative_ontology:cs_axiom_grounding('ef4b9448-2b82-47b2-805f-ebe0781cfb6e', categorical_limits_check_federal_power, conventional).
narrative_ontology:cs_axiom('ef4b9448-2b82-47b2-805f-ebe0781cfb6e', foundational, economic_activity_is_aggregable_noneconomic_is_not).
narrative_ontology:cs_axiom_status(economic_activity_is_aggregable_noneconomic_is_not, holdable).
narrative_ontology:cs_axiom_grounding('ef4b9448-2b82-47b2-805f-ebe0781cfb6e', economic_activity_is_aggregable_noneconomic_is_not, empirically_contingent).
narrative_ontology:cs_reference_frame('ef4b9448-2b82-47b2-805f-ebe0781cfb6e', channels_instrumentalities_effects_with_categorical_limits).
narrative_ontology:cs_drift_state('ef4b9448-2b82-47b2-805f-ebe0781cfb6e', contemporary_post_aggregation_revival, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('ef4b9448-2b82-47b2-805f-ebe0781cfb6e', '').
narrative_ontology:cs_kernel_id(commerce_clause_scope__intermediate_channels, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__intermediate_channels, federal_government).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__intermediate_channels, state_governments).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__intermediate_channels, national_market_participants).
narrative_ontology:constraint_victim(commerce_clause_scope__intermediate_channels, intrastate_economic_actors).
narrative_ontology:constraint_victim(commerce_clause_scope__intermediate_channels, local_noncommercial_conduct_subjects).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(commerce_clause_scope__intermediate_channels, state_governments).
narrative_ontology:constraint_vindicates(commerce_clause_scope__intermediate_channels, enumerated_powers_doctrine).
narrative_ontology:constraint_vindicates(commerce_clause_scope__intermediate_channels, dual_sovereignty_federalism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts statutes under the commerce power — comprehensive schemes governing agriculture, drugs, workplace conditions, public-accommodation access, environmental discharge — and defends their reach in court. Gains prosecutorial and regulatory jurisdiction each time a statute's nexus language survives review. Cannot relinquish the power unilaterally; when an adverse ruling closes one route, it reauthorizes through taxing and spending, as with the individual-mandate litigation.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, federal_government, agenda_setter,
    institutional, generational, constrained, national).

% Decides which federal statutes exceed the commerce power: it has struck down a school-zone gun statute and a civil remedy for gender-motivated violence, and upheld comprehensive drug regulation reaching cannabis grown for personal home use. Owns the line-drawing; cannot decline the cases, cannot delegate the boundary, and revisits it whenever Congress drafts around the last ruling.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, supreme_court, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_scope__intermediate_channels, supreme_court, observer).

% Retain primary authority over family law, criminal law, education, and land use so long as the categorical limits hold, and litigate to defend that reservation. Simultaneously watch economic and environmental regulation migrate to Washington, ending up administering federal programs they did not design and seeing their own economic rules preempted. Secession and nullification are off the table; their tools are suits, interstate compacts, and bargaining inside cooperative-federalism arrangements.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, state_governments, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(commerce_clause_scope__intermediate_channels, state_governments, payer).

% Farmers growing for home consumption, small producers selling to local customers, patients growing medicine for personal use — their conduct is local and modest in scale, but once classified as economic it counts toward cumulative national totals and places them inside federal regulatory schemes. Leaving the market does not remove them from the tally; the accounting method itself reaches the withdrawal.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, intrastate_economic_actors, payer,
    moderate, biographical, trapped, local).

% Individuals whose conduct — possessing a firearm near a school, acts of interpersonal violence, home possession offenses — is not commerce, yet draws federal charge when a statutory allegation ties the incident to some object or person that once crossed a state line. Their protection is the categorical exclusion of non-economic conduct; their exposure is how easily a jurisdictional allegation can be pleaded around it.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, local_noncommercial_conduct_subjects, payer,
    powerless, biographical, trapped, local).

% Firms operating across state lines get one set of federal commercial rules instead of fifty, cutting the cost of national operations. They bear the compliance burdens of those same federal schemes, and they move between the levels as advantage shifts — pressing for preemption where state rules bind tighter, invoking state authority where federal rules bind tighter.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, national_market_participants, beneficiary,
    powerful, biographical, arbitrage, global).

% State prosecutors, courts, and defenders lose cases to federal dockets when federal prosecutors take gun, drug, and violence charges carrying heavier sentences. They hold no seat in the doctrinal conversation that reallocates their caseload; they learn the new boundary from charging decisions handed down in their own courthouses' shadow.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, local_criminal_justice_systems, excluded,
    organized, biographical, trapped, regional).

% Scholars and commentators map the doctrine's movements, document where the limiting principles bite and where they dissolve into recital, and supply the vocabulary both sides carry into litigation. They decide nothing, but they keep the ledger of what the categories have absorbed.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, legal_academy, observer,
    moderate, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_scope__intermediate_channels, federal_government).
narrative_ontology:fixing_cost_class(commerce_clause_scope__intermediate_channels, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates regulatory authority between the national government and the states for a continental economy: the three categories tell Congress which problems are national in character, and the limiting principles reserve family law, criminal law, education, and land use to state institutions. Solves the collective-action failure of state-by-state commercial governance — conflicting tariffs, inconsistent rules, barriers against neighboring states.
% TRANSFER_FUNCTION: Moves regulatory jurisdiction — and with it enforcement power, criminal-liability exposure, and compliance burdens — from state institutions to federal institutions for economic conduct, while formally reserving non-economic local conduct to the states. Litigation costs of contesting the boundary fall on the parties whose conduct is classified.
% ABSENT_VOICES: Local criminal justice systems would object that their caseload and sentencing authority are reallocated without representation; state legislatures whose economic regulations are preempted participate only through amicus briefs and invited testimony; defendants learn the boundary's new position from indictments rather than from any process in which they had a voice before it moved.
% DISAPPEARANCE_RATIONALE: If the framework vanished overnight, the constitutional validity of thousands of federal statutes — agricultural schemes, drug control, labor standards, environmental regulation — becomes uncertain at once; national market governance fragments into case-by-case relitigation; states would reclaim some spheres unevenly while Congress reroutes through taxing and spending; decades of settled enforcement expectations unwind.
% FOUNDING_PROBLEM: Under the Articles of Confederation, the national legislature could not govern commerce among the states: each state erected tariffs and navigation rules against its neighbors, the economy fragmented, and the 1787 Convention was called substantially to repair this. The commerce power — and the recurring question of where it stops — is the standing solution to that founding failure, redrawn each time the economy integrates a new domain.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: the ratification-era record (Federalist No. 42 attributing the Articles' failures to the want of a commercial power; contemporaneous documentation of state tariff wars in the 1780s congressional journals) and modern economic histories of the Confederation period written without stake in current doctrine. State governments — beneficiaries of the limiting principles — independently attest the founding problem was real, since they were the barrier-erectors the power was built to disarm.
narrative_ontology:disappearance_verdict(commerce_clause_scope__intermediate_channels, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_scope__intermediate_channels, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_scope__intermediate_channels, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(commerce_clause_scope__intermediate_channels, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_scope__intermediate_channels, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_scope__intermediate_channels_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_scope__intermediate_channels, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_scope__intermediate_channels_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.55 (medium, per the manifest's expected delta): the economic sphere is extensively federalized and cumulative-effect accounting reaches deliberately local producers, but the categorical exclusions do carve out real protected zones. Suppression is 0.45 — structural, unscaled: states and individuals subject to federal supremacy have no exit from it, but alternatives (state administration, compacts, cooperative federalism) persist and the framework leaves genuine regulatory space. Accessibility collapse is low-moderate (0.40) because the alternatives do not vanish on understanding the framework; resistance is moderate-high (0.60) reflecting continuous litigation by states, defendants, and challengers. Theater ratio 0.38: the limits occasionally strike statutes (real work) but increasingly survive as recitals. The temporal series share one grid ({0,6,12,18,24,30}) so every metric is authored at every examined point. The series tell one story: extractiveness rises to a peak when aggregation revives (t12) then plateaus; theater climbs as jurisdictional elements turn into drafting conventions; and suppression_requirement FALLS steadily (0.62 to 0.34) — active judicial enforcement of the limits decays as compliance normalizes, while structural suppression stays flat. That divergence is the point: a framework whose limits are enforced less and less but whose power persists unchanged is drifting toward maintenance-by-inertia of the limiting apparatus specifically, even as the underlying power remains fully functional.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the federal seat the framework is a managed allocation it both exercises and submits to (it loses in court often enough to feel checked). From the intrastate producer's seat the same framework is an accounting method that converts local modesty into federal reach — exit-proof because withdrawal is itself the counted effect. From the states' seat it is double-edged: a wall protecting family and criminal law, and a door through which economic regulation departs. From the excluded local justice systems' seat the boundary is drawn entirely over their heads, announced through charging decisions. The engine computes these per-seat types from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. federal_government: agenda-setter and named beneficiary — jurisdictional gains accrue demonstrably to this seat (d near the beneficiary end). state_governments: declared beneficiary, but the derivation from beneficiary-plus-constrained-exit would push d too low; they carry preemption of their own economic rules and administer federal programs they did not design, so an override sets d to 0.35 (net beneficiary, materially burdened). national_market_participants: declared beneficiary with arbitrage-grade exit, which would drive d toward 0.0; but they bear the compliance costs of the schemes they benefit from and lobby both directions, so an override sets d to 0.2. intrastate_economic_actors and local_noncommercial_conduct_subjects: declared victims with trapped exit — d sits near the full-target end; the second group's victimhood is partial (the categorical exclusion shields most of their conduct) but where caught they bear the full weight, and the derivation handles this through the victim declaration rather than an override. supreme_court and legal_academy collect no rents and bear no transfer; they take the analytical/administrative seats the fallback supplies.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — national commercial governance after the Articles' fragmentation — is live, so this is not a resolved mandate kept alive by habit. The tangled classification prevents mislabeling in both directions: calling this a pure coordination device erases the cumulative-effect victims and the jurisdictional migration the measurements record; calling it pure extraction erases the real coordination (uniform commercial rules, the states' genuinely protected spheres) and the fact that the limits sometimes strike statutes. The drift the temporal series traces is subtler than mandatrophy: the POWER remains functional while the LIMITS decay toward performance — a partial, asymmetric atrophy inside a live framework. If the limiting principles finish collapsing into recital, the honest successor story is the broad_effects_test sibling, not a piton: the framework will not be dead, only unchecked.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    one_reading_of_commerce_kernel,
    'This constraint instantiates only the intermediate_channels reading of the commerce_clause_scope kernel (''Congress shall have power to regulate commerce among the several states''). How would the sibling readings — narrow_originalist and broad_effects_test — change the structural data if instantiated as their own stories?',
    'Generate the sibling stories and compare victim sets, epsilon, and per-seat classifications: narrow_originalist shrinks federal reach to trade crossing state lines (epsilon collapses toward zero for the federal seat; states become near-full beneficiaries); broad_effects_test deletes the categorical limits (aggregation reaches all conduct with cumulative impact; the victim set expands to all local conduct and federal extractiveness rises).',
    'The classification computed here is valid only for this reading. A corpus that averaged across readings would manufacture a constraint with no stable epsilon; the kernel must decompose into three linked stories.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(one_reading_of_commerce_kernel, conceptual, 'Committer structure: kernel membership, reading identity, and sibling structural deltas.').

omega_variable(
    economic_noneconomic_line_stability,
    'Is the economic/non-economic distinction stable enough to bear the weight of the limiting principles (including the ban on regulation via attenuated causal chains), or is it inherently manipulable — classifying gun possession near a school as non-economic, homegrown cannabis as economic, depending on the outcome sought?',
    'Track classification outcomes across novel regulatory domains (cybercrime, health-insurance mandates, environmental discharge) and test whether the economic label tracks the character of the conduct or the policy preference of the majority; if the latter, the line is performative.',
    'If the line is unstable, the limiting principles are theater rather than limits, the reading collapses toward broad_effects_test in operation, and the effective victim set expands to all local conduct. This omega carries the manifest''s ''conceptual coherence'' harm in actor terms: litigants bear the cost of unpredictable classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_noneconomic_line_stability, empirical, 'Whether the doctrinal line doing the limiting work is real or outcome-driven.').

omega_variable(
    aggregation_outer_boundary,
    'Does aggregation of cumulative effects have an outer boundary, or does the Wickard-to-Raich trajectory (home-consumed wheat, then homegrown medicine) extend until every intrastate activity counts toward a national total?',
    'Identify a class of intrastate economic conduct the Court refuses to aggregate, or establish across successive cases that no such class survives.',
    'With no outer boundary, the third category swallows the limiting principles from inside: the reading''s distinguishing axiom (aggregation for economic activity only, bounded) fails and the operative constraint becomes the broad_effects_test sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aggregation_outer_boundary, empirical, 'Whether cumulative-effect reasoning is bounded or totalizing.').

omega_variable(
    jurisdictional_element_substance,
    'Do the jurisdictional elements required after the school-zone and violence-against-women decisions impose genuine proof burdens, or do they function as boilerplate recitals (''in or affecting interstate commerce'') that prosecutors plead and courts never test?',
    'Sample post-1995 federal statutes and indictments; measure how often the jurisdictional element is contested, what quantum of evidence satisfies it, and whether any charge fails for want of a genuine nexus.',
    'If elements are boilerplate, the nominal limit is satisfied by drafting convention rather than adjudication — the theater component of the measured ratio understates nothing and the Lopez-style protection of local conduct is nominal only.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(jurisdictional_element_substance, empirical, 'Whether the jurisdictional-element requirement is substantive or ceremonial.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__intermediate_channels, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, commerce_clause_scope__intermediate_channels, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(comm_tr_t0, observed).
narrative_ontology:measurement(comm_tr_t6, commerce_clause_scope__intermediate_channels, theater_ratio, 6, 0.26).
narrative_ontology:measurement_basis(comm_tr_t6, observed).
narrative_ontology:measurement(comm_tr_t12, commerce_clause_scope__intermediate_channels, theater_ratio, 12, 0.38).
narrative_ontology:measurement_basis(comm_tr_t12, observed).
narrative_ontology:measurement(comm_tr_t18, commerce_clause_scope__intermediate_channels, theater_ratio, 18, 0.42).
narrative_ontology:measurement_basis(comm_tr_t18, observed).
narrative_ontology:measurement(comm_tr_t24, commerce_clause_scope__intermediate_channels, theater_ratio, 24, 0.41).
narrative_ontology:measurement_basis(comm_tr_t24, observed).
narrative_ontology:measurement(comm_tr_t30, commerce_clause_scope__intermediate_channels, theater_ratio, 30, 0.38).
narrative_ontology:measurement_basis(comm_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, commerce_clause_scope__intermediate_channels, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(comm_be_t0, observed).
narrative_ontology:measurement(comm_be_t6, commerce_clause_scope__intermediate_channels, base_extractiveness, 6, 0.52).
narrative_ontology:measurement_basis(comm_be_t6, observed).
narrative_ontology:measurement(comm_be_t12, commerce_clause_scope__intermediate_channels, base_extractiveness, 12, 0.58).
narrative_ontology:measurement_basis(comm_be_t12, observed).
narrative_ontology:measurement(comm_be_t18, commerce_clause_scope__intermediate_channels, base_extractiveness, 18, 0.56).
narrative_ontology:measurement_basis(comm_be_t18, observed).
narrative_ontology:measurement(comm_be_t24, commerce_clause_scope__intermediate_channels, base_extractiveness, 24, 0.55).
narrative_ontology:measurement_basis(comm_be_t24, observed).
narrative_ontology:measurement(comm_be_t30, commerce_clause_scope__intermediate_channels, base_extractiveness, 30, 0.55).
narrative_ontology:measurement_basis(comm_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, commerce_clause_scope__intermediate_channels, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(comm_su_t0, observed).
narrative_ontology:measurement(comm_su_t6, commerce_clause_scope__intermediate_channels, suppression_requirement, 6, 0.55).
narrative_ontology:measurement_basis(comm_su_t6, observed).
narrative_ontology:measurement(comm_su_t12, commerce_clause_scope__intermediate_channels, suppression_requirement, 12, 0.46).
narrative_ontology:measurement_basis(comm_su_t12, observed).
narrative_ontology:measurement(comm_su_t18, commerce_clause_scope__intermediate_channels, suppression_requirement, 18, 0.4).
narrative_ontology:measurement_basis(comm_su_t18, observed).
narrative_ontology:measurement(comm_su_t24, commerce_clause_scope__intermediate_channels, suppression_requirement, 24, 0.36).
narrative_ontology:measurement_basis(comm_su_t24, observed).
narrative_ontology:measurement(comm_su_t30, commerce_clause_scope__intermediate_channels, suppression_requirement, 30, 0.34).
narrative_ontology:measurement_basis(comm_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_scope__intermediate_channels, enforcement_mechanism).
narrative_ontology:affects_constraint(commerce_clause_scope__intermediate_channels, commerce_clause_scope__narrow_originalist).
narrative_ontology:affects_constraint(commerce_clause_scope__intermediate_channels, commerce_clause_scope__broad_effects_test).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'the commerce clause scope' covers three structurally distinct claims with different epsilon values, victim sets, and empirical status. The intermediate_channels reading (this file) is the middle position — bounded effects jurisdiction with categorical limits. The narrow_originalist sibling (upstream in textual warrant, downstream in doctrinal influence since the founding era) and the broad_effects_test sibling (downstream in reach, ascendant in operational practice since the aggregation revival) are separate stories. Each member links the others via affects_constraints; averaging across the family would fabricate a constraint with no stable epsilon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(commerce_clause_scope__intermediate_channels, organized, 0.35).
constraint_indexing:directionality_override(commerce_clause_scope__intermediate_channels, powerful, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
